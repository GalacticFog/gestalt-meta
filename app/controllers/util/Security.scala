package controllers.util


import java.util.UUID

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.GestaltResourceInput
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltAccountCreateWithRights
import com.galacticfog.gestalt.security.api.GestaltGroup
import com.galacticfog.gestalt.security.api.GestaltGroupCreateWithRights
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.GestaltOrgCreate
import com.galacticfog.gestalt.security.api.GestaltOrgSync
import com.galacticfog.gestalt.security.api.GestaltPasswordCredential
import com.galacticfog.gestalt.security.api.GestaltSecurityClient
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.api.{ResourceLink => SecurityLink}
import play.api.Logger
import scala.language.postfixOps

object Security {
  
  private[this] val log = Logger(this.getClass)
  
  def getOrgSyncTree(orgId: Option[UUID], auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[GestaltOrgSync] = {
    Try(Await.result(GestaltOrg.syncOrgTree(orgId)(client.withCreds(auth.creds)), 5 seconds))
  } 
  
  def getRootOrg(auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[GestaltOrg] = {
    def unwrap(os: Seq[GestaltOrg]) = Try {
      os.size match {
        case 1 => os(0)
        case 0 => throw new RuntimeException( Error.ROOT_ORG_NOT_FOUND )
        case _ => throw new RuntimeException( Error.ROOT_ORG_MULTIPLE  )
      }
    }
    getAllOrgs(None, auth) match {
      case Success(os) => unwrap( os filter { o => o.parent.isEmpty } )
      case Failure(ex) => throw ex
    }
  }
  
  def getRootUser(auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[GestaltOrg] = {
    ???
  }
  
  def getGroups(auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[Seq[GestaltGroup]] = {
    Try(Await.result(GestaltGroup.listGroups()(client.withCreds(auth.creds)), 5 seconds))
  }
  
  def getGroupAccounts(groupId: UUID, auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[Seq[GestaltAccount]] = {
    Try(Await.result(GestaltGroup.listAccounts(groupId)(client.withCreds(auth.creds)), 5 seconds))
  }
  
  def createGroup(org: UUID, auth: AuthAccountWithCreds, group: GestaltResourceInput)(implicit client: GestaltSecurityClient): Try[GestaltGroup] = {
    log.debug(s"createGroup(...)")
    Try {
      val newGroup = GestaltGroupCreateWithRights(
        name = group.name,
        rights = None,
        description = group.description
      )
      Await.result(GestaltOrg.createGroup(org, newGroup)(client.withCreds(auth.creds)), 5 seconds)
    }
  }
  
  def getAccountGroups(auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[Seq[GestaltGroup]] = {
    Try{Await.result(GestaltAccount.listGroupMemberships(auth.account.id)(client.withCreds(auth.creds)), 5 seconds)}
  }
  
  def getAccountGroups(accountId: UUID, auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[Seq[GestaltGroup]] = {
    Try{Await.result(GestaltAccount.listGroupMemberships(accountId)(client.withCreds(auth.creds)), 5 seconds)}
  }
  
  def deleteGroup(id: UUID, auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[Boolean] = {
    Try{Await.result(GestaltGroup.deleteGroup(id)(client.withCreds(auth.creds)), 5 seconds)}
  }
  
  def getAllOrgs(org: Option[UUID], auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[Seq[GestaltOrg]] = {
    Try{Await.result(GestaltOrg.listOrgs()(client.withCreds(auth.creds)), 5 seconds)}
  }
  
  def createOrg(parent: UUID, auth: AuthAccountWithCreds, org: GestaltResourceInput)(implicit client: GestaltSecurityClient): Try[GestaltOrg] = {
    log.debug(s"createOrg($parent, <auth>, <org>)")
    Try{Await.result(GestaltOrg.createSubOrg(
      parentOrgId = parent,
      create = GestaltOrgCreate(
        name = org.name,
        createDefaultUserGroup = false,
        inheritParentMappings = Some(true),
        description = org.description
      ))(client.withCreds(auth.creds)), 5 seconds )}
  }
  
  def deleteOrg(org: UUID, auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[Boolean] = {
    log.debug(s"Attempting to DELETE Org ${org.toString}. Account: ${auth.account}")
    Try{Await.result(GestaltOrg.deleteOrg(org)(client.withCreds(auth.creds)), 5 seconds)}
  }
  
  def createAccount(org: UUID, auth: AuthAccountWithCreds, user: GestaltResourceInput)(implicit client: GestaltSecurityClient): Try[GestaltAccount] = {
    val props = stringmap(user.properties) getOrElse { 
      throw new BadRequestException(s"Invalid user. Cannot create.") 
    }
    
    val account = GestaltAccountCreateWithRights(
      username = user.name,
      firstName = props("firstName"),
      lastName = props("lastName"),
      email = props.get("email").filter(!_.isEmpty),
      phoneNumber = props.get("phoneNumber").filter(!_.isEmpty),
      credential = GestaltPasswordCredential( props("password")) ,
      groups = None,
      rights = None,
      description = user.description
    )
    log.debug(s"Creating account in gestalt-security (org = $org")
    Try{Await.result( GestaltOrg.createAccount(org, account)(client.withCreds(auth.creds)), 5 seconds )}
  }

  def getAllAccounts(org: Option[UUID], auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[Seq[GestaltAccount]] = {
    Try{Await.result(GestaltOrg.listAccounts(org.get)(client.withCreds(auth.creds)), 5 seconds)}
  }
  
  def deleteAccount(id: UUID, auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[Boolean] = {
    log.debug(s"Attempting to delete Account ${id} from Gestalt Security.")
    Try(Await.result(GestaltAccount.deleteAccount(id)(client.withCreds(auth.creds)), 5 seconds))
  }

  def addAccountsToGroup(group: UUID, accounts: Seq[UUID])(implicit client: GestaltSecurityClient): Try[Seq[SecurityLink]] = {
    Try(Await.result(GestaltGroup.updateMembership(group, accounts, Seq()), 5 seconds))
  }
  
  def removeAccountsFromGroup(group: UUID, accounts: Seq[UUID])(implicit client: GestaltSecurityClient): Try[Seq[SecurityLink]] = {
    Try(Await.result(GestaltGroup.updateMembership(group, Seq(), remove = accounts), 5 seconds))
  }
  
  private object Error {
    val ROOT_ORG_NOT_FOUND = "Could not find root Org. Contact Administrator."
    val ROOT_ORG_MULTIPLE  = "More than one root Org found. Contact Administrator."
  }  
  
}