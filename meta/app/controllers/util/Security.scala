package controllers.util


import java.util.UUID
import javax.inject.Inject

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.meta.api.errors.{BadRequestException, ResourceNotFoundException}
import com.galacticfog.gestalt.meta.api.sdk.GestaltResourceInput
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltAccountUpdate
import com.galacticfog.gestalt.security.api.GestaltAccountCreateWithRights
import com.galacticfog.gestalt.security.api.GestaltBasicCredentials
import com.galacticfog.gestalt.security.api.GestaltGroup
import com.galacticfog.gestalt.security.api.GestaltGroupUpdate
import com.galacticfog.gestalt.security.api.GestaltGroupCreateWithRights
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.GestaltOrgCreate
import com.galacticfog.gestalt.security.api.GestaltOrgSync
import com.galacticfog.gestalt.security.api.GestaltPasswordCredential
import com.galacticfog.gestalt.security.api.GestaltSecurityClient
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.api.{ResourceLink => SecurityLink}
import modules.SecurityClientProvider
import play.api.Logger
import scala.language.postfixOps
import scala.concurrent.Future


class Security @Inject()(secClientProvider: SecurityClientProvider) {
  
  private[this] val log = Logger(this.getClass)
  
  def client() = secClientProvider.client
  
  def clientUrl(): String = {
    val c = secClientProvider.client
    "%s://%s%s".format(c.protocol, c.hostname, c.port)
  }
  
  def searchAccounts(org: UUID, auth: AuthAccountWithCreds, criteria: (String,String)*): Future[Seq[GestaltAccount]] = {
    GestaltOrg.listAccounts(org, criteria:_*)(secClientProvider.client.withCreds(auth.creds))
  }
  
  def searchGroups(org: UUID, auth: AuthAccountWithCreds, criteria: (String,String)*): Future[Seq[GestaltGroup]] = {
    GestaltOrg.listGroups(org, criteria:_*)(secClientProvider.client.withCreds(auth.creds))
  }
  
  def getRootUser(auth: AuthAccountWithCreds): Try[GestaltAccount] = {
    for {
      org    <- getRootOrg(auth)
      tree   <- getOrgSyncTree(Some(org.id), auth)
      link  = tree.admin getOrElse {
        throw new ResourceNotFoundException(s"Could not find 'admin' in security sync-tree.")
      }
      a1 <- getAccount(org.id, link.id, auth) 
      a2 = a1 getOrElse {
        throw new ResourceNotFoundException(s"Could not find root user in gestalt-security.")
      }
    } yield a2
  }
  
  def getRootUser()(implicit client: GestaltSecurityClient) = {
    for {
      tree <- getOrgSyncTree2()
      link = tree.admin getOrElse {
        throw new ResourceNotFoundException(s"Could not find 'admin' in security sync-tree.")
      }
    } yield link
  }
  
  def getRootInfo(auth: AuthAccountWithCreds): (GestaltOrg, Option[GestaltAccount]) = {
    (for {
      org    <- getRootOrg(auth)
      tree   <- getOrgSyncTree(Some(org.id), auth)
      link = tree.admin
      admin  <- this.getAccount(org.id, link.get.id, auth)
    } yield (org, admin)).get
  }  
  
  def getOrgSyncTree(orgId: Option[UUID], auth: AuthAccountWithCreds): Try[GestaltOrgSync] = {
    Try(Await.result(GestaltOrg.syncOrgTree(orgId)(secClientProvider.client.withCreds(auth.creds)), 30 seconds))
  }
  
  def getOrgSyncTree2()(implicit client: GestaltSecurityClient): Try[GestaltOrgSync] = {
    Try(Await.result(GestaltOrg.syncOrgTree(None), 5 seconds))
  }  
  
  /**
   * Use explicit credentials
   */
  def getOrgSyncTree(orgId: Option[UUID], creds: GestaltBasicCredentials): Try[GestaltOrgSync] = {
    Try(Await.result(GestaltOrg.syncOrgTree(orgId)(secClientProvider.client.withCreds(creds)), 30 seconds))
  }
  
  def getRootOrg(auth: AuthAccountWithCreds): Try[GestaltOrg] = {
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

  def getGroups(auth: AuthAccountWithCreds): Try[Seq[GestaltGroup]] = {
    Try(Await.result(GestaltGroup.listGroups()(secClientProvider.client.withCreds(auth.creds)), 30 seconds))
  }
  
  def getGroupAccounts(groupId: UUID, auth: AuthAccountWithCreds): Try[Seq[GestaltAccount]] = {
    Try(Await.result(GestaltGroup.listAccounts(groupId)(secClientProvider.client.withCreds(auth.creds)), 30 seconds))
  }

  def createGroup(org: UUID, auth: AuthAccountWithCreds, group: GestaltResourceInput): Try[GestaltGroup] = {
    log.debug(s"createGroup(...)")
    Try {
      val newGroup = GestaltGroupCreateWithRights(
        name = group.name,
        rights = None,
        description = group.description
      )
      Await.result(GestaltOrg.createGroup(org, newGroup)(secClientProvider.client.withCreds(auth.creds)), 30 seconds)
    }
  }
  
//  def updateAccount(accountId: UUID, auth: AuthAccountWithCreds, update: GestaltAccountUpdate) = {
//    Try{Await.result(GestaltAccount.updateAccount(accountId, update)(secClientProvider.client.withCreds(auth.creds)), 5 seconds)}
//  }
  
  def updateGroup(groupId: UUID, auth: AuthAccountWithCreds, update: GestaltGroupUpdate) = {
    Try {Await.result(GestaltGroup.updateGroup(groupId, update)(secClientProvider.client.withCreds(auth.creds)), 30.seconds)}  
  }
  
  def getAccountGroups(auth: AuthAccountWithCreds): Try[Seq[GestaltGroup]] = {
    Try{Await.result(GestaltAccount.listGroupMemberships(auth.account.id)(secClientProvider.client.withCreds(auth.creds)), 30 seconds)}
  }
  
  def getAccountGroups(accountId: UUID, auth: AuthAccountWithCreds): Try[Seq[GestaltGroup]] = {
    Try{Await.result(GestaltAccount.listGroupMemberships(accountId)(secClientProvider.client.withCreds(auth.creds)), 30 seconds)}
  }
  
  def deleteGroup(id: UUID, auth: AuthAccountWithCreds): Try[Boolean] = {
    Try{Await.result(GestaltGroup.deleteGroup(id)(secClientProvider.client.withCreds(auth.creds)), 30 seconds)}
  }
  
  def getAllOrgs(org: Option[UUID], auth: AuthAccountWithCreds): Try[Seq[GestaltOrg]] = {
    Try{Await.result(GestaltOrg.listOrgs()(secClientProvider.client.withCreds(auth.creds)), 30 seconds)}
  }
  
  def createOrg(parent: UUID, auth: AuthAccountWithCreds, org: GestaltResourceInput): Try[GestaltOrg] = {
    log.debug(s"createOrg($parent, <auth>, <org>)")
    Try{Await.result(GestaltOrg.createSubOrg(
      parentOrgId = parent,
      create = GestaltOrgCreate(
        name = org.name,
        createDefaultUserGroup = false,
        inheritParentMappings = Some(true),
        description = org.description
      ))(secClientProvider.client.withCreds(auth.creds)), 5 seconds )}
  }
  
  def deleteOrg(org: UUID, auth: AuthAccountWithCreds): Try[Boolean] = {
    log.debug(s"Attempting to DELETE Org ${org.toString}. Account: ${auth.account}")
    Try{Await.result(GestaltOrg.deleteOrg(org)(secClientProvider.client.withCreds(auth.creds)), 5 seconds)}
  }
  
  def createAccount(org: UUID, auth: AuthAccountWithCreds, user: GestaltResourceInput): Try[GestaltAccount] = {
    val props = stringmap(user.properties) getOrElse { 
      throw new BadRequestException(s"Invalid user. Cannot create.") 
    }
    
    val account = GestaltAccountCreateWithRights(
      username = user.name,
      firstName = props("firstName"),
      lastName = props("lastName"),
      email = props.get("email").filter(!_.isEmpty),
      phoneNumber = props.get("phoneNumber").filter(!_.isEmpty),
      credential = GestaltPasswordCredential(props("password")) ,
      groups = None,
      rights = None,
      description = user.description
    )

    log.debug(s"Creating account in gestalt-security (org = $org")
    Try{Await.result( GestaltOrg.createAccount(org, account)(secClientProvider.client.withCreds(auth.creds)), 5 seconds )}
  }

  def getAccount(org: UUID, accountId: UUID, auth: AuthAccountWithCreds): Try[Option[GestaltAccount]] = {
    Try(Await.result(GestaltOrg.getAccountById(org, accountId)(secClientProvider.client.withCreds(auth.creds)), 5 seconds))
  }
  
  def getAllAccounts(org: Option[UUID], auth: AuthAccountWithCreds): Try[Seq[GestaltAccount]] = {
    Try{Await.result(GestaltOrg.listAccounts(org.get)(secClientProvider.client.withCreds(auth.creds)), 5 seconds)}
  }
  
  def updateAccount(accountId: UUID, auth: AuthAccountWithCreds, update: GestaltAccountUpdate) = {
    Try{Await.result(GestaltAccount.updateAccount(accountId, update)(secClientProvider.client.withCreds(auth.creds)), 5 seconds)}
  }

  def deleteAccount(id: UUID, auth: AuthAccountWithCreds): Try[Boolean] = {
    log.debug(s"Attempting to delete Account ${id} from Gestalt Security.")
    Try(Await.result(GestaltAccount.deleteAccount(id)(secClientProvider.client.withCreds(auth.creds)), 5 seconds))
  }

  def addAccountsToGroup(group: UUID, accounts: Seq[UUID]): Try[Seq[SecurityLink]] = {
    Try(Await.result(GestaltGroup.updateMembership(group, accounts, Seq())(secClientProvider.client), 5 seconds))
  }
  
  def removeAccountsFromGroup(group: UUID, accounts: Seq[UUID]): Try[Seq[SecurityLink]] = {
    Try(Await.result(GestaltGroup.updateMembership(group, Seq(), remove = accounts)(secClientProvider.client), 5 seconds))
  }
  
  private object Error {
    val ROOT_ORG_NOT_FOUND = "Could not find root Org. Contact Administrator."
    val ROOT_ORG_MULTIPLE  = "More than one root Org found. Contact Administrator."
  }  
  
}