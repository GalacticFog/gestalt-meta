package controllers.util

import java.util.UUID

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.{Try,Success,Failure}


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

import scala.concurrent.ExecutionContext



class SecurityAsync(client: GestaltSecurityClient) {
  
  private[this] val log = Logger(this.getClass)
  
  def getSyncTree(orgId: Option[UUID], auth: AuthAccountWithCreds)
      (implicit ec: ExecutionContext): Future[GestaltOrgSync] = {
    GestaltOrg.syncOrgTree(orgId)(client.withCreds(auth.creds))
  } 
  
//  def getRootOrg(auth: AuthAccountWithCreds)
//    (implicit client: GestaltSecurityClient, ec: ExecutionContext): Future[GestaltOrg] = {
//    
//    def unwrap(os: Seq[GestaltOrg]) = Try {
//      os.size match {
//        case 1 => os(0)
//        case 0 => throw new RuntimeException( Error.ROOT_ORG_NOT_FOUND )
//        case _ => throw new RuntimeException( Error.ROOT_ORG_MULTIPLE  )
//      }
//    }
//    getAllOrgs(None, auth) match {
//      case Success(os) => unwrap( os filter { o => o.parent.isEmpty } )
//      case Failure(ex) => throw ex
//    }
//  }
  
  def getRootUser(auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Future[GestaltOrg] = {
    ???
  }
  
  /**
   * List all Groups
   */
  def getGroups(auth: AuthAccountWithCreds)(implicit ec: ExecutionContext): Future[Seq[GestaltGroup]] = {
    
    GestaltGroup.listGroups()(client.withCreds(auth.creds))
  }
  
  /**
   * List all Accounts in the given Group
   * 
   * @param groupId The group you want to retrieve Accounts for
   * @param auth Account to authenticate against security
   */
  def getGroupAccounts(groupId: UUID, auth: AuthAccountWithCreds)
      (implicit ec: ExecutionContext): Future[Seq[GestaltAccount]] = {
    
    GestaltGroup.listAccounts(groupId)(client.withCreds(auth.creds))
  }
  
  /**
   * Create a new Group in gestalt-security.
   * 
   * @param org UUID of the Org where the Group wil be created
   * @param auth Account to authenticate against security
   * @param group GestaltResourceInput containing data for new Group
   */
  def createGroup(org: UUID, group: GestaltResourceInput, auth: AuthAccountWithCreds)
      (implicit client: GestaltSecurityClient): Future[GestaltGroup] = {
    
    val newgroup = GestaltGroupCreateWithRights(
      name   = group.name,
      rights = None,
      description = group.description
    )
    GestaltOrg.createGroup(org, newgroup)(client.withCreds(auth.creds))
  }

  /**
   * List Groups the given Account is a member of
   * 
   * @param accountId UUID of Account to get Groups for
   * @param auth Account to authenticate against security
   */
  def getAccountGroups(accountId: UUID, auth: AuthAccountWithCreds)
      (implicit ec: ExecutionContext): Future[Seq[GestaltGroup]] = {
    
    GestaltAccount.listGroupMemberships(accountId)(client.withCreds(auth.creds))
  }
  
  /**
   * Delete a Group from gestalt-security
   * 
   * @param id UUID of the Group to delete
   * @param auth Account to authenticate against security  
   */
  def deleteGroup(id: UUID, auth: AuthAccountWithCreds)(implicit ec: ExecutionContext): Future[Boolean] = {
    
    GestaltGroup.deleteGroup(id)(client.withCreds(auth.creds))
  }
  
  /**
   * List Orgs in gestalt-security, either global or filtered by parent Org
   * 
   * @param org UUID of Org to search for sub-Orgs in. If None Org search will be global
   * @param auth Account to authenticate against security
   */
  def getAllOrgs(org: Option[UUID], auth: AuthAccountWithCreds)
      (implicit ec: ExecutionContext): Future[Seq[GestaltOrg]] = {
    
    if (org.isDefined) 
      GestaltOrg.listSubOrgs(org.get)(client.withCreds(auth.creds)) 
    else GestaltOrg.listOrgs()(client.withCreds(auth.creds))
  }
  
  /**
   * Create a new Org in gestalt-security
   * 
   * @param parent UUID of the parent Org. The new Org will be created as a child (sub-org) of the parent
   * @param org GestaltResourceInput containing data for the new Org
   * @param auth Account to authenticate against security
   */
  def createOrg(parent: UUID, org: GestaltResourceInput, auth: AuthAccountWithCreds )
      (implicit ec: ExecutionContext): Future[GestaltOrg] = {

    GestaltOrg.createSubOrg(
      parentOrgId = parent,
      create = GestaltOrgCreate(
        name = org.name,
        createDefaultUserGroup = false,
        inheritParentMappings  = Some(true),
        description = org.description ))(client.withCreds(auth.creds))
  }
  
  /**
   * Delete an Org from gestalt-security
   * 
   * @param org UUID of the Org to delete
   * @param auth Account to authenticate against security
   */
  def deleteOrg(org: UUID, auth: AuthAccountWithCreds)(implicit ec: ExecutionContext): Future[Boolean] = {
    log.debug("Attempting to DELETE Org ${org.toString}. Account: ${auth.account}")
    
    GestaltOrg.deleteOrg(org)(client.withCreds(auth.creds))
  }
  
  /**
   * Create a new user account in an Org in gestalt-security
   * 
   * @param org UUID of the Org where the user will be created
   * @param user GestaltResourceInput containing data for the new user
   * @param auth Account to authenticate against security
   */
  def createAccount(org: UUID, user: GestaltResourceInput, auth: AuthAccountWithCreds)
      (implicit ec: ExecutionContext): Future[GestaltAccount] = {
    
    val props = stringmap(user.properties) getOrElse { 
      throw new BadRequestException(s"Invalid user. Cannot create.") 
    }
    
    val account = GestaltAccountCreateWithRights(
      username    = user.name,
      firstName   = props("firstName"),
      lastName    = props("lastName"),
      email       = props.get("email").filter(!_.isEmpty),
      phoneNumber = props.get("phoneNumber").filter(!_.isEmpty),
      credential  = GestaltPasswordCredential( props("password")) ,
      groups      = None,
      rights      = None,
      description = user.description
    )
    GestaltOrg.createAccount(org, account)(client.withCreds(auth.creds))
  }

  /**
   * List all accounts in gestalt-security. Optionally filtered by Org
   * 
   * @param org UUID of the Org to search for Accounts in. If None Account search is global
   * @param auth Account to authenticate against security
   */
  def getAllAccounts(org: Option[UUID], auth: AuthAccountWithCreds)
      (implicit ec: ExecutionContext): Future[Seq[GestaltAccount]] = {
    
    GestaltOrg.listAccounts(org.get)(client.withCreds(auth.creds))
  }
  
  /**
   * Delete an Account from gestalt-security
   * 
   * @param id UUID of the Account to delete
   * @param auth Account to authenticate against security
   */
  def deleteAccount(id: UUID, auth: AuthAccountWithCreds)(implicit ec: ExecutionContext): Future[Boolean] = {

    GestaltAccount.deleteAccount(id)(client.withCreds(auth.creds))
  }

  /**
   * Add one or more Accounts to the given group
   * 
   * @param group UUID of the Group the Accounts will be added to
   * @param auth Account to authenticate against security
   */
  def addAccountsToGroup(group: UUID, accounts: Seq[UUID])
      (implicit ec: ExecutionContext): Future[Seq[SecurityLink]] = {
    
    GestaltGroup.updateMembership(group, accounts, Seq())(client)
  }
  
  /**
   * Remove one or more Accounts from a Group
   * 
   * @param group UUID of the Group the Accounts will be removed from
   * @param auth Account to authenticate against security
   */
  def removeAccountsFromGroup(group: UUID, accounts: Seq[UUID])
      (implicit ec: ExecutionContext): Future[Seq[SecurityLink]] = {
    
    GestaltGroup.updateMembership(group, Seq(), remove = accounts)(client)
  }
  
  private object Error {
    val ROOT_ORG_NOT_FOUND = "Could not find root Org. Contact Administrator."
    val ROOT_ORG_MULTIPLE  = "More than one root Org found. Contact Administrator."
  }  
  
}