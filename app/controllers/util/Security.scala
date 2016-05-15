package controllers.util


import play.api.{Logger => log}

import play.api.Play.current
import play.api.libs.ws._
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import scala.concurrent.Future

import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.mvc.RequestHeader
import play.api.mvc.AnyContent

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Try, Success, Failure}

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api._

import com.galacticfog.gestalt.tasks.io.TaskStatus
import com.galacticfog.gestalt.tasks.play.actors.TaskEventMessage
import com.galacticfog.gestalt.tasks.play.io._
//import com.galacticfog.gestalt.tasks.io._

import controllers.util._
import controllers.util.db._

import play.mvc.Result

import java.util.UUID
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._

import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltBaseAuthProvider
import com.galacticfog.gestalt.security.play.silhouette.GestaltSecuredController
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController


import com.mohiva.play.silhouette.api.services.AuthenticatorService
import com.mohiva.play.silhouette.impl.authenticators.{DummyAuthenticatorService, DummyAuthenticator}

import com.galacticfog.gestalt.security.api._

import com.galacticfog.gestalt.security.api.json.JsonImports

import play.api.libs.json._

import com.mohiva.play.silhouette.api.util.Credentials


import com.galacticfog.gestalt.security.api.json.JsonImports.{orgFormat,linkFormat}

import scala.concurrent.duration._
import scala.concurrent.Await


object Security {

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
  
  def getGroups(auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[Seq[GestaltGroup]] = {
    Try(Await.result(GestaltGroup.getGroups(auth.creds.identifier, auth.creds.password), 5 seconds))
  }
  
  def getGroupAccounts(groupId: UUID, auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[Seq[GestaltAccount]] = {
    Try(Await.result(GestaltGroup.listAccounts(groupId), 5 seconds))
  }
  
  def createGroup(org: UUID, auth: AuthAccountWithCreds, group: GestaltResourceInput)(implicit client: GestaltSecurityClient): Try[GestaltGroup] = {
    log.debug(s"createGroup(...)")
    Try {
      val newGroup = GestaltGroupCreateWithRights(group.name)
      Await.result(GestaltOrg.createGroup(org, newGroup, auth.creds.identifier, auth.creds.password), 5 seconds)
    }
  }
  
  def getAccountGroups(auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[Seq[GestaltGroup]] = {
    //GestaltAccount.listGroupMemberships(accountId)
    ???
  }
  
  def deleteGroup(id: UUID, auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[Boolean] = {
    Try{Await.result(GestaltGroup.deleteGroup(id, auth.creds.identifier, auth.creds.password), 5 seconds)}
  }
  
  def getRootUser(auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[GestaltOrg] = {
    ???
  }
  
  def getOrgSyncTree(orgId: Option[UUID], auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[GestaltOrgSync] = {
    Try(Await.result(GestaltOrg.syncOrgTree(orgId, auth.creds.identifier, auth.creds.password), 5 seconds))
  }
  
  def getAllOrgs(org: Option[UUID], auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[Seq[GestaltOrg]] = {
    Try{Await.result(GestaltOrg.listOrgs(auth.creds.identifier, auth.creds.password), 5 seconds)}
  }
  
  def createOrg(parent: UUID, auth: AuthAccountWithCreds, org: GestaltResourceInput)(implicit client: GestaltSecurityClient): Try[GestaltOrg] = {
    log.debug(s"createOrg($parent, <auth>, <org>)")
    Try{Await.result(GestaltOrg.createSubOrg(parent, org.name, auth.creds.identifier, auth.creds.password), 5 seconds )}
  }
  
  def deleteOrg(org: UUID, auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[Boolean] = {
    log.debug(s"Attempting to DELETE Org ${org.toString}. Account: ${auth.account}")
    Try{Await.result(GestaltOrg.deleteOrg(org, auth.creds.identifier, auth.creds.password), 5 seconds)}
  }
  
  def createAccount(org: UUID, auth: AuthAccountWithCreds, user: GestaltResourceInput)(implicit client: GestaltSecurityClient): Try[GestaltAccount] = {
    val props = stringmap(user.properties) getOrElse { 
      throw new BadRequestException(s"Invalid user. Cannot create.") 
    }
    
    val account = GestaltAccountCreateWithRights(
        username = user.name,
        firstName = props("firstName"),
        lastName = props("lastName"),
        email = props("email"),
        phoneNumber = props("phoneNumber"),
        credential = GestaltPasswordCredential( props("password")) )
    Try{Await.result( GestaltOrg.createAccount(org, account, auth.creds.identifier, auth.creds.password), 5 seconds )}
  }
  
  def getAllAccounts(org: Option[UUID], auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[Seq[GestaltAccount]] = {
    Try{Await.result(GestaltOrg.getOrgAccounts(org.get, auth.creds.identifier, auth.creds.password), 5 seconds)}
  }
  
  def deleteAccount(id: UUID, auth: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[Boolean] = {
    Try(Await.result(GestaltAccount.deleteAccount(id, auth.creds.identifier, auth.creds.password), 5 seconds))
  }  
  
  def addAccountsToGroup(group: UUID, accounts: Seq[UUID])(implicit client: GestaltSecurityClient): Try[Seq[GestaltAccount]] = {
    Try(Await.result(GestaltGroup.updateMembership(group, accounts, Seq()), 5 seconds))
  }
  
  def removeAccountsFromGroup(group: UUID, accounts: Seq[UUID])(implicit client: GestaltSecurityClient): Try[Seq[GestaltAccount]] = {
    Try(Await.result(GestaltGroup.updateMembership(group, Seq(), remove = accounts), 5 seconds))
  }
  
  private object Error {
    val ROOT_ORG_NOT_FOUND = "Could not find root Org. Contact Administrator."
    val ROOT_ORG_MULTIPLE  = "More than one root Org found. Contact Administrator."
  }  
  
}