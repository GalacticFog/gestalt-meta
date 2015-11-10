package controllers


import play.api.{ Logger => log }

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
import scala.util.{ Try, Success, Failure }
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data.models.{ ResourceLink => MetaLink }

import com.galacticfog.gestalt.meta.services.ResourceQueryService
import com.galacticfog.gestalt.tasks.io.TaskStatus
import com.galacticfog.gestalt.tasks.play.actors.TaskEventMessage
import com.galacticfog.gestalt.tasks.play.io._
import controllers.util._
import controllers.util.db._
import play.mvc.Result
import java.util.UUID
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltBaseAuthProvider
import com.galacticfog.gestalt.security.play.silhouette.GestaltSecuredController
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.mohiva.play.silhouette.api.services.AuthenticatorService
import com.mohiva.play.silhouette.impl.authenticators.{ DummyAuthenticatorService, DummyAuthenticator }
import com.galacticfog.gestalt.security.api.{GestaltResource => SecurityResource}
import com.galacticfog.gestalt.security.api.{ResourceLink => SecurityLink}

import com.galacticfog.gestalt.security.api._
import com.galacticfog.gestalt.security.api.json.JsonImports
import play.api.libs.json._
import com.galacticfog.gestalt.security.api.json.JsonImports.{ orgFormat, linkFormat, acctFormat }
import com.mohiva.play.silhouette.api.util.Credentials

import com.galacticfog.gestalt.meta.api.output._ //JsonImports._

import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.security.api.{ GestaltResource => SecuredResource }


object ResourceController extends GestaltFrameworkSecuredController[DummyAuthenticator]
  with MetaController with NonLoggingTaskEvents {

  private val qs = ResourceQueryService
  
  /**
   * Get a list of all Orgs in Meta
   */
  def getAllOrgs = GestaltFrameworkAuthAction(nullOptString(None)) { implicit request =>
    trace("getAllOrgs()")
    Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.Org)))
  }
  
  
  /**
   * Get a list of child Orgs by Org UUID.
   */
  def getChildOrgs(org: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    trace(s"getChildOrgs($org)")
    getOrg(org) match {
      case Some(_) => childOrgs(org)  
      case None => NotFound(toError(404, Errors.ORG_NOT_FOUND(org.toString)))
    }
  }
  
  /**
   * Get a list of child Orgs by Org FQON.
   */
  def getChildOrgsFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"getChildOrgsFqon($fqon)")
    orgFqon(fqon) match {
      case Some(org) => childOrgs(org.id)  
      case None => NotFound(toError(404, Errors.ORG_NOT_FOUND(fqon)))
    }
  }

  /**
   * Get a list of all users in the system from root Org down.
   */
  def getAllUsersFromRoot = GestaltFrameworkAuthAction(nullOptString(None)) { implicit request =>
    trace("getAllUsersFromRoot()")
    Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.User)))
  }
  
  /**
   * Get a list of all Users in the given Org by UUID.
   */
  def getAllUsers(org: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    trace(s"getAllUsers(org = ${org.toString}")
    Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.User, org)))
  }
  
  /**
   * Get a list of all Users in the given Org by FQON.
   */  
  def getAllUsersFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"getAllUsersFqon($fqon)")
    orgFqon(fqon) match {
      case Some(org) => Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.User, org.id)))
      case None => OrgNotFound(fqon)
    }
  }
  
  /**
   * Get a single user by their ID and Org UUID.
   */
  def getUserById(org: UUID, id: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    trace(s"getUserById($org, $id")
    ResourceFactory.findById(ResourceIds.User, id) match {
      case Some(user) => Ok(Output.renderInstance(user))
      case None       => NotFound(toError(404, s"User '${id}' not found."))
    }
  }
  
  /**
   * Get a single user by their ID and Org FQON.
   */  
  def getUserByIdFqon(fqon: String, id: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"getUserByIdFqon($fqon, $id")
    orgFqon(fqon) match {
      case Some(org) => ResourceFactory.findById(ResourceIds.User, id) match {
        case Some(user) => Ok(Output.renderInstance(user))
        case None       => NotFound(toError(404, s"User '${id}' not found."))
      }
      case None => OrgNotFound(fqon)
    } 
  }

  
  /**
   * Get a List of ResourceLinks by Org UUID
   * [Implements]: GET /orgs/:uuid/:resources, i.e. /orgs/:uuid/workspaces
   */
//  def getAllByOrgId(org: UUID, resource: String) = GestaltFrameworkAuthAction(Some(org)) { implicit securedRequest =>
//    trace(s"getAllByOrgId($org, $resource)")
//    resourceUUID(resource) match {
//      case Some(id) => getAll(org, id)  
//      case None => NotFound(s"Invalid resource-type: $resource")
//    }
//  }

  
  /**
   * Get a List of ResourceLinks by FQON
   * [Implements]: GET /orgs/:fqon/:resources, i.e. /orgs/gf.engineering.core/workspaces
   */
  def getAllByFqon(fqon: String, resource: String) = GestaltFrameworkAuthAction(Some(fqon)) { implicit securedRequest =>
    trace(s"getAllByFqon($fqon, $resource)")
    ResourceFactory.findByPropertyValue(ResourceIds.Org, "fqon", fqon) match {
      case Some(org) => {
        resourceUUID(resource) match {
          case Some(typeId) => Ok {
            Output.renderLinks(ResourceFactory.findAll(typeId))
          }
          case None => NotFound(toError(404, s"Invalid resource name '${resource}'."))
        }
      } // This shouldn't happen since security would send back 'unauthorized'
      case None => NotFound(s"Org FQON '${fqon}' does not exist.")
    }
  }

  
  /**
   * TODO: this can be made to work for any resource queries up to 2 levels deep
   * (which is the maximum we're going to go).
   */
//  def getResourcesByPathWithOrgId(org: UUID, path: String) = Action.async {
//
//    val pc = path.split("/").toList
//    val m = if (pc.size == 1) {
//      "*** Getting ALL: " + pc(0)
//    } else if (pc.size == 2) {
//      s"*** Getting SINGLE : ${pc(0)}/${pc(1)}"
//    } else if (pc.size == 3) {
//      s"*** Getting ALL :  ${pc(2)}"
//    } else "I don't know what you're asking for."
//
//    Future {
//      Ok(s"RESOLVING (path.size=${pc.size}): " + m)
//    }
//  }

  /**
   * Determine whether a given identity name is a gestalt-security
   * user or group account.
   */
  def resolveSecuredIdentity(name: String): Option[SecuredResource] = {
    ???
  }

  // TODO: Do I use this to get a default 'owning-org' ???
  def getCurrentOrg(implicit client: GestaltSecurityClient): Future[GestaltOrg] = {
    client.get[GestaltOrg]("orgs/current")
  }


  def getResourcesByPathWithFqon(fqon: String, path: String) = Action.async {
    Future {
      Ok("RESOLVING : " + path)
    }
  }

  /*
   * 
   * With FQON matching String, we have to check if that String is a resource-type.
   * That means we have to prevent registration of an Org with a name that conflicts with our Resource names.
   * Is that acceptable?
   * 
   */

  // --------------------------------------------------------------------------
  // ORGS
  // --------------------------------------------------------------------------

  def getOrgById(id: UUID) = GestaltFrameworkAuthAction(Some(id)) { implicit securedRequest =>
    trace(s"getByOrgId($id)")
    getById(org = id, ResourceIds.Org, id)
  }

  // GET /some.org.name
  def getOrgByFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"getOrgByFqon(${fqon})")
    orgFqon(fqon) match {
      case Some(org) => getById(org = org.id, ResourceIds.Org, org.id)
      case None => OrgNotFound(fqon)
    }
  }

  // --------------------------------------------------------------------------
  // WORKSPACES
  // --------------------------------------------------------------------------  

  def getAllWorkspaces(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)) { implicit securedRequest =>
    println(securedRequest.identity.account)
    val acc = securedRequest.identity
    ???
  }

  
  object AuthorizationHandler {
    def getSingle(org: UUID, typeId: UUID, id: UUID, account: AuthAccountWithCreds) = {
      //
      // TODO: Perform Authorization Checking Here! Use AuthMap
      // 
      val identities = account.account +: account.groups map { _.id }
      getById(org, typeId, id)
    }
  }

  //  def createWorkspace() = Action.async(parse.json) { implicit request =>
  //    Future {
  //      val event = newPostEvent(TaskStatus.Pending)
  //            .withActionArgs(Map(
  //                "org_id"   -> "6add206f-58e5-43c4-b958-787879ab2d6a", 
  //                "owner_id" -> "ee7b94e2-5f0c-4b23-8d45-c18de9116c1f") )
  //      raiseTaskEvent {
  //        TaskEventMessage( event ) {
  //          ResourceCommandService.createResource
  //        }
  //      }
  //      Accepted( event.task.toJson )
  //    }  
  //  }

  //  def getBlueprintById(id: UUID, expand: Option[String]) = Action.async {
  //    if (expand.isDefined && expand.get.toLowerCase == "all") {
  //      Future( okNotFound( qs.getExpandedBlueprint( id ) ) )
  //    } else getById( ResourceIds.Blueprint, id )
  //  }

//  private def getAll(org: UUID, typeId: UUID) = /*Future*/ {
//    okNotFound(qs.findAll(typeId))
//  }

//  private def getAllWithOrg(org: UUID, typeId: UUID) = Future {
//    okNotFound(qs.findAllWithOrgId(org, typeId))
//  }

  private def getOrg(id: UUID) = {
    ResourceFactory.findById(ResourceIds.Org, id)
  }

  private def childOrgs(org: UUID) = {
    Ok(Output.renderLinks(ResourceFactory.findChildrenOfType(ResourceIds.Org, org)))
  }
  
  private def getById(org: UUID, resourceTypeId: UUID, id: UUID) = {
    okNotFound(qs.findById(org, resourceTypeId, id))
  }

}
