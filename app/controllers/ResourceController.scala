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
import com.galacticfog.gestalt.meta.api.sdk.{ ResourceLink => MetaLink }

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
import com.galacticfog.gestalt.meta.api.errors._

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._

object ResourceController extends MetaController with NonLoggingTaskEvents {

  private val qs = ResourceQueryService

  /**
   * Get a list of all Orgs in Meta (global)
   */
  def getAllOrgs = Authenticate() { implicit request =>
    Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.Org)))
  }

  
  /**
   * Get a single Org
   */
  def getOrgById(id: UUID) = Authenticate(id) { implicit securedRequest =>
    trace(s"getByOrgId($id)")
    getById(org = id, ResourceIds.Org, id)
  }

  def getOrgByFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    trace(s"getOrgByFqon($fqon)")
    orgFqon(fqon) match {
      case Some(org) => getById(org = org.id, ResourceIds.Org, org.id)
      case None => OrgNotFound(fqon)
    }
  }
  
  
  /**
   * Get a list of child Orgs 
   */
  def getChildOrgs(org: UUID) = Authenticate(org) { implicit request =>
    getOrg(org) match {
      case Some(_) => childOrgs(org)
      case None    => OrgNotFound(org)
    }
  }

  def getChildOrgsFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => childOrgs(org.id)
      case None      => OrgNotFound(fqon)
    }
  }

  /**
   * Get a list of all users in the system (global)
   */
  def getAllUsersGlobal = Authenticate() { implicit request =>
    Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.User)))
  }

  
  /**
   * Get a list of all Users in the given Org
   */
  def getAllUsers(org: UUID) = Authenticate(org) { implicit request =>
    Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.User, org)))
  }

  def getAllUsersFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.User, org.id)))
      case None => OrgNotFound(fqon)
    }
  }
  

  
  /**
   * Get a single user by ID
   */
  def getUserById(org: UUID, id: UUID) = Authenticate(org) { implicit request =>
    ResourceFactory.findById(ResourceIds.User, id) match {
      case Some(user) => Ok(Output.renderInstance(user))
      case None       => NotFoundResult(s"User '${id}' not found.")
    }
  }

  def getUserByIdFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => ResourceFactory.findById(ResourceIds.User, id) match {
        case Some(user) => Ok(Output.renderInstance(user))
        case None       => NotFoundResult(s"User '${id}' not found.")
      }
      case None => OrgNotFound(fqon)
    } 
  }
  
  /**
   * Get all Resources by Type ID
   */
  def getAllResourcesByTypeOrg(org: UUID, typeId: UUID) = Authenticate(org) { implicit request =>
    Ok(Output.renderLinks(ResourceFactory.findAll(typeId, org)))
  }
  
  def getAllResourcesByTypeFqon(fqon: String, typeId: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case None => OrgNotFound(fqon)  
      case Some(org) => Ok(Output.renderLinks(ResourceFactory.findAll(typeId, org.id)))
    }     
  }

  val resources = ResourceFactory
  val references = ReferenceFactory
  
  def resourceLinks(org: UUID, typeId: UUID, baseurl: Option[String] = None): String = {
    Output.renderLinks(resources.findAll(typeId, org), baseurl)
  }
  

  def getAllSystemResourcesByName(org: UUID, restName: String) = Authenticate(org)  { implicit request =>
    extractByName(org, restName) match {
      case Left(result) => result
      case Right((org, typeId)) => Ok(renderResourceLinks(org, typeId, META_URL))      
    }
  }
  
  def getAllSystemResourcesByNameFqon(fqon: String, restName: String) = Authenticate(fqon) { implicit request =>
    extractByName(fqon, restName) match {
      case Left(result) => result
      case Right((org, typeId)) => Ok(renderResourceLinks(org, typeId, META_URL))
    }
  }
  
  
  private def renderResourceLinks[T](org: UUID, typeId: UUID, url: Option[String]) = {
    if (references.isReferenceType(typeId)) {
      //"REFERENCE_TYPE"
      referenceLinks(org, typeId)
    }
    else {
      resourceLinks(org, typeId, url)
    }
    
  }
  
  /* TODO: This is extremely temporary */
  private def referenceLinks(org: UUID, typeId: UUID) = {
    val refs = references.findAll(org, typeId) map { r => "{\"id\": \"%s\", \"name\": \"%s\"}".format(r.id, r.name) }
    Json.prettyPrint(Json.parse("[%s]".format(refs.mkString(","))))
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
  def getAllByFqon(fqon: String, resource: String) = Authenticate(fqon) { implicit securedRequest =>
    ResourceFactory.findByPropertyValue(ResourceIds.Org, "fqon", fqon) match {
      case Some(org) => {
        resourceUUID(resource) match {
          case Some(typeId) => Ok {
            Output.renderLinks(ResourceFactory.findAll(typeId))
          }
          case None => NotFoundResult(s"Invalid resource name '${resource}'.")
        }
      } // This shouldn't happen since security would send back 'unauthorized'
      case None => NotFound(s"Org FQON '${fqon}' does not exist.")
    }
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

  /**
   * 
   * With FQON matching String, we have to check if that String is a resource-type.
   * That means we have to prevent registration of an Org with a name that conflicts with our Resource names.
   * Is that acceptable?
   * 
   */

  // --------------------------------------------------------------------------
  // ORGS
  // --------------------------------------------------------------------------

  def getAllResources(org: UUID) = Authenticate(org) { implicit request =>
    Ok(Output.renderLinks(ResourceFactory.findAllByOrg(org)))
  }

  def getAllResourcesFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => Ok(Output.renderLinks(ResourceFactory.findAllByOrg(org.id)))
      case None => OrgNotFound(fqon)
    }
  }
  
  
  def getResourceByIdFqon(fqon: String, id: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => ResourceFactory.findById(id) match {
        case Some(res) => Ok(Output.renderInstance(res))
        case None => NotFoundResult(Errors.RESOURCE_NOT_FOUND(id))
      }
      case None => OrgNotFound(fqon)
    }
  }
  
  def getResourceById(org: UUID, id: UUID) = Authenticate(org) { implicit request =>
    ResourceFactory.findById(id) match {
      case Some(res) => Ok(Output.renderInstance(res))
      case None      => NotFoundResult(Errors.RESOURCE_NOT_FOUND(id))
    }
  }
  
  
  // --------------------------------------------------------------------------
  // WORKSPACES
  // --------------------------------------------------------------------------  

  def getAllWorkspaces(org: UUID) = Authenticate(org) { implicit request =>
    Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.Workspace, org)))  
  }
  
  def getAllWorkspacesFqon(fqon: String) = Authenticate(fqon) { implicit securedRequest =>
    orgFqon(fqon) match {
      case None => OrgNotFound(fqon)  
      case Some(org) => Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.Workspace, org.id)))
    }
  }

  def getWorkspaceById(org: UUID, id: UUID) = Authenticate(org) { implicit request =>
    ResourceFactory.findById(ResourceIds.Workspace, id) match {
      case Some(res) => Ok(Output.renderInstance(res))
      case None       => NotFoundResult(s"Workspace '${id}' not found.")
    }
  }
  
  def getWorkspaceByIdFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => ResourceFactory.findById(ResourceIds.Workspace, id) match {
        case Some(res) => Ok(Output.renderInstance(res))
        case None => NotFoundResult(Errors.RESOURCE_NOT_FOUND(id))
      }
      case None => OrgNotFound(fqon)
    }  
  }
  

  

  
  // --------------------------------------------------------------------------
  // ENVIRONMENTS
  // --------------------------------------------------------------------------   
  def getAllEnvironments(org: UUID) = Authenticate(org) { implicit request =>
    ???
  }
  
  def getAllEnvironmentsFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    ???
  }
  
  def getEnvironmentById(org: UUID, id: UUID) = Authenticate(org) { implicit request =>
    FindByIdResult(org, ResourceIds.Environment, id)
  }
  
  def getEnvironmentByIdFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => FindByIdResult(org.id, ResourceIds.Environment, id)
      case None => OrgNotFound(fqon)
    }
  }
  
  
  def FindByIdResult(org: UUID, typeId: UUID, id: UUID) = {
    ResourceFactory.findById(typeId, id) match {
      case Some(res) => Ok(Output.renderInstance(res))
      case None       => NotFoundResult(s"${ResourceLabel(typeId)} '${id}' not found.")
    }    
  }  
  
  def getEnvironmentsByWorkspace(org: UUID, workspaceId: UUID) = Authenticate(org) { implicit request =>
    Ok(Output.renderLinks(ResourceFactory.findAllByPropertyValueOrg(org, ResourceIds.Environment, "workspace", workspaceId.toString)))  
  }
  
  def getEnvironmentsByWorkspaceFqon(fqon: String, workspaceId: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) =>
        Ok(Output.renderLinks(ResourceFactory.findAllByPropertyValueOrg(org.id, ResourceIds.Environment, "workspace", workspaceId.toString)))
      case None => OrgNotFound(fqon)
    }
  }
  
  def putEnvironment(org: UUID, id: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    ???
  }
  
  def postEnvironmentFqon(fqon: String, id: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    ???
  }
  
  def postEnvironment(org: UUID, id: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    ???
  }  
  
  def putEnvironmentFqon(fqon: String, id: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    ???
  }
  
  def patchEnvironment(org: UUID, id: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    ???
  }
  
  def patchEnvironmentFqon(fqon: String, id: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    ???
  }
  
  def deleteEnvironment(org: UUID, id: UUID) = Authenticate(org) { implicit request =>
    ???
  }

  def deleteEnvironmentFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    ???
  }  
  
  
  
  // --------------------------------------------------------------------------
  // MACHINE_SPECS
  // --------------------------------------------------------------------------
  
  
  // --------------------------------------------------------------------------
  // SERVICES
  // --------------------------------------------------------------------------  
  

  // --------------------------------------------------------------------------
  // NODE_TEMPLATES
  // --------------------------------------------------------------------------
  
  
  // --------------------------------------------------------------------------
  // CLUSTER_TEMPLATES
  // --------------------------------------------------------------------------
  
  
  // --------------------------------------------------------------------------
  // BLUEPRINTS
  // --------------------------------------------------------------------------  
  
  
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

  
  def getAllInOrg(typeId: UUID, org: UUID): List[GestaltResourceInstance] = {
    ResourceFactory.findAll(typeId, org)
  }
  
  
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
