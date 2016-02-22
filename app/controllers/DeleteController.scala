package controllers


import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import java.util.UUID

import scala.util.Failure
import scala.util.Success

import com.galacticfog.gestalt.meta.api.sdk._

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.TypeFactory
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.security.api.errors.SecurityRESTException
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.galacticfog.gestalt.tasks.play.io.NonLoggingTaskEvents
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator

import controllers.util._
import play.api.{Logger => log}

import com.galacticfog.gestalt.data.models._


object DeleteController extends GestaltFrameworkSecuredController[DummyAuthenticator] 
    with MetaController with NonLoggingTaskEvents with SecurityResources {
  
  val handlers = Map(
      "workspaces" -> HardDeleteWorkspace,
      "environments" -> HardDeleteEnvironment )
  
  import com.galacticfog.gestalt.meta.api._
  
  
  def lookupfn(restName: String) = {
    resourceUUID(restName) match {
      case Some(id) => ResourceFactory.findById(typeId = id, _ : UUID)
      case None => throw new ResourceNotFoundException("")
    }
  }
  
  def removeEndpointImplementation(endpoint: UUID) = {
      
  }
  
  import play.api.libs.json._
  def deleteLambda(org: UUID, lambda: UUID) = Authenticate(org) { implicit request =>
    /*
     * 1.) Get list of associated endpoints.
     * 2.) Update each endpoint (removing, implementation property)
     * 3.) Delete the lambda
     */
//    ResourceFactory.findEndpointsByLambda(lambda) foreach { p =>
//      val pjson = Json.toJson(p)
//    }
    ???  
  }
  
  def deleteLevel1Resource(org: UUID, restName1: String, id1: UUID) = Authenticate(org) { implicit request =>
    trace(s"deleteLevel1Resource($org, $restName1, $id1)")
    if (!handlers.contains(restName1)) NotFoundResult(request.path)
    else {
      lookupfn(restName1)(id1) match {
        case None => NotFoundResult("not found.")
        case Some(_) => {
          val force = getForceParam(request.queryString)
          handlers(restName1).delete(id1, force) match {
            case Success(_) => NoContent
            case Failure(e) => HandleRepositoryExceptions(e)
          }
          
        }
      }
    }
  }
  
  import scala.util.{Try,Success,Failure}
  
  def getForceParam(qs: Map[String,Seq[String]]): Boolean = {
    if (!qs.contains("force")) false
    else {
      val fp = qs("force")
      Try {
        fp.mkString.toBoolean
      } match {
        case Success(b) => b == true
        case Failure(_) => throw new BadRequestException(s"Value of 'force' parameter must be true or false. found: $fp")
      }
    }
  }
  
  def deleteLevel2Resource(org: UUID, restName1: String, id1: UUID, restName2: String, id2: UUID) = Authenticate(org) { implicit request =>
    trace(s"deleteLevel1Resource($org, $restName1, $id1, $restName2, $id2)")
    Ok(s"DELETING $restName2 : ${id2.toString}")  
  }
  
  def hardDeleteWorkspaceProvider(org: UUID, workspace: UUID, id: UUID) = Authenticate(org) { implicit request =>
    hardDeleteMetaResource(id, ResourceIds.ApiGatewayProvider)
  }
  
  def hardDeleteWorkspaceProviderFqon(fqon: String, workspace: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => hardDeleteMetaResource(id, ResourceIds.ApiGatewayProvider)
      case None => OrgNotFound(fqon)
    }
  }
  
  def hardDeleteLambda(org: UUID, id: UUID) = Authenticate(org) { implicit request =>
    hardDeleteMetaResource(id, ResourceIds.Lambda)
  }
  
  def hardDeleteLambdaFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => hardDeleteMetaResource(id, ResourceIds.Lambda)
      case None => OrgNotFound(fqon) 
    }
  }  
  
  def hardDeleteWorkspaceDomain(org: UUID, workspace: UUID, id: UUID) = Authenticate(org) { implicit request =>
    hardDeleteMetaResource(id, ResourceIds.Domain)
  }
  
  def hardDeleteWorkspaceDomainFqon(fqon: String, workspace: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => hardDeleteMetaResource(id, ResourceIds.Domain)
      case None => OrgNotFound(fqon)
    }
  }
  
  
  /**
   * Permanently delete an Org from Security and Meta
   */
  def hardDeleteOrg(org: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    trace(s"hardeDeleteOrg($org)")
    hardDeleteSecure(org, request.identity, Security.deleteOrg)    
  }
  
  /**
   * 
   */
  def hardDeleteOrgFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"hardDeleteOrgFqon($fqon)")
    orgFqon(fqon) match {
      case Some(org) => hardDeleteSecure(org.id, request.identity, Security.deleteOrg)
      case None      => OrgNotFound(fqon)
    }    
  }
  
  /**
   * Permanently delete a User/Account from Security and Meta
   */
  def hardDeleteUser(org: UUID, id: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    trace(s"hardDeleteUser(org = $org, user = $id")
    hardDeleteSecure(id, request.identity, Security.deleteAccount)
  }
  
  /**
   * 
   */
  def hardDeleteUserFqon(fqon: String, id: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"hardDeleteUserFqon($fqon, user = $id")
    orgFqon(fqon) match {
      case Some(org) => hardDeleteSecure(id, request.identity, Security.deleteAccount)
      case None      => OrgNotFound(fqon)
    }
  }
  
  /**
   * Permanently delete a ResourceType along with any associated TypeProperties
   */
   def hardDeleteResourceTypeFqon(fqon: String, id: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
     trace(s"hardDeleteResourceTypeFqon($fqon, $id)")
     orgFqon(fqon) match {
       case Some(org) => hardDeleteResourceType(id)
       case None      => OrgNotFound(fqon)
     }
   }
   
   def hardDeleteResourceType(typeId: UUID) = {
     TypeFactory.hardDeleteType(typeId) match {
       case Success(_) => NoContent
       case Failure(e) => e match {
         case iae : IllegalArgumentException => BadRequestResult(iae.getMessage)
         case x => GenericErrorResult(500, x.getMessage)
       }
     }
   }
   
  /**
   * Permanently delete a Resource from Meta and Security
   */   
  def hardDeleteSecure(id: UUID, auth: AuthAccountWithCreds, fn: SecurityDelete) = {
    
    hardDeleteSynchronized(id, auth, fn) match {
      case Success(_) => NoContent
      case Failure(e) => {
        log.error(s"hardDeleteSecure: ERROR: " + e.getMessage)
        e match {
          case s: SecurityRESTException     => handleSecurityApiException(s)
          case r: ResourceNotFoundException => NotFoundResult(r.getMessage)
          case _ => GenericErrorResult(500, e.getMessage)
        }
      }
    }
  }
  
  /**
   * 
   */
  def hardDeleteEnvironment(org: UUID, id: UUID) = Authenticate(org) { implicit request =>
    ???
  }
  
  
  // TODO: Simpler since resources aren't synced with security.
  private def hardDeleteMetaResource(id: UUID, typeId: UUID) = {
    ResourceFactory.hardDeleteResource(typeId, id) match {
      case Success(_) => NoContent
      case Failure(e) => {
        log.error(s"hardDeleteMetaResource($id, $typeId)")
        HandleRepositoryExceptions(e)
      }
    }
  }
    
}

