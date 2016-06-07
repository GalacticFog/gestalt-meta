package controllers

import java.util.UUID
import java.net.URL
import play.api.http.HttpVerbs
import play.api.libs.ws.WS
import play.api.Play.current

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.PropertyValidator
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.ResourceType
import com.galacticfog.gestalt.data.illegal
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.{ PatchOp, PatchDocument, PatchHandler }
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.{ GestaltResource => SecurityResource }
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.galacticfog.gestalt.tasks.play.io.NonLoggingTaskEvents
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import controllers.util.JsonUtil._
import controllers.util.db._
import controllers.util.MetaController
import controllers.util.Security
import play.api.{ Logger => log }
import play.api.libs.json._
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import controllers.util.stringmap
import controllers.util.trace
import com.galacticfog.gestalt.meta.api._
import play.api.mvc.Result
import com.galacticfog.gestalt.laser._
import  com.galacticfog.gestalt.security.api.json.JsonImports.linkFormat


object AuthorizationController extends MetaController with NonLoggingTaskEvents {
  
  def postEntitlementOrgFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    val org = fqid(fqon)
    postEntitlementCommon(org, ResourceIds.Org, org)
  }
  
  def postEntitlementFqon(fqon: String, typeId: String, resourceId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    postEntitlementCommon(fqid(fqon), typeId, resourceId)
  }
   
  def deleteEntitlementOrgFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    val org = fqid(fqon)
    deleteEntitlementCommon(org, org, id)
  }
  
  def deleteEntitlementFqon(fqon: String, typeId: String, resourceId: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    deleteEntitlementCommon(fqid(fqon), resourceId, id)
  }
  
  private [controllers] def deleteEntitlementCommon(org: UUID, parent: UUID, id: UUID) = {
    ResourceFactory.findChildOfType(ResourceIds.Entitlement, parent, id) match {
      case None => NotFoundResult(s"Entitlement with ID '$id' not found.")
      case Some(res) => {
        ResourceFactory.hardDeleteResource(ResourceIds.Entitlement, id) match {
          case Success(_) => NoContent
          case Failure(e) => HandleExceptions(e)
        }
      }
    }
  }
  
  def patchEntitlementFqon(fqon: String, typeId: String, resourceId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    ???
  }
  
  def getEntitlementsOrgFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    val org = fqid(fqon)
    //getEntitlementsCommon(org, ResourceIds.Org, org)
    val ents = ResourceFactory.findEffectiveEntitlements(org)
    handleExpansion(entitlementsAll(ents), request.queryString, META_URL)
  }
  
  def getEntitlementsFqon(fqon: String, typeId: String, resourceId: UUID) = Authenticate(fqon) { implicit request =>
    getEntitlementsCommon(fqid(fqon), typeId, resourceId)
  }
  
  def getEntitlementByIdOrgFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    ResourceFactory.findById(ResourceIds.Entitlement, id) match {
      case Some(res) => Ok(Output.renderInstance(res, META_URL))
      case None => NotFoundResult(request.uri)
    }
  }
  
  def getEntitlementByIdFqon(fqon: String, typeId: String, resourceId: UUID, id: UUID) = Authenticate(fqon) { implicit request =>  
    ???
  }
  
//  ***
//  
//  Write rollup for entitlements and variables
//  
//  ***

  private[controllers] def postEntitlementCommon(org: UUID, typeId: UUID, resourceId: UUID)(
      implicit request: SecuredRequest[JsValue]) = Future {
    
    // This is the resource we're creating the Entitlement for.
    val parentResource = ResourceFactory.findById(typeId, resourceId)
    
    parentResource match {
      case None => NotFoundResult(request.uri)
      case Some(_) => {
        
        validateEntitlementPayload(request.body) match {
          case Failure(e) => HandleExceptions(e)
          case Success(_) => CreateResourceResult(org, ResourceIds.Entitlement, resourceId)
        }
      }
    }
  }
  
  
//  Implement type-hierarchy:
//  Resource->Runnable->ResourceContainer->{Org, Workspace}
//  Resource->Runnable->{Container,Lambda}
  
  private[controllers] def validateEntitlementPayload(payload: JsValue): Try[Unit] = Try {
    /*
     * Validations:
     * 1.) Caller has SetEntitlements permission
     * 2.) Action is valid
     * 3.) Entitlement for given action DOES NOT already exist
     * 4.) Given identities ALL exist
     */
    log.warn("AuthorizationController::validateEntitlementPayload() IS NOT IMPLEMENTED!!!")
  }
  
  
  private[controllers] def getEntitlementByIdCommon(org: UUID, typeId: UUID, resourceId: UUID, id: UUID)(implicit request: SecuredRequest[_]) = {
    ResourceFactory.findChildOfType(ResourceIds.Entitlement, resourceId, id) match {
      case Some(res) => Ok(Output.renderInstance(res, META_URL))
      case None => NotFoundResult(request.uri)
    }
  }
  
  
  private def entitlementsAll(es: Map[Int, Seq[GestaltResourceInstance]]): Seq[GestaltResourceInstance] = {
    es flatMap { case (_,v) => v } toSeq
  }  
  
  private def getEntitlementsCommon(org: UUID, typeId: UUID, resourceId: UUID)(implicit request: SecuredRequest[_]) = {
    handleExpansion(ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, resourceId),
        request.queryString, META_URL)    
  }
  
  
}

