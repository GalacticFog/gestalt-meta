package controllers


import play.api.{ Logger => log }

import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
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
import com.galacticfog.gestalt.meta.api.sdk.{ ResourceLink => GestaltLink }

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

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.meta.api.output._

import com.galacticfog.gestalt.security.api.{ GestaltResource => SecuredResource }
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._

object TypeController extends GestaltFrameworkSecuredController[DummyAuthenticator]
  with MetaController with NonLoggingTaskEvents {
  

  def getAllResourceTypes(org: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    trace(s"getAllResourceTypes($org)")
    Ok(renderTypeLinks(TypeFactory.findAll(ResourceIds.ResourceType, org)))
  }
  
  def getAllResourceTypesFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"getAllResourceTypeFqon($fqon)")
    orgFqon(fqon) match {
      case Some(org) => OkTypeLinksResult(org.id)
      case None => NotFoundResult(Errors.ORG_NOT_FOUND(fqon))
    }
  }
  
  def getResourceTypeById(org: UUID, id: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    trace(s"getResourceTypeById($org, $id)")
    OkTypeByIdResult(org, id)
  }
  
  def getResourceTypeByIdFqon(fqon: String, id: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"getResourceTypeByIdFqon($fqon, $id)")
    orgFqon(fqon) match {
      case Some(org) => OkTypeByIdResult(org.id, id)
      case None => NotFoundResult(Errors.TYPE_NOT_FOUND(id))
    }
  }
  
  def createResourceType(org: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    trace(s"createResourceType($org)")
    CreateTypeWithPropertiesResult(org, request.body)
  }
  
  def createResourceTypeFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    trace(s"createResourceTypeFqon($fqon)")
    orgFqon(fqon) match {
      case Some(org) => CreateTypeWithPropertiesResult(org.id, request.body)
      case None      => Future { OrgNotFound(fqon) }
    }
  }

  private def CreateTypeWithPropertiesResult[T](org: UUID, typeJson: JsValue)(implicit request: SecuredRequest[T]) = {
    Future {
      createTypeWithProperties(org, typeJson) match {
        case Failure(e) => GenericErrorResult(500, e.getMessage)
        case Success(newtype) => Ok(Output.renderResourceTypeOutput( newtype ))
      }
    }
  }
  
  /**
   * Decompose a GestaltResourceTypeInput into a tuple2 where:
   * _1 : A GestaltResourceType
   * _2 : An Option[Seq[GestaltTypeProperty]]
   */
  private def deconstructType(org: UUID, owner: UUID, typeJson: JsValue)(): 
      (GestaltResourceType, Option[Seq[GestaltTypeProperty]]) = {    
    
    val input: GestaltResourceTypeInput = safeGetTypeJson(typeJson).get

    val domain: GestaltResourceType = typeFromInput(org, owner, input).get

    val definitions: Option[Seq[GestaltTypeProperty]] = 
      input.property_defs map { ps => ps map { p => 
        PropertyController.propertyFromInput(org, owner, p.copy(applies_to = Some(domain.id))).get } 
    }
    
    (domain, definitions)
  }

  
  private def createTypeWithProperties[T](org: UUID, typeJson: JsValue)(implicit request: SecuredRequest[T]) = {
    trace(s"createTypeWithProperties($org, [json])")
    
    Try {
      val owner = request.identity.account.id
      
      log.debug("Converting input types to domain...")
      val (domain, propdefs) = deconstructType(org, owner, typeJson)
      
      log.debug("Creating new ResourceType...")
      val newtype = TypeFactory.create(owner)(domain).get
      
      log.debug("Checking for TypeProperties...")
      if (propdefs.isDefined) {
        for (prop <- propdefs.get) {
          log.debug(s"Creating TypeProperty : ${prop.name}")
          PropertyFactory.create(owner)(prop).get
        }
      }
      trace("createTypeWithProperties => COMPLETE.")
      newtype
    }
  }
  
  
  /* DELETE /orgs/:uuid/resourcetypes/:uuid */
  def deleteResourceTypeById(org: UUID, id: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    ???    
  }  

  
  /** Get a list of ResourceTypes */
  private def OkTypeLinksResult(org: UUID) = {
    Ok(renderTypeLinks(TypeFactory.findAll(ResourceIds.ResourceType, org)))
  }  
  
  /** Get a single ResourceType by ID */
  private def OkTypeByIdResult(org: UUID, id: UUID) = {
    TypeFactory.findById(id) match {
      case Some(t) => Ok(Output.renderResourceTypeOutput(t))
      case None    => NotFoundResult(Errors.TYPE_NOT_FOUND(id.toString))
    }
  }
  
  /** Convert GestaltResourceTypeInut to GestaltResourceType */
  private def typeFromInput(org: UUID, owner: UUID, r: GestaltResourceTypeInput) = Try {
    val ownerLink = ResourceOwnerLink(ResourceIds.User, owner)
    GestaltResourceType(
        id = r.id.getOrElse(UUID.randomUUID),
        typeId = ResourceIds.ResourceType,
        extend = r.extend,
        state = r.resource_state.getOrElse(ResourceState.id(ResourceStates.Active)),
        orgId = org,
        owner = ownerLink,
        name = r.name,
        description = r.description,
        created = None, modified = None,
        properties = r.properties, 
        variables = r.variables, 
        tags = r.tags, 
        auth = r.auth)
  }
  
  def renderTypeLinks(rs: Seq[GestaltResourceType]) = {
    Json.prettyPrint(Json.toJson(rs map { toTypeLink(_) }))
  }
  
  def toTypeLink(r: GestaltResourceType) = {
    GestaltLink(r.typeId, r.id.toString, Some(r.name), Some(toHref( r )))
  }

  def toHref(r: GestaltResourceType) = {
    "http://dummy_host/orgs/%s/%s/%s".format(r.orgId, "{typename}", r.id)
  }
  
  def toPropertyHref(r: GestaltTypeProperty) = {
    "http://dummy_host/orgs/%s/%s/%s".format(r.orgId, "{typename}", r.id)
  }
  
  private def safeGetTypeJson(json: JsValue): Try[GestaltResourceTypeInput] = Try {
    json.validate[GestaltResourceTypeInput].map{
      case resource: GestaltResourceTypeInput => resource
    }.recoverTotal{
      e => throw new BadRequestException(JsError.toFlatJson(e).toString)
    }
  }
  


}
