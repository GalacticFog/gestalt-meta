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
import com.galacticfog.gestalt.meta.api.sdk.{ ResourceLink => GestaltLink }

import com.galacticfog.gestalt.meta.services.ResourceQueryService

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

import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.meta.api.output._

import com.galacticfog.gestalt.security.api.{ GestaltResource => SecuredResource }
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._


object PropertyController extends Authorization {
  //
  // RESOURCE_TYPE_PROPERTIES
  //
  
  /* GET /{org}/resourcetypes/:uuid/typeproperties */
  
  def getAllProperties(org: UUID) = Authenticate(org) { implicit request =>
    Ok(Output.renderPropertyLinks(PropertyFactory.findAllByOrg(org)))
  }
  
  def getAllPropertiesFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => Ok(Output.renderPropertyLinks(PropertyFactory.findAllByOrg(org.id))) 
      case None => OrgNotFound(fqon)
    }
  }
  
  def getAllPropertiesByType(org: UUID, typeId: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    trace(s"getAllProperties($org, $typeId)")
    Ok(Output.renderPropertyLinks(PropertyFactory.findAll(typeId, org)))
  }
  
  def getAllPropertiesByTypeFqon(fqon: String, typeId: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"getAllPropertiesFqon($fqon, $typeId)")
    orgFqon(fqon) match {
      case Some(org) => Ok(Output.renderPropertyLinks(PropertyFactory.findAll(typeId, org.id)))
      case None      => NotFoundResult(Errors.ORG_NOT_FOUND(fqon))
    }
  }
  
  
  /* GET /orgs/:uuid/typeproperties */
  
  def getTypePropertyById(org: UUID, id: UUID) = Authenticate(org) { implicit request =>
    trace(s"getTypePropertyById($org, $id)")
    OkNotFoundProperty(id)
  }  
  
//  def getTypePropertyByIdFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
//    trace(s"getTypePropertyByIdFqon($fqon, $id)")
//    OkNotFoundProperty(id)
//  }  
//  
  def getTypePropertyByIdFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    trace(s"getTypePropertyByIdFqon($fqon, $id)")
    OkNotFoundProperty(id)
  }
  /*
   * TODO: The 'typeId' arg is unused at the moment - this should be used to validate that the requested
   * property is actually a member of the parent resource_type (as it is, you could get any valid property
   * from this URL, even if it was from another resource_type - this is misleading).
   */
  def getPropertyById(org: UUID, typeId: UUID, id: UUID) = Authenticate(org) { implicit request =>
    trace(s"getPropertyById($org, $id)")
    OkNotFoundProperty(id)
  }
  
  def getPropertyByIdFqon(fqon: String, typeId: UUID, id: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"getPropertyByIdFqon($fqon, $id)")
    orgFqon(fqon) match {
      case Some(org) => OkNotFoundProperty(id)//Ok(renderProperty(PropertyFactory.findById(ResourceIds.TypeProperty, id)))
      case None      => OrgNotFound(fqon)
    }
  }

  
  /* POST /{org}/resourcetypes/:uuid/typeproperties */
  
  /** 
   * Create a new TypeProperty on the given ResourceType 
   * 
   * TODO: applies_to is an Option in PropertyInput - when the property is created directly
   * (independent of the type it applies to) applies_to MUST be specified.  Enforce that here.
   */
  def createTypeProperty(org: UUID, typeId: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    trace(s"createTypeProperty($org, $typeId)")
    CreateTypePropertyResult(org, typeId, request.body)
  }

  
  def createTypePropertyFqon(fqon: String, typeId: UUID) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => CreateTypePropertyResult(org.id, typeId, request.body)
      case None => Future { OrgNotFound(fqon) }
    }
  }
  
  private def CreateTypePropertyResult[T](org: UUID, typeId: UUID, propertyJson: JsValue)(implicit request: SecuredRequest[T]) = {
    Future {
      val user = request.identity.account.id
      
      safeGetPropertyJson(propertyJson) match {
        case Failure(e) => BadRequestResult(e.getMessage)
        case Success(input) => propertyFromInput(org, user, input) match {
          case Success(d) => PropertyFactory.create(request.identity.account.id)(d) match {
            case Success(p) => Ok(Output.renderTypePropertyOutput(p))
            case Failure(e) => GenericErrorResult(500, e.getMessage)
          }
          case Failure(e) => BadRequest(s"Could not read input: " + e.getMessage)
        }
      }
    }
  } 
  
  
  def deleteTypeProperty(org: UUID, typeId: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    ???
  }

  
  /** Find a TypeProperty by ID or 404 Not Found */
  private def OkNotFoundProperty(id: UUID) = {
    PropertyFactory.findById(ResourceIds.TypeProperty, id) match {
      case Some(property) => Ok(Output.renderTypePropertyOutput(property))
      case None => NotFoundResult(Errors.PROPERTY_NOT_FOUND(id))
    }
  }

  /** Convert GestaltTypePropertyInput to GestaltTypeProperty */
  protected[controllers] def propertyFromInput(org: UUID, owner: UUID, r: GestaltTypePropertyInput, appliesToType: Option[UUID] = None): Try[GestaltTypeProperty] = {
    val ownerLink = ResourceOwnerLink(ResourceIds.User, owner)
    Try {
      
      val applies = if (appliesToType.isDefined) appliesToType.get
        else {
          if (r.applies_to.isDefined) r.applies_to.get
          else {
            throw new BadRequestException("No value found for TypeProperty -> applies_to")
          }
        }
      
      GestaltTypeProperty(
        id = r.id.getOrElse(UUID.randomUUID), 
        typeId = ResourceIds.TypeProperty, 
        state = ResourceState.id(r.resource_state.getOrElse(ResourceStates.Active)), 
        orgId = org, 
        owner = ownerLink, 
        name = r.name, 
        description = r.description, 
        created = None, 
        modified = None,
        appliesTo = applies, 
        datatype = DataType.id(r.data_type), 
        defaultValue = r.default_value, 
        isSealed = r.is_sealed.getOrElse(false), 
        isSystem = r.is_system.getOrElse(false), 
        requirementType = RequirementType.id(r.requirement_type),
        visibilityType = VisibilityType.id(r.visibility_type.getOrElse("plain")), 
        refersTo = r.refers_to, 
        properties = r.properties, variables = r.variables, tags = r.tags, auth = r.auth)
    }
  }  
  
  
  /*
   * TODO: Get rid of all of this rendering code!!!  
   */
  
//  def renderProperty(r: GestaltTypeProperty) = Json.prettyPrint(Json.toJson(r))
//  def renderProperties(rs: GestaltTypeProperty*) = Json.prettyPrint(Json.toJson(rs))
//  def toPropertyLink(r: GestaltTypeProperty) = {
//    GestaltLink(r.typeId, r.id.toString, Some(r.name), Some(toPropertyHref( r )))
//  }
//  def renderPropertyLinks(rs: Seq[GestaltTypeProperty]) = {
//    Json.prettyPrint(Json.toJson(rs map { toPropertyLink(_) }))
//  }
  
  private def safeGetPropertyJson(json: JsValue): Try[GestaltTypePropertyInput] = Try {
    json.validate[GestaltTypePropertyInput].map{
      case resource: GestaltTypePropertyInput => resource
    }.recoverTotal{
      e => throw new BadRequestException(JsError.toFlatJson(e).toString)
    }      
  }
  
}