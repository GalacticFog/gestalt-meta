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
import com.galacticfog.gestalt.data.models.{ ResourceLink => GestaltLink }

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

import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.meta.api.output._

import com.galacticfog.gestalt.security.api.{ GestaltResource => SecuredResource }


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
      case None => NotFound(toError(404, Errors.ORG_NOT_FOUND(fqon)))
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
      case None => NotFound(toError(404, Errors.TYPE_NOT_FOUND(id)))
    }
  }
  
  def createResourceType(org: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    trace(s"createResourceType($org)")
    CreateTypeResult(org, request.body)
  }
  
  def createResourceTypeFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    trace(s"createResourceTypeFqon($fqon)")
    orgFqon(fqon) match {
      case Some(org) => CreateTypeWithPropertiesResult(org.id, request.body)
      case None      => Future { OrgNotFound(fqon) }
    }
  }
  
  
  /** Create ResourceType, return Play Result */
  private def CreateTypeResult[T](org: UUID, typeJson: JsValue)(implicit request: SecuredRequest[T]) = {
    Future {
      val user = request.identity.account.id
      
      safeGetTypeJson(typeJson) match {
        case Failure(e) => BadRequest(toError(400, e.getMessage))
        case Success(input) => typeFromInput(org, user, input) match {
          case Success(resource) => TypeFactory.create(user)(resource) match {
            case Success(t) => Ok(renderType(t))
            case Failure(e) => InternalServerError(toError(500, e.getMessage))
          }
          case Failure(e) => BadRequest(toError(400, "Could not read input: " + e.getMessage))
        }
      }
    }
  }

  private def CreateTypeWithPropertiesResult[T](org: UUID, typeJson: JsValue)(implicit request: SecuredRequest[T]) = {
    Future {
      createTypeWithProperties(org, typeJson) match {
        case Failure(e) => InternalServerError(toError(500, e.getMessage))
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
      case None    => NotFound(toError(404, Errors.TYPE_NOT_FOUND(id.toString)))
    }
  }
  
  
//  //
//  // RESOURCE_TYPE_PROPERTIES
//  //
//  
//  
//  /* GET /{org}/resourcetypes/:uuid/typeproperties */
//  
//  def getAllProperties(org: UUID, typeId: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
//    trace(s"getAllProperties($org, $typeId)")
//    Ok(renderPropertyLinks(PropertyFactory.findAll(typeId, org)))
//  } 
//  
//  def getAllPropertiesFqon(fqon: String, typeId: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
//    trace(s"getAllPropertiesFqon($fqon, $typeId)")
//    orgFqon(fqon) match {
//      case Some(org) => Ok(renderPropertyLinks(PropertyFactory.findAll(typeId, org.id)))
//      case None      => NotFound(toError(404, Errors.ORG_NOT_FOUND(fqon)))
//    }
//  }
//
//  
//  /* GET /orgs/:uuid/typeproperties */
//  
//  def getPropertyById(org: UUID, typeId: UUID, id: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
//    trace(s"getPropertyById($org, $id)")
//    OkNotFoundProperty(id)
//  }
//  
//  def getPropertyByIdFqon(fqon: String, typeId: UUID, id: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
//    trace(s"getPropertyByIdFqon($fqon, $id)")
//    orgFqon(fqon) match {
//      case Some(org) => OkNotFoundProperty(id)//Ok(renderProperty(PropertyFactory.findById(ResourceIds.TypeProperty, id)))
//      case None      => OrgNotFound(fqon)
//    }
//  }
//
//  
//  /* POST /{org}/resourcetypes/:uuid/typeproperties */
//  
//  /** 
//   * Create a new TypeProperty on the given ResourceType 
//   * 
//   * TODO: applies_to is an Option in PropertyInput - when the property is created directly
//   * (independent of the type it applies to) applies_to MUST be specified.  Enforce that here.
//   */
//  def createTypeProperty(org: UUID, typeId: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
//    trace(s"createTypeProperty($org, $typeId)")
//    CreateTypePropertyResult(org, typeId, request.body)
//  }
//
//  
//  def createTypePropertyFqon(fqon: String, typeId: UUID) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
//    orgFqon(fqon) match {
//      case Some(org) => CreateTypePropertyResult(org.id, typeId, request.body)
//      case None => Future { OrgNotFound(fqon) }
//    }
//  }
//  
//  private def CreateTypePropertyResult[T](org: UUID, typeId: UUID, propertyJson: JsValue)(implicit request: SecuredRequest[T]) = {
//    Future {
//      val user = request.identity.account.id
//      
//      safeGetPropertyJson(propertyJson) match {
//        case Failure(e) => BadRequest(toError(400,e.getMessage))
//        case Success(input) => propertyFromInput(org, user, input) match {
//          case Success(d) => PropertyFactory.create(request.identity.account.id)(d) match {
//            case Success(p) => Ok(renderProperty(p))
//            case Failure(e) => InternalServerError(toError(500, e.getMessage))
//          }
//          case Failure(e) => BadRequest(s"Could not read input: " + e.getMessage)
//        }
//      }
//    }
//  } 
//  
//  
//  def deleteTypeProperty(org: UUID, typeId: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
//    ???
//  }
//
//  
//  /** Find a TypeProperty by ID or 404 Not Found */
//  private def OkNotFoundProperty(id: UUID) = {
//    PropertyFactory.findById(ResourceIds.TypeProperty, id) match {
//      case Some(property) => Ok(Output.renderTypePropertyOutput(property))
//      case None => NotFound(toError(404, Errors.PROPERTY_NOT_FOUND(id)))
//    }
//  }

  
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

//  /** Convert GestaltTypePropertyInput to GestaltTypeProperty */
//  private def propertyFromInput(org: UUID, owner: UUID, r: GestaltTypePropertyInput, appliesToType: Option[UUID] = None): Try[GestaltTypeProperty] = {
//    val ownerLink = ResourceOwnerLink(ResourceIds.User, owner)
//    Try {
//      
//      val applies = if (appliesToType.isDefined) appliesToType.get
//        else {
//          if (r.applies_to.isDefined) r.applies_to.get
//          else {
//            illegal("No value found for TypeProperty -> applies_to")
//          }
//        }
//      
//      GestaltTypeProperty(
//        id = r.id.getOrElse(UUID.randomUUID), 
//        typeId = ResourceIds.TypeProperty, 
//        state = ResourceState.id(r.resource_state.getOrElse(ResourceStates.Active)), 
//        orgId = org, 
//        owner = ownerLink, 
//        name = r.name, 
//        description = r.description, 
//        created = None, 
//        modified = None,
//        appliesTo = applies, 
//        datatype = DataType.id(r.data_type), 
//        defaultValue = r.default_value, 
//        isSealed = r.is_sealed.getOrElse(false), 
//        isSystem = r.is_system.getOrElse(false), 
//        requirementType = RequirementType.id(r.requirement_type),
//        visibilityType = VisibilityType.id(r.visibility_type.getOrElse("plain")), 
//        refersTo = r.refers_to, 
//        properties = r.properties, variables = r.variables, tags = r.tags, auth = r.auth)
//    }
//  }
  
  


  
  def renderType(r: GestaltResourceType) = Json.prettyPrint(Json.toJson(r))
  def renderTypes(rs: GestaltResourceType*) = Json.prettyPrint(Json.toJson(rs))  
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
      e => illegal(JsError.toFlatJson(e).toString)
    }
  }
  


}
