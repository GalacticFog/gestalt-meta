package controllers

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.data.DataType
import com.galacticfog.gestalt.data.PropertyFactory
import com.galacticfog.gestalt.data.RequirementType
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.data.VisibilityType
import com.galacticfog.gestalt.data.models.GestaltTypeProperty
import com.galacticfog.gestalt.data.session
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.GestaltTypePropertyInput
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import com.galacticfog.gestalt.meta.api.sdk.ResourceStates
import com.galacticfog.gestalt.meta.api.sdk.gestaltTypePropertyInputFormat
import com.galacticfog.gestalt.meta.auth.Authorization

import controllers.util.BadRequestResult
import controllers.util.Errors
import controllers.util.GenericErrorResult
import controllers.util.NotFoundResult
import controllers.util.trace
import play.api.libs.json.JsError
import play.api.libs.json.JsValue


object PropertyController extends Authorization {
  
  def getAllPropertiesFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => Ok(Output.renderPropertyLinks(PropertyFactory.findAllByOrg(org.id))) 
      case None => OrgNotFound(fqon)
    }
  }
  
  def getAllPropertiesByTypeFqon(fqon: String, typeId: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"getAllPropertiesFqon($fqon, $typeId)")
    orgFqon(fqon) match {
      case Some(org) => Ok(Output.renderPropertyLinks(PropertyFactory.findAll(typeId, org.id)))
      case None      => NotFoundResult(Errors.ORG_NOT_FOUND(fqon))
    }
  }
  
  def getTypePropertyByIdFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    trace(s"getTypePropertyByIdFqon($fqon, $id)")
    OkNotFoundProperty(id)
  }
  /*
   * TODO: The 'typeId' arg is unused at the moment - this should be used to validate that the requested
   * property is actually a member of the parent resource_type (as it is, you could get any valid property
   * from this URL, even if it was from another resource_type - this is misleading).
   */
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

  private def safeGetPropertyJson(json: JsValue): Try[GestaltTypePropertyInput] = Try {
    json.validate[GestaltTypePropertyInput].map{
      case resource: GestaltTypePropertyInput => resource
    }.recoverTotal{
      e => throw new BadRequestException(JsError.toFlatJson(e).toString)
    }      
  }
  
}