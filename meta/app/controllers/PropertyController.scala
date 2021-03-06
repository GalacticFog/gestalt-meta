package controllers


import java.util.UUID

import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.data.{DataType, PropertyFactory, RequirementType, ResourceState, TypeFactory, VisibilityType, session, uuid2string}
import com.galacticfog.gestalt.data.models.GestaltTypeProperty
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.GestaltTypePropertyInput
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import com.galacticfog.gestalt.meta.api.sdk.ResourceStates
import com.galacticfog.gestalt.meta.api.sdk.gestaltTypePropertyInputFormat
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.{GestaltFrameworkSecurity, GestaltFrameworkSecurityEnvironment}
import com.google.inject.Inject
import controllers.util._
import play.api.i18n.MessagesApi
import play.api.libs.json._
import com.galacticfog.gestalt.json.Js
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import javax.inject.Singleton
import play.api.mvc.RequestHeader

@Singleton
class PropertyController @Inject()(messagesApi: MessagesApi,
                                   sec: GestaltFrameworkSecurity)
  extends SecureController(messagesApi = messagesApi, sec = sec) with Authorization {

  import PropertyController._
  
  def getAllPropertiesFqon(fqon: String) = Audited(fqon) { implicit request =>
    Ok(Output.renderPropertyLinks(PropertyFactory.findAllByOrg(fqid(fqon)))) 
  }
  
  def getAllPropertiesByTypeFqon(fqon: String, typeId: UUID) = Audited(fqon) { implicit request =>
    Ok(Output.renderPropertyLinks(PropertyFactory.findAll(typeId, fqid(fqon))))
  }
  
  def getTypePropertyByIdFqon(fqon: String, id: UUID) = Audited(fqon) { implicit request =>
    OkNotFoundProperty(id)
  }
  
  /*
   * TODO: The 'typeId' arg is unused at the moment - this should be used to validate that the requested
   * property is actually a member of the parent resource_type (as it is, you could get any valid property
   * from this URL, even if it was from another resource_type - this is misleading).
   */
  def getPropertyByIdFqon(fqon: String, typeId: UUID, id: UUID) = Audited(fqon) { implicit request =>
    OkNotFoundProperty(id)
  }

  /* POST /{org}/resourcetypes/:uuid/typeproperties */
  
  /** 
   * Create a new TypeProperty on the given ResourceType 
   * 
   * TODO: applies_to is an Option in PropertyInput - when the property is created directly
   * (independent of the type it applies to) applies_to MUST be specified.  Enforce that here.
   */
  def createTypePropertyFqon(fqon: String, typeId: UUID) = AsyncAudited(fqon) { implicit request =>
    CreateTypePropertyResult(fqid(fqon), typeId, request.body.as[JsObject])
  }
  
  private def CreateTypePropertyResult[T](org: UUID, typeId: UUID, propertyJson: JsObject)(implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,T]) = {
    Future {
      val user = request.identity.account.id

      val property = for {
        in <- safeGetPropertyJson(normalizeInputProperty(typeId, propertyJson))
        p1 <- propertyFromInput(org, user, in) // in-memory
        p2 <- PropertyFactory.create(user)(p1) // persisted
      } yield p2
      
      property match {
        case Failure(e) => HandleExceptions(e)
        case Success(r) => Ok(Output.renderTypePropertyOutput(r))
      }
    }
  } 

  /**
   * Find a TypeProperty by ID or 404 Not Found 
   */
  private def OkNotFoundProperty(id: UUID)(implicit request: RequestHeader) = {
    PropertyFactory.findById(ResourceIds.TypeProperty, id).fold {
      NotFoundResult(Errors.PROPERTY_NOT_FOUND(id))
    }{ property =>
      Ok(Output.renderTypePropertyOutput(property, Some(META_URL)))
    }
  }

  private def safeGetPropertyJson(json: JsValue): Try[GestaltTypePropertyInput] = {
    json.validate[GestaltTypePropertyInput] match {
      case JsSuccess(resource, _) => Success(resource)
      case e: JsError => Failure(
        new BadRequestException(Js.errorString(e))
      )
    }
  }
  
}

object PropertyController {

  /**
    * Convert GestaltTypePropertyInput to GestaltTypeProperty
    */
  protected[controllers] def propertyFromInput(
                                                org: UUID,
                                                owner: UUID,
                                                input: GestaltTypePropertyInput): Try[GestaltTypeProperty] = Try {

    val ownerLink = ResourceOwnerLink(ResourceIds.User, owner)

    GestaltTypeProperty(
      id = input.id.getOrElse(UUID.randomUUID),
      typeId = ResourceIds.TypeProperty,
      state = ResourceState.id(input.resource_state.getOrElse(ResourceStates.Active)),
      orgId = org,
      owner = ownerLink,
      name = input.name,
      description = input.description,
      created = None,
      modified = None,
      appliesTo = input.applies_to.get,
      datatype = DataType.id(input.data_type),
      defaultValue = input.default_value,
      isSealed = input.is_sealed.getOrElse(false),
      isSystem = input.is_system.getOrElse(false),
      requirementType = RequirementType.id(input.requirement_type),
      visibilityType = VisibilityType.id(input.visibility_type.getOrElse("plain")),
      refersTo = input.refers_to,
      properties = input.properties, variables = input.variables, tags = input.tags, auth = input.auth)

  }

  /**
    * Ensure we have a valid 'applies_to' value in the property input JSON.
    * Also, optionally convert 'refers_to' from resource type name to resource type id
    */
  def normalizeInputProperty(parentTypeId: UUID, propertyJson: JsObject): JsObject = {

    def validateUUIDRefersTo(id: UUID): JsResult[UUID] = {
      TypeFactory.findById(id) match {
        case Some(_) => JsSuccess(id)
        case None => JsError(JsPath \ "refers_to", "error.expected.existing-resource-type-id")
      }
    }

    def validateStringRefersTo(name: String): JsResult[UUID] = {
      TypeFactory.findByName(name) match {
        case Some(tpe) => JsSuccess(tpe.id)
        case None => JsError(JsPath \ "refers_to", "error.expected.existing-resource-type-name")
      }
    }

    val appliesTo = Js.find(propertyJson, "/applies_to").map(_.validate[UUID]) match {
      case None =>
        Json.obj("applies_to" -> parentTypeId)
      case Some(JsSuccess(tid, _)) if tid == parentTypeId =>
        Json.obj("applies_to" -> parentTypeId)
      case _ =>
        throw new BadRequestException(s"Given 'applies_to' ID does not match parent resource type ID.")
    }
    val maybeRefersTo = Js.find(propertyJson, "/refers_to").map {
      nameOrUUID =>
        nameOrUUID.validate[UUID] match {
          case JsSuccess(id, _) =>
            // if it parses as a UUID, it must refer to a valid resource type id
            validateUUIDRefersTo(id)
          case _ =>
            // otherwise, must be a string which refers to a valid resource type name
            nameOrUUID.validate[String] match {
              case JsSuccess(name,_) => validateStringRefersTo(name)
              case e: JsError => JsError(JsPath \ "refers_to", "error.expected.uuid-type-id-or-string-type-name")
            }
        }
    }

    maybeRefersTo match {
      case Some(JsError(errs)) =>
        throw new BadRequestException(Js.errorString(errs))
      case Some(JsSuccess(refersTo, _)) =>
        propertyJson ++ appliesTo ++ Json.obj("refers_to" -> refersTo)
      case None =>
        propertyJson ++ appliesTo
    }
  }

}