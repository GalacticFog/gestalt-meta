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
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import play.api.i18n.MessagesApi
import play.api.libs.json._
import com.galacticfog.gestalt.json.Js
import javax.inject.Singleton

@Singleton
class PropertyController @Inject()(messagesApi: MessagesApi,
                                   env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {

  import PropertyController._
  
  def getAllPropertiesFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    Ok(Output.renderPropertyLinks(PropertyFactory.findAllByOrg(fqid(fqon)))) 
  }
  
  def getAllPropertiesByTypeFqon(fqon: String, typeId: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    Ok(Output.renderPropertyLinks(PropertyFactory.findAll(typeId, fqid(fqon))))
  }
  
  def getTypePropertyByIdFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    OkNotFoundProperty(id)
  }
  
  /*
   * TODO: The 'typeId' arg is unused at the moment - this should be used to validate that the requested
   * property is actually a member of the parent resource_type (as it is, you could get any valid property
   * from this URL, even if it was from another resource_type - this is misleading).
   */
  def getPropertyByIdFqon(fqon: String, typeId: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    OkNotFoundProperty(id)
  }

  /* POST /{org}/resourcetypes/:uuid/typeproperties */
  
  /** 
   * Create a new TypeProperty on the given ResourceType 
   * 
   * TODO: applies_to is an Option in PropertyInput - when the property is created directly
   * (independent of the type it applies to) applies_to MUST be specified.  Enforce that here.
   */
  def createTypePropertyFqon(fqon: String, typeId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    CreateTypePropertyResult(fqid(fqon), typeId, request.body.as[JsObject])
  }
  
  private def CreateTypePropertyResult[T](org: UUID, typeId: UUID, propertyJson: JsObject)(implicit request: SecuredRequest[T]) = {
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
   * Ensure we have a valid 'applies_to' value in the property input JSON.
   */
  private def normalizeInputProperty(parentTypeId: UUID, propertyJson: JsObject): JsObject = {
    Js.find(propertyJson, "/applies_to").fold {
      propertyJson ++ Json.obj("applies_to" -> Json.toJson(parentTypeId))
    } { tid =>
      if (UUID.fromString(tid.as[String]) == parentTypeId)
        propertyJson ++ Json.obj("applies_to" -> Json.toJson(parentTypeId))
      else throw new BadRequestException(s"Given 'applies_to' ID does not match parent resource type ID.")
    }    
  }
  
  def deleteTypeProperty(org: UUID, typeId: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    ???
  }
  
  /** 
   * Find a TypeProperty by ID or 404 Not Found 
   */
  private def OkNotFoundProperty(id: UUID)(implicit request: SecuredRequest[_]) = {
    PropertyFactory.findById(ResourceIds.TypeProperty, id).fold {
      NotFoundResult(Errors.PROPERTY_NOT_FOUND(id))
    }{ property =>
      Ok(Output.renderTypePropertyOutput(property, META_URL))
    }
  }

  private def safeGetPropertyJson(json: JsValue): Try[GestaltTypePropertyInput] = Try {
    json.validate[GestaltTypePropertyInput].map{
      case resource: GestaltTypePropertyInput => resource
    }.recoverTotal{
      e => throw new BadRequestException(Js.errorString(e))
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
}