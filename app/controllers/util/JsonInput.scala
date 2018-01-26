package controllers.util

import play.api.Logger
import com.galacticfog.gestalt.data._
import scala.util.Try
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import java.util.UUID
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.errors._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.json.Js

trait JsonInput {
  
  val log = Logger(this.getClass)
  
  def CreateNewResource(
      org: UUID,
      creator: AuthAccountWithCreds,
      json: JsValue,
      typeId: Option[UUID],
      parent: Option[UUID]): Try[GestaltResourceInstance] = {
    
    // This version of safeGetInputJson can be replaced with Js.parse[GestaltResourceInput](resourceJson)
    
    toInput(json) flatMap { input =>
      val tid = Option(assertValidTypeId(input, typeId))
      val newResource = withInputDefaults(org, input, creator, tid)
      ResourceFactory.create(ResourceIds.User, creator.account.id)(newResource, parent)
    }
  }
  
  /**
   * Convert input JSON to an in-memory GestaltResourceInstance
   */
  def j2r(org: UUID, creator: AuthAccountWithCreds, json: JsValue, typeId: Option[UUID] = None) = {
    withInputDefaults(
      org = org, 
      typeId = typeId,
      input = toInput(json).get, 
      creator = creator)
  }
  
  def toInput(json: JsValue, typeId: Option[UUID] = None, requireTypeId: Boolean = false): Try[GestaltResourceInput] = {
    Js.parse[GestaltResourceInput](json.as[JsObject]).map { input =>
      if (requireTypeId) {
        val tid = assertValidTypeId(input, typeId)
        input.copy(resource_type = Some(tid))
      } else input
    }
  }
  
  def toInputValidated(json: JsValue, typeId: Option[UUID] = None): Try[GestaltResourceInput] = {
    toInput(json, typeId, requireTypeId = true).map { input =>
      val validation = PropertyValidator.validate(typeId.get, stringmap(input.properties))
      if (validation._1) input 
      else throwBadRequest(validation._2.get)      
    }
  }
  
  
  /**
   * Parse JSON to GestaltResourceInput
   */
//  def safeGetInputJson(
//      json: JsValue, 
//      typeId: Option[UUID] = None): Try[GestaltResourceInput] = Try {
//
//    json.validate[GestaltResourceInput].map {
//      case input: GestaltResourceInput => {
//        if (typeId.isEmpty) input
//        else {
//          if (!typeExists(typeId.get)) throwBadRequest(s"Invalid type ID. found: $typeId")
//          else {
//            val validation = PropertyValidator.validate(typeId.get, stringmap(input.properties))
//            if (validation._1) input else throwBadRequest(validation._2.get)
//          }
//        }
//      }
//    }.recoverTotal { e => 
//      log.error("Error parsing request JSON: " + Js.errorString(e))
//      throwBadRequest(Js.errorString(e))
//    }
//  }
  
  /**
   * Convert raw JSON to a GestaltResourceInstance
   */
  def jsonToInput(org: UUID, creator: AuthAccountWithCreds, json: JsValue): Instance = {
    withInputDefaults(
      org     = org, 
      input   = toInput(json).get,
      creator = creator,
      typeId  = None)
  }
  
  def asResource(
      org: UUID, 
      json: JsValue,  
      creator: AuthAccountWithCreds,
      typeId: Option[UUID] = None): Try[GestaltResourceInstance] = {
    
    Js.parse[GestaltResourceInput](json).map { input =>
      resourceWithDefaults(org, input, creator, typeId)
    }
  }
  

  
 /**
  * Inspect a GestaltResourceInput, supplying default values where possible. Asserts validity
  * of a given Type UUID.
  */
  def resourceWithDefaults(
      org: UUID, 
      input: GestaltResourceInput,  
      creator: AuthAccountWithCreds,
      typeId: Option[UUID] = None): GestaltResourceInstance = {
    
    inputToInstance(org, input.copy(
      id             = input.id orElse Option(UUID.randomUUID()), 
      owner          = input.owner orElse Option(ownerFromAccount(creator)), 
      resource_state = input.resource_state orElse Option(ResourceStates.Active),
      resource_type  = Option(assertValidTypeId(input, typeId))))
  }  
  
  
 /**
  * Inspect a GestaltResourceInput, supplying default values where possible. Asserts validity
  * of a given Type UUID.
  */
  def withInputDefaults(
      org: UUID, 
      input: GestaltResourceInput,  
      creator: AuthAccountWithCreds,
      typeId: Option[UUID] = None): GestaltResourceInstance = {
    
    inputToInstance(org, input.copy(
      id             = input.id orElse Option(UUID.randomUUID()), 
      owner          = input.owner orElse Option(ownerFromAccount(creator)), 
      resource_state = input.resource_state orElse Option(ResourceStates.Active),
      resource_type  = Option(assertValidTypeId(input, typeId))))
  }
  
  private[this] def typeExists(typeId: UUID) = !TypeFactory.findById(typeId).isEmpty

  /**
   * Ensure a valid resource_type_id is available, either in the Input object or a
   * standalone type ID.
   * 
   * When a resource is created it is possible to specify the resource-type in the create payload,
   * or the type may be inferred by the API based on the endpoint it was posted to. This function is
   * used to ensure we have a type-id in at least one of those places. If the ID is given in both
   * places, this implementation will prefer the value given in the input payload.
   */
  protected[controllers] def assertValidTypeId(r: GestaltResourceInput, typeId: Option[UUID]): UUID = {
    (r.resource_type orElse typeId).fold {
      throw new BadRequestException(Errors.RESOURCE_TYPE_NOT_GIVEN) 
    }{ id => 
      if (typeExists(id)) id 
      else throwBadRequest(Errors.TYPE_NOT_FOUND(id))
    }
  }
  
  /**
   * Create a ResourceOwnerLink from AuthAccountWithCreds
   */
  private[this] def ownerFromAccount(account: AuthAccountWithCreds): ResourceOwnerLink = {
    toOwnerLink(
      ResourceIds.User,
      account.account.id, 
      name = Some(account.account.name), 
      orgId = account.account.directory.orgId )  
  }
  
  /**
   * Convert GestaltResourceInput to GestaltResourceInstance
   */
  def inputToInstance(org: UUID, in: GestaltResourceInput) = {
    GestaltResourceInstance(
      id = in.id getOrElse UUID.randomUUID,
      typeId = in.resource_type.get,
      state = resolveResourceState(in.resource_state),
      orgId = org,
      owner = in.owner.get,
      name = in.name,
      description = in.description,
      properties = stringmap(in.properties),
      variables = in.variables,
      tags = in.tags,
      auth = in.auth)
  }  
  
  /**
   * Convert a string to a resource-state UUID. If state is empty, state = Active.
   */
  private[this] def resolveResourceState(state: Option[String]) = {
    ResourceState.id(state getOrElse ResourceStates.Active)
  }  
  
  private[this] def throwBadRequest(message: String) =
    throw new BadRequestException(message)
  
}
