package controllers.util


import play.api.http.HeaderNames

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import com.galacticfog.gestalt.meta.api._

import com.galacticfog.gestalt.data.util._

import controllers.util.db._

import play.api.Logger
import scala.util.{Success,Failure}

import org.postgresql.util.PSQLException

import scalikejdbc._
import com.galacticfog.gestalt.data._
import scala.util.Try
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltBaseAuthProvider
import com.galacticfog.gestalt.security.play.silhouette.GestaltSecuredController

import com.mohiva.play.silhouette.api.services.AuthenticatorService
import com.mohiva.play.silhouette.impl.authenticators.{DummyAuthenticatorService, DummyAuthenticator}

import java.util.UUID

import com.galacticfog.gestalt.data.models._

import com.galacticfog.gestalt.meta.api.errors._

import com.galacticfog.gestalt.security.api.errors.SecurityRESTException
import com.galacticfog.gestalt.security.api.errors.{ BadRequestException => SecurityBadRequestException }
import com.galacticfog.gestalt.security.api.errors.{ UnauthorizedAPIException => SecurityUnauthorizedAPIException }
import com.galacticfog.gestalt.security.api.errors.{ ForbiddenAPIException => SecurityForbiddenAPIException }
import com.galacticfog.gestalt.security.api.errors.{ ResourceNotFoundException => SecurityResourceNotFoundException }
import com.galacticfog.gestalt.security.api.errors.{ ConflictException => SecurityConflictException }
import com.galacticfog.gestalt.security.api.errors.{ UnknownAPIException => SecurityUnknownAPIException }
import com.galacticfog.gestalt.security.api.errors.{ APIParseException => SecurityAPIParseException }
import play.api.mvc.RequestHeader

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._

import play.api.libs.json._
import play.api.mvc.Result
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.output._
import controllers.SecurityResources
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
    
    safeGetInputJson(json) flatMap { input =>
      val tid = Option(assertValidTypeId(input, typeId))
      val newResource = withInputDefaults(org, input, creator, tid)
      ResourceFactory.create(ResourceIds.User, creator.account.id)(newResource, parent)
    }
  }
  
  /**
   * Parse JSON to GestaltResourceInput
   */
  def safeGetInputJson(
      json: JsValue, 
      typeId: Option[UUID] = None, 
      withDefaults: Boolean = false): Try[GestaltResourceInput] = Try {

    json.validate[GestaltResourceInput].map {
      case input: GestaltResourceInput => {
        if (typeId.isEmpty) input
        else {
          if (!typeExists(typeId.get)) throwBadRequest(s"Invalid type ID. found: $typeId")
          else {
            val validation = PropertyValidator.validate(typeId.get, stringmap(input.properties))
            if (validation._1) input else throwBadRequest(validation._2.get)
          }
        }
      }
    }.recoverTotal { e => 
      log.error("Error parsing request JSON: " + Js.errorString(e))
      throwBadRequest(Js.errorString(e))
    }
  }
  
  /**
   * Convert raw JSON to a GestaltResourceInput
   */
  def jsonToInput(org: UUID, creator: AuthAccountWithCreds, json: JsValue) = {
    withInputDefaults(
      org     = org, 
      input   = safeGetInputJson(json).get,
      creator = creator,
      typeId  = None)
  }
  
 /**
  * Inspect a GestaltResourceInput, supplying default values where possible.
  */
//  def withInputDefaults(org: UUID, input: GestaltResourceInput, creator: AuthAccountWithCreds): Instance = {
//    inputToInstance(org, input.copy(
//        id             = input.id orElse Option(UUID.randomUUID()), 
//        owner          = input.owner orElse Option(ownerFromAccount(creator)), 
//        resource_state = input.resource_state orElse Option(ResourceStates.Active)))    
//  }
  
 /**
  * Inspect a GestaltResourceInput, supplying default values where possible. Asserts validity
  * of a given Type UUID.
  */  
  def withInputDefaults(
      org: UUID, 
      input: GestaltResourceInput,  
      creator: AuthAccountWithCreds,
      typeId: Option[UUID] = None) = {
    
    inputToInstance(org, input.copy(
          id             = input.id orElse Option(UUID.randomUUID()), 
          owner          = input.owner orElse Option(ownerFromAccount(creator)), 
          resource_state = input.resource_state orElse Option(ResourceStates.Active),
          resource_type  = Option(assertValidTypeId(input, typeId))))
  }
  
  private[this] def typeExists(typeId: UUID) = !TypeFactory.findById(typeId).isEmpty  
  
  private[util] def resolveTypeId(r: GestaltResourceInput, typeId: Option[UUID]): Option[UUID] = {
    if (r.resource_type.isDefined) r.resource_type
    else if (typeId.isDefined) typeId
    else None
  }
  
  private[util] def assertValidTypeId(r: GestaltResourceInput, typeId: Option[UUID]): UUID = {
    resolveTypeId(r, typeId).fold {
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
