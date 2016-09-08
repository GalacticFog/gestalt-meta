package controllers.util


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


trait MetaController extends SecureController with SecurityResources {
  
  val log = Logger(this.getClass)
  
  protected val connection = Session.connection

  /**
   * Get the base URL for this Meta instance
   */
  def META_URL[T](implicit request: SecuredRequest[T]) = { 
    val protocol = if (request.secure) "https" else "http"
    Some( "%s://%s".format(protocol, request.host) )
  }
  
  /**
   * Render a single GestaltResourceInstance to JSON
   */
  private[controllers] def RenderSingle(res: GestaltResourceInstance)
      (implicit request: SecuredRequest[_]): JsValue = {
    Output.renderInstance(res, META_URL)
  }
  
  /**
   * Render a Seq of GestaltResourceInstances to Result[Ok(JsValue)]
   */
  private[controllers] def RenderList(rss: Seq[GestaltResourceInstance])
      (implicit request: SecuredRequest[_]): Result = {
    handleExpansion(rss, request.queryString, META_URL)  
  }
  
  protected[controllers] def ResourceNotFound(typeId: UUID, id: UUID) = 
    NotFoundResult(s"${ResourceLabel(typeId)} with ID '$id' not found.")  
  
  /** 
   * Format a 404 for 'Org Not Found' 
   */
  protected[controllers] def OrgNotFound(orgIdentifier: String) = {
    NotFoundResult(Errors.ORG_NOT_FOUND(orgIdentifier))
  }
  

  


  /**
   * Handles the 'expand' querystring parameter.
   */
  def handleExpansion(rs: Seq[ResourceLike], qs: QueryString, baseUri: Option[String] = None) = {
    if (getExpandParam(qs)) {
      Ok(Json.toJson(rs map { r => Output.renderInstance(r.asInstanceOf[GestaltResourceInstance], baseUri) }))
    }
    else Ok(Output.renderLinks(rs, baseUri))
  }


  /** 
   * Get an Org by FQON 
   */
  protected[controllers] def orgFqon(fqon: String): Option[GestaltResourceInstance] = {
    ResourceFactory.findByPropertyValue(ResourceIds.Org, "fqon", fqon)
  }

  
  private[controllers] def createResourceCommon(
      org: UUID, 
      parentId: UUID, 
      typeId: UUID, 
      json: JsValue)(implicit request: SecuredRequest[JsValue]): Future[Result] = {
    
    Future {
      safeGetInputJson(typeId, json) match {
        case Failure(e) => HandleExceptions(e)
        case Success(input) => 
          CreateNewResourceResult(org, request.identity, input, Option(typeId), Option(parentId))  
      } 
    }
  }
  
  protected[controllers] def CreateResourceResult(
      org: UUID, 
      resourceJson: JsValue, 
      user: AuthAccountWithCreds, 
      typeId: UUID,
      parentId: UUID): Result = {
    
    log.trace(s"CreateResourceResult($org, [json], [account])")
    
    HandleCreate {
      CreateResource(ResourceIds.User, user.account.id, org, resourceJson, user, Some(typeId), Some(parentId))
    }
  }
  
  def createResourceD(
      org: UUID, 
      json: JsValue, 
      typeId: Option[UUID] = None, 
      parentId: Option[UUID] = None)
     (implicit request: SecuredRequest[JsValue]): Future[Result] = {
    Future {
      CreateResourceResult(
          ResourceIds.User, 
          request.identity.account.id, 
          org, 
          json, 
          request.identity, 
          typeId,
          parentId)
    }    
  }    
  
  
  /**
   * TODO: Remove - we don't care about different creator-types right now.
   */
  protected[controllers] def CreateResourceResult(
      creatorType: UUID, 
      creatorId: UUID, 
      org: UUID, 
      resourceJson: JsValue, 
      user: AuthAccountWithCreds, 
      typeId: Option[UUID] = None,
      parentId: Option[UUID] = None) = {
    
    log.trace(s"CreateResourceResult($org, [json], [account])")
    
    HandleCreate {
      CreateResource(creatorType, creatorId, org, resourceJson, user, typeId, parentId)
    }
  }
  
  protected[controllers] def CreateResource(
    creatorType: UUID,
    creator: UUID,
    org: UUID,
    resourceJson: JsValue,
    user: AuthAccountWithCreds,
    typeId: Option[UUID] = None,
    parentId: Option[UUID] = None): Try[GestaltResourceInstance] = {

    safeGetInputJson(resourceJson) flatMap { input =>
      val tid = assertValidTypeId(input, typeId)
      ResourceFactory.create(creatorType, creator)(
        inputWithDefaults(org, input.copy(resource_type = Some(tid)), user), parentId = parentId)
    }
  }  
  
  /*
   * owningOrgId
   * creatorId
   * resourceJson
   * typeId: Option[UUID],
   * parentId: Option[UUID]
   * 
   */
  
  
  /* 
   * 
   * TODO: Replace other Create* : Try[Instance] methods with this!
   * 
   */
  protected[controllers] def CreateNewResource(
      owningOrgId: UUID,
      creator: AuthAccountWithCreds,
      resourceJson: JsValue,
      typeId: Option[UUID],
      parentId: Option[UUID]): Try[GestaltResourceInstance] = {
    
    safeGetInputJson(resourceJson) flatMap { input =>
      val tid = Option(assertValidTypeId(input, typeId))
      val newResource = inputWithDefaults(owningOrgId, input, tid, creator)
      ResourceFactory.create(ResourceIds.User, creator.account.id)(newResource, parentId)
    }
  }
  
  protected[controllers] def CreateNewResource(
      owningOrgId: UUID,
      creator: AuthAccountWithCreds,
      resourceInput: GestaltResourceInput,
      typeId: Option[UUID],
      parentId: Option[UUID]): Try[GestaltResourceInstance] = {
    
    val tid = Option(assertValidTypeId(resourceInput, typeId))
    val newResource = inputWithDefaults(owningOrgId, resourceInput, tid, creator) 
    ResourceFactory.create(ResourceIds.User, creator.account.id)(newResource, parentId)
  }  
  
  protected[controllers] def CreateNewResourceResult(
      owningOrgId: UUID,
      creator: AuthAccountWithCreds,
      resourceJson: JsValue,
      typeId: Option[UUID],
      parentId: Option[UUID]): Result = {  
  
    HandleCreate {
      CreateNewResource(owningOrgId, creator, resourceJson, typeId, parentId)
    } 
  }
  
  protected[controllers] def CreateNewResourceResult(
      owningOrgId: UUID,
      creator: AuthAccountWithCreds,
      resourceInput: GestaltResourceInput,
      typeId: Option[UUID],
      parentId: Option[UUID]): Result = {  
  
    HandleCreate {
      CreateNewResource(owningOrgId, creator, resourceInput, typeId, parentId)
    }
  }
  
  /*
   * this is only used by Meta.createWorkspaceCommon and Meta.postEnvironmentResult
   */
  protected[controllers] def CreateResource(
    org: UUID,
    resourceJson: JsValue,
    caller: AuthAccountWithCreds,
    typeId: UUID,
    parentId: UUID): Try[GestaltResourceInstance] = {
    
    safeGetInputJson(resourceJson) flatMap { input =>
      val tid = assertValidTypeId(input, Option(typeId))
      ResourceFactory.create(ResourceIds.User, caller.account.id)(
        inputWithDefaults(org, input.copy(resource_type = Some(tid)), caller), parentId = Option(parentId))
    }

  }     
  

  
  def createResourceInstance(
      org: UUID, 
      json: JsValue, 
      typeId: Option[UUID] = None, 
      parentId: Option[UUID] = None)
    (implicit request: SecuredRequest[JsValue]): Try[GestaltResourceInstance] = {
    
    CreateResource(
      ResourceIds.User,
      request.identity.account.id,
      org,
      json,
      request.identity,
      typeId,
      parentId)
  }



  def HandleCreate(typeId: UUID, json: JsValue)(resource : => Try[GestaltResourceInstance]): Result = {
    safeGetInputJson(typeId, json) match {
      case Failure(error) => BadRequestResult(error.getMessage)
      case Success(input) => HandleCreate(resource)
    }
  }
  
  
  def HandleCreate(resource : => Try[GestaltResourceInstance]): Result = {
    resource match {
      case Success(res) => Created(Output.renderInstance(res))
      case Failure(err) => {
        log.error("ERROR CREATING RESOURCE: " + err.getMessage)
        HandleExceptions(err)  
      }
    }
  }  
  
  /**
   * Parse JSON to GestaltResourceInput
   */
  protected[controllers] def safeGetInputJson(json: JsValue): Try[GestaltResourceInput] = Try {

    json.validate[GestaltResourceInput].map {
      case resource: GestaltResourceInput => resource
    }.recoverTotal { e => 
      log.error("Error parsing request JSON: " + JsError.toFlatJson(e).toString)
      throwBadRequest(JsError.toFlatJson(e).toString)
    }
  }

  /**
   * Parse JSON to GestaltResourceInput and validate type-properties.
   */
  protected[controllers] def safeGetInputJson(typeId: UUID, json: JsValue): Try[GestaltResourceInput] = {
    log.debug(s"safeGetInputJson($typeId, [json])")

    safeGetInputJson(json) map { input =>
      val validation = PropertyValidator.validate(typeId, stringmap(input.properties))
      if (validation._1) input else throwBadRequest(validation._2.get)
    }
  }
  
  /**
   * Convert a string FQON to corresponding Org UUID.
   */
  def fqid(fqon: String): UUID = orgFqon(fqon) map { _.id } getOrElse {
    throw new ResourceNotFoundException(s"Org FQON '$fqon' not found.")
  }
  
  /**
   * Convert a string to a resource-state UUID. If state is empty, state = Active.
   */
  protected[controllers] def resolveResourceState(state: Option[String]) = {
    ResourceState.id( state getOrElse ResourceStates.Active )
  }
  
  protected[controllers] def safeGetPatchDocument(json: JsValue): Try[PatchDocument] = Try {
    PatchDocument.fromJsValue(json)
  }  
  
  protected[controllers] def typeExists(typeId: UUID) = !TypeFactory.findById(typeId).isEmpty  
  
  protected[controllers] def assertValidTypeId(r: GestaltResourceInput, typeId: Option[UUID]): UUID = {
    resolveTypeId(r, typeId).fold(throw new BadRequestException(Errors.RESOURCE_TYPE_NOT_GIVEN)) {
      id => if (typeExists(id)) id 
      else throwBadRequest(Errors.TYPE_NOT_FOUND(id))
    }
  }
  
  def resolveTypeId(r: GestaltResourceInput, typeId: Option[UUID]) = {
    if (r.resource_type.isDefined) r.resource_type
    else if (typeId.isDefined) typeId
    else None
  }      
  
  def inputToResource(org: UUID, creator: AuthAccountWithCreds, json: JsValue) = {
    inputWithDefaults(
      org = org, 
      input = safeGetInputJson(json).get, 
      creator = creator)
  }  
  
 /**
   * Inspect a GestaltResourceInput, supplying default values where appropriate.
   */
  def inputWithDefaults(org: UUID, input: GestaltResourceInput, creator: AuthAccountWithCreds) = {
    val owner = if (input.owner.isDefined) input.owner else Some(ownerFromAccount(creator))
    val resid = if (input.id.isDefined) input.id else Some(UUID.randomUUID())
    val state = if (input.resource_state.isDefined) input.resource_state else Some(ResourceStates.Active)
    fromResourceInput(org, input.copy(id = resid, owner = owner, resource_state = state))    
  }

  def inputWithDefaults(
      org: UUID, 
      input: GestaltResourceInput, 
      typeId: Option[UUID], 
      creator: AuthAccountWithCreds) = {
    
    val resid = if (input.id.isDefined) input.id else Some(UUID.randomUUID())
    val owner = if (input.owner.isDefined) input.owner else Some(ownerFromAccount(creator))
    val state = if (input.resource_state.isDefined) input.resource_state else Some(ResourceStates.Active)
    val tpeid = Option(assertValidTypeId(input, typeId))
    
    val newInput = input.copy(
        id = resid, 
        owner = owner,
        resource_type = tpeid,
        resource_state = state)
    
    fromResourceInput(org, newInput)    
  }  
  
  /**
   * Convert GestaltResourceInput to GestaltResourceInstance
   */
  def fromResourceInput(org: UUID, in: GestaltResourceInput) = {
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
  
  protected[controllers] def throwBadRequest(message: String) =
    throw new BadRequestException(message)
}

/**
 * Singleton holder for objects shared amongst Controllers.
 */
object Session {
  /*
   * JDBC Connection info for binding to Meta's DataStore.
   * Getting the connection info DOES NOT validate the data - it
   * only asserts that values were supplied for all connection attributes.
   */
  private[util] val connection = ConnectionManager.config
  
  implicit val session: DBSession = AutoSession  
}

object MetaController {}
