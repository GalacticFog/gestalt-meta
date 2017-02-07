package controllers.util


import play.api.http.HeaderNames

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import controllers.util.db._
import play.api.Logger

import scala.util.{Failure, Success}
import scalikejdbc._
import com.galacticfog.gestalt.data._

import scala.util.Try
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import java.util.UUID

import com.galacticfog.gestalt.data.models._
import play.api.mvc.{Request, RequestHeader, Result}
import com.galacticfog.gestalt.meta.api.errors._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.output._
import controllers.SecurityResources
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.patch._

trait MetaControllerUtils {
  
  private[this] val log = Logger(this.getClass)
  
  /**
    * Get an Org by FQON
    */
  protected[controllers] def orgFqon(fqon: String): Option[GestaltResourceInstance] = {
    ResourceFactory.findByPropertyValue(ResourceIds.Org, "fqon", fqon)
  }

  /**
    * Convert a string to a resource-state UUID. If state is empty, state = Active.
    */
  protected[controllers] def resolveResourceState(state: Option[String]) = {
    ResourceState.id( state getOrElse ResourceStates.Active )
  }

  protected[controllers] def throwBadRequest(message: String) =
    throw new BadRequestException(message)

  /**
    * Inspect a GestaltResourceInput, supplying default values where appropriate.
    */
  def inputWithDefaults(org: UUID, input: GestaltResourceInput, creator: AuthAccountWithCreds): Instance = {
    val owner = if (input.owner.isDefined) input.owner else Some(SecurityResources.ownerFromAccount(creator))
    val resid = if (input.id.isDefined) input.id else Some(UUID.randomUUID())
    val state = if (input.resource_state.isDefined) input.resource_state else Some(ResourceStates.Active)
    fromResourceInput(org, input.copy(id = resid, owner = owner, resource_state = state))
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
}

import com.galacticfog.gestalt.meta.auth.AuthorizationMethods

trait MetaController extends AuthorizationMethods with SecurityResources with MetaControllerUtils with JsonInput { this: SecureController =>
  
  private[this] val log = Logger(this.getClass)
  
  protected val connection = Session.connection

  /**
    * Get the base URL for this Meta instance
    */
  def META_URL(implicit requestHeader: RequestHeader) = {
    val protocol = (requestHeader.headers.get(HeaderNames.X_FORWARDED_PROTO) getOrElse {
      if (requestHeader.secure) "https" else "http"
    }).toLowerCase
    val host = requestHeader.headers.get(HeaderNames.X_FORWARDED_HOST) getOrElse requestHeader.host
    Some(
      "%s://%s".format(protocol, host)
    )
  }

  /**
   * Render a single GestaltResourceInstance to JSON
   */
  private[controllers] def RenderSingle(res: GestaltResourceInstance)
                                       (implicit rh: RequestHeader): JsValue = {
    Output.renderInstance(res, META_URL)
  }
  
  /**
   * Render a Seq of GestaltResourceInstances to Result[Ok(JsValue)]
   */
  private[controllers] def RenderList(rss: Seq[GestaltResourceInstance])
                                     (implicit request: Request[_]): Result = {
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



  private[controllers] def createResourceCommon(
      org: UUID, 
      parentId: UUID, 
      typeId: UUID, 
      json: JsValue)(implicit request: SecuredRequest[JsValue]): Future[Result] = {
    
    Future {
      safeGetInputJson(json) match {
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
      parentId: Option[UUID] = None): Result = {
    
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
        withInputDefaults(org, input, user, Some(tid)), 
        parentId = parentId)
    }
  }  
  
  protected[controllers] def CreateResource(
    org: UUID,
    resourceJson: JsValue,
    caller: AuthAccountWithCreds,
    typeId: UUID,
    parentId: UUID): Try[GestaltResourceInstance] = {
    
    safeGetInputJson(resourceJson) flatMap { input =>
      val tid = assertValidTypeId(input, Option(typeId))
      ResourceFactory.create(ResourceIds.User, caller.account.id)(
        withInputDefaults(org, input, caller, Option(tid)), 
        parentId = Option(parentId))
    }

  }   
  
  protected[controllers] def CreateNewResource(
      owningOrgId: UUID,
      creator: AuthAccountWithCreds,
      resourceInput: GestaltResourceInput,
      typeId: Option[UUID],
      parentId: Option[UUID]): Try[GestaltResourceInstance] = {
    
    val tid = Option(assertValidTypeId(resourceInput, typeId))
    val newResource = withInputDefaults(owningOrgId, resourceInput, creator) 
    ResourceFactory.create(ResourceIds.User, creator.account.id)(newResource, parentId) map { r =>
      val es = setNewEntitlements(owningOrgId, r.id, creator, parentId)
      
      println("CREATED ENTITLEMENTS:")
      es foreach { e =>
        println("%s, %s".format(e.get.name, e.get.properties.get("identities")))
      }
      r
    }
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
    safeGetInputJson(json) match {
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
      log.error("Error parsing request JSON: " + Js.errorString(e))
      throwBadRequest(Js.errorString(e))
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
  

  protected[controllers] def safeGetPatchDocument(json: JsValue): Try[PatchDocument] = Try {
    PatchDocument.fromJsValue(json)
  }  
  
  protected[controllers] def typeExists(typeId: UUID) = !TypeFactory.findById(typeId).isEmpty  
  
//  protected[controllers] def assertValidTypeId(r: GestaltResourceInput, typeId: Option[UUID]): UUID = {
//    resolveTypeId(r, typeId).fold(throw new BadRequestException(Errors.RESOURCE_TYPE_NOT_GIVEN)) {
//      id => if (typeExists(id)) id
//      else throwBadRequest(Errors.TYPE_NOT_FOUND(id))
//    }
//  }
//
//  def resolveTypeId(r: GestaltResourceInput, typeId: Option[UUID]) = {
//    if (r.resource_type.isDefined) r.resource_type
//    else if (typeId.isDefined) typeId
//    else None
//  }      
  
  def inputToResource(org: UUID, creator: AuthAccountWithCreds, json: JsValue) = {
    inputWithDefaults(
      org = org, 
      input = safeGetInputJson(json).get, 
      creator = creator)
  }  
  
  def inputWithDefaults(
      org: UUID,
      input: GestaltResourceInput,
      typeId: Option[UUID],
      creator: AuthAccountWithCreds) = {

    val resid = if (input.id.isDefined) input.id else Some(UUID.randomUUID())
    val owner = if (input.owner.isDefined) input.owner else Some(SecurityResources.ownerFromAccount(creator))
    val state = if (input.resource_state.isDefined) input.resource_state else Some(ResourceStates.Active)
    val tpeid = Option(assertValidTypeId(input, typeId))

    val newInput = input.copy(
        id = resid,
        owner = owner,
        resource_type = tpeid,
        resource_state = state)

    fromResourceInput(org, newInput)
  }

//  protected[controllers] def throwBadRequest(message: String) =
//    throw new BadRequestException(message)
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