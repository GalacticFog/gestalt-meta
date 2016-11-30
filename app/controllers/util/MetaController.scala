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


trait MetaController extends SecureController 
  with SecurityResources with JsonInput {
  
  //private[this] val log = Logger(this.getClass)
  
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
   * Convert a string FQON to corresponding Org UUID.
   */
  def fqid(fqon: String): UUID = orgFqon(fqon) map { _.id } getOrElse {
    throw new ResourceNotFoundException(s"Org FQON '$fqon' not found.")
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