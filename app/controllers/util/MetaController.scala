package controllers.util


import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import com.galacticfog.gestalt.meta.api._

import com.galacticfog.gestalt.data.util._

import db._

import play.api.{Logger => log}

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

  
  
  /**
   * Get the base URL for this Meta instance
   */
  def META_URL[T](implicit request: SecuredRequest[T]) = { 
    val protocol = if (request.secure) "https" else "http"
    Some( "%s://%s".format(protocol, request.host) )
  }

  
  def HandleExceptions(e: Throwable) = {
    (metaApiExceptions orElse securityApiExceptions orElse genericApiException)(e)
  }
  
  val metaApiExceptions: PartialFunction[Throwable,play.api.mvc.Result] = {
    case e: ResourceNotFoundException     => NotFound(e.asJson)
    case e: BadRequestException           => BadRequest(e.asJson)
    case e: UnrecognizedResourceException => BadRequest(e.asJson)
    case e: ConflictException             => Conflict(e.asJson)    
  }
  
  val securityApiExceptions: PartialFunction[Throwable, play.api.mvc.Result] = {
    case e: SecurityBadRequestException       => BadRequestResult(e.getMessage)
    case e: SecurityResourceNotFoundException => NotFoundResult(e.getMessage)
    case e: SecurityConflictException         => ConflictResult(e.getMessage)
    case e: SecurityUnknownAPIException       => BadRequestResult(e.getMessage)
    case e: SecurityAPIParseException         => GenericErrorResult(500, e.getMessage)
    case e: SecurityUnauthorizedAPIException  => Unauthorized(e.getMessage)
    case e: SecurityForbiddenAPIException     => Forbidden(e.getMessage)      
  }
  
  val genericApiException: PartialFunction[Throwable, play.api.mvc.Result] = {
    case x => GenericErrorResult(500, x.getMessage)
  }
  
  def HandleRepositoryExceptions(e: Throwable) = e match {
    case e: ResourceNotFoundException     => NotFound(e.asJson)
    case e: BadRequestException           => BadRequest(e.asJson)
    case e: UnrecognizedResourceException => BadRequest(e.asJson)
    case e: ConflictException             => Conflict(e.asJson)
    case x => {
      if (x.isInstanceOf[SecurityRESTException])
        HandleSecurityApiException(x)
      else GenericErrorResult(500, x.getMessage)
    }
  }
  
  def HandleSecurityApiException(e: Throwable) = e.asInstanceOf[SecurityRESTException] match {
    case e: SecurityBadRequestException       => BadRequestResult(e.getMessage)
    case e: SecurityResourceNotFoundException => NotFoundResult(e.getMessage)
    case e: SecurityConflictException         => ConflictResult(e.getMessage)
    case e: SecurityUnknownAPIException       => BadRequestResult(e.getMessage)
    case e: SecurityAPIParseException         => GenericErrorResult(500, e.getMessage)
    case e: SecurityUnauthorizedAPIException  => Unauthorized(e.getMessage)
    case e: SecurityForbiddenAPIException     => Forbidden(e.getMessage)  
  }  
  
  /**
   * Get the Org and ResourceType Ids when given FQON and REST name.
   * 
   * @param fqon FQON of the target Org
   * @param rest REST name of ResourceType (i.e., workspaces, environments)
   */
//  protected def extractByName(fqon: String, rest: String): Either[play.api.mvc.Result,(UUID,UUID)] = {
//    orgFqon(fqon) match {
//      case None => Left(OrgNotFound(fqon))
//      case Some(org) => resourceUUID(rest) match {
//        case None => Left(Errors.INVALID_RESOURCE_TYPE(rest))
//        case Some(typeId) => Right((org.id, typeId))
//      }
//    }
//  }
  

  
  protected def extractByName(fqon: String, rest: String): Either[play.api.mvc.Result,(UUID,UUID)] = {
    orgFqon(fqon) match {
      case Some(org) => extractByName(org.id, rest)
      case None      => Left(OrgNotFound(fqon))
    }
  }
  
  protected def extractByName(org: UUID, rest: String): Either[play.api.mvc.Result, (UUID, UUID)] = {
    resourceUUID(rest) match {
      case Some(typeId) => Right((org, typeId))  
      case None         => Left(Errors.INVALID_RESOURCE_TYPE(rest))
    }
  }

  protected val connection = Session.connection
//  protected def getApiPrefix() = "https://gf.com/api/v1.1"
  
  /* 
   * 'Null' functions for SecuredAuthAction - can't just pass None to function
   * since it can't figure out if it's a None:String, or None:UUID (???)
   */
  protected def nullOptString(n: Option[String]) = n
  protected def nullOptUUID(n: Option[UUID]) = n
  
  /*
   * Test connection to Meta's DataStore.
   */
  protected def verifyDataStore() = {
    MetaDS.assertOnline( onFail = System.exit(1), failMessage = "SHUTTING DOWN" )
  }
  
  /** Get an Org by FQON */
  protected[controllers] def orgFqon(fqon: String): Option[GestaltResourceInstance] = {
    ResourceFactory.findByPropertyValue(ResourceIds.Org, "fqon", fqon)
  }
  
  
  /** Format a 404 for 'Org Not Found' */
  protected[controllers] def OrgNotFound(orgIdentifier: String) = {
    NotFoundResult(Errors.ORG_NOT_FOUND(orgIdentifier))
  }
  
 
  protected object Errors {
    def ORG_NOT_FOUND(id: String) = s"Org '${id}' not found."
    def PROPERTY_NOT_FOUND(id: String) = s"TypeProperty '${id}' not found."
    def TYPE_NOT_FOUND(id: String) = s"ResourceType '${id}' not found."
    def RESOURCE_NOT_FOUND(id: String) = s"Resource '${id}' not found."
    def INVALID_OWNER_TYPE(id: UUID) = s"Type ID must be Gestalt Org or User. Found: ${id.toString}"
    def INVALID_RESOURCE_TYPE(identifier: String) = NotFoundResult(s"Invalid resource type '${identifier}'")
  }
  
  
  /**
   * Only return resources that the given account (user) has 'read' access to.
   */
  private def filterReads(rs: Seq[GestaltResourceInstance], account: AuthAccountWithCreds, auth: Option[Hstore]) = {
    // TODO: What do we do if there is no authorization data on a resource?
    if (auth.isEmpty) Seq()
    else {

    }
    ???
  }


  def createResource(org: UUID, json: JsValue, typeId: Option[UUID] = None)(implicit request: SecuredRequest[JsValue]) = {
    Future {
      CreateResourceResult(
          ResourceIds.User, 
          request.identity.account.id, 
          org, 
          json, 
          request.identity, 
          typeId)
    }    
  }    
  
  protected[controllers] def createResourceCommon(org: UUID, parentId: UUID, typeId: UUID)(implicit request: SecuredRequest[JsValue]) = {
    Future {
      
      safeGetInputJson(typeId, request.body) match {
        case Failure(e)     => BadRequestResult(e.getMessage)
        case Success(input) => {
          CreateResourceResult(ResourceIds.User, request.identity.account.id,
              org, request.body, request.identity,
              typeId = Some(typeId), 
              parentId = Some(parentId))
        }
      }
    }
  }  
  
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

    trace(s"CreateResource($org, [json], [account])")

    safeGetInputJson(resourceJson) flatMap { input =>
      val tid = assertValidTypeId(input, typeId)
      ResourceFactory.create(creatorType, creator)(
        inputWithDefaults(org, input.copy(resource_type = Some(tid)), user), parentId = parentId)
    }

  }    
  
 /**
   * Inspect a GestaltResourceInput, supplying default values where appropriate.
   */
  protected[controllers] def inputWithDefaults(org: UUID, input: GestaltResourceInput, creator: AuthAccountWithCreds) = {
    val owner = if (input.owner.isDefined) input.owner else Some(ownerFromAccount(creator))
    val resid = if (input.id.isDefined) input.id else Some(UUID.randomUUID())
    val state = if (input.resource_state.isDefined) input.resource_state else Some(ResourceStates.Active)
    fromResourceInput(org, input.copy(id = resid, owner = owner, resource_state = state))    
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
        println(err)
        HandleExceptions(err)  
      }
    }
  }  
  
  /**
   * Parse JSON to GestaltResourceInput
   */
  protected[controllers] def safeGetInputJson(json: JsValue): Try[GestaltResourceInput] = Try {
    trace(s"safeGetInputJson([json]")
    implicit def jsarray2str(arr: JsArray) = arr.toString

    json.validate[GestaltResourceInput].map {
      case resource: GestaltResourceInput => resource
    }.recoverTotal { e => 
      log.error("Error parsing request JSON: " + JsError.toFlatJson(e).toString)
      throw new BadRequestException(JsError.toFlatJson(e).toString)
    }
  }

  /**
   * Parse JSON to GestaltResourceInput and validate type-properties.
   */
  protected[controllers] def safeGetInputJson(typeId: UUID, json: JsValue): Try[GestaltResourceInput] = Try {
    trace(s"safeGetInputJson($typeId, [json]")

    safeGetInputJson(json) match {
      case Failure(e)   => throw e
      case Success(res) => {
        val validation = PropertyValidator.validate(typeId, stringmap(res.properties))
        if (validation._1) res else throw new BadRequestException(validation._2.get)
      }
    }
  }
  
  /**
   * Convert GestaltResourceInput to GestaltResourceInstance
   */
  protected[controllers] def fromResourceInput(org: UUID, in: GestaltResourceInput) = {
    GestaltResourceInstance(
      id = in.id getOrElse UUID.randomUUID,
      typeId = in.resource_type.get,
      state = resolveResourceState(in.resource_state), //ResourceState.id(in.resource_state.get),
      orgId = org,
      owner = in.owner.get,
      name = in.name,
      description = in.description,
      /* TODO: Here is where we transform from map(any) to map(string) */
      properties = stringmap(in.properties),
      variables = in.variables,
      tags = in.tags,
      auth = in.auth)
  }
  
  protected[controllers] object Err {
    def RESOURCE_TYPE_NOT_FOUND(m: UUID) = s"Given ResourceType 'resource_type : $m' does not exist."
    val RESOURCE_TYPE_NOT_GIVEN = "resource_type must be specified."
  }
  
  
  /**
   * Convert a string FQON to corresponding Org UUID.
   */
  def id(fqon: String): UUID = {
    val org = orgFqon(fqon) getOrElse {
      throw new ResourceNotFoundException(s"Org ID '$fqon' not found.")
    }
    org.id
  }  
  
  def resolveResourceState(state: Option[String]) = {
    ResourceState.id( state getOrElse ResourceStates.Active )
  }  
  
  protected[controllers] def safeGetPatchDocument(json: JsValue): Try[PatchDocument] = Try {
    PatchDocument.fromJsValue(json)
  }  
  
  
  protected[controllers] def typeExists(typeId: UUID) = !TypeFactory.findById(typeId).isEmpty  
  
  def assertValidTypeId(r: GestaltResourceInput, typeId: Option[UUID]): UUID = {
    val id = resolveTypeId(r, typeId) getOrElse {
      throw new BadRequestException(Err.RESOURCE_TYPE_NOT_GIVEN)
    }
    if (typeExists(id)) id
    else throw new BadRequestException(Err.RESOURCE_TYPE_NOT_FOUND(id))
  }
  
  def resolveTypeId(r: GestaltResourceInput, typeId: Option[UUID]) = {
    if (r.resource_type.isDefined) r.resource_type
    else if (typeId.isDefined) typeId
    else None
  }      
  
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


object MetaDS extends {
  
  lazy val online = metaOnline( ConnectionManager.config )
  
  def assertOnline(onFail: => Unit, failMessage: String) = {
    if ( online ) {
      log.info("Meta Data-Store ONLINE")
    } 
    else {
      log.error("FATAL: Cannot connect to Meta Data-Store")
      log.error("Current configuration:")
      log.error(ConnectionManager.toString)
      log.error(failMessage)
      onFail
    }
  }
  
  /**
   * Ensure we can reach the server specified in configuration and
   * that the specified database exists.
   */
  def metaOnline(config: JdbcConnectionInfo): Boolean = {
    
    log.info("Pinging Meta Repository...")
    
    PostgresHealth.verifyDataStore( config.database ) match {
      case Success( _ )  => {
        log.info( "data-store: " + /*green(*/"Available"/*)*/ )
        log.info( ConnectionManager.toString )
        true
      }
      case Failure( ex ) => ex match {
        case p: PSQLException => {
          log.error( s"data-store: ${p.getMessage}" )
          false
        }
        case e: Throwable => {
          log.error( "Unexpected-Error : " + e.getMessage )
          false
        }
      }
    }  
  }

}