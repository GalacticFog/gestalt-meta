package controllers

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

import com.galacticfog.gestalt.meta.services._
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
import com.galacticfog.gestalt.security.api.{ GestaltResource => SecurityResource }
import com.galacticfog.gestalt.security.api.{ ResourceLink => SecurityLink }

import com.galacticfog.gestalt.security.api._
import com.galacticfog.gestalt.security.api.json.JsonImports
import play.api.libs.json._
import com.galacticfog.gestalt.security.api.json.JsonImports.{ orgFormat, linkFormat, acctFormat }
import com.mohiva.play.silhouette.api.util.Credentials

import com.galacticfog.gestalt.meta.api.output._


/**
 * Code for integrating Security and Meta. Handles resource synchronization between
 * the two systems (Orgs and Accounts), and implements the REST endpoints for creating
 * Orgs and Users in Meta (ensuring they are created in Security as well).
 */
object Meta extends GestaltFrameworkSecuredController[DummyAuthenticator] with MetaController with NonLoggingTaskEvents {

  //
  // TODO: Tighten up error-handling - too many 500s
  // TODO: ResourceFactory::create - failed property validation assertion is being swallowed!
  //
  
  /**
   * Synchronize Meta with Security Orgs and Accounts.
   */
  def sync() = GestaltFrameworkAuthAction(nullOptString(None)) { implicit request => 
    syncSecurityResources(ResourceIds.Org, None) match {
      case Success(_) => {
        syncSecurityResources(ResourceIds.User, None) match {
          case Success(_) => Ok
          case Failure(e) => InternalServerError(e.getMessage)
        }
      }
      case Failure(e) => InternalServerError(e.getMessage)
    }
  }  
  
  
  //
  // TODO: Add creator to auth at create!
  //  
  
  
//  IMPLEMENT CREATE ORG FQON
//  IMPLEMENT CREATE USER FQON
  
  
  /** TODO: Return async GestaltTask
   * Create an Org in Security, then in Meta
   * API implements => POST /orgs/:uuid
   */
  def createOrg(org: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    trace(s"createOrg($org)")
    CreateSynchronizedResult(org, ResourceIds.Org, request.body)
  }
  
  def createOrgFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    trace(s"createOrgFqon($fqon)")
    orgFqon(fqon) match {
      case Some(org) => CreateSynchronizedResult(org.id, ResourceIds.Org, request.body)
      case None      => Future { OrgNotFound(fqon) }
    }
  }
  
  // TODO: Refactor to use CrateOrgResult
  def createTopLevelOrg() = GestaltFrameworkAuthAction(nullOptString(None)).async(parse.json) { implicit request =>
    Future {
      val input = request.body.as[GestaltResourceInput]
      val root = Security.getRootOrg(request.identity).get.id
      createSynchronized(ResourceIds.Org, root, input) match {
        case Success(_) => Ok
        case Failure(e) => InternalServerError(e.getMessage)
      }
    }
  }  
  
  
//  ...IMPLEMENT CREATE_USER FQON
//  CREATE RESOURCE_TYPE_WITH_PROPERTIES
  
  
  
  /** TODO: Return async GestaltTask
   * Create a User Account in Security, then in Meta
   * API implements => POST /orgs/:uuid/users
   */
  def createUser(org: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    CreateSynchronizedResult(org, ResourceIds.User, request.body)
  }
  
  def createUserFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => CreateSynchronizedResult(org.id, ResourceIds.User, request.body)
      case None      => Future { OrgNotFound(fqon) }
    }
  }
  
  /*
   * TODO: This method is shared between the Org and User resources. With minimal specification
   * the two input types are identical - validate properties to ensure things to to the right place.
   */  
  private def CreateSynchronizedResult[T](org: UUID, typeId: UUID, json: JsValue)(implicit request: SecuredRequest[T]) = {
    trace(s"CreateSynchronizedResult($org, $typeId, ${json.toString})")
    Future {
      safeGetInputJson(json) match {
        case Failure(e) => BadRequest(e.getMessage)
        case Success(input) => createSynchronized(typeId, org, input) match {
          case Success(_) => Ok
          case Failure(e) => InternalServerError(e.getMessage)
        }
      }
    }
  }

  
  private def safeGetInputJson(json: JsValue): Try[GestaltResourceInput] = Try {
    json.validate[GestaltResourceInput].map {
      case resource: GestaltResourceInput => resource
    }.recoverTotal{
      e => illegal(toError(400, JsError.toFlatJson(e).toString))
    }
  }


  /**
   * Create a resource in both Security and Meta. For new Orgs and Accounts 
   * created via the Meta API. Currently handles new Orgs and Users.
   */
  private def createSynchronized[T](typeId: UUID, org: UUID, input: GestaltResourceInput)(implicit request: SecuredRequest[T]) = Try {
    //
    // TODO: Convert function resolvers to PartialFunction to avoid redundant
    // checks on typeId while avoiding MatchError warnings.
    //
    
    // Resolve Security function to create resources.
    val securityCreate: (UUID, AuthAccountWithCreds, GestaltResourceInput) => Try[SecurityResource] = typeId match {
      case ResourceIds.Org  => Security.createOrg _
      case ResourceIds.User => Security.createAccount _
      case _ => illegal(Errors.INVALID_RESOURCE_TYPE_ID(typeId))
    }
    
    // Resolve Meta function to create resources.
    val metaCreate: (UUID, SecurityResource*) => Try[Unit] = typeId match {
      case ResourceIds.Org  => createNewMetaOrgs[T] _
      case ResourceIds.User => createNewMetaUsers[T] _
      case _ => illegal(Errors.INVALID_RESOURCE_TYPE_ID(typeId))
    }
      (for {
        s <- securityCreate(org, request.identity, input.copy(resource_type = Some(typeId)))
        r <- metaCreate(org, s)
      } yield r).get  
  }
  
  
  
  /**
   * Ensure Security and Meta contain the same resources of the given type.
   * 
   * @param typeId the Meta resource_type_id you want to synchronize
   * @param owningOrg the Org used as the value for org_id on the new resource
   */
  private def syncSecurityResources[T](typeId: UUID, owningOrg: Option[UUID])(implicit request: SecuredRequest[T]) = Try {
    
    // TODO: This defaults to the root Org if None given (is that correct?).
    val owner = orgOrElseRoot(owningOrg)
    
    // Resolve function to pull resources from Security
    val securityReader: (Option[UUID], AuthAccountWithCreds) => Try[Seq[SecurityResource]] = typeId match {
      case ResourceIds.Org  => Security.getAllOrgs _
      case ResourceIds.User => Security.getAllAccounts _
      case _ => illegal(Errors.INVALID_RESOURCE_TYPE_ID(typeId))
    }
    
    // Get the resources from Security
    val securityResources = securityReader(Some(owner), request.identity) match {
      case Success(rs) => rs.map(r => (r.id, r)).toMap
      case Failure(ex) => throw ex
    }
    
    // Get resource of the same type from Meta
    val metaResources = ResourceFactory.findAll(typeId, owner).map(r => (r.id, r)).toMap
    
    /*
     * Compare keys from both maps to determine which resources to 
     * create/delete in Meta (Security is authoritative).
     */
    val securityIds = securityResources.keys.toList
    val metaIds = metaResources.keys.toList
    val resourcesCreate = securityIds.diff(metaIds) map { id => securityResources(id) }
    val resourcesDelete = metaIds.diff(securityIds) map { metaResources(_) }
    
    // Resolve function to create new resources in Meta.
    val metaWriter: (UUID, SecurityResource*) => Try[Unit] = typeId match {
      case ResourceIds.Org  => createNewMetaOrgs[T] _
      case ResourceIds.User => createNewMetaUsers[T] _
      case _ => illegal(Errors.INVALID_RESOURCE_TYPE_ID(typeId))
    }
    
    // TODO: Implement DELETE.
    if (!resourcesCreate.isEmpty) metaWriter(owner, resourcesCreate : _ *) // else Success
  }
  
  
  /**
   * Create one or more Orgs in Meta from Security::GestaltOrg
   */
  private def createNewMetaOrgs[T](owningOrg: UUID, orgs: SecurityResource*)(implicit securedRequest: SecuredRequest[T]) = {
    trace(s"createNewMetaOrgs($owningOrg, [${orgs.size} orgs])")
    Try {
      for (org <- orgs.asInstanceOf[Seq[GestaltOrg]]) {
        val parent = if (org.parent.isDefined) Some(org.parent.get.id) else None
        val props = Some(Map("fqon" -> org.fqon))
        ResourceFactory.create(toMeta(org, ResourceIds.Org, owningOrg, props), parent)
      }
    }
  }
  
  
  /**
   * Create one or more Users in Meta from Security::GestaltAccount
   */
  private def createNewMetaUsers[T](owningOrg: UUID, accounts: SecurityResource*)(implicit securedRequest: SecuredRequest[T]) = {
    trace(s"createNewMetaUsers($owningOrg, [${accounts.size} users])")
    Try {
      for (acc <- accounts.asInstanceOf[Seq[GestaltAccount]]) {
        trace(s"Creating User : ${acc.id}, ${acc.name}")
        val props = Some(Map(
          "email" -> acc.email,
          "firstName" -> acc.firstName,
          "lastName" -> acc.lastName,
          "phoneNumber" -> acc.phoneNumber))
        ResourceFactory.create(toMeta(acc, ResourceIds.User, owningOrg, props))
      }
    }
  }
  
  /**
   * Convert a Security Org or Account to a Meta representation. 
   * (could be used for other object types as well)
   */
  private def toMeta[T](sr: SecurityResource, typeId: UUID, org: UUID, properties: Option[Hstore] = None)(implicit request: SecuredRequest[T]) = {
    GestaltResourceInstance(
      id = sr.id,
      typeId = typeId,
      orgId = org, // this needs to be org from URI
      owner = ResourceOwnerLink(ResourceIds.User, request.identity.account.id),
      name = sr.name,
      properties = properties)
  }
  
  /**
   * Unwrap the given UUID or get the root org_id if None.
   */
  private def orgOrElseRoot[T](org: Option[UUID])(implicit request: SecuredRequest[T]) = org getOrElse {
    Security.getRootOrg(request.identity) match {
      case Success(org) => org.id
      case Failure(ex)  => throw ex
    }
  }
  
  /**
   * TODO: This will be replaced with an appropriate type-renderer.
   */
  private def pretty(r: GestaltResourceInstance) = Json.prettyPrint(Json.toJson(r))
  
}