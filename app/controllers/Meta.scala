package controllers

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.PropertyValidator
import com.galacticfog.gestalt.data.ResourceFactory

import com.galacticfog.gestalt.data.ResourceType
import com.galacticfog.gestalt.data.illegal
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.{ PatchOp, PatchDocument, PatchHandler }

import com.galacticfog.gestalt.meta.api.output._

//import com.galacticfog.gestalt.meta.api.rte
//import com.galacticfog.gestalt.meta.api.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.{ GestaltResource => SecurityResource }

import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.galacticfog.gestalt.tasks.play.io.NonLoggingTaskEvents
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator

import controllers.util._
import controllers.util.MetaController
import controllers.util.Security

import play.api.{ Logger => log }
import play.api.libs.json.JsError
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._

import controllers.util.stringmap

import controllers.util.trace
import com.galacticfog.gestalt.meta.api._

/**
 * Code for integrating Security and Meta. Handles resource synchronization between
 * the two systems (Orgs and Accounts), and implements the REST endpoints for CRUD
 * operations on Orgs and Users in Meta (ensuring they are created in Security as well).
 *
 * TODO: Security doesn't have PUT/PATCH endpoints - need that.
 */
object Meta extends GestaltFrameworkSecuredController[DummyAuthenticator]
  with MetaController with NonLoggingTaskEvents with SecurityResources {
  
  import play.api.libs.json._
  
  implicit def str2js(s: String) = JsString(s)
  
  //
  // TODO: Add creator to auth at create!
  //  

  def createTopLevelOrg() = Authenticate().async(parse.json) { implicit request =>
    val root = Security.getRootOrg(request.identity).get.id
    Security.getRootOrg(request.identity) match {
      case Success(root) =>
        CreateSynchronizedResult(root.id, ResourceIds.Org, request.body)(
          Security.createOrg, createNewMetaOrg[JsValue])
      case Failure(err) => Future { handleSecurityApiException(err) }
    }
  }  
  
  /**
   * TODO: Return async GestaltTask
   * Create an Org in Security, then in Meta
   * API implements => POST /orgs/:uuid
   */
  def createOrg(org: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    trace(s"createOrg($org)")
    CreateSynchronizedResult(org, ResourceIds.Org, request.body)(
      Security.createOrg, createNewMetaOrg[JsValue])
  }

  def createOrgFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    trace(s"createOrgFqon($fqon)")
    orgFqon(fqon) match {
      case None => Future { OrgNotFound(fqon) }
      case Some(org) => {
        CreateSynchronizedResult(org.id, ResourceIds.Org, request.body)(
          Security.createOrg, createNewMetaOrg[JsValue])
      }
    }
  }

  def createResource(org: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    Future {
      CreateResourceResult(ResourceIds.User, request.identity.account.id, org, request.body, request.identity)
    }
  }
  
  def createResourceFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    trace(s"createResource($fqon)")
    Future {
      orgFqon(fqon) match {
        case Some(org) => 
          CreateResourceResult(ResourceIds.User, request.identity.account.id, org.id, request.body, request.identity)
        case None      => OrgNotFound(fqon)
      }
    }
  }

  def createResource2(org: UUID, typeId: Option[UUID] = None)(implicit request: SecuredRequest[JsValue]) = {
    Future {
      CreateResourceResult(
          ResourceIds.User, 
          request.identity.account.id, 
          org, 
          request.body, 
          request.identity, 
          typeId)
    }
  }

  def createSystemResource(fqon: String, restName: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    trace(s"createSystemResource($fqon)")
    Future { 
      extractByName(fqon, restName) match {
        case Left(result) => result
        case Right((org, pathtype)) => CreateNoConflictResult(org, pathtype)
      }
    }
  }
  

  
//  private def pathCreateError(pathType: UUID, givenType: UUID) = {
//    toError(409, "conflicting resource_type_ids found.")
//  }
  
  private def CreateConflictResult(pathType: UUID, input: GestaltResourceInput) = {
    //Conflict(pathCreateError(pathType, input.resource_type.get))
    ConflictResult("conflicting resource_type_ids found.")
  }

  private def CreateNoConflictResult(org: UUID, pathType: UUID)(implicit request: SecuredRequest[JsValue]) = {
    getWellFormedInput(org, request.body, Some(pathType)) match {
      case Failure(error) => HandleRepositoryExceptions(error)
      case Success(input) => {

        // Ensure no conflict between given resource_type (if any) and pathtype
        // (i.e. can't create a Node in /clusters)

        if (pathType != input.resource_type) 
             CreateConflictResult(pathType, input)
        else CreateResourceResult2(ResourceIds.User, request.identity.account.id, org, input, request.identity)
      }
    }    
  }  
  
  def patchResource(org: UUID, id: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    trace(s"patchResource($org, $id)")
    resourcePatch(id)
  }
  
  def patchResourceFqon(fqon: String, id: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    trace(s"patchResourceFqon($fqon, $id)")
    resourcePatch(id)
  }
  
  def resourcePatch(id: UUID)(implicit request: SecuredRequest[JsValue]) = {
    Future {
      safeGetPatchDocument(request.body) match {
        case Failure(e) => BadRequestResult(e.getMessage)
        case Success(patch) => {
          
          // TODO: Don't currently use typeId, but will in future.
          val identity = request.identity.account.id
          PatchHandler(UUID.randomUUID(), id, patch).applyPatch(ResourceIds.User, identity) match {
            case Success(r) => Ok(Output.renderInstance(r))
            case Failure(e) => HandleRepositoryExceptions(e) 
          }
        }
      }      
    }
  }
  
  def postWorkspace(org: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    Meta.createResource2(org, Some(ResourceIds.Workspace))
  }
  
  def postWorkspaceFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => Meta.createResource2(org.id, Some(ResourceIds.Workspace))
      case None => Future { OrgNotFound(fqon) }
    }
  }  
  

  def postEnvironmentWorkspaceFqon(fqon: String, workspaceId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    orgFqon(fqon) match {
      case None => Future { OrgNotFound(fqon) }  
      case Some(org) => {
        Meta.createResource2(org.id, Some(ResourceIds.Workspace))
      }
    }
  }

  private object Err {
    
    def RESOURCE_TYPE_NOT_FOUND(m: UUID) = s"Given ResourceType 'resource_type : $m' does not exist."
    
    val RESOURCE_TYPE_NOT_GIVEN = "resource_type must be specified."
    
  }

  /**
   * Ensure the given JSON can be serialized to a GestaltResourceInput with required values.
   * Currently checks are limited to whether a resource_type_id is supplied, and whether
   * that type_id exists.
   */
  private def getWellFormedInput(org: UUID, inputJson: JsValue, typeId: Option[UUID] = None): Try[GestaltResourceInput] = {
    
    /* Assert that the given type_id exists */
    def assertTypeExists(typeId: UUID) = if (!TypeFactory.findById(typeId).isEmpty) {
      throw new ResourceNotFoundException(Err.RESOURCE_TYPE_NOT_FOUND(typeId))
    }
    
    def resolveInputTypeId(in: GestaltResourceInput) = in.resource_type orElse { typeId }

    safeGetInputJson(inputJson) map { input =>
      assertTypeExists {
        resolveInputTypeId(input) getOrElse {
          throw new BadRequestException(Err.RESOURCE_TYPE_NOT_GIVEN)
        }
      }
      input
    }
  }
  
  
  /**
   * Inspect a GestaltResourceInput, supplying default values where appropriate.
   */
  private def inputWithDefaults(org: UUID, input: GestaltResourceInput, creator: AuthAccountWithCreds) = {
    val owner = if (input.owner.isDefined) input.owner else Some(ownerFromAccount(creator))
    val resid = if (input.id.isDefined) input.id else Some(UUID.randomUUID())
    val state = if (input.resource_state.isDefined) input.resource_state else Some(ResourceStates.Active)
    fromResourceInput(org, input.copy(id = resid, owner = owner, resource_state = state))    
  }
  
  
  private def CreateResourceResult2(creatorType: UUID, creator: UUID, org: UUID, input: GestaltResourceInput, user: AuthAccountWithCreds) = {
    trace(s"CreateResourceResult($org, [json], [account])")

    ResourceFactory.create(creatorType, creator)(inputWithDefaults(org, input, user)) match {
      case Success(res) => Ok(Output.renderInstance(res))
      case Failure(err) => {
        log.error(s"Internal Server Error: ${err.getMessage}")
        GenericErrorResult(500, err.getMessage)
      }
    }
  }  
  
  def resolveTypeId(r: GestaltResourceInput, typeId: Option[UUID]) = {
    if (r.resource_type.isDefined) r.resource_type
    else if (typeId.isDefined) typeId
    else None
  }
  
  
  /*
   * TODO: This needs to be broken up further - need the ability to inspect/inject properties before create.
   * For instance, user shouldn't have to provide 'workspace' property when creating an environment at the
   * POST /workspaces/:id/environments. In order to make that possible, the implementing action needs the
   * ability to inject the workspace ID.
   */
  private def CreateResourceResult(creatorType: UUID, creator: UUID, org: UUID, resourceJson: JsValue, user: AuthAccountWithCreds, typeId: Option[UUID] = None) = {
    trace(s"CreateResourceResult($org, [json], [account])")
    
    def typeExists(typeId: UUID) = !TypeFactory.findById(typeId).isEmpty
    
    safeGetInputJson(resourceJson) match {
      case Failure(e) => BadRequestResult(e.getMessage)
      case Success(input) => {
  
        val tid = resolveTypeId(input, typeId)
        
        //if (input.resource_type.isEmpty) {
        if (tid.isEmpty) {
          log.error(s"No resource_type specified.")
          BadRequestResult(Err.RESOURCE_TYPE_NOT_GIVEN)
        } 
        else if (!typeExists(/*input.resource_type.get*/tid.get)) {
          log.error(Err.RESOURCE_TYPE_NOT_FOUND(tid.get))
          NotFoundResult(Err.RESOURCE_TYPE_NOT_FOUND(tid.get))
        } 
        else {
          
          val owner = if (input.owner.isDefined) input.owner else Some(ownerFromAccount(user))
          val resid = if (input.id.isDefined) input.id else Some(UUID.randomUUID())
          val state = if (input.resource_state.isDefined) input.resource_state else Some(ResourceStates.Active)
          val domain = fromResourceInput(org,
            input.copy(id = resid, owner = owner, resource_state = state, resource_type = tid))

          /* 
           * TODO: create now returns better errors - match them to be more specific 
           */
          ResourceFactory.create(creatorType, creator)(domain) match {
            case Success(res) => {
              println("========== INSTANCE ==========")
              println(res)
              Ok(Output.renderInstance(res))
            }
            case Failure(err) => {
              log.error(s"Internal Server Error: ${err.getMessage}")
              GenericErrorResult(500, err.getMessage)
            }
          }
        }
      }
    }
  }



  /**
   * Create a User Account in Security, then in Meta
   * API implements => POST /orgs/:uuid/users
   */
  def createUser(org: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    trace(s"createUser(org = $org)")
    CreateSynchronizedResult(org, ResourceIds.User, request.body)(
      Security.createAccount, createNewMetaUser[JsValue])
  }

  def createUserFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    trace(s"createUserFqon(fqon = $fqon)")
    orgFqon(fqon) match {
      case None => Future { OrgNotFound(fqon) }
      case Some(org) =>
        CreateSynchronizedResult(org.id, ResourceIds.User, request.body)(
          Security.createAccount, createNewMetaUser[JsValue])
    }
  }

  private def CreateSynchronizedResult[T](org: UUID, typeId: UUID, json: JsValue)(sc: SecurityResourceFunction, mc: MetaResourceFunction)(implicit request: SecuredRequest[T]) = {
    trace(s"CreateSynchronizedResult($org, $typeId, ${json.toString})")
    Future {
      safeGetInputJson(typeId, json) match {
        case Failure(e) => BadRequestResult(e.getMessage)
        case Success(input) => {

          createSynchronized(org, typeId, input)(sc, mc) match {
            case Success(resource) => Ok(Output.renderInstance(resource))
            case Failure(e)        => InternalServerError(e.getMessage)
          }

        }
      }
    }
  }

  private def createSynchronized[T](org: UUID, typeId: UUID, input: GestaltResourceInput)
    (sc: SecurityResourceFunction, mc: MetaResourceFunction)(implicit request: SecuredRequest[T]) = {

    trace("createSynchronized(...)")
    //log.debug(s"Creating ${ResourceType.name(typeId)}")

    val stringprops = stringmap(input.properties)
    val creator = request.identity.account.id

    PropertyValidator.validate(typeId, stringprops) match {
      case (false, message) => Failure(illegal(message.get))
      case _ => for {
        sr <- sc(org, request.identity, input)
        mr <- mc(creator, org,  sr, stringprops)
      } yield mr
    }
  }
  
  
  private def safeGetPatchDocument(json: JsValue): Try[PatchDocument] = Try {
    PatchDocument.fromJsValue(json)
  }

  
  private def safeGetInputJson(json: JsValue): Try[GestaltResourceInput] = Try {

    implicit def jsarray2str(arr: JsArray) = arr.toString

    json.validate[GestaltResourceInput].map {
      case resource: GestaltResourceInput => {
        log.debug("RESOURCE : " + resource)
        resource
      }
    }.recoverTotal { e =>
      log.error("Error parsing request JSON: " + JsError.toFlatJson(e).toString)
      illegal(JsError.toFlatJson(e).toString)
    }
  }

  
  private def safeGetInputJson(typeId: UUID, json: JsValue): Try[GestaltResourceInput] = Try {
    val res = json.validate[GestaltResourceInput].map {
      case resource: GestaltResourceInput => resource
    }.recoverTotal {
      e => illegal(JsError.toFlatJson(e).toString)
    }

    log.debug(s"Validating instance properties for type ${ResourceType.name(typeId)}")
    val validation = PropertyValidator.validate(typeId, stringmap(res.properties))

    if (validation._1) res else illegal(validation._2.get)
  }


  /**
   * Convert GestaltResourceInput to GestaltResourceInstance
   */
  private def fromResourceInput(org: UUID, in: GestaltResourceInput) = {
    GestaltResourceInstance(
      id = in.id.get,
      typeId = in.resource_type.get,
      state = ResourceState.id(in.resource_state.get),
      orgId = org,
      owner = in.owner.get,
      name = in.name,
      description = in.description,
      /* TODO: Here is where we transform from map(any) to map(string) */
      properties = stringmap(in.properties),
      tags = in.tags,
      auth = in.auth)
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