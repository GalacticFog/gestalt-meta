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

  /*
   * FOR LAMBDAS: Decide: create and inject properties.parent, or build dynamically on request?
   */
  
  def postApi(org: UUID, environment: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    trace(s"postApi($org, $environment)")
    createResourceCommon(org, environment, ResourceIds.Api)
  }
  
  def postApiFqon(fqon: String, parent: UUID) = Authenticate().async(parse.json) { implicit request =>
    trace(s"postApiFqon($fqon, $parent)")
    orgFqon(fqon) match {
      case Some(org) => createResourceCommon(org.id, parent, ResourceIds.Api)
      case None => Future { OrgNotFound(fqon) }
    }
  }  
  
  def postApiEndpoint(org: UUID, parent: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    postEndpoint(org, parent)
    Future { Ok("nothing") }
  }
  
  def postApiEndpointFqon(fqon: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    trace(s"postApiEndpointFqon($fqon, $parent)")
    Future {
      orgFqon(fqon) match {
        case Some(org) => postEndpoint(org.id, parent) match {
          case Success(endpoint) => Ok(Output.renderInstance(endpoint))
          case Failure(error) => HandleRepositoryExceptions(error)
        }
        case None => OrgNotFound(fqon)
      }  
    }
  }

  def getJsonPropertyField(json: JsValue, field: String) = {
    json \ "properties" \ field match {
      case u: JsUndefined => None
      case v => Some(v)
    }
  }
  
  def getJsonField(json: JsValue, field: String) = {
    json \ field match {
      case u: JsUndefined => None
      case v => Some(v)
    }
  }
  


  def getImplProps(json: JsValue): Try[Map[String,String]] = Try {
    
    val tpe = getJsonField(json, "type") match {
      case None => throw new BadRequestException("Must specify 'type' property.")
      case Some(tpe) => {
        val t = tpe.as[String]
        if (t.trim.toLowerCase != "lambda") {
          throw new BadRequestException(s"Only supporting implementations of type 'Lambda' at this time. found: $tpe")
        }
        t
      }
    }
    
    val id = getJsonField(json, "id") match {
      case None => throw new BadRequestException("Must specify 'id' property.")
      case Some(f) => {
        parseUUID(f.as[String]) getOrElse {
          throw new BadRequestException(s"'id' property must be a valid v4 UUID. found: $f")
        }        
      }
    }
    
    val func = getJsonField(json, "function") match {
      case None => throw new BadRequestException("Must specify 'function' property")
      case Some(f) => f.as[String]
    }
    
    Map("type" -> tpe, "id" -> id, "function" -> func)
  }
  
  import scala.util.Either
  
  def getEndpointImplementation(json: JsValue): Either[Throwable, Option[Map[String,String]]] = {
    
    println
    println("JSON-FIELD : name : " + getJsonField(json, "name"))
    println("JSON-FIELD : implementation : " + getJsonPropertyField(json, "implementation"))
    println("JSON-FIELD : " + json)
    println
    
    getJsonPropertyField(json, "implementation") match {
      case None => Right(None)
      case Some(impl) => {
        getImplProps(impl) match {
          case Success(props) => Right(Some(props))
          case Failure(error) => Left(error)
        }
      }
    }
  }
  
  
  
  def postEndpoint(org: UUID, parent: UUID)(implicit request: SecuredRequest[JsValue]) = Try {
    trace(s"postEndpoint($org, $parent)")
    
    
    val (lambda, props) = getEndpointImplementation(request.body) match {
      case Left(err) => throw err
      case Right(props) => {
        println("****************getEndpointImplementation SUCCEEDED!!!!")
        if (props.isEmpty) (None,None) else {
          val id = props.get("id")
          (ResourceFactory.findById(ResourceIds.Lambda, id) match {
            case Some(res) => Some(res)
            case None => throw new ResourceNotFoundException(s"Lambda with ID '$id' not found.")
          }, props)
        }
      }
    }
    
    println
    println("*************************************")
    println("LAMBDA : " + lambda)
    println("PROPS : " + props)
    println("*************************************")
    println
    
    // Create the ApiEndpoint
    val typeId = ResourceIds.ApiEndpoint
    val endpoint = safeGetInputJson(typeId, request.body) match {
      case Failure(e)     => throw new BadRequestException(e.getMessage)
      case Success(input) => {
        CreateResource(ResourceIds.User, request.identity.account.id,
            org, request.body, request.identity,
            typeId = Some(typeId), 
            parentId = Some(parent))
      }
    }
    
    endpoint match {
      case Failure(err) => throw err
      case Success(enp) => {
        if (lambda.isDefined) {
          ResourceFactory.addEndpointToLambda(enp.id, lambda.get.id, props.get("function"))    
        }
        enp
      }
    }
    
  }
  

  def idFromImpl(json: JsValue): UUID = {
    val tpe = json \ "type" match {
      case u: JsUndefined => throw new BadRequestException("Must specify 'type'")
      case t => t.as[String]
    }
    
    if (tpe.trim.toLowerCase != "lambda") {
      throw new BadRequestException(s"Only supporting implementations of type 'Lambda' at this time. found: $tpe")
    }
    
    val id = json \ "id" match {
      case u : JsUndefined => throw new BadRequestException("Must specify 'id'")
      case i => i.as[String]
    }
    
    if (parseUUID(id).isEmpty)
      throw new BadRequestException(s"'id' property must be a valid v4 UUID. found: $id")
    
    UUID.fromString(id)
  }
  
  def postLambda(org: UUID, parent: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    trace(s"postLambda($org, $parent)")
    createLambdaCommon(org, parent)
  }
  
  def postLambdaFqon(fqon: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    trace(s"postLambdaFqon($fqon, $parent)")
    orgFqon(fqon) match {
      case Some(org) => createLambdaCommon(org.id, parent)
      case None => Future { OrgNotFound(fqon) }
    }  
  }
  
  def postProviderConfig(org: UUID, parent: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    trace(s"postProviderConfig($org, $parent)")
    createResourceCommon(org, parent, ResourceIds.ApiGatewayProvider)  
  }
  
  def postProviderConfigFqon(fqon: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    trace(s"postProviderConfigFqon($fqon, $parent)")
    orgFqon(fqon) match {
      case Some(org) => createResourceCommon(org.id, parent, ResourceIds.ApiGatewayProvider)
      case None => Future { OrgNotFound(fqon) }
    }
  }  
  
  def postDomain(org: UUID, parent: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    createResourceCommon(org, parent, ResourceIds.Domain)
  }
  
  def postDomainFqon(fqon: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => createResourceCommon(org.id, parent, ResourceIds.Domain)
      case None => Future { OrgNotFound(fqon) }
    }
  }

  
  def createResourceCommon(org: UUID, parentId: UUID, typeId: UUID)(implicit request: SecuredRequest[JsValue]) = {
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
  
  /*
   * 
   * 1.) Transform JSON from request to ResourceInstance
   * 2.) Create or update the instance
   * 3.) Return an HTTP Result
   * 
   */
  
  def createResource(org: UUID, parentId: UUID, typeId: UUID)(implicit request: SecuredRequest[JsValue]) = {
      safeGetInputJson(typeId, request.body) match {
        case Failure(e)     => BadRequestResult(e.getMessage)
        case Success(input) => {
          CreateResource(ResourceIds.User, request.identity.account.id,
              org, request.body, request.identity,
              typeId = Some(typeId), 
              parentId = Some(parentId))
        }
      } 
  }
  
  def createLambdaCommon(org: UUID, parentId: UUID)(implicit request: SecuredRequest[JsValue]) = {
   
    Future {
    
      safeGetInputJson(ResourceIds.Lambda, request.body) match {
        case Failure(e)     => BadRequestResult(e.getMessage)
        case Success(input) => {
          CreateResourceResult(
              ResourceIds.User, 
              request.identity.account.id,
              org, request.body, request.identity,
              typeId = Some(ResourceIds.Lambda), 
              parentId = Some(parentId) )
        }
      }
      
    }
  }
  
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

  // Same as createResource2 but takes JSON arg instead of extracting from request.body
  def createResource3(org: UUID, json: JsValue, typeId: Option[UUID] = None)(implicit request: SecuredRequest[JsValue]) = {
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


  def postLambdaWorksapce(org: UUID, workspace: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    // Check if parent object is given in properties
    // YES: verify type and id
    // NO: create a new parent object from this workspace and inject
    ???  
  }  
  
  
  def normalizeResourceType(obj: JsValue, expectedType: UUID) = Try {
    (obj \ "resource_type" match {
      case u : JsUndefined => obj.as[JsObject] + ("resource_type" -> expectedType.toString)
      case t => {
        if (!(t.as[UUID] == expectedType))
          throw new BadRequestException(
              s"Unexpected resource_type. found: ${t.toString}, required: ${expectedType.toString}")
        else obj
      }
    }).as[JsObject]    
  }
  
  // Inject 'properties/workspace' field if missing from JSON object
  def normalizeWorkspace(obj: JsObject, value: UUID) = Try {
    obj \ "properties" \ "workspace" match {
      case u : JsUndefined => {
        val ps  = replaceJsonPropValue(obj, "workspace", value.toString)
        replaceJsonProps(obj, ps)
      }
      case _ => obj
    }    
  }
  
  // Replace environment_type simple-name into type UUID in environment properties
  def normalizeEnvironmentType(env: JsObject) = Try {
    val envTypeId = env \ "properties" \ "environment_type" match {
      case u : JsUndefined => throw new BadRequestException("Missing required property : 'environment_type'")
      case n => EnvironmentType.id(n.as[String])
    }
    env ++ Json.obj(
        "properties" -> 
        replaceJsonPropValue(env, "environment_type", envTypeId.toString)) 
  }
  
  def normalizeEnvironment(env: JsValue, wk: Option[UUID] = None) = {
    for {
      a <- normalizeResourceType(env, ResourceIds.Environment)
      b <- normalizeWorkspace(a, wk.get)
      c <- normalizeEnvironmentType(b)
    } yield c    
  }
  
  def postEnvironment(org: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    postEnvironmentResult(org)
  }
  
  def postEnvironmentFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
      orgFqon(fqon) match {
        case Some(org) => postEnvironmentResult(org.id)
        case None => Future { OrgNotFound(fqon) }
      }
  }
  
  def postEnvironmentResult(org: UUID)(implicit request: SecuredRequest[JsValue]) = {
    (for {
      a <- normalizeResourceType(request.body, ResourceIds.Environment)
      b <- normalizeEnvironmentType(a)
    } yield b) match {
      case Success(env) => Meta.createResource3(org, env, Some(ResourceIds.Environment))
      case Failure(err) => Future { HandleRepositoryExceptions(err) }
    }    
  }
  
  def postEnvironmentWorkspace(org: UUID, workspaceId: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    normalizeEnvironment(request.body, Some(workspaceId)) match {
      case Success(env) => Meta.createResource3(org, env, Some(ResourceIds.Environment))
      case Failure(err) => Future { HandleRepositoryExceptions(err) }
    }
  }
  
  def postEnvironmentWorkspaceFqon(fqon: String, workspaceId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    orgFqon(fqon) match {
      case None => Future { OrgNotFound(fqon) }  
      case Some(org) => {
        //Meta.createResource2(org.id, Some(ResourceIds.Workspace))
        normalizeEnvironment(request.body, Some(workspaceId)) match {
          case Success(env) => Meta.createResource3(org.id, env, Some(ResourceIds.Environment))
          case Failure(err) => Future { HandleRepositoryExceptions(err) }
        }        
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
  private def CreateResourceResult(
      creatorType: UUID, 
      creator: UUID, 
      org: UUID, 
      resourceJson: JsValue, 
      user: AuthAccountWithCreds, 
      typeId: Option[UUID] = None,
      parentId: Option[UUID] = None) = {
    
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
          ResourceFactory.create(creatorType, creator)(domain, parentId = parentId) match {
            case Success(res) => Ok(Output.renderInstance(res))
            case Failure(err) => HandleRepositoryExceptions(err) 
//            {
//              log.error(s"Internal Server Error: ${err.getMessage}")
//              GenericErrorResult(500, err.getMessage)
//            }
          }
        }
      }
    }
  }
  
  
 private def CreateResource(
      creatorType: UUID, 
      creator: UUID, 
      org: UUID, 
      resourceJson: JsValue, 
      user: AuthAccountWithCreds, 
      typeId: Option[UUID] = None,
      parentId: Option[UUID] = None): Try[GestaltResourceInstance] = {
    
    trace(s"CreateResourceResult($org, [json], [account])")
    
    def typeExists(typeId: UUID) = !TypeFactory.findById(typeId).isEmpty
    
    safeGetInputJson(resourceJson) match {
      case Failure(e) => throw new BadRequestException(e.getMessage)
      case Success(input) => {
  
        val tid = resolveTypeId(input, typeId)
        
        if (tid.isEmpty) {
          log.error(s"No resource_type specified.")
          throw new BadRequestException(Err.RESOURCE_TYPE_NOT_GIVEN)
        } 
        else if (!typeExists(/*input.resource_type.get*/tid.get)) {
          log.error(Err.RESOURCE_TYPE_NOT_FOUND(tid.get))
          throw new ResourceNotFoundException(Err.RESOURCE_TYPE_NOT_FOUND(tid.get))
        } 
        else {
          
          val owner = if (input.owner.isDefined) input.owner else Some(ownerFromAccount(user))
          val resid = if (input.id.isDefined) input.id else Some(UUID.randomUUID())
          val state = if (input.resource_state.isDefined) input.resource_state else Some(ResourceStates.Active)
          val domain = fromResourceInput(org,
            input.copy(id = resid, owner = owner, resource_state = state, resource_type = tid))

          ResourceFactory.create(creatorType, creator)(domain, parentId = parentId)
        }
      }
    }
  }  
  

  def normalizeUserJson(json: JsObject, account: AuthAccountWithCreds) = {
    // Check if default_org property is set - if not
    // create property and set to 'root' Org.
    
    json \ "properties" \ "default_org" match {
      case u: JsUndefined => {
        val root = Security.getRootOrg(account)
        val props = replaceJsonPropValue(json, "gestalt_home", root.get.id.toString)
        replaceJsonProps(json, props)
      }
      case _ => json
    }
  }
  
  /**
   * Create a User Account in Security, then in Meta
   * API implements => POST /orgs/:uuid/users
   */
  def createUser(org: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    trace(s"createUser(org = $org)")
    val userJson = normalizeUserJson(request.body.as[JsObject], request.identity)
    
    CreateSynchronizedResult(org, ResourceIds.User, userJson)(
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
    trace(s"safeGetInputJson($typeId, [json]")

    val res = json.validate[GestaltResourceInput].map {
      case resource: GestaltResourceInput => resource
    }.recoverTotal {
      e => illegal(JsError.toFlatJson(e).toString)
    }
    
    log.debug(s"Validating instance properties for type ${ResourceType.name(typeId)}")
    val validation = PropertyValidator.validate(typeId, stringmap(res.properties))

    if (validation._1) res else illegal(validation._2.get)
  }


  def resolveResourceState(state: Option[String]) = {
//    if (state.isDefined) ResourceState.id(state.get)
//    else ResourceState.id(ResourceStates.Active)
    
    ResourceState.id( state getOrElse ResourceStates.Active )
  }
  
  
  /**
   * Convert GestaltResourceInput to GestaltResourceInstance
   */
  private def fromResourceInput(org: UUID, in: GestaltResourceInput) = {
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