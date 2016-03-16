package controllers


import java.util.UUID
import java.net.URL
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
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.{ GestaltResource => SecurityResource }
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.galacticfog.gestalt.tasks.play.io.NonLoggingTaskEvents
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import controllers.util.JsonUtil._
import controllers.util.db._
import controllers.util.MetaController
import controllers.util.Security
import play.api.{ Logger => log }
import play.api.libs.json._
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import controllers.util.stringmap
import controllers.util.trace
import com.galacticfog.gestalt.meta.api._
import play.api.mvc.Result
import com.galacticfog.gestalt.laser._


/**
 * Code for POST and PATCH of all resource types.
 *
 */
object Meta extends GestaltFrameworkSecuredController[DummyAuthenticator]
  with MetaController with NonLoggingTaskEvents with SecurityResources {
  

  // --------------------------------------------------------------------------
  // ORGS
  // --------------------------------------------------------------------------  
  
  def postTopLevelOrg() = Authenticate().async(parse.json) { implicit request =>
    Security.getRootOrg(request.identity) match {
      case Success(root) =>
        CreateSynchronizedResult(root.id, ResourceIds.Org, request.body)(
          Security.createOrg, createNewMetaOrg[JsValue])
      case Failure(err) => Future { HandleExceptions(err) }
    }
  }  

  def postOrg(org: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    createOrgCommon(org, request.body)
  }

  def postOrgFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    createOrgCommon(id(fqon), request.body)
  }

  def createOrgCommon(org: UUID, json: JsValue)(implicit request: SecuredRequest[JsValue]) = {
    CreateSynchronizedResult(org, ResourceIds.Org, json)(
      Security.createOrg, createNewMetaOrg[JsValue])    
  }
  
  // --------------------------------------------------------------------------
  // USERS
  // --------------------------------------------------------------------------  
  
  /**
   * Create a User Account in Security, then in Meta
   * API implements => POST /orgs/:uuid/users
   */
  def postUser(org: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    createUserCommon(org, request.body)
  }

  
  def postUserFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    createUserCommon(id(fqon), request.body)
  }

  def createUserCommon(org: UUID, json: JsValue)(implicit request: SecuredRequest[JsValue]) = {
    val root = Security.getRootOrg(request.identity).get.fqon
    val userJson = upsertProperty(request.body.as[JsObject], "gestalt_home", JsString(root))
    
    CreateSynchronizedResult(org, ResourceIds.User, userJson.get)(
      Security.createAccount, createNewMetaUser[JsValue])    
  }  
  
  
  
  // --------------------------------------------------------------------------
  // GENERIC RESOURCE
  // --------------------------------------------------------------------------
  
  def postResource(org: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    Future {
      CreateResourceResult(
          ResourceIds.User, 
          request.identity.account.id, 
          org, request.body, request.identity)
    }
  }
  
  def postResourceFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    Future {
      CreateResourceResult(
          ResourceIds.User, 
          request.identity.account.id, 
          id(fqon), request.body, request.identity)
    }
  }

  
  // --------------------------------------------------------------------------
  // RESOURCE PATCH
  // --------------------------------------------------------------------------
  
  def patchResource(org: UUID, id: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    resourcePatch(id)
  }
  
  def patchResourceFqon(fqon: String, id: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
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
            case Failure(e) => HandleExceptions(e) 
          }
        }
      }      
    }
  }
  
  // --------------------------------------------------------------------------
  // WORKSPACES
  // --------------------------------------------------------------------------
  
  def postWorkspace(org: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    createWorkspaceCommon(org, request.body, request.identity, META_URL)
  }

  def postWorkspaceFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    createWorkspaceCommon(id(fqon), request.body, request.identity, META_URL)
  }

  def createWorkspaceCommon(org: UUID, json: JsValue, user: AuthAccountWithCreds, baseUri: Option[String]) = {
    Future {
      /* Create the workspace */
      val workspace = CreateResource(ResourceIds.User, user.account.id, org, json, user, Some(ResourceIds.Workspace)) 
      
      /* Add ALL gateway providers to new workspace. */
      workspace match {
        case Failure(e) => HandleExceptions(e)
        case Success(workspace) => {
          
          // TODO: [TEMPORARY]: Create ApiGatewayProvider under workspace
          LaserController.getLaserProviders(org) foreach { p =>
            CreateResource(ResourceIds.User, user.account.id, org, p, user,
              Some(ResourceIds.ApiGatewayProvider), Some(workspace.id))
          }
          
          // TODO: [TEMPORARY]: Create MarathonProvider under workspace
          val marathon = newMarathonProvider("Marathon::" + UUID.randomUUID.toString)
          log.debug("Attaching MarathonProvider to workspace:\n" + Json.prettyPrint(marathon))
          
          CreateResource(ResourceIds.User, user.account.id, org, marathon, user,
              Some(ResourceIds.MarathonProvider), Some(workspace.id)) match {
            case Success(instance) => "Successfully created marathon provider: " + instance.id
            case Failure(error) => log.error(error.getMessage)
          }
              
          Created(Output.renderInstance(workspace, baseUri))
        }
      }
    }
  }

  /** 
   *  TODO: [TEMPORARY]: Create a new MarathonProvider configuration using URL
   *  contained in the GESTALT_MARATHON_PROVIDER environment variable.
   */
  def newMarathonProvider(name: String) = {
    Json.obj("name" -> name, 
        "properties" -> Json.obj("config" -> 
          Json.obj("url" -> EnvConfig.marathonUrl)))
  }
  
  // --------------------------------------------------------------------------
  // ENVIRONMENTS
  // --------------------------------------------------------------------------

  def postEnvironment(org: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    postEnvironmentResult(org)
  }

  def postEnvironmentFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => postEnvironmentResult(org.id)
      case None      => Future { OrgNotFound(fqon) }
    }
  }
  
  def postEnvironmentResult(org: UUID)(implicit request: SecuredRequest[JsValue]) = {
    (for {
      a <- normalizeResourceType(request.body, ResourceIds.Environment)
      b <- normalizeEnvironmentType(a)
    } yield b) match {
      case Success(env) => createResourceD(org, env, Some(ResourceIds.Environment))
      case Failure(err) => Future { HandleRepositoryExceptions(err) }
    }    
  }
  
  def postEnvironmentWorkspace(org: UUID, workspaceId: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    normalizeEnvironment(request.body, Some(workspaceId)) match {
      case Success(env) => createResourceD(org, env, Some(ResourceIds.Environment))
      case Failure(err) => Future { HandleRepositoryExceptions(err) }
    }
  }
  
  def postEnvironmentWorkspaceFqon(fqon: String, workspaceId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    orgFqon(fqon) match {
      case None => Future { OrgNotFound(fqon) }  
      case Some(org) => {
        normalizeEnvironment(request.body, Some(workspaceId)) match {
          case Success(env) => createResourceD(org.id, env, Some(ResourceIds.Environment))
          case Failure(err) => Future { HandleRepositoryExceptions(err) }
        }        
      }
    }
  }

  // --------------------------------------------------------------------------
  // DOMAINS
  // --------------------------------------------------------------------------
  
  def postDomain(org: UUID, parent: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    createResourceCommon(org, parent, ResourceIds.Domain)
  }
  
  def postDomainFqon(fqon: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => createResourceCommon(org.id, parent, ResourceIds.Domain)
      case None => Future { OrgNotFound(fqon) }
    }
  }  

  // --------------------------------------------------------------------------
  // GATEWAY_PROVIDERS
  // --------------------------------------------------------------------------
  
  def postProviderConfig(org: UUID, parent: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    createResourceCommon(org, parent, ResourceIds.ApiGatewayProvider)  
  }
  
  def postProviderConfigFqon(fqon: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => createResourceCommon(org.id, parent, ResourceIds.ApiGatewayProvider)
      case None => Future { OrgNotFound(fqon) }
    }
  }  
  

  // --------------------------------------------------------------------------
  //
  // UTILITY FUNCTIONS
  //
  // --------------------------------------------------------------------------

  private def CreateSynchronizedResult[T](org: UUID, typeId: UUID, json: JsValue)
      (sc: SecurityResourceFunction, mc: MetaResourceFunction)
      (implicit request: SecuredRequest[T]) = {

    Future {
      safeGetInputJson(typeId, json) match {
        case Failure(error) => BadRequestResult(error.getMessage)
        case Success(input) => HandleCreate(createSynchronized(org, typeId, input)(sc, mc))
      }
    }
  }

  
  private def createSynchronized[T](org: UUID, typeId: UUID, input: GestaltResourceInput)
    (sc: SecurityResourceFunction, mc: MetaResourceFunction)
    (implicit request: SecuredRequest[T]): Try[GestaltResourceInstance] = {

    /*
     * Extract and validate resource.properties before attempting create.
     */
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
  
  
  // TODO: Need to generalize name->uuid lookups
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
      b <- upsertProperty(a, "workspace", JsString(wk.get.toString))
      c <- normalizeEnvironmentType(b)
    } yield c    
  }
  
  def normalizeUserJson(json: JsObject, account: AuthAccountWithCreds) = {
    // Check if gestalt_home property is set - if not
    // create property and set to 'root' Org.
    
    json \ "properties" \ "gestalt_home" match {
      case u: JsUndefined => {
        val root = Security.getRootOrg(account)
        val props = replaceJsonPropValue(json, "gestalt_home", root.get.fqon)
        replaceJsonProps(json, props)
      }
      case _ => json
    }
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

  /**
   * Unwrap the given UUID or get the root org_id if None.
   */
  private def orgOrElseRoot[T](org: Option[UUID])(implicit request: SecuredRequest[T]) = org getOrElse {
    Security.getRootOrg(request.identity) match {
      case Success(org) => org.id
      case Failure(ex)  => throw ex
    }
  }

}