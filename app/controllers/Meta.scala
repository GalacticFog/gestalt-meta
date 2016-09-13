package controllers


import java.util.UUID
import java.net.URL
import play.api.http.HttpVerbs
import play.api.libs.ws.WS
import play.api.Play.current

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
import play.api.mvc.Action
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.security.api.json.JsonImports.linkFormat
import com.galacticfog.gestalt.laser.ApiResponse
import com.galacticfog.gestalt.meta.auth.Actions 
import com.galacticfog.gestalt.meta.auth.Authorization
 
/**
 * Code for POST and PATCH of all resource types.
 *
 */
object Meta extends Authorization {
  

  // --------------------------------------------------------------------------
  // ORGS
  // --------------------------------------------------------------------------  
  
  def postTopLevelOrg() = Authenticate().async(parse.json) { implicit request =>
    
    Security.getRootOrg(request.identity) match {
      case Failure(err)  => { 
        log.error(s"Failed to create top-level Org.")
        Future(HandleExceptions(err)) 
      }
      case Success(root) => createOrgCommon(root.id)
    }  
  }  

  def postOrgFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    createOrgCommon(fqid(fqon))
  }
  
  def createOrgCommon(parentOrg: UUID)(implicit request: SecuredRequest[JsValue]) = Future {
    
    val json = request.body
    val user = request.identity
    
    Authorize(parentOrg, Actions.Org.Create, request.identity) {
      
      CreateSynchronized(parentOrg, ResourceIds.Org, json)(Security.createOrg, createNewMetaOrg[JsValue]) match {
        case Failure(err) => HandleExceptions(err)
        case Success(res) => {
          
          //
          // TODO: Raise error if any Entitlement create fails.
          //
          setNewOrgEntitlements(res.id, res.id, user, Option(parentOrg))

          Created(Output.renderInstance(res, META_URL))
        }
      }

    }    
  }

  // --------------------------------------------------------------------------
  // ACTIONS
  // --------------------------------------------------------------------------    
  def postTypeActionFqon(fqon: String, typeId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
//    Future {
//      
//      val actionName = request.body \ "name" match {
//        case u: JsUndefined => ???
//        case r => r.as[String]
//      }
//      
//      val existingActions = ResourceFactory.findChildrenOfType(ResourceIds.Action, typeId)
//      if (existingActions exists { _.name == actionName }) 
//        ConflictResult(s"Action '$actionName' already exists for this resource type")
//      else {
//        // Create the Action Resource.
//        ???
//      }
//    }
    ???
  }
  
  
  // --------------------------------------------------------------------------
  // GROUPS
  // --------------------------------------------------------------------------   
  def postGroupFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    Future {
      createGroupCommon(fqid(fqon), request.body)  
    }
  }
  
  
  def createGroupCommon(org: UUID, json: JsValue)(implicit request: SecuredRequest[JsValue]) = {
    
    Authorize(org, Actions.Group.Create, request.identity) {
      
      CreateSynchronized(org, ResourceIds.Group, request.body)(
          Security.createGroup, createNewMetaGroup[JsValue]) match {
        case Failure(err) => HandleExceptions(err)
        case Success(res) => {
          /*
           * TODO: f the group was created with a users array, add the users!
           * val users = res.properties.get.get("users")
           */
          val user = request.identity
          Entitle(org, ResourceIds.Group, res.id, user, Option(org)) {
            generateEntitlements(
              user.account.id, org, res.id,
              Seq(ResourceIds.Group), ACTIONS_CRUD)
          }
          Created(Output.renderInstance(res, META_URL))
        }
      }
    }
  }
  
  
  /**
   * 
   * Add one or more users to a group.
   * 
   */
  def patchGroupUsers(fqon: String, group: UUID) = Authenticate(fqon) { implicit request =>
    val qs = request.queryString
    
    val uids = Try {
      if (!qs.contains("id"))
        throw new BadRequestException("Must provide 'id' in querystring.")
      else {
        // -------------------------------------------------------------------------        
        // TODO: Need to manually validate the UUIDs since java.util.UUID.fromString
        // *fixes* broken strings if it thinks it can.
        // -------------------------------------------------------------------------
        val ids = qs("id") map { UUID.fromString(_) }
        val users = ResourceFactory.findAllIn(ResourceIds.User, ids)
        val found = users map { _.id }
        
        // Ensure we got all the users we asked for.
        val delta = ids.diff(found)
        if (!delta.isEmpty) throw new BadRequestException("The following user(s) not found: " + delta.mkString(","))
        else found
      }
    }
    
    log.debug("Attempting to add following users to group: " + uids)
    
    uids match {
      case Failure(err) => HandleExceptions(err)
      case Success(ids) => {
        Security.addAccountsToGroup(group, ids) match {
          case Success(users) => Ok(Json.toJson(users))
          case Failure(errors) => HandleExceptions(errors)
        }
      }
    }
    
  }
  
  
  /* 
   * Remove a user from a group - note if a user that is not in the group (or user that doesn't exist)
   * is specified, no error is thrown.
   */
  def deleteGroupUsers(fqon: String, group: UUID) = Authenticate(fqon) { implicit request =>
    
    val qs = request.queryString
    
    val uids = Try {
      if (!qs.contains("id"))
        throw new BadRequestException("Must provide 'id' parameter in querystring.")
      else {
        // Make sure we actually got a value for 'id'
        val ids = {
          if (qs("id").isEmpty) throw new BadRequestException("Must provide at least one user-id")
          else qs("id") map { UUID.fromString(_) }
        }
        ResourceFactory.findAllIn(ResourceIds.User, ids) map { _.id }
      }
    }

    uids match {
      case Success(ids) => {
        log.debug(s"Attempting to REMOVE ${ids.size} user(s) from group ($group): " + ids)
        Security.removeAccountsFromGroup(group, ids) match {
          case Success(members) => Ok(Json.toJson(members))
          case Failure(errors)  => HandleExceptions(errors)
        }
      }
      case Failure(err) => HandleExceptions(err)
    }

  }
  
  // --------------------------------------------------------------------------
  // USERS
  // --------------------------------------------------------------------------  
  
  /**
   * Create a User Account in Security, then in Meta
   */

  def postUserFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    createUserCommon(fqid(fqon), request.body)
  }
  

  def createUserCommon(org: UUID, json: JsValue)(implicit request: SecuredRequest[JsValue]) = Future {
    log.debug(s"createUserCommon($org, ...) => JSON: ${json.toString}")
    
    Authorize(org, Actions.User.Create, request.identity) {
      
      val root = Security.getRootOrg(request.identity).get.fqon
      val home = JsonUtil.find(request.body.as[JsObject], "/properties/gestalt_home") getOrElse JsString(root)
      
      log.debug(s"Setting 'gestalt_home' to $home")
      
      val userJson = upsertProperty(request.body.as[JsObject], "gestalt_home", /*JsString(root)*/ home)
      
      CreateSynchronized(org, ResourceIds.User, userJson.get)(
          Security.createAccount, createNewMetaUser[JsValue]) match {
            case Failure(err) => {
              log.error("Response from Security.createAccount() : " + err.getMessage)
              HandleExceptions(err)
            }
            case Success(res) => {
          
            val user = request.identity
            
            Entitle(org, ResourceIds.User, res.id, user, Option(org)) {
              generateEntitlements(user.account.id, org, res.id, Seq(ResourceIds.User), ACTIONS_CRUD)
            }
            Created(Output.renderInstance(res, META_URL))
        }
      }

    }
  }        

  // --------------------------------------------------------------------------
  // GENERIC RESOURCE
  // --------------------------------------------------------------------------
  
  def postResourceFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    Future {
      CreateResourceResult(
          ResourceIds.User, 
          request.identity.account.id, 
          fqid(fqon), request.body, request.identity)
    }
  }

  // --------------------------------------------------------------------------
  // RESOURCE PATCH
  // --------------------------------------------------------------------------

  def patchResourceOrgFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    resourcePatch(fqid(fqon))
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
  

  def postWorkspaceFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    createWorkspaceResult(fqid(fqon), request.body, request.identity, META_URL)
  }

  private[controllers] def createWorkspaceResult2(org: UUID, json: JsValue, user: AuthAccountWithCreds, baseUri: Option[String]) = {

    Future {
      
      Authorize(org, Actions.Workspace.Create, user) {
        CreateNewResource(org, user, json, Option(ResourceIds.Workspace), Option(org)) match {
          case Failure(e) => HandleExceptions(e)
          case Success(w) => {
            setNewWorkspaceEntitlements(org, w.id, user)
            Created(Output.renderInstance(w, baseUri))
          }
        }
      }
      
    }
  }

  
  import com.galacticfog.gestalt.keymgr.GestaltLicense
  import com.galacticfog.gestalt.keymgr.GestaltFeature

  implicit def featureToString(feature: GestaltFeature) = feature.getLabel
  
  private[controllers] def createWorkspaceResult(org: UUID, json: JsValue, user: AuthAccountWithCreds, baseUri: Option[String]) = {

    Future {

      val operations = standardMethods(ResourceIds.Workspace, "workspace.create")
      
      val options = RequestOptions(user, 
          authTarget = Option(org), 
          policyOwner = Option(org), 
          policyTarget = Option(j2r(org, user, json, Option(ResourceIds.Workspace))))
    
      SafeRequest (operations, options) Protect { maybeState =>      
        
        CreateNewResource(org, user, json, Option(ResourceIds.Workspace), Option(org)) match {
          case Failure(e) => HandleExceptions(e)
          case Success(w) => {
            setNewWorkspaceEntitlements(org, w.id, user)
            Created(Output.renderInstance(w, baseUri))
          }
        }
   
      }
      
    }
  }  

  /**
   * Convert input JSON to an in-memory GestaltResourceInstance
   */
  def j2r(org: UUID, creator: AuthAccountWithCreds, json: JsValue, typeId: Option[UUID] = None) = {
    inputWithDefaults(
          org = org, 
          typeId = typeId,
          input = safeGetInputJson(json).get, 
          creator = creator)
  }

// --------------------------------------------------------------------------
// ENVIRONMENTS
// --------------------------------------------------------------------------

  /*
   * POST /{fqon}/environments
   */
  def postEnvironmentFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    parseWorkspaceId(request.body) match {
      case Failure(err) => Future { HandleExceptions(err) }
      case Success(workspace) => createEnvironmentResult(fqid(fqon), workspace)
    }          
  }
  
  /*
   * POST /{fqon}/workspaces/{id}/environments
   */
  def postEnvironmentWorkspaceFqon(fqon: String, workspaceId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    createEnvironmentResult(fqid(fqon), workspaceId)
  }
  
  private[controllers] def createEnvironmentResult(org: UUID, workspace: UUID)(implicit request: SecuredRequest[JsValue]) = {
    log.debug(s"createEnvironmentResult($org, $workspace)")
    
    Future {
      val user = request.identity
      Authorize(workspace, Actions.Environment.Create, user) {
        (for {
          json <- normalizeEnvironment(request.body, Option(workspace))
          env  <- CreateNewResource(org, user, json, Option(ResourceIds.Environment), Option(workspace))
        } yield env) match {
          case Failure(e) => HandleExceptions(e)
          case Success(env) => {
            setNewEnvironmentEntitlements(org, env.id, user, workspace)
            Created(Output.renderInstance(env, META_URL))
          }
        }             
      }
    }
  }
  
  /**
   * Parse properties.workspace from Environment input JSON.
   */
  private[controllers] def parseWorkspaceId(json: JsValue): Try[UUID] = Try {
    JsonUtil.find(json.as[JsObject], "/properties/workspace").fold {
      throw new BadRequestException(s"You must provide a valid 'workspace' property.")
    } { workspace =>
      UUID.fromString(workspace.as[String])
    }
  }

  // --------------------------------------------------------------------------
  // DOMAINS
  // --------------------------------------------------------------------------
  
  def postDomain(org: UUID, parent: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    createResourceCommon(org, parent, ResourceIds.Domain, request.body)
  }
  
  
  def postDomainFqon(fqon: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    Future {      
      val org = fqid(fqon)
      val user = request.identity
      
      Authorize(parent, Actions.Domain.Create) {
        CreateNewResource(org, user, request.body, Option(ResourceIds.Domain), Option(parent)) match {
          case Failure(e) => HandleExceptions(e)
          case Success(d) => {
            setNewDomainEntitlements(org, d.id, user, parent)        
            Created(Output.renderInstance(d, META_URL))
          }
        }
      }
    }
  }
  
  
  // --------------------------------------------------------------------------
  // GATEWAY_PROVIDERS
  // --------------------------------------------------------------------------
  
  def resolveProviderType(json: JsValue): UUID = {
    json \ "resource_type" match {
      case u: JsUndefined => 
        throw new BadRequestException("You must supply 'resource_type', i.e., resource_type = Provider::Type::Name")
      case v => {
        
        val validProviderTypes: Map[String,UUID] = 
          ResourceFactory.findTypesWithVariance(CoVariant(ResourceIds.Provider)).map { p =>
            (p.name -> p.id) 
          }.toMap

        log.debug("Parsed provider-type as : " + v.as[String])
        validProviderTypes.get(v.as[String]).fold {
          throw new BadRequestException(s"Unknown provider type : '${v.as[String]}'")
        }{ id => id }
        
      }
    }    
  }
  
  def postProviderConfigOrgFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    val orgId = fqid(fqon)
    postProviderCommon(orgId, ResourceIds.Org.toString, orgId, request.body)
  }
  
  def postProviderConfigFqon(fqon: String, parentType: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    postProviderCommon(fqid(fqon), parentType, parent, request.body)
  }

  def postProviderCommon(org: UUID, parentType: String, parent: UUID, json: JsValue)(implicit request: SecuredRequest[JsValue]) = {

    val providerType = resolveProviderType(json)
    val parentTypeId = UUID.fromString(parentType)

    ResourceFactory.findById(parentTypeId, parent).fold {
      Future(ResourceNotFound(parentTypeId, parent))
    } { parentResource =>
        
      if (providerType == ResourceIds.ApiGatewayProvider) {
        log.debug("Creating ApiGatewayProvider...")
        postGatewayProvider(org, parentResource)
      } 
      else {

        ResourceFactory.findById(UUID.fromString(parentType), parent).fold {
          Future(ResourceNotFound(parentType, parent))
        }{ p =>
          // We found the parent, inject resource_type and parent into the incoming JSON.
          JsonUtil.upsertProperty(json.as[JsObject], "parent", Json.toJson(toLink(p, META_URL))) match {
            case Failure(e) => Future(HandleRepositoryExceptions(e))
            case Success(j) => {
              val newjson = j ++ Json.obj("resource_type" -> providerType.toString)
              createResourceCommon(org, parent, providerType, newjson)
            }
          }
        }
      }

    }    
  }
  
  def laserClient(lambdaUrl: String, gatewayUrl: String) = {
    val gateway = HostConfig.make(new URL(gatewayUrl))
    val lambda  = HostConfig.make(new URL(lambdaUrl))
    new Laser(gateway, lambda, Option(EnvConfig.securityKey), Option(EnvConfig.securitySecret))
  }  
  
  
  lazy val gatewayConfig = HostConfig.make(new URL(EnvConfig.gatewayUrl))
  lazy val lambdaConfig = HostConfig.make(new URL(EnvConfig.lambdaUrl))
  lazy val laser = new Laser(
      gatewayConfig, lambdaConfig, 
      Option(EnvConfig.securityKey), 
      Option(EnvConfig.securitySecret))  
  
  def postGatewayProvider(org: UUID, parent: GestaltResourceInstance)(implicit request: SecuredRequest[JsValue]) = {
    import ApiGateway._
    
    // TODO: Check resource_type if not gateway-type.
    val input = JsonUtil.safeParse[GatewayInput](request.body)
    
    val url = input.properties.config.url
    //val laser = client(url)
    
    log.debug("Creating provider resource in gestalt-apigateway...")
    val laserProvider = LaserProvider(None, name = input.name)
    val providerId = parseLaserResponseId(laser.createProvider(laserProvider))
    
    log.debug("Creating Location resource in gestalt-apigateway...")
    val location = getGatewayLocation(input)
    val laserLocation = LaserLocation(None, name = location.name, providerId)
    val locationId = parseLaserResponseId(laser.createLocation(laserLocation))
    
    log.debug("Creating Gateway resource in gestalt-apigateway...")
    val gatewayInfo = buildGatewayInfo(input)
    val laserGateway = LaserGateway(None, input.name, locationId, gatewayInfo)
    val gatewayId = parseLaserResponseId(laser.createGateway(laserGateway))

    log.debug("Creating ApiGatewayProvider in Meta...")
    setMetaGatewayProps(request.body, UUID.randomUUID, providerId, Json.toJson(toLink(parent, META_URL))) match {
      case Failure(err) => Future { HandleExceptions(err) }
      case Success(jsn) => {
        createResourceCommon(org, parent.id, ResourceIds.ApiGatewayProvider, jsn)
      }
    }
    
  }  
  
  // --------------------------------------------------------------------------
  //
  // UTILITY FUNCTIONS
  //
  // --------------------------------------------------------------------------

  private def CreateSynchronized[T](org: UUID, typeId: UUID, json: JsValue)
    (sc: SecurityResourceFunction, mc: MetaResourceFunction)
    (implicit request: SecuredRequest[T]): Try[GestaltResourceInstance] = {
    
    log.debug(s"CreateSynchronized(org = $org, typeId = $typeId)")

    for {
      input    <- safeGetInputJson(typeId, json)
      resource <- syncWithSecurity(org, typeId, input)(sc, mc)
    } yield resource
  }
  
  private def syncWithSecurity[T](org: UUID, typeId: UUID, input: GestaltResourceInput)
    (sc: SecurityResourceFunction, mc: MetaResourceFunction)
    (implicit request: SecuredRequest[T]): Try[GestaltResourceInstance] = {

    val stringprops = stringmap(input.properties)
    val creator = request.identity.account.id
    
    for {
      sr <- sc(org, request.identity, input)
      mr <- mc(creator, org,  sr, stringprops, input.description)
    } yield mr
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
