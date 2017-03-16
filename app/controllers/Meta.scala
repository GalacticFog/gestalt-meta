package controllers


import java.net.URL
import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.data.{CoVariant, EnvironmentType, ResourceFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.keymgr.{GestaltFeature, GestaltLicense}
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.output.toLink
import com.galacticfog.gestalt.meta.api.sdk.GestaltResourceInput
import com.galacticfog.gestalt.meta.api.sdk.HostConfig
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.resourceLinkFormat
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.api.json.JsonImports.linkFormat
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import controllers.util.JsonUtil.replaceJsonPropValue
import controllers.util.JsonUtil.replaceJsonProps
import controllers.util.JsonUtil.str2js
import controllers.util.JsonUtil.upsertProperty
import controllers.util.db.EnvConfig
import play.api.i18n.MessagesApi
import play.api.libs.json._


import scala.language.implicitConversions
import com.galacticfog.gestalt.json.Js

/**
 * Code for POST and PATCH of all resource types.
 */
import javax.inject.Singleton

@Singleton
class Meta @Inject()(messagesApi: MessagesApi,
                     env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
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
    
    Authorize(parentOrg, "org.create", request.identity) {
      
      CreateSynchronized(parentOrg, ResourceIds.Org, json)(Security.createOrg, createNewMetaOrg[JsValue]) match {
        case Failure(err) => HandleExceptions(err)
        case Success(res) => {

          setNewEntitlements(res.id, res.id, user, parent = Option(parentOrg))
          
          Created(Output.renderInstance(res, META_URL))
        }
      }

    }    
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
    
    Authorize(org, "group.create", request.identity) {
      
      CreateSynchronized(org, ResourceIds.Group, request.body)(
          Security.createGroup, createNewMetaGroup[JsValue]) match {
        
        case Failure(err) => HandleExceptions(err)
        case Success(res) => {
          val addusers: Seq[UUID] = Js.find(request.body.as[JsObject], "/properties/users").fold {
            Seq().asInstanceOf[Seq[UUID]]
            }{
            users => JsonUtil.safeParse[Seq[UUID]](users)
          }
          
          val newjson = { 
            if (addusers.isEmpty) Output.renderInstance(res, META_URL)
            else {
              log.debug(s"Adding ${addusers.size} users to new group.")
              Security.addAccountsToGroup(res.id, addusers) match {
                case Success(users) => {
                  val jsonusers = Json.toJson(users)
                  JsonUtil.upsertProperty(RenderSingle(res).as[JsObject], "users", jsonusers).get
                }
                case Failure(error) => {
                  log.error("Failed adding users to new group: " + error.getMessage)
                  throw error
                }
              }
            }
          }
          setNewEntitlements(org, res.id, request.identity, parent = Option(org))
          Created( newjson )
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
    
    Authorize(org, "user.create", request.identity) {
      
      val root = Security.getRootOrg(request.identity).get.fqon
      val home = Js.find(request.body.as[JsObject], "/properties/gestalt_home") getOrElse JsString(root)
      
      log.debug(s"Setting 'gestalt_home' to $home")
      
      val userJson = upsertProperty(request.body.as[JsObject], "gestalt_home", /*JsString(root)*/ home)
      
      CreateSynchronized(org, ResourceIds.User, userJson.get)(
        Security.createAccount, createNewMetaUser[JsValue]) match {
          case Failure(err) => {
            log.error("Response from Security.createAccount() : " + err.getMessage)
            HandleExceptions(err)
          }
          case Success(res) => {
            setNewEntitlements(org, res.id, request.identity, parent = Option(org))        
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
  
  import com.galacticfog.gestalt.patch.PatchDocument
  
//  def patchResourceOrgFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
//    resourcePatch(ResourceIds.Org, fqid(fqon))
//  }

//  def patchResourceFqon(fqon: String, id: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
//    ResourceFactory.findById(id).fold(Future(NotFoundResult(request.uri))) { r =>
//      if (r.typeId == ResourceIds.Lambda) {
//        LambdaMethods.patchGestaltLambda(r, request.body).get
//      }
//      resourcePatch(r.typeId, id)
//    }
//  }

//  def resourcePatch(typeId: UUID, id: UUID)(implicit request: SecuredRequest[JsValue]) = {
//    Future {
//      //safeGetPatchDocument(request.body) match {
//      Try(PatchDocument.fromJsValue(request.body)) match {
//        case Failure(e) => BadRequestResult(e.getMessage)
//        case Success(patch) => {
//          
//          val identity = request.identity.account.id
//          
//          patch.applyPatch(json)
//          
//          PatchHandler(UUID.randomUUID(), id, patch).applyPatch(ResourceIds.User, identity) match {
//            case Success(r) => Ok(Output.renderInstance(r))
//            case Failure(e) => HandleExceptions(e) 
//          }
//        }
//      }      
//    }
//  }
  
  
  // --------------------------------------------------------------------------
  // WORKSPACES
  // --------------------------------------------------------------------------
  
  def postWorkspaceFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    createWorkspaceResult(fqid(fqon), request.body, request.identity, META_URL)
  }

  implicit def featureToString(feature: GestaltFeature) = feature.getLabel
  
  private[controllers] def createWorkspaceResult(org: UUID, json: JsValue, user: AuthAccountWithCreds, baseUri: Option[String]) = {

    Future {

      val operations = standardMethods(ResourceIds.Workspace, "workspace.create")
      
      val options = RequestOptions(user, 
          authTarget = Option(org), 
          policyOwner = Option(org), 
          policyTarget = Option(j2r(org, user, json, Option(ResourceIds.Workspace))),
          data = Option(Map("host" -> baseUri.get)))
      
      SafeRequest (operations, options) Protect { maybeState =>      
        
        CreateNewResource(org, user, json, Option(ResourceIds.Workspace), Option(org)) match {
          case Failure(e) => HandleExceptions(e)
          case Success(w) => {
            setNewEntitlements(org, w.id, user, parent = Option(org))
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
    
    withInputDefaults(
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
      Authorize(workspace, "environment.create", user) {
        (for {
          json <- normalizeEnvironment(request.body, Option(workspace))
          env  <- CreateNewResource(org, user, json, Option(ResourceIds.Environment), Option(workspace))
        } yield env) match {
          case Failure(e) => HandleExceptions(e)
          case Success(env) => {
            setNewEntitlements(org, env.id, user, parent = Option(workspace))
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
    Js.find(json.as[JsObject], "/properties/workspace").fold {
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
      
      Authorize(parent, "domain.create") {
        CreateNewResource(org, user, request.body, Option(ResourceIds.Domain), Option(parent)) match {
          case Failure(e) => HandleExceptions(e)
          case Success(d) => {   
            setNewEntitlements(org, d.id, user, parent = Option(parent))
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


  import com.galacticfog.gestalt.meta.providers._

  def load(rootProvider: ProviderMap, identity: UUID) = {
    ProviderManager.loadProviders(Some(rootProvider)) map { seq =>
      
      seq.flatMap { case (provider, services) => 
        
        log.debug(s"Provider provisioning complete. Processing: ${provider.name}[${provider.id}]")
        log.debug("Checking for dynamic updates to provider environment variables.")
        
        if (services.isEmpty) {
          log.debug("No variable updates necessary.")
          Option(Try(provider.root))
        }
        else {
          provider.env map { vs =>
            log.debug(s"Updating configuration for provider: ${provider.name}(${provider.id})")
            saveProvider(provider, vs, identity)
          }
        }
      }
    }    
  }
  
  /**
   * Update a provider resource in Meta DB, replacing its properties.config.env object
   * with the given ProviderEnv. 
   */
  private[controllers] def saveProvider(provider: ProviderMap, env: ProviderEnv, identity: UUID): Try[GestaltResourceInstance] = {
    val tryResource = ProviderMap.replaceEnvironmentConfig(provider.root, env)
    tryResource match {
      case Failure(e) => {
        log.error(s"Failed updating environment configuration for resource: ${provider.name}(${provider.id})")
        tryResource
      }
      case Success(resource) => {
        log.debug(s"Successfully updated env configuration for: ${resource.name}(${resource.id})")
        ResourceFactory.update(resource, identity)
        log.debug(s"Updated: ${resource.name}(${resource.id})")
        tryResource
      }
    }
  }
  
  /**
   * Filters the target provider from the result-set obtained when creating a new provider.
   */
  private[controllers] def getCurrentProvider(targetId: UUID, results: Future[Seq[Try[GestaltResourceInstance]]]) = {
    for {
      seq <- results
      out = {
        log.debug(s"Finding provider in result-set. Seeking: ${targetId}")
        seq filter { t => t.isSuccess && t.get.id == targetId } map { _.get }
      }
    } yield {
      if (out.size > 1) {
        log.error(s"Could not find Provider ${targetId} in results-list.")
        throw new RuntimeException("An error occurred launching the Provider")
      } else out.head
    }    
  }
  
  /**
   * Currently just injects the given UUID and resource_type UUID
   * into the POST JSON paylod for a new Provider.
   */
  private[controllers] def normalizeProviderPayload(
      payload: JsValue, 
      newId: UUID, 
      providerType: UUID,
      parent: GestaltResourceInstance, 
      baseUrl: Option[String]): Try[JsObject] = {

    lazy val addKubeDefaultNetworks = (__ \ 'properties \ 'config).json.update(
      __.read[JsObject].map{ _ ++ Json.obj(
        "networks" -> Json.arr(Json.obj("name" -> "default"))
      ) }
    )

    for {
      withSpecifics <- providerType match {
        case ResourceIds.KubeProvider => Try {
          payload.transform(addKubeDefaultNetworks).get
        }
        case _ => Success(payload.as[JsObject])
      }
      withParentProp <- JsonUtil.upsertProperties(withSpecifics,
        "parent" -> Json.toJson(toLink(parent, baseUrl))
      )
      withIdAndType = withParentProp ++ Json.obj(
        "id" -> newId.toString,
        "resource_type" -> providerType.toString
      )
    } yield withIdAndType
  }
  
  private[controllers] def providerRequestOptions(org: UUID, user: AuthAccountWithCreds, payload: JsValue, baseUrl: String) = {
    RequestOptions(user, 
      authTarget = Option(org), 
      policyOwner = Option(org), 
      policyTarget = Option(j2r(org, user, payload, Option(ResourceIds.Provider))),
      data = Option(Map("host" -> baseUrl)))    
  }
  
  
  def postProviderCommon(org: UUID, parentType: String, parentId: UUID, json: JsValue)(
      implicit request: SecuredRequest[JsValue]) = {
    
    val providerType = resolveProviderType(json)
    val parentTypeId = UUID.fromString(parentType)
    
    ResourceFactory.findById(parentTypeId, parentId).fold {
      Future(ResourceNotFound(parentTypeId, parentId))
    }{ parent =>
      
      // This is the ID we create the new provider with.
      val targetid = Js.find(json.as[JsObject], "/id").fold(UUID.randomUUID) {
        uid => UUID.fromString(uid.as[String])
      }
      
      val user = request.identity
      val payload = normalizeProviderPayload(json, targetid, providerType, parent, META_URL).get
      val operations = standardMethods(ResourceIds.Provider, "provider.create")
      val options = providerRequestOptions(org, user, payload, META_URL.get)
      
      SafeRequest (operations, options) ProtectAsync { maybeState =>           
        
        // Ensure linked providers parse
        Js.find(payload, "/properties/linked_providers") map { lp =>
          LinkedProvider.fromJson(lp)
        }
        
        val identity = user.account.id
        val created = for {
          input <- Js.parse[GestaltResourceInput](payload)
          res   <- CreateNewResource(org, user, input, Some(providerType), Some(parentId))
        } yield res
        
        created match {
          case Failure(e) => {
            /*
             * TODO: Update container as 'FAILED' in Meta
             */
            HandleExceptionsAsync(e)
          }
          case Success(newMetaProvider) => {
            
            val results = load(ProviderMap(newMetaProvider), identity)
            getCurrentProvider(targetid, results) map { t => 
              Created(RenderSingle(t)) 
            } recover { 
              case e => {
                /*
                 * TODO: Update container as 'FAILED' in Meta
                 */                
                HandleExceptions(e)
              }
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
  
//  def postGatewayProvider(org: UUID, parent: GestaltResourceInstance)(implicit request: SecuredRequest[JsValue]) = {
//    import ApiGateway._
//    
//    // TODO: Check resource_type if not gateway-type.
//    val input = JsonUtil.safeParse[GatewayInput](request.body)
//    
//    val url = input.properties.config.url
//    //val laser = client(url)
//    
//    log.debug("Creating provider resource in gestalt-apigateway...")
//    val laserProvider = LaserProvider(None, name = input.name)
//    val providerId = parseLaserResponseId(laser.createProvider(laserProvider))
//    
//    log.debug("Creating Location resource in gestalt-apigateway...")
//    val location = getGatewayLocation(input)
//    val laserLocation = LaserLocation(None, name = location.name, providerId.toString)
//    val locationId = parseLaserResponseId(laser.createLocation(laserLocation))
//    
//    log.debug("Creating Gateway resource in gestalt-apigateway...")
//    val gatewayInfo = buildGatewayInfo(input)
//    val laserGateway = LaserGateway(None, input.name, locationId.toString, gatewayInfo)
//    val gatewayId = parseLaserResponseId(laser.createGateway(laserGateway))
//
//    log.debug("Creating ApiGatewayProvider in Meta...")
//    setMetaGatewayProps(request.body, UUID.randomUUID, providerId, Json.toJson(toLink(parent, META_URL))) match {
//      case Failure(err) => Future { HandleExceptions(err) }
//      case Success(jsn) => {
//        createResourceCommon(org, parent.id, ResourceIds.ApiGatewayProvider, jsn)
//      }
//    }
//    
//  }  
  
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
      input    <- safeGetInputJson(json, Option(typeId))
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
