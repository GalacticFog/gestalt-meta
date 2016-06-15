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
import  com.galacticfog.gestalt.security.api.json.JsonImports.linkFormat

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
    createOrgCommon(fqid(fqon), request.body)
  }

  def createOrgCommon(org: UUID, json: JsValue)(implicit request: SecuredRequest[JsValue]) = {
    CreateSynchronizedResult(org, ResourceIds.Org, json)(
      Security.createOrg, createNewMetaOrg[JsValue])    
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
    createGroupCommon(fqid(fqon), request.body)  
  }
  
  def createGroupCommon(org: UUID, json: JsValue)(implicit request: SecuredRequest[JsValue]) = {
    CreateSynchronizedResult(org, ResourceIds.Group, json)(
      Security.createGroup, createNewMetaGroup[JsValue])
  }
  
  /**
   * 
   * Add one or more users to a group.
   * 
   */
  def patchGroupUsers(fqon: String, group: UUID) = Authenticate(fqon) { implicit request =>
    println("QUERYSTRING : " + request.queryString)
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
        val users = ResourceFactory.findAllIn(fqid(fqon), ResourceIds.User, ids)

        // TODO: Ensure we got all the users we searched for.
        users map { _.id }
      }
    }
    
    println("Attempting to add following users to group: " + uids)
    uids match {
      case Success(ids) => {
        Security.addAccountsToGroup(group, ids) match {
          case Success(users) => Ok(Json.toJson(users))
          case Failure(errors) => HandleExceptions(errors)
        }
      }
      case Failure(err) => HandleExceptions(err)
    }
    
  }
  
  
  // Remove a user from a group
  def deleteGroupUsers(fqon: String, group: UUID) = Authenticate(fqon) { implicit request =>
    
    val qs = request.queryString
    
    val uids = Try {
      if (!qs.contains("id"))
        throw new BadRequestException("Must provide 'id' parameter in querystring.")
      else {
        // Make sure we actually got a value for 'id'
        val ids = {
          if (qs("id").isEmpty) throw new BadRequestException("Must provide at least one user-id")
          else qs("id")(0).trim.split(",") map { UUID.fromString(_) }
        }
        // TODO: Ensure we find all the users contained in 'ids'
        ResourceFactory.findAllIn(fqid(fqon), ResourceIds.User, ids) map { _.id }
      }
    }

    uids match {
      case Success(ids) => {
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
   * API implements => POST /orgs/:uuid/users
   */
  def postUser(org: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    createUserCommon(org, request.body)
  }

  def postUserFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    createUserCommon(fqid(fqon), request.body)
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
          fqid(fqon), request.body, request.identity)
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
    createWorkspaceCommon(fqid(fqon), request.body, request.identity, META_URL)
  }

  def createWorkspaceCommon(org: UUID, json: JsValue, user: AuthAccountWithCreds, baseUri: Option[String]) = {
    Future {
      /* Create the workspace */
      val workspace = CreateResource(
          ResourceIds.User, user.account.id, org, json, user, Some(ResourceIds.Workspace),
          parentId = Some(org)) 
      
      /* Add ALL gateway providers to new workspace. */
      workspace match {
        case Failure(e) => HandleExceptions(e)
        case Success(workspace) => {
          
          

          
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
    val workspace = request.body \ "properties" \ "workspace" match {
      case u: JsUndefined => throw new BadRequestException(s"You must provide a valid 'workspace' property.")
      case j => UUID.fromString(j.as[String])
    }
    (for {
      a <- normalizeResourceType(request.body, ResourceIds.Environment)
      b <- normalizeEnvironmentType(a)
    } yield b) match {
      case Success(env) => createResourceD(org, env, Some(ResourceIds.Environment), parentId = Some(workspace))
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
          case Success(env) => createResourceD(org.id, env, Some(ResourceIds.Environment), parentId = Some(workspaceId))
          case Failure(err) => Future { HandleRepositoryExceptions(err) }
        }        
      }
    }
  }

  // --------------------------------------------------------------------------
  // DOMAINS
  // --------------------------------------------------------------------------
  
  def postDomain(org: UUID, parent: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    createResourceCommon(org, parent, ResourceIds.Domain, request.body)
  }
  
  def postDomainFqon(fqon: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => createResourceCommon(org.id, parent, ResourceIds.Domain, request.body)
      case None => Future { OrgNotFound(fqon) }
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
        
        val types: Map[String,UUID] = 
          ResourceFactory.findTypesWithVariance(CoVariant(ResourceIds.Provider)).map { p =>
            (p.name -> p.id) 
          }.toMap
          
        log.debug("Parsed provider-type as : " + v.as[String])
        v.as[String] match {
          case a if types.contains(a) => types(a)
          case e => throw new BadRequestException(s"Unknown provider type : '$e'")
        }
      }
    }    
  }


  // TODO: [TEMPORARY]: Create ApiGatewayProvider under workspace
  //          val gateway = LaserController.getLaserProviders(org) foreach { p =>
  //            
  //            // Get LaserProvider ID
  //            val laserProviderId = p \ "id" match {
  //              case u: JsUndefined => throw new RuntimeException(s"Unable to parse provider JSON from gestalt-apigateway. recieved: ${p.toString}")
  //              case v => v
  //            }
  //            
  //            log.debug("LASER-PROVIDER-JSON: " + Json.prettyPrint(p))
  //            log.debug("LASER-PROVIDER-ID: " + laserProviderId)
  //            
  //            // Set ID and external_id on JSON
  //            val json = withJsonPropValue(
  //                  obj       = p ++ Json.obj("id" -> UUID.randomUUID.toString),
  //                  propName  = "external_id",
  //                  propValue = laserProviderId)
  //            
  //            val parentJson = Json.toJson(toLink(workspace, None))
  //            log.debug("PARENT-LINK:\n" + Json.prettyPrint(parentJson))
  //            
  //            val json2 = JsonUtil.upsertProperty(json.as[JsObject], "parent", parentJson).get
  //            
  //            log.debug("FINAL:\n" + Json.prettyPrint(json2))
  //            
  //            log.debug("Attaching GatewayProvider to workspace:\n" + Json.prettyPrint(json))
  //            CreateResource(ResourceIds.User, user.account.id, org, 
  //                json2, 
  //                user,
  //              Some(ResourceIds.ApiGatewayProvider), Some(workspace.id)) match {
  //              case Failure(e) => throw new RuntimeException("Unable to create GatewayProvider: " + e.getMessage)
  //              case Success(r) => log.debug(s"Successfully create GatewayProvider: ${r.id.toString}");
  //            }
  //          }
  
  //          // TODO: [TEMPORARY]: Create MarathonProvider under workspace
  //          val marathon = newMarathonProvider("Marathon::" + UUID.randomUUID.toString)
  //          log.debug("Attaching MarathonProvider to workspace:\n" + Json.prettyPrint(marathon))
  //          
  //          CreateResource(ResourceIds.User, user.account.id, org, marathon, user,
  //              Some(ResourceIds.MarathonProvider), Some(workspace.id)) match {
  //            case Success(instance) => log.debug("Successfully created MarathonProvider: " + instance.id)
  //            case Failure(error)    => throw new RuntimeException("Unable to create MarathonProvider: " + error.getMessage)
  //          }

  case class GatewayInputLocation(name: String, enabled: Boolean = true)
  case class GatewayInputAuth(scheme: String, username: String, password: String)
  case class GatewayInputConfig(auth: GatewayInputAuth, url: String, extra: Option[String] = None)
  case class GatewayInputProperties(config: GatewayInputConfig, locations: Seq[GatewayInputLocation])
  case class GatewayInput(name: String, description: Option[String], resource_type: String, properties: GatewayInputProperties)

  implicit lazy val gatewayInputLocationFormat = Json.format[GatewayInputLocation]
  implicit lazy val gatewayInputAuth = Json.format[GatewayInputAuth]
  implicit lazy val gatewayInputConfig = Json.format[GatewayInputConfig]
  implicit lazy val gatewayInputProperties = Json.format[GatewayInputProperties]
  implicit lazy val gatewayInput = Json.format[GatewayInput]

  
  import java.net.URL
  import java.net.MalformedURLException

  lazy val gatewayConfig = HostConfig.make(new URL(EnvConfig.gatewayUrl))
  lazy val lambdaConfig = HostConfig.make(new URL(EnvConfig.lambdaUrl))
  lazy val laser = new Laser(gatewayConfig, lambdaConfig)
  
  
  /*
   * 
   * TODO: This function needs transaction enforcement. The workflow entails creating three resources on the
   * gateway service: 1) Provider, 2) Location, 3) Gateway.  If any of them fail, nothing is to be created
   * in Meta, and any gateway resources that *were* successfully created should deleted (or otherwise scrubbed).
   * 
   */
  def postGatewayProvider(parentType: UUID, parentId: UUID)(implicit request: SecuredRequest[JsValue]) = {
    
    val input = request.body.validate[GatewayInput].map {
      case success: GatewayInput => success
    }.recoverTotal { e =>
      throw new BadRequestException("Failed to parse provider JSON : " + JsError.toFlatJson(e).toString)  
    }

    // Create Provider in gateway service.
    val laserProvider = LaserProvider(None, name = input.name)
    val providerId = parseLaserResponseId(laser.createProvider(laserProvider))
    
    
    // Create Location in gateway service.
    val location = getGatewayLocation(input)
    val laserLocation = LaserLocation(None, name = location.name, providerId)
    val locationId = parseLaserResponseId(laser.createLocation(laserLocation))
    
    
    // Create Gateway in gateway service.     
    val gatewayInfo = buildGatewayInfo(input)
    val laserGateway = LaserGateway(None, input.name, locationId, gatewayInfo)
    val gatewayId = parseLaserResponseId(laser.createGateway(laserGateway))
    
    
    /*
     * 
     * TODO: Last step is to now create the Meta ApiGatewayProvider
     * 1.) Inject 'external_id' property
     * 2.) Inject 'parent' link
     * 3.) Create the resource in Meta:
     * 
     */

    
  //            CreateResource(ResourceIds.User, user.account.id, org, 
  //                json2, 
  //                user,
  //              Some(ResourceIds.ApiGatewayProvider), Some(workspace.id)) match {
  //              case Failure(e) => throw new RuntimeException("Unable to create GatewayProvider: " + e.getMessage)
  //              case Success(r) => log.debug(s"Successfully create GatewayProvider: ${r.id.toString}");
  //            }    
    
    
    
  }
  
  import com.galacticfog.gestalt.laser.ApiResponse
  
  private def parseJsonId(json: JsValue) = {
    (json \ "id") match {
      case u: JsUndefined => throw new RuntimeException(
        "Could not find 'id' in JSON returned from gateway server.")
      case v => UUID.fromString(v.as[String]) // TODO: Test for FormatException
    }
  }  
  
  private def parseLaserResponseId(response: => Try[ApiResponse]) = {
    response match {
      case Failure(err) => throw err
      case Success(res) => {
        log.debug("Successfully created Location in gateway service. Response:\n" + res)
        parseJsonId(res.output.get)
      }
    }
  }
  
  
  def getGatewayLocation(input: GatewayInput) = {
    val numLocations = input.properties.locations.size
    numLocations match {
      case z if z <= 0 => throw new BadRequestException("You must provide a location.")
      case n if n > 1  => throw new ConflictException("Multiple locations found. The current implementation only supports a SINGLE location.")
      case _ => input.properties.locations(0)
    }     
  }
  
  def buildGatewayInfo(input: GatewayInput) = {

    def normalizeUrl(url: String) = {
       if (url.trim.toLowerCase.startsWith("http")) url else s"http://$url"
    }
     
    def getKongPublicInfo(serviceAddress: String, publicAddress: Option[String]) = {
      val out = publicAddress match {
        case Some(address) => new URL(normalizeUrl(address))
        case None => {
          val u = new URL(normalizeUrl(serviceAddress))
          new URL("%s://%s:%d".format(u.getProtocol, "admin." + u.getHost, u.getPort))
        }
      }
      (out.getHost, if (out.getPort == -1) 80 else out.getPort)
    }
    
    val kongServiceUrl = new URL(normalizeUrl(input.properties.config.url))
    val kongServiceAddress = kongServiceUrl.getHost
    val kongServicePort = if (kongServiceUrl.getPort == -1) 80 else kongServiceUrl.getPort
    
    val username = input.properties.config.auth.username
    val password = input.properties.config.auth.password
    
    val (kongPublicHost,kongPublicPort) = getKongPublicInfo(kongServiceAddress, input.properties.config.extra)
    
    Json.obj(
      "host" -> kongServiceAddress,
      "port" -> kongServicePort,
      "username" -> username,
      "password" -> password,
      "gatewayHost" -> kongPublicHost,
      "gatewayPort" -> kongPublicPort)

  }    
  
  def postProviderCommon(org: UUID, parentType: String, parent: UUID, json: JsValue)(implicit request: SecuredRequest[JsValue]) = {
    val providerType = resolveProviderType(json)
    
    log.debug("Translated provider-type to UUID => " + providerType)
    
    // If providerType == ApiGateway - do special stuff.
    {
      // Get list of all providers specified in JSON.
    }
    val parentTypeId = UUID.fromString(parentType)
    if (providerType == ResourceIds.ApiGatewayProvider) {
      postGatewayProvider(parentTypeId, parent)
    }
    
    Future( Ok("TESTING CREATE PROVIDER : " + providerType) )
    
//    // Lookup the parent of the current Provider.
//    val providerParent = ResourceFactory.findById(UUID.fromString(parentType), parent)
//    
//    providerParent match {
//      case None => Future(NotFoundResult(s"${ResourceLabel(parentType)} with ID '${parent}' not found."))
//      case Some(p) => {
//            
//        // We found the parent, inject resource_type and parent into the incoming JSON.
//        JsonUtil.upsertProperty(json.as[JsObject], "parent", Json.toJson(toLink(p, META_URL))) match {
//          case Failure(e) => Future(HandleRepositoryExceptions(e))
//          case Success(j) => {
//            val newjson = j ++ Json.obj("resource_type" -> providerType.toString)
//            createResourceCommon(org, parent, providerType, newjson)
//          }
//        }
//      }
//    }

//    val newjson = {
//      json.as[JsObject] ++ 
//        Json.obj("resource_type" -> providerType.toString) 
//    }
//    createResourceCommon(org, parent, providerType, newjson)
  }
  
  def postProviderConfig(org: UUID, parentType: String, parent: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    postProviderCommon(org, parentType, parent, request.body)
  }
  
  def postProviderConfigOrgFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    val orgId = fqid(fqon)
    postProviderCommon(orgId, ResourceIds.Org.toString, orgId, request.body)
  }
  
  def postProviderConfigFqon(fqon: String, parentType: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    postProviderCommon(fqid(fqon), parentType, parent, request.body)
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
        case Success(input) => {
          println("INPUT:\n" + input)
          HandleCreate(createSynchronized(org, typeId, input)(sc, mc))
        }
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
        mr <- mc(creator, org,  sr, stringprops, input.description)
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