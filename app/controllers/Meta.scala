package controllers


import java.net.URL
import java.util.UUID

import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.data.{CoVariant, EnvironmentType, ResourceFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.keymgr.{GestaltFeature, GestaltLicense}
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.output.toLink
import com.galacticfog.gestalt.meta.api.sdk.GestaltResourceInput
import com.galacticfog.gestalt.meta.api.sdk.HostConfig
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.Resources
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
import com.galacticfog.gestalt.meta.providers.ProviderManager
import javax.inject.Singleton

import com.galacticfog.gestalt.meta.api.sdk
import com.galacticfog.gestalt.meta.providers._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.events._
import controllers.util.LambdaMethods
import com.galacticfog.gestalt.meta.actions._
import play.api.Logger

@Singleton
class Meta @Inject()( messagesApi: MessagesApi,
                      env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
                      security: Security,
                      securitySync: SecuritySync,
                      lambdaMethods: LambdaMethods,
                      actionMethods: ProviderActionMethods,
                      providerManager: ProviderManager,
                      genericResourceMethods: GenericResourceMethods )

      extends SecureController(messagesApi = messagesApi, env = env)
        with Authorization with MetaControllerUtils {
  
  // --------------------------------------------------------------------------
  // ORGS
  // --------------------------------------------------------------------------
  def postTopLevelOrg() = AsyncAudited() { implicit request =>
    security.getRootOrg(request.identity) match {
      case Failure(err)  => { 
        log.error(s"Failed to create top-level Org.")
        Future(HandleExceptions(err)) 
      }
      case Success(root) => createOrgCommon(root.id)
    }  
  }  
  
  def postOrgFqon(fqon: String) = AsyncAudited(fqon) { implicit request =>
    createOrgCommon(fqid(fqon))
  }
  
  def createOrgCommon(parentOrg: UUID)(implicit request: SecuredRequest[JsValue]) = Future {
    
    val json = request.body
    val user = request.identity
    
    Authorize(parentOrg, "org.create", request.identity) {
      
      CreateSynchronized(parentOrg, ResourceIds.Org, json)(security.createOrg, createNewMetaOrg[JsValue]) match {
        case Failure(err) => HandleExceptions(err)
        case Success(res) => {

          setNewResourceEntitlements(res.id, res.id, user, parent = Option(parentOrg))
          
          Created(Output.renderInstance(res, Some(META_URL)))
        }
      }

    }    
  }
  
  // --------------------------------------------------------------------------
  // GROUPS
  // --------------------------------------------------------------------------   
  def postGroupFqon(fqon: String) = AsyncAudited(fqon) { implicit request =>
    Future {
      createGroupCommon(fqid(fqon), request.body)  
    }
  }
  
  def createGroupCommon(org: UUID, json: JsValue)(implicit request: SecuredRequest[JsValue]) = {
    
    Authorize(org, "group.create", request.identity) {
      
      CreateSynchronized(org, ResourceIds.Group, request.body)(
          security.createGroup, createNewMetaGroup[JsValue]) match {
        
        case Failure(err) => HandleExceptions(err)
        case Success(res) => {
          val addusers: Seq[UUID] = Js.find(request.body.as[JsObject], "/properties/users").fold {
            Seq().asInstanceOf[Seq[UUID]]
            }{
            users => JsonUtil.safeParse[Seq[UUID]](users)
          }
          
          val newjson = { 
            if (addusers.isEmpty) Output.renderInstance(res, Some(META_URL))
            else {
              log.debug(s"Adding ${addusers.size} users to new group.")
              security.addAccountsToGroup(res.id, addusers) match {
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
          setNewResourceEntitlements(org, res.id, request.identity, parent = Option(org))
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
  def patchGroupUsers(fqon: String, group: UUID) = Audited(fqon) { implicit request =>
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
        security.addAccountsToGroup(group, ids) match {
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
  def deleteGroupUsers(fqon: String, group: UUID) = Audited(fqon) { implicit request =>
    
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
        security.removeAccountsFromGroup(group, ids) match {
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

  def postUserFqon(fqon: String) = AsyncAudited(fqon) { implicit request =>
    createUserCommon(fqid(fqon), request.body)
  }
  

  def createUserCommon(org: UUID, json: JsValue)(implicit request: SecuredRequest[JsValue]) = Future {
    log.debug(s"createUserCommon($org, ...) => JSON: ${json.toString}")
    
    Authorize(org, "user.create", request.identity) {
      
      val rootOrg = security.getRootOrg(request.identity).get
      val rootFqon = rootOrg.fqon
      val home = Js.find(request.body.as[JsObject], "/properties/gestalt_home") getOrElse JsString(rootOrg.fqon)
      
      log.debug(s"Setting 'gestalt_home' to $home")
      
      val userJson = upsertProperty(request.body.as[JsObject], "gestalt_home", /*JsString(root)*/ home)
      
      CreateSynchronized(org, ResourceIds.User, userJson.get)(
        security.createAccount, createNewMetaUser[JsValue]) match {
          case Failure(err) => {
            log.error("Response from security.createAccount() : " + err.getMessage)
            HandleExceptions(err)
          }
          case Success(res) => {
            val callerId = request.identity.account.id
            setNewResourceEntitlements(org, res.id, request.identity, parent = Option(org))
            securitySync.grantNewUserPermissions(callerId, res.id, rootOrg.id)
            Created(Output.renderInstance(res, Some(META_URL)))
          }
        }
    }
  }
  
  
  implicit def featureToString(feature: GestaltFeature) = feature.getLabel
  
  
  def postResourceToOrg(fqon: String, typ: String) = AsyncAudited(fqon) { implicit request =>
    val org = fqid(fqon)
    val typeid = UUID.fromString(typ)
    newDefaultResourceResult(org, typeid, parent = org, payload = request.body)
  }
  
  def postResource(fqon: String, typ: String, parent: UUID) = AsyncAudited(fqon) { implicit request =>
    val org = fqid(fqon)
    val typeid = UUID.fromString(typ)
    newDefaultResourceResult(org, typeid, parent, request.body)
  }
  
  def postResource2(fqon: String, typ: String, parent: UUID) = AsyncAudited(fqon) { implicit request =>
    val org = fqid(fqon)
    val typeid = UUID.fromString(typ)
    newResourceResult2(org, typeid, parent, request.body) { resource =>
      CreateWithEntitlements(org, request.identity, resource, Some(parent)) match {
        case Failure(err) => HandleExceptions(err)
        case Success(res) => Created(RenderSingle(res))
      }    
    }
  }
  
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
        
        log.debug("VALID PROVIDERS")
        validProviderTypes.foreach { case (k,v) => log.debug(s"[$v] - $k") }
          
        validProviderTypes.get(v.as[String]).fold {
          throw new BadRequestException(s"Unknown provider type : '${v.as[String]}'")
        }{ id => id }
        
      }
    }    
  }
  
  def postProviderConfigOrgFqon(fqon: String) = AsyncAudited(fqon) { implicit request =>
    val orgId = fqid(fqon)
    postProviderCommon(orgId, ResourceIds.Org.toString, orgId, request.body)
  }
  
  def postProviderConfigFqon(fqon: String, parentType: String, parent: UUID) = AsyncAudited(fqon) { implicit request =>
    postProviderCommon(fqid(fqon), parentType, parent, request.body)
  }
  
  def redeployProvider(fqon: String, id: UUID) = AsyncAuditedAny(fqon) { implicit request =>
    ResourceFactory.findById(id).fold {
      Future.successful(NotFound(s"Provider with ID '$id' not found."))
    }{ p =>
      /*
       * TODO: This is currently returning a 202 due to an issue with the method
       * that should return the provider after redeploy. See Meta Issue #353.
       */
//      for {
//        (_, ps) <- providerManager.processProvider(ProviderMap(p), forceLaunch = true)
//        out = getCurrentProvider(id, ps)
//      } yield Ok(RenderSingle(out))
      
      providerManager.processProvider(ProviderMap(p), forceLaunch = true) map { _ =>
        Accepted
      } recover {
        case e => {
          /*
           * TODO: Update Provider as 'FAILED' in Meta
           */
          HandleExceptions(e)
        }
      }
    }
  }
  
  def load(rootProvider: ProviderMap, identity: UUID) = {
    log.info("Beginning provider initialization...")
    providerManager.loadProviders(Some(rootProvider)) map { seq =>
      
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
    log.debug("entered getCurrentProvider(_, _)")
    for {
      seq <- results
      out = {
        log.debug(s"Finding provider in result-set. Seeking: ${targetId}")
        seq collect {
          case Success(t) if t.id == targetId => t
        }
      }
    } yield {
      if (out.size > 1) {
        log.error(s"Could not find Provider ${targetId} in results-list.")
        throw new RuntimeException("An error occurred launching the Provider")
      } else out.head
    }
  }

  private[controllers] def getCurrentProvider(targetId: UUID, results: Seq[GestaltResourceInstance]) = {
    log.debug("entered getCurrentProvider(_, _)")
    val out = results.filter(_.id == targetId)
    if (out.size > 1) {
      log.error(s"Could not find Provider ${targetId} in results-list.")
      throw new RuntimeException("An error occurred launching the Provider")
    } else out.head
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

    log.debug("Checking for valid environment_types...")
    val acceptableEnvTypes = Seq("development", "test", "production")
    
    Js.find(payload.as[JsObject], "/properties/environment_types").map { types =>
      val given = types.as[Seq[String]]
      val delta = given.diff(acceptableEnvTypes)
      if (delta.nonEmpty) {
        val msg = s"Invalid environment_types. expected: one of (${acceptableEnvTypes.mkString(",")}). found: (${delta.mkString(",")})"
        throw new UnprocessableEntityException(msg)
      }
    }
    
    lazy val addDefaultNetwork = (__ \ 'properties \ 'config).json.update(
      __.read[JsObject].map{ c => c ++
        Json.obj(
          "networks" -> (c \ "networks").asOpt[Seq[JsObject]].getOrElse[Seq[JsObject]](Seq(Json.obj("name" -> "default")))
        )
      }
    )

    val validName = "([a-z0-9]|[a-z0-9][a-z0-9\\-]*[a-z0-9])".r

    
    for {
      withValidatedName <- (payload \ "name").asOpt[String] match {
        case Some(validName(name)) => Success(payload)
        case Some(invalidName) => Failure(new BadRequestException(s"provider name is not valid, must conform to '${validName.regex}'"))
        case None => Failure(new BadRequestException("provider name is missing"))
      }

      withSpecifics <- providerType match {
        case ResourceIds.KubeProvider | ResourceIds.DockerProvider => Try {
          log.debug("Adding default kube networks...")
          val result = payload.transform(addDefaultNetwork)
          log.debug("RESULT:\n" + result.toString())
          result.get
        }
        case _ => Success(payload.as[JsObject])
      }
      
      withParentProp <- {
        log.debug("adding parent prop...")
        JsonUtil.upsertProperties(withSpecifics,
        "parent" -> Json.toJson(toLink(parent, baseUrl))
      )}

      withIdAndType = withParentProp ++ Json.obj(
        "id" -> newId.toString,
        "resource_type" -> providerType.toString
      )

      output <- updateLinkedProviders(withIdAndType)
    } yield output
    
  }
  
  private[controllers] def providerRequestOptions(org: UUID, user: AuthAccountWithCreds, payload: JsValue, baseUrl: String) = {
    RequestOptions(user, 
      authTarget = Option(org), 
      policyOwner = Option(org), 
      policyTarget = Option(jsonToResource(org, user, payload, Option(ResourceIds.Provider)).get),
      data = Option(Map("host" -> baseUrl)))    
  }
  
  import com.galacticfog.gestalt.patch._
  def updateLinkedProviders(json: JsObject): Try[JsObject] = {

    val lps = Js.find(json, "/properties/linked_providers") map { lp =>
      LinkedProvider.fromJson(lp)
    } getOrElse Seq.empty
    
    val updatedLinks = {
      val linkids = lps map { _.id }
      
      // Lookup all given linked_providers by their ID - load into a map keyed by ID
      val providers = ResourceFactory.findAllIn(linkids).map { 
        res => (res.id, res) 
      }.toMap 
      
			// If we found fewer providers than we asked for the request is invalid.
      val missing = linkids.diff( (providers.keys.toSeq) )
      if (missing.nonEmpty) {
        throw new UnprocessableEntityException(
            s"Invalid linked_providers. The following were not found: [${missing.mkString(", ")}]")
      }
      
      // Inject the type-id of each provider into the given links.
      lps map { k =>
        k.copy(
          typeId = Some(providers(k.id).typeId),
          `type` = Some(sdk.ResourceName(providers(k.id).typeId))
        )
      }
    }

    if (updatedLinks.isEmpty) {
      defaultLinkedProviders(json)
    }
    else {
      val jsonLinks = Json.toJson(updatedLinks)
      val patch = PatchDocument(PatchOp.Replace("/properties/linked_providers", jsonLinks))
      patch.applyPatch(json)
    }
  }
  
  /**
   * Inject empty properties.linked_providers if not found in given payload.
   * 
   * @param json JsObject representation of a Provider resource
   */
  def defaultLinkedProviders(json: JsObject): Try[JsObject] = {
    if ( Js.find(json, "/properties/linked_providers").isDefined )
      Try(json)
    else
      PatchDocument(PatchOp.Add("/properties/linked_providers", Json.arr())).applyPatch(json)
  }
  
  def newResourceId(json: JsObject): UUID = 
    Js.find(json.as[JsObject], "/id").fold(UUID.randomUUID) {
      uid => UUID.fromString(uid.as[String])
    }

//  import com.galacticfog.gestalt.meta.api.sdk.{ResourceStates,ResourceOwnerLink}  
//  implicit lazy val resourceConfigFormat = Json.format[ResourceConfig]
//  
//  case class ResourceConfig(`private`: Option[Boolean], data: JsValue) {
//    def isPrivate() = `private`.isDefined && `private`.get == true
//  }
//
//  
//  def handleConfiguration(org: UUID, owner: AuthAccountWithCreds, parentJson: JsValue, parentId: UUID, configPath: String) = {
//    extractConfig(parentJson).map { c =>
//      val cfg = newConfigResource(org, owner.account.id, parentId, c)
//      if (c.isPrivate) {
//        log.debug("Creating PRIVATE configuration")
//      } else {
//        log.debug("Creating Configuration with standard permissions.")
//      }
//      CreateWithEntitlements(org, owner, cfg, Some(parentId))
//    }    
//  }
//  
//  def extractConfig(resourceJson: JsValue): Option[ResourceConfig] = {
//    Js.find(resourceJson.as[JsObject], "/properties/configuration").map { c =>
//      Js.parse[ResourceConfig](c).get
//    }
//  }
//  
//  def newConfigResource(org: UUID, owner: UUID, parent: UUID, config: ResourceConfig) = {
//    GestaltResourceInstance(
//        id     = UUID.randomUUID,
//        typeId = ResourceIds.Configuration,
//        state  = ResourceState.id(ResourceStates.Active),
//        orgId  = org,
//        owner  = ResourceOwnerLink(ResourceIds.User, owner),
//        name   = "%s.%s".format(parent, "config"),
//        description = None,
//      properties = Some(Map("data" -> Json.stringify(config.data))))    
//  }  
  
  def postProviderCommon(org: UUID, parentType: String, parentId: UUID, json: JsValue)(
    implicit request: SecuredRequest[JsValue]) = {

    val providerType = resolveProviderType(json)
    val parentTypeId = UUID.fromString(parentType)

    ResourceFactory.findById(parentTypeId, parentId).fold {
      Future(ResourceNotFound(parentTypeId, parentId))
    } { parent =>

      // This is the ID we create the new provider with.
      val targetid = newResourceId(json.as[JsObject])

      val user    = request.identity
      val payload = normalizeProviderPayload(json, targetid, providerType, parent, Some(META_URL)).get

      newResourceResultAsync(org, providerType, parent.id, payload) { _ =>

        val identity = user.account.id
                
        CreateWithEntitlements(org, user, payload, providerType, Some(parentId)) match {
          case Failure(e) => {
            /*
             * TODO: Update Provider as 'FAILED' in Meta
             */
            log.error("CreateResource() FAILED!")
            Future.successful(HandleExceptions(e))
          }
          case Success(newMetaProvider) => {

            val results = load(ProviderMap(newMetaProvider), identity)

            getCurrentProvider(targetid, results) map { newprovider =>


              val output: GestaltResourceInstance = {

                if (!ProviderMethods.isActionProvider(newprovider.typeId)) newprovider
                else {
                  /*
                   * This call will create a new Environment for this Provider. This is where we
                   * will store the Lambda, Api, and ApiEndpoints for any specified ProviderActions.
                   */
                  val providerEnv = providerManager.getOrCreateProviderEnvironment(newprovider, user)
                  log.debug("Creating Provider Actions...")

                  val acts   = createProviderActions(newprovider, payload, user, providerEnv, parentId)
                  val json   = Output.renderLinks(acts, Some(META_URL))
                  val props  = newprovider.properties map { ps =>
                    ps ++ Map("provider_actions" -> Json.stringify(json))
                  }
                  newprovider.copy(properties = props)
                  ProviderMethods.injectProviderActions(newprovider)
                }
              }

              Created(RenderSingle(output))
              
            } recover {
              case e => {
                /*
                 * TODO: Update Provider as 'FAILED' in Meta
                 */
                HandleExceptions(e)
              }
            }
          }
        }
      }
    }
  }

  
  def invokeAction(fqon: String, id: UUID) = AsyncAudited(fqon) { implicit request =>
    log.info(s"invokeAction($fqon, $id)")
    val maybeEvents = for {
      action   <- Try { ResourceFactory.findById(ResourceIds.ProviderAction, id) getOrElse {
        throw new ResourceNotFoundException(s"Action with ID '$id' not found.")
      }}
      lambda   <- actionMethods.lambdaFromAction(action.id)
      endpoint <- actionMethods.buildActionMessageEndpoint(lambda)
      
      client  = AmqpClient {
        AmqpConnection(endpoint.service_host, endpoint.service_port.get, heartbeat = 300)
      }
      
      eventEp = AmqpEndpoint(endpoint.message_exchange, endpoint.message_topic)
    } yield (lambda, client, eventEp)
    
    maybeEvents match {
      case Failure(e) => throw e
      case Success((lambda, client, endpoint)) => {
        
        val event = ActionInvokeEvent(
          lambdaId = lambda.id,
          payload = Some(Json.stringify(request.body)),
          params = request.queryString)

        client.publish(endpoint, Json.stringify(Json.toJson(event)))
      }
    }
    Future( Ok(Json.obj("status" -> "ok", "message" -> "sent")))
  }  
  
  
  /**
   * 
   * @param r the Provider instance to create actions for
   * @param payload the JSON used to create the Provider
   * @param creator the user who initiated this call
   * @param providerEnv the new environment where action lambdas will be created
   * @param parentId UUID of Provider parent resource
   */
  def createProviderActions(
        r: GestaltResourceInstance, 
        payload: JsObject, 
        creator: AuthAccountWithCreds, 
        providerEnv: GestaltResourceInstance,
        parentId: UUID) = {
    
    // Parse the ActionSpec JSON from the Provider type definition.

    val actionSpecs = for {
      s <- TypeFactory.findById(r.typeId)
      p <- s.properties
      a <- p.get("provider_actions")
      jsv  = Json.parse(a)
      acts = jsv.as[JsArray].value.map { Js.parse[ProviderActionSpec](_)(providerActionFormat) }
    } yield acts
    
    val (failedParse, successfulParse) = (actionSpecs getOrElse Seq.empty) partition { _.isFailure }
    
    if (failedParse.nonEmpty) {
      // TODO: There were errors parsing ProviderActions - DO SOMETHING!
      throw new RuntimeException("FAILED PARSING PROVIDER-ACTIONS")
    }
    
    val fActions = successfulParse.map { spec =>
      /*
       * TODO: Idea is that we can use a pre-existing lambda or create a new one
       * from a provided specification. First-pass we will only consider the
       * new creation from a spec.
       */
      log.debug("Creating Action Lambda...")
      log.debug(Json.prettyPrint(Json.toJson(spec.get)))
      
      log.info("Attempting to resolve action-implementation...")
      
      val impl = spec.get.implementation
      
      val action = impl.kind.trim.toLowerCase match {
        case "lambda" => {
          actionMethods.resolveLambdaImplementation(r.orgId, impl, providerEnv, creator).map { lam =>
            spec.get.toResource(r.orgId, r.id, r.owner, implId = Some(lam.id))
          }
        }
        case "metacallback" => Future(spec.get.toResource(r.orgId, r.id, r.owner))
      }      

      action map { act =>
        CreateWithEntitlements(r.orgId, creator, act, Some(r.id)) match {
          case Success(res) => res
          case Failure(err) => throw new RuntimeException("Failed creating ProviderAction: " + err.getMessage)
        }        
      }
      
    }
    import scala.concurrent.Await
    import scala.concurrent.duration.DurationInt

    Await.result(Future.sequence(fActions), 15.seconds)
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
      input    <- toInput(json, Option(typeId))
      resource <- syncWithSecurity(org, typeId, input)(sc, mc)
    } yield resource
  }
  
  private def syncWithSecurity[T](org: UUID, typeId: UUID, input: GestaltResourceInput)
    (sc: SecurityResourceFunction, mc: MetaResourceFunction)
    (implicit request: SecuredRequest[T]): Try[GestaltResourceInstance] = {

    val stringprops = stringmap(input.properties)
    val creator = request.identity//.account.id
    
    for {
      sr <- sc(org, request.identity, input)
      mr <- mc(creator, org,  sr, stringprops, input.description)
    } yield mr
  }
  
  def normalizeUserJson(json: JsObject, account: AuthAccountWithCreds) = {
    // Check if gestalt_home property is set - if not
    // create property and set to 'root' Org.
    
    json \ "properties" \ "gestalt_home" match {
      case u: JsUndefined => {
        val root = security.getRootOrg(account)
        val props = replaceJsonPropValue(json, "gestalt_home", root.get.fqon)
        replaceJsonProps(json, props)
      }
      case _ => json
    }
  }
  
  /**
   * Unwrap the given UUID or get the root org_id if None.
   */
  private def orgOrElseRoot[T](org: Option[UUID])(implicit request: SecuredRequest[T]) = org getOrElse {
    security.getRootOrg(request.identity) match {
      case Success(org) => org.id
      case Failure(ex)  => throw ex
    }
  }

  def postProviderAction(fqon: String, id: java.util.UUID, action: String) = AsyncAuditedAny(fqon) { implicit request =>
    log.debug(s"postProviderAction(${fqon},${id},${action})")
    for {
      org <- findOrgOrFail(fqon)
      provider <- ResourceFactory.findById(id) match {
        case Some(p) if ResourceFactory.isSubTypeOf(p.typeId, ResourceIds.Provider) => Future.successful(p)
        case Some(np) => Future.failed(new ResourceNotFoundException(s"Resource '${id}' is not a subtype of Provider"))
        case None => Future.failed(new ResourceNotFoundException(s"Resource '${id}' not found"))
      }
      result <- genericResourceMethods.performProviderBackedAction(
        org = org,
        identity = request.identity,
        body = request.body,
        resourceType = provider.typeId,
        providerType = provider.typeId,
        actionVerb = action,
        resourceId = provider.id,
        specificProviderId = Some(provider.id)
      )
    } yield result
  }

}
