package controllers


import java.util.UUID

import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.data.{ResourceFactory, TypeFactory, string2uuid, uuid, uuid2string}
import com.galacticfog.gestalt.meta.api.ContainerSpec.ExistingVolumeMountSpec
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceInfo, ResourceLabel, resourceInfoFormat}
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.security.api.errors.ForbiddenAPIException
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltFrameworkSecurity, GestaltFrameworkSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import controllers.util.JsonUtil._
import controllers.util._
import javax.inject.Singleton
import play.api.i18n.MessagesApi
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc.{Action, Result}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data.{DataType,EnvironmentType,ResourceState,VisibilityType}

import com.galacticfog.gestalt.data.PropertyFactory
import com.galacticfog.gestalt.json._
import com.galacticfog.gestalt.marathon.containerWithDefaults

import services.ProviderContext  
import play.api.mvc.{Request, RequestHeader}
import play.api.mvc.AnyContent

import java.math.BigInteger
import java.security.SecureRandom
import com.galacticfog.gestalt.data.parseUUID
import com.galacticfog.gestalt.meta.providers._      
import com.galacticfog.gestalt.meta.providers.ui._
import com.galacticfog.gestalt.data.ResourceFactory.findTypesWithVariance
import com.galacticfog.gestalt.data.{CoVariant, Invariant, ResourceType, Variance}  
import com.galacticfog.gestalt.meta.auth._  
import com.galacticfog.gestalt.meta.api.output.toLink



@Singleton
class ResourceController @Inject()( 
    messagesApi: MessagesApi,
    sec: GestaltFrameworkSecurity,
    security: Security,
    containerService: ContainerService,
    genericResourceMethods: GenericResourceMethods,
    lambdaMethods: LambdaMethods )
    
  extends SecureController(messagesApi = messagesApi, sec = sec)
    with Authorization with MetaControllerUtils {
  
  type TransformFunction = (GestaltResourceInstance, AuthAccountWithCreds, Option[QueryString]) => Try[GestaltResourceInstance]
  type FilterFunction    = ((Seq[ResourceLike], Map[String, Seq[String]]) => Seq[ResourceLike])
  type QueryString = Map[String, Seq[String]]
  
  type Lookup    = (ResourcePath, AuthAccountWithCreds, Option[QueryString]) => Option[GestaltResourceInstance]
  type LookupSeq = (ResourcePath, AuthAccountWithCreds, QueryString) => Seq[GestaltResourceInstance]

  def embed( embeddings: Map[String,(GestaltResourceInstance,AuthAccountWithCreds,Option[QueryString]) => GestaltResourceInstance] ): TransformFunction = performOptionalEmbeddings(
     embeddings, _: GestaltResourceInstance, _: AuthAccountWithCreds, _: Option[QueryString]
  )

  private[controllers] val transforms: Map[UUID, TransformFunction] = Map(
      ResourceIds.Group  -> transformGroup,
      ResourceIds.User   -> transformUser,
      ResourceIds.Policy -> transformPolicy,
      ResourceIds.ApiEndpoint -> transformApiEndpoint,
      ResourceIds.Lambda -> embed(Map("apiendpoints" -> embedEndpoints, "provider" -> embedProvider)),
      ResourceIds.Container -> embed(Map("apiendpoints" -> embedEndpoints, "provider" -> embedProvider, "volumes" -> embedVolumes)),
      migrations.V13.VOLUME_TYPE_ID -> embed(Map("container" -> embedContainerMountInfo)),
      migrations.V7.STREAM_SPEC_TYPE_ID -> transformStreamSpec
  )

  private[controllers] val lookups: Map[UUID, Lookup] = Map(
    ResourceIds.Container   -> lookupContainer,
    ResourceIds.Entitlement -> lookupEntitlement,
    ResourceIds.Provider    -> lookupProvider
  )

  private[controllers] val lookupSeqs: Map[UUID, LookupSeq] = Map(
    ResourceIds.Provider    -> lookupSeqProviders,
    ResourceIds.Secret      -> lookupSeqSecrets,
    ResourceIds.Org         -> lookupSeqOrgs,
    ResourceIds.Container   -> lookupContainers,
    ResourceIds.Entitlement -> lookupSeqEntitlements,
    ResourceIds.Rule        -> lookupPolicyRules,
    ResourceIds.ProviderAction -> lookupProviderActions
  )


  def lookupProviderActions(path: ResourcePath, user: AuthAccountWithCreds, qs: QueryString): Seq[GestaltResourceInstance] ={
    val mapPathData = Resource.mapListPathData(path.path)
    val parent = mapPathData(Resource.ParentId)

    if (qs.contains("q") && (qs("q")(0).toLowerCase == "entitlements")) {
      ResourceFactory.rollupActionEntitlements(parent)
    } else {
      ResourceFactory.findChildrenOfType(ResourceIds.ProviderAction, UUID.fromString(parent))
    }
  }


  def lookupPolicyRules(path: ResourcePath, user: AuthAccountWithCreds, qs: QueryString): Seq[GestaltResourceInstance] ={
    val mapPathData = Resource.mapListPathData(path.path)
    val policy = mapPathData(Resource.ParentId)
    ResourceFactory.findChildrenOfSubType(ResourceIds.Rule, policy)
  }

  def lookupProvider(path: ResourcePath, user: AuthAccountWithCreds, qs: Option[QueryString]): Option[GestaltResourceInstance] = {
    log.debug("Lookup function : lookupProvider(_,_,_)...")

    Resource.toInstance(path) map { res =>
      (maybeInjectActions _  andThen maybeMaskCredentials(qs)) apply res
    }
  }

  def lookupContainer(path: ResourcePath, user: AuthAccountWithCreds, qs: Option[QueryString]): Option[GestaltResourceInstance] = {
    log.debug("Lookup function : lookupContainer(_,_,_)...")
    Resource.toInstance(path) flatMap { r =>
      val fqon = Resource.getFqon(path.path)
      val env  = ResourceFactory.findParent(r.id) getOrElse throwBadRequest("could not determine environment parent for container")

      log.debug("fqon: %s, env: %s[%s]".format(fqon, env.name, env.id.toString))
      log.debug("Calling external CaaS provider...")

      Await.result(
        containerService.getEnvironmentContainer(fqon, env.id, r.id),
        5 seconds
      ) map (_._1)
    }
  }

  def lookupContainers(path: ResourcePath, account: AuthAccountWithCreds, qs: QueryString): Seq[GestaltResourceInstance] = {
    if (getExpandParam(qs)) {
      // rs map transformMetaResourceToContainerAndUpdateWithStatsFromMarathon
      val mapPathData = Resource.mapListPathData(path.path)
      val (fqon,eid) = List(Resource.Fqon, Resource.ParentType, Resource.ParentId) flatMap ( mapPathData.get _ ) match {
        case List(fqon,ptype,pid) if ptype == "environments" => (fqon,pid)
        case _ => throwBadRequest("container lookup can happen only in the context of an environment")
      }
      
      /*
       * Have to pre-process the container-list
       */
      
      Await.result(
        containerService.listEnvironmentContainers(fqon, eid),
        5 seconds
      ) map (_._1)
    } else Resource.listFromPath(path.path, qs)
  }

  def FqonNotFound(fqon: String) = {
    throw new BadRequestException(s"Org with FQON '${fqon}' not found.")
  }
  
  def getOrgFqon(fqon: String) = Audited() { implicit request =>
    val action = "org.view"
    log.debug(s"Authorizing lookup : user=${request.identity.account.id}, ${action}")
    Authorize(fqid(fqon), action, request.identity) {
      Ok(RenderSingle(Resource.fromPath(fqon).get))
    }
  }

  /**
   * Get a top-level resource list. Pulls resources from across all Orgs (no leading FQON).
   * i.e., GET /users
   */
  def getGlobalResourceList(targetTypeId: String) = Audited() { implicit request =>
    val typeId = uuid(targetTypeId)
    val action = actionInfo(typeId).prefix + ".view"
    AuthorizeList(action) {
      ResourceFactory.findAll(typeId)
    }
  }
  
  /**
   * Custom lookup providing a shortcut to Provider containers.
   */
  def getProviderContainers(fqon: String, provider: UUID) = Audited(fqon) { implicit request =>
    ResourceFactory.findById(provider).fold {
      ResourceNotFound(ResourceIds.Provider, provider)
    }{ _ =>
      AuthorizeList("container.view") {
        ResourceFactory.findDescendantsOfType(ResourceIds.Container, provider)
      }
    }
  }

  def getProviderContainer(fqon: String, provider: UUID, container: UUID) = Audited(fqon) { implicit request =>
    ResourceFactory.findById(provider).fold {
      ResourceNotFound(ResourceIds.Provider, provider)
    }{ _ =>
      Authorize(container, "container.view") {
        /*
         * TODO: This doesn't check if the container is a descendant of
         * the provider.
         */
        ResourceFactory.findById(ResourceIds.Container, container).fold {
          ResourceNotFound(ResourceIds.Container, container)
        }{ c => Ok(RenderSingle(c)) }
      }
    }
  }

  def findResourceGlobal(typeName: String, id: UUID) = Audited() { implicit request =>
    GlobalResource.getResource(typeName, id) match {
      case Failure(e) => HandleExceptions(e)
      case Success(res) => Ok(RenderSingle(res))
    }
  }


  /**
   * Find the parent of a resource given it's path (default to given fqon)
   */
  def findResourceParent(rp: ResourcePath, fqon: String) = {
    ResourceFactory.findById(
        typeId = rp.parentTypeId getOrElse ResourceIds.Org,
        id = rp.parentId getOrElse fqid(fqon)
      ) match {
        case Some(parent) => Success(parent)
        case None => Failure(new ConflictException("parent not specified/does not exist/does match type"))
      }
  }
 
  /**
   * Get JSON from a Request - fail if content-type isn't JSON.
   */
  def requireJsonRequest(request: SecuredRequest[GestaltFrameworkSecurityEnvironment, AnyContent])= {
    request.map { ac => 
      ac.asJson.getOrElse {
        throw new UnsupportedMediaTypeException("Expecting text/json or application/json")
      }
    }
  }
  
  /**
   * Execute a generic 'create' request. If the target resource is provider-backed,
   * delegate creation to the appropriate provider. If not provider-backed, use the
   * default 'create in Meta' function.
   */
  def execGenericCreate(
      org: GestaltResourceInstance,
      targetTypeId: UUID,
      parent: GestaltResourceInstance,
      payload: JsValue)(implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue])
        : Future[GestaltResourceInstance] = {
      
    /*
     * Determine if the resource we're creating is backed by a provider.
     */    
    getBackingProviderType(targetTypeId) match {
      case None => {
        log.debug("**Request to create non-provider-backed resource")
        newDefaultResource(org.id, targetTypeId, parent.id, payload)(request)
      }
      case Some(backingProviderType) => {
        genericResourceMethods.createProviderBackedResource(
          org = org,
          identity = request.identity,
          body = payload,
          parent = parent,
          resourceType = targetTypeId,
          providerType = backingProviderType,
          actionVerb = "create")       
      }
    }
  }
  
  /**
   * Execute an arbitrary generic resource action. Action will fail if the 
   * given resource is NOT provider-backed.
   */
  def execGenericAction(
      org: GestaltResourceInstance,
      targetTypeId: UUID,
      targetId: UUID,
      action: String)(implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,AnyContent]) = {
    
    /*
     * Ensure the target resource is backed by a provider.
     */
    getBackingProviderType(targetTypeId) match {
      case None => {
        Future.successful(
            BadRequestResult("actions can only be performed against provider-backed resources"))
      }
      case Some(backingProviderType) => { 
        genericResourceMethods.performProviderBackedAction(
          org = org,
          identity = request.identity,
          body = request.body,
          resourceType = targetTypeId,
          providerType = backingProviderType,
          actionVerb = action,
          resourceId = targetId)
      }
    }
  }
  
  def requireActionParam(request: Request[AnyContent]) = {
    request.getQueryString("action").getOrElse {
      throwBadRequest("Missing parameter: action")
    }    
  }
  
  def requireTargetId(path: ResourcePath) = {
    path.targetId match {
      case Some(targetId) => Future.successful(UUID.fromString(targetId))
      case None => Future.failed(
        new ResourceNotFoundException("actions must be performed against a specific resource")
      )
    }    
  }
  

  
  def createContainer(
      org: UUID, 
      environment: UUID)(implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]) = {
    
    val created = for {
      payload   <- Future.fromTry(normalizeCaasPayload(request.body, environment))
      proto     <- Future.fromTry(jsonToResource(org, request.identity, normalizeInputContainer(payload), None))
      spec      <- Future.fromTry(ContainerSpec.fromResourceInstance(proto))
      context   = ProviderContext(request, spec.provider.id, None)
      container <- containerService.createContainer(context, request.identity, spec, Some(proto.id))
    } yield container
  }  
  
  
  /**
   * Ensure Container input JSON is well-formed and valid. Ensures that required properties
   * are given and fills in default values where appropriate.
   */
  private [this] def normalizeInputContainer(inputJson: JsValue): JsObject = {
    val defaults = containerWithDefaults(inputJson)
    val newprops = Json.toJson(defaults).as[JsObject] ++ (inputJson \ "properties").as[JsObject]
    inputJson.as[JsObject] ++
      Json.obj("resource_type" -> ResourceIds.Container.toString) ++
      Json.obj("properties" -> newprops)
  }  
  
  private[controllers] def normalizeCaasPayload(payloadJson: JsValue, environment: UUID): Try[JsObject] = Try {
  
    val payload = payloadJson.as[JsObject]
    
    val (env, provider) = (for {
      pid <- Try(Js.find(payload, "/properties/provider/id") getOrElse {
            throw new BadRequestException(s"`/properties/provider/id` not found.")
          })
      env <- Try(ResourceFactory.findById(ResourceIds.Environment, environment) getOrElse {
            throw new BadRequestException(s"Environment with ID '$environment' not found.")
          })
      prv <- Try(ResourceFactory.findById(UUID.fromString(pid.as[String])) getOrElse {
            throw new BadRequestException(s"CaasProvider with ID '$pid' not found")
          })
    } yield (env, prv)).get
    
    /*
     *  This throws an exception if the environment is incompatible with the provider.
     */
    assertCompatibleEnvType(provider, env)

    // Inject `provider` object into container.properties
    val oldProperties = Js.find(payload, "/properties").flatMap(_.asOpt[JsObject]).getOrElse(Json.obj())
    payload ++ Json.obj(
      "properties" -> (oldProperties ++ Json.obj(
        "provider" -> Json.obj(
          "name"          -> provider.name,
          "id"            -> provider.id,
          "resource_type" -> sdk.ResourceName(provider.typeId)
        )
      ))
    )
  }  
  
  
  /**
   * Test that the given Provider is compatible with the given Environment. An
   * exception is thrown if the given Environment is incompatible.
   */
  private[controllers] def assertCompatibleEnvType(provider: GestaltResourceInstance, env: GestaltResourceInstance) {
    
    // These are the values tha Meta will accept for 'environment_type'
    val acceptableTypes = Set("development", "test", "production")
    
    provider.properties.get.get("environment_types").map { types =>
      
      // Environment types the provider is compatible with.
      val allowedByProvider = Json.parse(types).as[Seq[String]]
      
      // Environment type of the given environment.
      val envType = {
        val typeId = env.properties.get.get("environment_type") getOrElse {
          val msg = s"Environment with ID '${env.id}' does not contain 'environment_type' property. Data is corrupt"
          throw new RuntimeException(msg)
        }
        val target = EnvironmentType.name(UUID.fromString(typeId))
        if (acceptableTypes.contains(target.trim.toLowerCase)) target
        else {
          val msg = s"Invalid environment_type. Expected: one of (${acceptableTypes.mkString(",")}. found: '$target'"
          throw new UnprocessableEntityException(msg)
        }
      }
      
      // Given MUST be in the allowed list.
      if (allowedByProvider.nonEmpty && !allowedByProvider.contains(envType)) {
        val msg = s"Incompatible Environment type. Expected one of (${allowedByProvider.mkString(",")}). found: '$envType'"
        throw new ConflictException(msg)
      }
    }    
  }  
  
  
  private object StorageManager {

    def createStorageProvider() = {
      ???
    }

    def createStorageContainer(extraConfig: Option[JsValue]) = {
      ???
    }    
    
  }
  
  
  private object StorageType {
    
    def mergeEnvironmentVars(res: GestaltResourceInstance, vars: Map[String, String]) = {
      val ps = res.properties.getOrElse(Map.empty)
      val newvars = {
        Json.toJson {
          ps.get("env").fold(Map.empty[String,String]){ env =>
            val oldenv = Js.parse[Map[String,String]](Json.parse(env).as[JsObject]).get
            oldenv ++ vars
          }
        }
      }

      val newprops = try {
        ps ++ Map("env" -> Json.stringify(newvars))
      } catch {
        case e: Throwable => {
          println("****ERRROR : " + e.getMessage)
          throw e
        }
      }

      res.copy(properties = Some(newprops))
    }
    
    def getStorageProperty(res: GestaltResourceInstance): Option[String] = {
      for {
        ps <- res.properties
        st <- ps.get("storage")
      } yield st
    }
    
    def getUpstreamStorageVar(res: GestaltResourceInstance): Option[String] = {
      EnvironmentVars.get(res.orgId, res.id).get("GF_DEFAULT_OBJECT_STORAGE_ID")
    }
    
    def isExisting(s: String): Boolean = {
      test(s){ js => Js.find(js, "provider_id").isDefined }
    }
    
    def isNone(s: String): Boolean = {
      test(s){ js =>
        Js.find(js, "/kind").fold(false){ k =>
          k.as[String] == "none"}
      }
    }
    
    def isConfig(s: String): Boolean = {
      test(s){ js =>
        Js.find(js, "/provider_id").isEmpty &&
        Js.find(js, "/kind").isEmpty
      }
    }
    
    private def test(s: String)(f: JsObject => Boolean): Boolean = {
      try {
        f(Json.parse(s).as[JsObject])
      } catch {
        case e: Throwable => {
          throw new BadRequestException("Error parsing '/properties/storage': " + e.getMessage)
        }
      }
    }
  }
  

  
  def randomAlphaNum(chars: Int = 24): String = {
    new BigInteger(chars * 5, new SecureRandom()).toString(32)
  }

  def postCreateEnvironment(resource: GestaltResourceInstance)(implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment, JsValue]) = {
    
    def addEnvVar(r: GestaltResourceInstance, pair: (String, String)) = {
      val props = r.properties.getOrElse(Map.empty)
      val envs = props.get("env").map(Json.parse(_).as[JsObject]).getOrElse(Json.obj())
      
      val newEnvs = envs + (pair._1 -> JsString(pair._2))
      val newProps = props ++ Map("env" -> Json.stringify(newEnvs))
      r.copy(properties = Some(newProps))
    }
    
    def validateCaasProvider(providerString: String): Option[UUID] = {
      /*
       * TODO: Protect if can't parse as UUID
       */
      val pid = UUID.fromString(providerString)
      
      ResourceFactory.findById(pid).fold(Option.empty[UUID]) { prv =>
        if (ResourceFactory.isSubTypeOf(prv.typeId, ResourceIds.CaasProvider)) {
          Some(prv.id)
        } else 
          throw new BadRequestException(s"Invalid caas_provider property. found: '${providerString}'")
      }
    }
    
    // Check for CaaS provider property - set as env var if found
    val updated = for {
      ps     <- resource.properties
      value  <- ps.get("caas_provider")
      caas   <- validateCaasProvider(value)
      update = addEnvVar(resource, "GF_DEFAULT_CAAS_PROVIDER_ID" -> caas)
    } yield update

    /*
     * Evaluate storage creation
     */
    val updated2: Option[GestaltResourceInstance] = 
      StorageType.getStorageProperty(resource) match {
      
      case Some(storage) => {
        if (StorageType.isExisting(storage)) {
          /*
           * TODO: validate pointer and set env vars
           * - (set storage_id, storage_address, storage_key, storage_secret, storage_protocol)
           * 
           */
          val pid = Js.find(Json.parse(storage).as[JsObject], "/provider_id").get.as[String]
          println(s"***[Storage]: FOUND existing storage pointer [$pid]...validating")          
          updated
          
        } else if (StorageType.isNone(storage)) {
          /*
           * TODO: ???
           * Leave storage property set to 'none'? That way we know the creator explicitly
           * DID NOT want storage.
           */
          println("***[Storage]: Object Store creation has been SUPPRESSED.")
          updated
          
        } else if (StorageType.isConfig(storage)) {
          /*
           * TODO: Merge configuration ???
           * Remove storage property from map
           */
          println("***[Storage]: CONFIGURATION-ONLY storage object...merging config and provisioning.")
          /*
           * 1.) Get spec
           * 2.) Merge in new config (spec ++ newConfig)
           * 3.) Create the storage.
           */
          updated
        } else {
          /*
           * TODO: Test if value is complete inline storage-specification!!!
           */
          updated
        }
      }
      case None => {

        /*
         * We didn't find a storage configuration - check if there's
         * an object store in upstream variables.
         */
        val upstream = StorageType.getUpstreamStorageVar(resource)
        if (upstream.isDefined) {
          /*
           * TODO: We have an object store upstream that we're supposed 
           * to use. I don't think there's anything to do in this case.
           */
          println("***[Storage]: Found upstream storage!")
          updated
        } else {
          /*
           * TODO: No upstream storage was found. Provision new...
           */
          println("***[Storage]: Provisioning new Object Store...")
          val envs = Map(
            "GF_DEFAULT_OBJECT_STORAGE_ID" -> "ebb9a4f9-5e01-4484-a2e0-81d8ddf7bf0c",
            //"GF_DEFAULT_OBJECT_STORAGE_ADDRESS" -> "35.199.37.155",
            "GF_DEFAULT_OBJECT_STORAGE_PORT" -> "9000", 
            "GF_DEFAULT_OBJECT_STORAGE_ACCESS_KEY" -> randomAlphaNum(),
            "GF_DEFAULT_OBJECT_STORAGE_ACCESS_SECRET" -> randomAlphaNum()
          )

          createContainer(resource.orgId, resource.id)
          
          val x = Some(StorageType.mergeEnvironmentVars(updated.getOrElse(resource), envs))
          println("X : " + x)
          x
        }

      }
    }
    
    
//    resource.properties.get.get("storage") match {
//      case Some(storage) => {
//        val pid = Js.find(Json.parse(storage).as[JsObject], "/provider_id")
//        
//        
//        if (pid.isDefined) {
//          // Found pointer to existing storage
//          /*
//           * TODO: Validate existing storage provider - use if good.
//           */
//          println("***FOUND existing storage pointer...validating for use...")
//        } else if (Js.find(Json.parse(storage).as[JsObject], "/kind").isDefined) {
//          // No pointer - merge in configuration and create new.
//        }
//      }
//      case None => {
//        // Auto-create new storage
//        println("*** NO STORAGE INFO FOUND...")
//        println("*** Checking for default object store in scope")
//        /*
//         * TODO: Rollup env vars and test for 'GF_DEFAULT_OBJECT_STORAGE_ID'
//         */
//        val sid = EnvironmentVars.get(resource.orgId, resource.id).get("GF_DEFAULT_OBJECT_STORAGE_ID")
//        if (sid.isDefined) {
//          /*
//           * TODO: Found an existing default - nothing to do.
//           */
//        } else {
//          /*
//           * TODO: No storage given 
//           */
//        }
//      }
//    }
    
    updated2.getOrElse(updated.getOrElse(resource))
  } 
  
  
  def postCreateCheck(resource: GestaltResourceInstance, identity: UUID)
    (implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment, JsValue]): GestaltResourceInstance = {
    
    println("*****POST-CHECK resource of type : " + ResourceLabel(resource.typeId))
    
    /*
    // List of all spec types allowed by the current resource schema
    val allowedSpecs = PropertyFactory.findAll(resource.typeId).filter { t =>
      t.datatype == DataType.id("spec::uuid::link")
    }
    
    println("*****LOOKING for spec type properties...")
    val given = resource.properties.getOrElse(Map.empty)
    
    //val newSpecs = given.filter { case (k,v) => allowedSpecs.map(_.name).contains(k) }
    
    val newSpecs = allowedSpecs.filter { p => given.keySet.contains(p.name) }
    
    println("*****SPEC properties found: " + newSpecs.size)
    
    newSpecs foreach { p => println("***Creating : [%s] %s".format(p.id, p.name)) }
    */
    
    println("****Checking resource type for post-create...")
    
    val outputResource = if (resource.typeId == ResourceIds.Environment) {
      println("*****Type is Environment...checking for variable updates...")
      val update = postCreateEnvironment(resource)
      val x = ResourceFactory.update(update, identity)
      println("UPDATE STATUS : " + x)
      update
    } else resource
    
    /*
     * TODO: If specs are created we need to update the resource!
     */
     
    outputResource
  }  
  
  
  def genericResourcePost(fqon: String, path: String) = AsyncAuditedAny(fqon) { implicit request =>
    log.debug(s"genericResourcePost(${fqon},${path})")
    
    val rp = new ResourcePath(fqon, path)
    log.debug(rp.info.toString)
    
    if (rp.isList) { // create semantics
      val x = for {
        org         <- findOrgOrFail(fqon)
        parent      <- Future.fromTry(findResourceParent(rp, fqon))
        jsonRequest <- fTry(requireJsonRequest(request))
        securedRequest = SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue](request.identity, request.authenticator, jsonRequest)
        
        resource    <- execGenericCreate(org, rp.targetTypeId, parent, jsonRequest.body)(securedRequest)
        r2 = postCreateCheck(resource, request.identity.account.id)(securedRequest)
      } yield r2
      
      x.map (r => Created(Output.renderInstance(r, Some(META_URL))))
        .recover { case e => HandleExceptions(e) }  
      
    } else { // perform action
      for {
        org      <- findOrgOrFail(fqon)
        action   <- fTry( requireActionParam(request) )
        targetId <- requireTargetId(rp)
        
        result   <- execGenericAction(org, rp.targetTypeId, targetId, action)
      } yield result
    }
  }      
  
  /**
   * Get a Resource or list of Resources by path.
   */
  def getResources(fqon: String, path: String) = Audited(fqon) { implicit request =>
    val rp = new ResourcePath(fqon, path)

    val action = actionInfo(rp.targetTypeId).prefix + ".view"

    log.trace(s"getResources(_, $path)")
    log.debug("Action : " + action)

    if (rp.isList) {
      AuthorizedResourceList(rp, action, request.queryString)
    }
    else AuthorizedResourceSingle(rp, action)
  }

  private[controllers] def AuthorizedResourceSingle(path: ResourcePath, action: String)
      (implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,_]): Result = {

    log.debug(s"Authorizing lookup : user=${request.identity.account.id}, $action")

    Authorize(path.targetId.get, action) {

      val resource = lookups.get(path.targetTypeId).fold {
        Resource.toInstance(path)
      }{ f=>
        log.debug(s"Found custom lookup function for Resource. Executing...")
        f(path, request.identity, Option(request.queryString))
      }

      resource.fold(NotFoundResult(request.uri)) { res =>
        transformResource(res) match {
          case Failure(err) => HandleExceptions(err)
          case Success(res) => Ok( RenderSingle(res) )
        }
      }
    }
  }

  private[controllers] def AuthorizedResourceList(path: ResourcePath, action: String, qs: QueryString)
      (implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,_]): Result = {

    log.debug(s"AuthorizedResourceList(${path.path}, $action)")

    val rss = lookupSeqs.get(path.targetTypeId).fold {
      log.debug(s"Executing standard lookup function for resource list: ${path.path}")
      Resource.listFromPath(path.path, qs)
    }{ f => f(path, request.identity, request.queryString).toList }

    AuthorizeList(action) { rss.map(transformResource(_).get) }
  }

  private[controllers] def transformResource(res: GestaltResourceInstance)
                                            (implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,_]): Try[GestaltResourceInstance] = {
    transforms.get(res.typeId).fold(Try(res)) {
      f => f(res, request.identity, Option(request.queryString))
    }
  }

  private[controllers] def findResource(path: ResourcePath, account: AuthAccountWithCreds)
      : Option[GestaltResourceInstance] = {

    val resource = lookups.get(path.targetTypeId).fold {
      Resource.fromPath(path.path)
    }{ f =>
      log.debug(s"Found custom lookup function for Resource.")
      f(path, account, None)
    }

    resource map { res =>
      transforms.get(res.typeId).fold(res){ f =>
        log.debug(s"Found custom transformation function for Resource: ${res.id}")
        f(res, account, None).get
      }
    }

  }  


//  def getActionUi(fqon: String, actionId: UUID) = Audited(fqon) { implicit request =>
//
//    log.debug("Finding Action...")
//
//    ResourceFactory.findById(ResourceIds.ProviderAction, actionId).fold {
//      this.ResourceNotFound(ResourceIds.ProviderAction, actionId)
//
//    }{ act =>
//
//      log.debug("Finding target resource...")
//
//      val resource = for {
//        a   <- request.queryString.get("resource") orElse {
//          throw new BadRequestException("You must supply a `?resource={id}` query parameter")
//        }
//        b   <- a.headOption
//        id  = parseUUID(b) getOrElse {
//            throw new BadRequestException(s"Invalid resource UUID. found: '$b'")
//        }
//        res = ResourceFactory.findById(id) getOrElse {
//          throw new ResourceNotFoundException(s"Resource with ID '$id' not found.")
//        }
//      } yield res
//
//      val metaAddress = META_URL
//
//      log.debug("using META_ADDRESS : " + metaAddress)
//
//      val output = Assembler.assemble(
//          fqon,
//          metaAddress,
//          act,
//          resource.get,
//          request.identity,
//          request.queryString)
//      Ok(output).as("text/html")
//    }
//  }
  
  def getResourceContext(fqon: String, path: String) = Audited(fqon) { implicit request =>
    Ok(Json.toJson(mkPath2(fqon, path)))
  }
  
  import com.galacticfog.gestalt.meta.genericactions._
  
  
  def findActionsInScope(org: UUID, target: UUID, prefixFilter: Seq[String] = Seq()): Seq[JsObject] = {

    def makeActionUrl(provider: GestaltResourceInstance, action: String) = {
      val act = action.drop(action.indexOf(""".""")+1)
      val org = ResourceFactory.findById(ResourceIds.Org, provider.orgId) getOrElse {
        throw new RuntimeException(s"Could not find 'provider.org' with ID ${provider.orgId}")
      }
      val fqon = org.properties.get("fqon")
      "/%s/providers/%s?action=%s".format(fqon, provider.id, act)
    }
    
    /*
     * 
     */
    def toOutputJson(
        provider: GestaltResourceInstance, 
        endpoint: GestaltEndpoint, 
        function: GestaltFunction,
        response: FunctionResponse) = {
      Json.obj(
          "url"          -> makeActionUrl(provider, function.name),
          "action"       -> function.name,
          "display_name" -> function.display_name,
          "method"       -> "POST",
          "code"         -> response.code,
          "content_type" -> response.content_type,
          "render"       -> response.gestalt_ui.get.render,
          "locations"    -> response.gestalt_ui.get.locations)
    }
    
    /*
     * Determine if given function contains a UI action with a location
     * matching a filter (gestalt_ui.locations)
     */
    def inFilter(fn: GestaltFunction, filters: Seq[String]): Boolean = {
      if (filters.isEmpty) true
      else {
        val locations = for {
          r <- fn.getUiResponse
          u <- r.gestalt_ui
          locs = u.locations
        } yield locs
        
        locations.get.exists { loc =>
          filters.exists { f => loc.startsWith(f) }
        }
      }
    }
    
    ResourceFactory.findProvidersWithEndpoints(target).flatMap { p =>
      val config = getFunctionConfig(p).get
      config.endpoints.flatMap { ep =>
        val act = ep.getUiActions()
        act.collect { case ac if ac.hasUi && inFilter(ac, prefixFilter) =>
          val uiResponse = ac.post.get.getUiResponse.get
          toOutputJson(p, ep, ac, uiResponse)
        }
      }
    }
  }
  
  
//  def findActionsInScope(org: UUID, target: UUID, prefixFilter: Seq[String] = Seq()): Seq[GestaltResourceInstance] = {
//    log.debug("Looking up applicable actions...")
//
//    val actions = ResourceFactory.findById(target).fold {
//      throw new ResourceNotFoundException(s"Resource with ID '$target' not found.")
//    }{ _ =>
//      val prvs = for {
//        anc <- {
//          /*
//           * This selects the ancestor environment and workspace, as well as all 
//           * ancestor Orgs up to 'root'
//           */
//          val rs = ResourceFactory.findAncestorsOfSubType(ResourceIds.ResourceContainer, target)
//          val root = ResourceFactory.findAllByPropertyValue(ResourceIds.Org, "fqon", "root")
//          rs.reverse ++ root
//        }
//        prv <- {
//          /*
//           * TODO: Type ActionProvider no longer exists, and *any* Provider may now expose
//           * a 'UI action'. If we can't figure out a way to index or 'tag' a provider at a
//           * top-level to indicate that it exposes UI actions, we need to select ALL providers
//           * in context and inspect individually.
//           */
//          val rs = ResourceFactory.findChildrenOfSubType(ResourceIds.Provider, anc.id)
//          rs
//        }
//      } yield prv
//      ResourceFactory.findChildrenOfTypeIn(ResourceIds.ProviderAction, prvs.map(_.id)) distinct
//    }
//    
//    log.debug(s"Found ${actions.size} actions... filtering by prefix-filter : ${prefixFilter}")
//    
//    if (prefixFilter.isEmpty) actions
//    else actions filter { act =>
//      val spec = ProviderActionSpec.fromResource(act)
//      (spec.ui_locations.map(_.name) intersect prefixFilter).nonEmpty
//    }
//  }
  
//  def findActionsInScope(org: UUID, target: UUID, prefixFilter: Seq[String] = Seq()): Seq[GestaltResourceInstance] = {
//    log.debug("Looking up applicable actions...")
//
//    val actions = ResourceFactory.findById(target).fold {
//      throw new ResourceNotFoundException(s"Resource with ID '$target' not found.")
//    }{ _ =>
//      val prvs = for {
//        anc <- {
//          /*
//           * This selects the ancestor environment and workspace, as well as all 
//           * ancestor Orgs up to 'root'
//           */
//          val rs = ResourceFactory.findAncestorsOfSubType(ResourceIds.ResourceContainer, target)
//          val root = ResourceFactory.findAllByPropertyValue(ResourceIds.Org, "fqon", "root")
//          rs.reverse ++ root
//        }
//        prv <- {
//          /*
//           * TODO: Type ActionProvider no longer exists, and *any* Provider may now expose
//           * a 'UI action'. If we can't figure out a way to index or 'tag' a provider at a
//           * top-level to indicate that it exposes UI actions, we need to select ALL providers
//           * in context and inspect individually.
//           */
//          val rs = ResourceFactory.findChildrenOfSubType(ResourceIds.ActionProvider, anc.id)
//          rs
//        }
//      } yield prv
//      ResourceFactory.findChildrenOfTypeIn(ResourceIds.ProviderAction, prvs.map(_.id)) distinct
//    }
//    
//    log.debug(s"Found ${actions.size} actions... filtering by prefix-filter : ${prefixFilter}")
//    
//    if (prefixFilter.isEmpty) actions
//    else actions filter { act =>
//      val spec = ProviderActionSpec.fromResource(act)
//      (spec.ui_locations.map(_.name) intersect prefixFilter).nonEmpty
//    }
//  }

  
  def getResourceActionsOrg(fqon: String) = Audited(fqon) { implicit request =>
    val targetPrefix = request.queryString.get("filter") getOrElse Seq.empty
    val org = fqid(fqon)
//    RenderList {
//      findActionsInScope(org, org, targetPrefix)
//    }
    
    Ok(Json.toJson(findActionsInScope(org, org, targetPrefix)))
  }

  def getResourceActions(fqon: String, target: UUID) = Audited(fqon) { implicit request =>
    val targetPrefix = request.queryString.get("filter") getOrElse Seq.empty
//    RenderList {
//      findActionsInScope(fqid(fqon), target, targetPrefix)
//    }
    
    Ok(Json.toJson(findActionsInScope(fqid(fqon), target, targetPrefix)))
  }
  
  /*
   * This function is needed to keep root from showing up in the output of GET /root/orgs.
   * The query that selects the orgs uses the 'owning org' as a filter. root is the only
   * org that is owned by itself.
   */
  def lookupSeqOrgs(path: ResourcePath, account: AuthAccountWithCreds, qs: QueryString): List[GestaltResourceInstance] = {
    Resource.listFromPath(path.path, qs) filter { o =>
      o.properties.get("fqon") != path.fqon
    }
  }
  
  def lookupEntitlement(path: ResourcePath, account: AuthAccountWithCreds, qs: Option[QueryString]): Option[GestaltResourceInstance] = {
    Resource.toInstance(path) map { transformEntitlement(_, account).get }
  }
  
  def lookupSeqEntitlements(path: ResourcePath, account: AuthAccountWithCreds, qs: QueryString): List[GestaltResourceInstance] = {

    val rs = if (Resource.isTopLevel(path.path)) {
      val org = fqid(Resource.getFqon(path.path))
      ResourceFactory.findChildrenOfType(org, org, ResourceIds.Entitlement)
    } else Resource.listFromPath(path.path, qs)

    if (getExpandParam(qs)) rs map { transformEntitlement(_, account).get } else rs
  }
  
  /*
   * Type-Based Lookup Functions
   */
  def lookupSeqProviders(path: ResourcePath, account: AuthAccountWithCreds, qs: QueryString): List[GestaltResourceInstance] = {
    log.debug(s"lookupSeqProviders(${path.path}, user = ${account.account.id}")

    val parentId = {
      if (path.parentId.isDefined) path.parentId.get
      else orgFqon(path.fqon).fold(FqonNotFound(path.fqon)){ _.id }
    }

    ResourceFactory.findById(parentId).fold {
      throw new ResourceNotFoundException(parentId.toString)
    }{ _ =>
      val rs = ResourceFactory.findAncestorsOfSubType(ResourceIds.Provider, parentId)
      filterProvidersByType(rs, qs) map { res =>

        (maybeInjectActions _ andThen maybeMaskCredentials(Option(qs))) apply res
      }
    }
  }

  def lookupSeqSecrets(path: ResourcePath, account: AuthAccountWithCreds, qs: QueryString): List[GestaltResourceInstance] = {
    log.debug(s"lookupSeqSecrets(${path.path}, user = ${account.account.id}")

    val parentEnvId = if ( !path.parentTypeId.contains(ResourceIds.Environment) ) {
      throw new BadRequestException("Secret listing must have parent type Environment")
    } else {
      path.parentId.getOrElse(throw new BadRequestException("Secret listing missing environment UUID"))
    }

    ResourceFactory.findById(ResourceIds.Environment, parentEnvId).fold {
      throw new ResourceNotFoundException(parentEnvId.toString)
    }{ _ =>
      val rs = ResourceFactory.findChildrenOfType(ResourceIds.Secret, parentEnvId)
      qs.get("providerId").flatMap(_.headOption).map( UUID.fromString(_) ).fold[Seq[GestaltResourceInstance]](rs) {
        queryProviderId => rs.filter { secret =>
          val maybeProviderId = secret.properties.getOrElse(Map.empty).get("provider") map ( Json.parse ) flatMap ( prv => (prv \ "id").asOpt[UUID] )
          maybeProviderId.contains( queryProviderId )
        }
      } toList
    }
  }

  private[controllers] def providerTypeVariance(typeName: String): Variance[UUID] = {
    val typeid = ResourceType.id(typeName)
    val abstractProviders = Seq(
        ResourceIds.CaasProvider,
        ResourceIds.DataProvider,
        ResourceIds.MessageProvider,
        ResourceIds.ExecutorProvider)
    if (abstractProviders.contains(typeid)) CoVariant(typeid)
    else Invariant(typeid)
  }

  def filterProvidersByType(rs: List[GestaltResourceInstance], qs: QueryString) = {

    val allnames = TypeFactory.allProviderNames()
    val prefixes = TypeFactory.typeNamePrefixes(allnames)

    if (qs.get("type").isEmpty) rs
    else {
      val ids: Seq[UUID] = TypeFactory.validTypeFilter(qs("type").head, allnames, prefixes).fold {
        throw new BadRequestException(s"Unknown provider type : '${qs.get("type").head}'")
      }{ typename =>
        log.debug("Filtering providers for type : " + typename)

        val variance = providerTypeVariance(typename)
        findTypesWithVariance(variance) map { _.id }
      }
      rs filter { r =>
        ids.contains(r.typeId) }
    }
  }



  private[controllers] def transformEntitlement(res: GestaltResourceInstance, user: AuthAccountWithCreds, qs: Option[QueryString] = None) = Try {
    val props  = EntitlementProps.make(res)
    val output = props.identities map { ids =>

      // Get expanded resource for each UUID in properties.identities
      val identities = ResourceFactory.findAllIn(ids)

      // Transform identity Seq[UUID] to Seq[ResourceLink] JSON.
      val idJson = Output.renderLinks(identities, /*baseUrl*/None)

      res.copy(properties = {
        Option(res.properties.get ++ Map("identities" -> Json.stringify(idJson)))
      })
    }
    if (output.isDefined) output.get else res
  }

  private[this] def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }

  /**
   * Lookup and inject associated rules into policy.
   */
  private[controllers] def transformPolicy(
      res: GestaltResourceInstance,
      user: AuthAccountWithCreds,
      qs: Option[QueryString] = None) = Try {

    val ruleLinks = ResourceFactory.findChildrenOfSubType(ResourceIds.Rule, res.id) map {  toLink(_, None) }
    upsertProperties(res, "rules" -> Json.stringify(Json.toJson(ruleLinks)))
  }

  /**
    * Lookup and inject container mount info into upstream Volume object
    */
  private[controllers] def embedContainerMountInfo( res: GestaltResourceInstance,
                                                    user: AuthAccountWithCreds,
                                                    qs: Option[QueryString]): GestaltResourceInstance = {
    val containerMount: Option[(ExistingVolumeMountSpec,GestaltResourceInstance)] = {
      val mnts = for {
        env <- ResourceFactory.findParent(res.id).toList
        cntr <- ResourceFactory.findChildrenOfType(ResourceIds.Container, env.id)
        mnt <- Try {
          Json.parse(cntr.properties.getOrElse(Map.empty).getOrElse("volumes", "[]")).as[Seq[ContainerSpec.VolumeMountSpec]]
        } getOrElse Seq.empty collect {
          case ex: ExistingVolumeMountSpec => ex
        }
        if mnt.volume_id == res.id
      } yield mnt -> cntr
      if (mnts.size > 1) log.warn(s"multiple containers believe that they are mounting Volume ${res.id}")
      mnts.headOption
    }
    containerMount match {
      case None =>
        res
      case Some((mnt, container)) =>
        upsertProperties(res,
          "container" -> container.id,
          "mount_path" -> mnt.mount_path
        )
    }
  }

  private[controllers] def embedProvider(res: GestaltResourceInstance, user: AuthAccountWithCreds, qs: Option[QueryString]) = {
    val renderedRes = {
      for {
        ps <- res.properties
        pid = {
          val sid  = (Json.parse(ps("provider")) \ "id").as[String]
          UUID.fromString(sid)
        }
        prv <- ResourceFactory.findById(pid).map(maybeMaskCredentials(qs))
      } yield {
        val renderedPrv = Output.renderInstance(prv)
        upsertProperties(res, "provider" -> Json.stringify(renderedPrv))
      }
    }
    renderedRes.getOrElse(res)
  }

  private[controllers] def maybeMaskCredentials(qs: Option[QueryString])(provider: GestaltResourceInstance) : GestaltResourceInstance = {
    val isShowCredentials = qs.isDefined && qs.get.getOrElse("showcredentials", Seq.empty).headOption.contains("true")
    val isCaaSProvider = ProviderMethods.isCaaSProvider(provider.typeId)

    if (!isShowCredentials && isCaaSProvider) {
      ProviderMethods.maskCredentials(provider)
    } else provider
  }

  private[controllers] def maybeInjectActions(provider: GestaltResourceInstance) : GestaltResourceInstance = {
    if (ProviderMethods.isActionProvider(provider.typeId))
      ProviderMethods.injectProviderActions(provider) else provider
  }

  private[controllers] def embedEndpoints(res: GestaltResourceInstance, user: AuthAccountWithCreds, qs: Option[QueryString]) = {
    val endpoints = {
      val raw = ResourceFactory.findAllByPropertyValue(ResourceIds.ApiEndpoint, "implementation_id", res.id)
      raw.map { ep =>
        transformApiEndpoint(ep, user, None) match {
          case Success(xep) => xep
          case Failure(e) => ep
        }
      }
    }
    val rendered = Json.stringify(Json.toJson(endpoints.map(Output.renderInstance(_))))
    upsertProperties(res, "apiendpoints" -> rendered)
  }

  private[controllers] def embedVolumes(res: GestaltResourceInstance, user: AuthAccountWithCreds, qs: Option[QueryString]) = {
    val volumes = for {
      vol <- res.properties.getOrElse(Map.empty).get("volumes").flatMap(vs => Try(Json.parse(vs).as[Seq[JsObject]]).toOption).getOrElse(Seq.empty)
      vid <- (vol \ "volume_id").asOpt[UUID]
      v <- ResourceFactory.findById(migrations.V13.VOLUME_TYPE_ID, vid)
      vEmbed = embedContainerMountInfo(v, user, qs)
      j = Output.renderInstance(vEmbed).as[JsObject]
    } yield vol ++ Json.obj("volume_resource" -> j)
    upsertProperties(res, "volumes" -> Json.toJson(volumes).toString)
  }

  /**
    * Lookup and inject apiendpoints into upstream object according to implementation_id
    */
  private[controllers] def performOptionalEmbeddings(fns: Map[String, (GestaltResourceInstance, AuthAccountWithCreds, Option[QueryString]) => GestaltResourceInstance],
                                                     res: GestaltResourceInstance,
                                                     user: AuthAccountWithCreds,
                                                     qs: Option[QueryString] = None ) = Try {
    val embeds = for {
      em <- qs.getOrElse(Map.empty).getOrElse("embed", Seq.empty).distinct
      fn <- fns.get(em)
    } yield fn

    embeds.foldLeft(res) {
      case (res, fn) => fn(res, user, qs)
    }
  }
  
  /**
    * Lookup and inject Kong public_url into ApiEndpoint
    */
  private[controllers] def transformApiEndpoint( res: GestaltResourceInstance,
                                                 user: AuthAccountWithCreds,
                                                 qs: Option[QueryString] = None) = Try {

    val maybePublicUrl = GatewayMethods.getPublicUrl(res)

    maybePublicUrl.fold(res) {public_url =>
      upsertProperties(res, "public_url" -> public_url)
    }
  }

  private[controllers] def transformProvider(res: GestaltResourceInstance, user: AuthAccountWithCreds, qs: Option[QueryString] = None) = Try {
    val resJson = Json.toJson(res).as[JsObject]
    val renderedLinks: Seq[JsObject] = (resJson \ "properties" \ "linked_providers").asOpt[Seq[JsObject]].map { _.flatMap {
      js => for {
        id <- (js \ "id").asOpt[UUID]
        lp <- ResourceFactory.findById(id)
      } yield (js ++ Json.obj(
        "typeId" -> lp.typeId,
        "type" -> TypeMethods.typeName(lp.typeId)
      ))
    } } getOrElse Seq.empty
    val newResJson = replaceJsonProps(resJson, replaceJsonPropValue(resJson, "linked_providers", Json.toJson(renderedLinks)))
    newResJson.validate[GestaltResourceInstance].get
  }
  
  /**
   * Build the dynamic 'groups' property on a User instance.
   */
  private[controllers] def transformUser(res: GestaltResourceInstance, user: AuthAccountWithCreds, qs: Option[QueryString] = None) = Try {
    security.getAccountGroups(res.id, user) match {
      case Failure(er) => {
        throw new RuntimeException(Errors.USER_GROUP_LOOKUP_FAILED(res.id, er.getMessage))
      }
      case Success(gs) => {
        val grpIds = gs flatMap { grp => ResourceFactory.findById(ResourceIds.Group, grp.id).map(_.id.toString) } mkString(",")
        val props = res.properties.getOrElse(Map.empty) + ( "users" -> grpIds )
        res.copy(properties = Some(props))
      }
    }
  }
  
  def transformStreamSpec(r: GestaltResourceInstance, user: AuthAccountWithCreds, qs: Option[QueryString] = None): Try[GestaltResourceInstance] = Try {
    log.debug("Entered transformStreamSpec...")
    val streams = lambdaMethods.getLambdaStreams(r, user).get
    val oldprops = r.properties.get
    val newprops = oldprops ++ Map("streams" -> Json.stringify(streams))
    r.copy(properties = Some(newprops))
  }
  
  /**
   * Add users to the Group's properties collection. Users are looked up dynamically
   * in gestalt-security.
   */
  def transformGroup(r: GestaltResourceInstance, user: AuthAccountWithCreds, qs: Option[QueryString] = None): Try[GestaltResourceInstance] = {
    // TODO: there's no check here that the caller is permitted to see the account ids returned by gestalt-security
    // TODO: also, this is using the user credential in the call to gestalt-security, meaning that the user must have appropriate permissions
    // bug discussion: https://gitlab.com/galacticfog/gestalt-meta/issues/247
    security.getGroupAccounts(r.id, user)
      .recover {
        case a403: ForbiddenAPIException =>
          log.warn("user credentials did not have permission to view group membership against gestalt-security, using empty list", a403)
          Seq.empty
        case a404: com.galacticfog.gestalt.security.api.errors.ResourceNotFoundException => {
          log.warn(s"Group ${r.id} was found in Meta but no longer exists in gestalt-security.", a404)
          Seq.empty
        }
      }
      .map { acs =>
        val accIds = acs flatMap { acc => ResourceFactory.findById(ResourceIds.User, acc.id).map(_.id.toString) } mkString(",")
        val outputProps = r.properties.getOrElse(Map.empty) + ( "users" -> accIds )
        r.copy(properties = Some(outputProps))
      }
  }

  
  // --------------------------------------------------------------------------
  // ENVIRONMENT VARIABLES
  // --------------------------------------------------------------------------  
  
  def getEnvVariablesOrgFqon(fqon: String) = Audited(fqon) { implicit request =>
    val org = fqid(fqon)
    Ok(Json.toJson(EnvironmentVars.get(org, org)))
  }
  
  def getEnvVariablesFqon(fqon: String, typeId: String, id: UUID) = Audited(fqon) { implicit request =>
    ResourceFactory.findById(UUID.fromString(typeId), id) match {
      case None => NotFoundResult(request.uri)
      case Some(_) => Ok(Json.toJson(EnvironmentVars.get(fqid(fqon), id)))
    }
  }
  
  /**
   * Get Environment variables for the given lambda.
   * 
   * TODO: This method is currently not authenticated.
   */
  def getTopLevelLambdaEnv(lambdaId: UUID) = Action {
    
    ResourceFactory.findById(lambdaId) match {
      case None => NotFoundResult(s"Lambda with ID '$lambdaId' not found.")
      case Some(lambda) => {
        val rs = ResourceFactory.findEnvironmentVariables(lambdaId)
        
        val all = rs map { case (k,v) =>
          if (v.properties.isEmpty) None
          else v.properties.flatMap(_.get("env")) match {
              case None => None
              case Some(vars) => {
                Option(k -> Json.parse(vars).validate[Map[String,String]].get)
            }
          }
        } filter { _.isDefined } flatMap { v => v }
        
        Ok(Json.toJson(EnvironmentVars.mergeBottomUp(all)))        
      }
    }    
  }
  
  // --------------------------------------------------------------------------
  // GROUPS/USERS
  // -------------------------------------------------------------------------- 
  /**
   * Get the Meta User corresponding with the caller identity.
   */
  def getUserSelf() = Audited() { implicit request =>
    val id = request.identity.account.id
    ResourceFactory.findById(ResourceIds.User, id).fold {
      InternalServerError(Errors.USER_SYNCHRONIZATION(id))
    }{ user => Ok( RenderSingle(user) ) }
  }

  /**
   * Find all members of the given Groups. Performs dynamic lookup in gestalt-security.
   * 
   * GET /{fqon}/groups/{id}/users
   * TODO: This does not perform the group-injection on the users. If expand=true, the
   * groups will NOT be displayed in user.properties.
   */
  def getGroupUsersFqon(fqon: String, group: UUID) = Audited(fqon) { implicit request =>
    security.getGroupAccounts(group, request.identity) match {
      case Failure(er) => HandleExceptions(er)
      case Success(gs) => {
        val userids = gs map { _.id }
        if (userids.isEmpty) Ok(Json.parse("[]")) else {
        handleExpandResourceResult(ResourceFactory.findAllIn(ResourceIds.User, userids),
            request.queryString, Some(META_URL))
        }
      }
    }  
  }
  
  /**
   * Find all Groups the given User is a member of.
   * 
   * GET /{fqon}/users/{id}/groups
   * TODO: This does not perform the user-injection on the groups. If expand=true, the
   * properties collection will NOT display the users in each group.
   */
  def getUserGroupsFqon(fqon: String, user: UUID) = Audited(fqon) { implicit request =>
    security.getAccountGroups(request.identity) match {
      case Failure(err) => HandleExceptions(err)      
      case Success(gs)  => {
        val groupids = gs map { _.id }
        if (groupids.isEmpty) Ok(Json.parse("[]")) else {
          handleExpandResourceResult(ResourceFactory.findAllIn(fqid(fqon), ResourceIds.Group, groupids),
              request.queryString, Some(META_URL))
        }
      }
    }
  }
  
  // --------------------------------------------------------------------------
  // CUSTOM-RESOURCES
  // --------------------------------------------------------------------------
  /* TODO: This function implements `GET /{fqon}/resourcetypes/{type-id}/resources` to return
   * a list of all resources of the give type in the Org. I guess that's useful for custom-resource
   * Get all Resources by Type ID
   */
  def getAllResourcesByTypeFqon(fqon: String, typeId: UUID) = Audited(fqon) { implicit request =>
    Ok(Output.renderLinks(ResourceFactory.findAll(typeId, fqid(fqon))))
  }

  
  def mkPath2(fqon: String, path: String) = {
    
    val rp = new ResourcePath(fqon, path)
    
    def tname(typeId: UUID): String = resourceRestName(typeId).dropRight(1).mkString
    
    val org = {
      Resource.findFqon(rp.fqon) map {
        toLink(_, None) }
    }
    val target = {
      ResourceFactory.findById(rp.targetTypeId, UUID.fromString(rp.targetId.get)) map { toLink(_, None) }
    }
    val parent = {
      if (rp.isSecondLevelResource) {
        val tid = rp.parentTypeId.get
        val pid = rp.parentId.get
        Some((tname(tid) -> ResourceFactory.findById(tid, pid).map { toLink(_, None) }))
      } else  None
    }
    
    Map("org" -> org, tname(rp.targetTypeId) -> target) ++ parent
    
//    Map(ResourceLabel(rp.parentTypeId.get) -> rp.parentId) 
    
  }

  def mapPath(fqon: String, path: String) = Audited(fqon) { implicit request =>
    
    def mkuri(fqon: String, r: GestaltResourceInstance) = {
      "/%s/%s/%s".format(fqon, resourceRestName(r.typeId).get, r.id)
    }
    
    def mkinfo(r: GestaltResourceInstance) = {
      ResourceInfo(r.id, r.name, mkuri(fqon, r))
    }
    
    def resolve(parent: UUID, cmps: List[(UUID,String)], dat: Map[String, ResourceInfo]): Try[Map[String,ResourceInfo]] = Try {
      cmps match {
        case Nil => dat
        case h :: t => {
          ResourceFactory.findChildByName(parent, h._1, h._2).fold {
            throw new ResourceNotFoundException(s"${ResourceLabel(h._1)} with name '${h._2}' not found.")
          }{ res => 
            resolve(res.id, t, dat ++ Map(ResourceLabel(h._1).toLowerCase -> mkinfo(res))).get
          }
        }
      }
    }
    
    val org = Resource.findFqon(fqon) getOrElse {
      throw new InternalErrorException(s"could not locate org with fqon '${fqon}'")
    }
    val keys = List(ResourceIds.Workspace, ResourceIds.Environment)
    val values = path.stripPrefix("/").stripSuffix("/").split("/").toList
    
    resolve(org.id, (keys zip values), Map("org" -> mkinfo(org))) match {
      case Success(m) => Ok(Json.toJson(m))
      case Failure(e) => HandleRepositoryExceptions(e)
    }
  }
  
  

  
  
  def findDataTypes() = Audited() { implicit request =>
    Ok( mapReferenceType(DataType.data) )
  }
  
  def findEnvironmentTypes() = Audited() { implicit request =>
    Ok( mapReferenceType(EnvironmentType.data) )
  }
  
  def findVisibilityTypes() = Audited() { implicit request =>
    Ok( mapReferenceType(VisibilityType.data) )
  }
  
  def findResourceStates() = Audited() { implicit request =>
    Ok( mapReferenceType(ResourceState.data) )
  }
  
  private[controllers] def mapReferenceType(m: Map[String, String]): JsValue = {
    Json.toJson(m.map( v => Json.obj("id" -> v._2, "name" -> v._1) ))
  }
  
  private[this] def standardRequestOptions(
    user: AuthAccountWithCreds,
    parent: UUID,
    resource: GestaltResourceInstance,
    data: Option[Map[String, String]] = None) = {

    RequestOptions(user,
      authTarget = Option(parent),
      policyOwner = Option(parent),
      policyTarget = Option(resource),
      data)
  }

  private[this] def standardRequestOperations(action: String) = {
    List(
      controllers.util.Authorize(action))
  }  
}
