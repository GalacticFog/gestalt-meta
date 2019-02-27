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
import com.galacticfog.gestalt.security.api.errors.ForbiddenAPIException
import com.galacticfog.gestalt.security.play.silhouette.{
  AuthAccountWithCreds, GestaltFrameworkSecurity, GestaltFrameworkSecurityEnvironment}
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

import play.api.mvc.Request
import play.api.mvc.AnyContent

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
    lambdaMethods: LambdaMethods,
    gatewayMethods: GatewayMethods  )
    
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
    ResourceIds.Rule        -> lookupPolicyRules
  )

  def lookupPolicyRules(path: ResourcePath, user: AuthAccountWithCreds, qs: QueryString): Seq[GestaltResourceInstance] ={
    val mapPathData = Resource.mapListPathData(path.path)
    val policy = mapPathData(Resource.ParentId)
    ResourceFactory.findChildrenOfSubType(ResourceIds.Rule, policy)
  }

  def lookupProvider(path: ResourcePath, user: AuthAccountWithCreds, qs: Option[QueryString]): Option[GestaltResourceInstance] = {
    log.debug("Lookup function : lookupProvider(_,_,_)...")

    Resource.toInstance(path) map { res =>
      maybeMaskCredentials(qs)(res)
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
        ResourceFactory.findDescendantsOfType(ResourceIds.Container, provider) ++
          ResourceFactory.findDescendantsOfType(migrations.V33.JOB_TYPE_ID, provider)
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
        ResourceFactory.findById(ResourceIds.Container, container)
          .orElse(ResourceFactory.findById(migrations.V33.JOB_TYPE_ID, container)).fold {
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
      
    // Inject parent link on all resources.
    val parentLink: JsValue = Json.toJson(toLink(parent, Some(META_URL)))
    val newPayload = JsonUtil.upsertProperty(payload.as[JsObject], "parent", parentLink).getOrElse {
      throw new RuntimeException(s"Failed injecting parent link into resource. This is a bug.")
    }
    
    /*
     * Determine if the resource we're creating is backed by a provider.
     */    
    getBackingProviderType(targetTypeId) match {
      case None => {
        log.debug("**Request to create non-provider-backed resource")
        newDefaultResource(org.id, targetTypeId, parent.id, newPayload)(request)
      }
      case Some(backingProviderType) => {
        genericResourceMethods.createProviderBackedResource(
          org = org,
          identity = request.identity,
          body = newPayload,
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
  
  private[controllers] def requireActionParam(request: Request[AnyContent]): String = {
    request.getQueryString("action").getOrElse {
      throwBadRequest("Missing parameter: action")
    }    
  }
  
  private[controllers] def requireTargetId(path: ResourcePath, qs: Map[String,Seq[String]]): Future[UUID] = {
    QueryString.single(qs, "resource_id").fold {
      log.debug("Taking target ID from path.")
      path.targetId match {
        case Some(targetId) => Future.successful(UUID.fromString(targetId))
        case None => Future.failed(
          new ResourceNotFoundException("actions must be performed against a specific resource")
        )
      }      
    }{ rid =>
      log.debug(s"Using target ID from querystring: ${rid}")
      Future.successful(UUID.fromString(rid)) 
    }
  }
  
  
  /**
   * Test that the given Provider is compatible with the given Environment. An
   * exception is thrown if the given Environment is incompatible.
   */
  private[controllers] def assertCompatibleEnvType(provider: GestaltResourceInstance, env: GestaltResourceInstance) {
    
    // These are the values tha Meta will accept for 'environment_type'
    val acceptableTypes = Set("development", "test", "production")
    
    //provider.properties.get.get("environment_types").map { types =>
    provider.properties.get.get("environment_types").foreach { types =>  
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
  
  def genericResourcePost(fqon: String, path: String) = AsyncAuditedAny(fqon) { implicit request =>
    log.debug(s"genericResourcePost(${fqon},${path})")
    
    val rp = new ResourcePath(fqon, path)
    log.debug(rp.info.toString)
    
    if (rp.isList) {
      // create semantics
      val x = for {
        org         <- findOrgOrFail(fqon)
        parent      <- Future.fromTry(findResourceParent(rp, fqon))
        jsonRequest <- fTry(requireJsonRequest(request))
        securedRequest = SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue](request.identity, request.authenticator, jsonRequest)
        resource    <- execGenericCreate(org, rp.targetTypeId, parent, jsonRequest.body)(securedRequest)
      } yield resource
      
      x.map (r => Created(Output.renderInstance(r, Some(META_URL))))
        .recover { case e => HandleExceptions(e) }  
      
    } else { 
      // perform action
      for {
        org      <- findOrgOrFail(fqon)
        action   <- fTry(requireActionParam(request))
        targetId <- requireTargetId(rp, request.queryString)
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
  
  def getResourceContext(fqon: String, path: String) = Audited(fqon) { implicit request =>
    Ok(Json.toJson(mkPath2(fqon, path)))
  }
  
  import com.galacticfog.gestalt.meta.genericactions._
  
  def getResourceActionsOrg(fqon: String) = Audited(fqon) { implicit request =>
    val targetPrefix = request.queryString.get("filter") getOrElse Seq.empty
    val org = fqid(fqon)
    Ok(Json.toJson(findActionsInScope(org, org, targetPrefix)))
  }

  def getResourceActions(fqon: String, target: UUID) = Audited(fqon) { implicit request =>
    val targetPrefix = request.queryString.get("filter") getOrElse Seq.empty
    Ok(Json.toJson(findActionsInScope(fqid(fqon), target, targetPrefix)))
  }
  
  /**
   * 
   */
  def findActionsInScope(org: UUID, target: UUID, prefixFilter: Seq[String] = Seq()): Seq[JsObject] = {
    log.debug("findActionsInScope()")
    
    def makeActionUrl(provider: GestaltResourceInstance, action: String) = {
      val act = {
        val cmps = action.split("""\.""")
        if (cmps(0).trim == "streamspec") {
          action
        } else {
          action.drop(action.indexOf(""".""")+1)  
        }
        //action.drop(action.indexOf(""".""")+1)
      }
      val org = ResourceFactory.findById(ResourceIds.Org, provider.orgId) getOrElse {
        throw new RuntimeException(s"Could not find 'provider.org' with ID ${provider.orgId}")
      }
      log.debug("Using action.string : " + act)
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
        response: FunctionResponse,
        params:   Map[String,Option[String]]) = {
      
      val icon = (for {
        ui <- response.gestalt_ui
        ic <- ui.icon
        sv <- ic.svg
      } yield sv)
      
      val url = {
        if (endpoint.kind.endsWith("-external")) endpoint.url
        else makeActionUrl(provider, function.name)
      }
        
      Json.obj(
        "url"          -> url, //makeActionUrl(provider, function.name),
        "action"       -> function.name,
        "display_name" -> function.display_name,
        "method"       -> "POST",
        "code"         -> response.code,
        "content_type" -> response.content_type,
        "render"       -> response.gestalt_ui.get.render,
        "locations"    -> response.gestalt_ui.get.locations,
        "params"       -> Json.toJson(params),
        "icon"         -> icon
      )
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
    
    def reWriteParams2(ps: Seq[RequestQueryParameter]): Map[String,Option[String]] = {
      ps.map(p => (p.name -> p.value)).toMap  
    }

    ResourceFactory.findProvidersWithEndpoints(target).flatMap { p =>
      getFunctionConfig(p).fold(Seq.empty[JsObject]) { config =>
        config.endpoints.flatMap { ep =>
          val act = ep.getUiActions()
          act.collect { case ac if ac.hasUi && inFilter(ac, prefixFilter) =>
            val uiResponse = ac.post.get.getUiResponse.get

            //
            // This finds any query params where .value isDefined
            //
            val params = ac.post.fold(Seq.empty[RequestQueryParameter]){ ps =>
              ps.query_parameters.getOrElse(Seq.empty[RequestQueryParameter]).
                    filter(_.value.isDefined)
            }

            // Now we translate those query params to the JSON output format.
            val y = reWriteParams2(params)
            toOutputJson(p, ep, ac, uiResponse, y)
          }
        }
      }
    }
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

        //(maybeMaskCredentials(Option(qs))) apply res
        maybeMaskCredentials(Option(qs))(res)
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

    val maybePublicUrl = gatewayMethods.getPublicUrl(res)

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
    val streams = Await.result(lambdaMethods.getLambdaStreams(r, user), 5 .seconds)
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
  
  import migrations.V30._
  
  def renderProfileJson(profile: GestaltResourceInstance, favs: Seq[ResourceFavorite]): JsValue = {
    val pjson = Output.renderInstance(profile).as[JsObject]
    JsonUtil.withJsonPropValue(pjson, "resource_favorites", Json.toJson(favs))
  }
  
  /*
   * TODO: Currently this just returns the first UserProfile in the list of
   * all UserProfiles the user has. This is fine as we'll only support a single
   * profile initially...but we'll need a method of tagging a given profile as 'default'
   */
  def findDefaultUserProfile(userId: UUID): Option[GestaltResourceInstance] = {
    ResourceFactory.findChildrenOfType(USERPROFILE_TYPE_ID, userId).headOption
  }
  
  def findUserProfileSelfDefault() = Audited() { implicit request =>
    val user = request.identity.account.id
    findDefaultUserProfile(user) match {
      case None => NoContent
      case Some(profile) => findUserProfileCommon(user, profile.id)
    }
  }
  
  def addFavoriteSelfDefault() = AsyncAudited() { implicit request =>
    val user = request.identity.account.id
    findDefaultUserProfile(user) match {
      case None => HandleExceptionsAsync(new ResourceNotFoundException(s"User '${user}' has not profiles defined."))
      case Some(profile) => addFavoriteCommon(user, profile.id)
    }
  }
  
  def deleteFavoriteSelfDefault() = Audited() { implicit request =>
    val user = request.identity.account.id
    findDefaultUserProfile(user) match {
      case None => NoContent
      case Some(profile) => deleteFavoriteCommon(user, profile.id, request.queryString)
    }
  }
  
  /*
   * POST /users/self/userprofiles
   */
  def postUserProfileSelf() = AsyncAudited() { implicit request =>
    postUserProfileCommon(request.identity.account.id)
  }
  
  /*
   * POST /top/users/{userid}/userprofiles
   */
  def postUserProfile(userId: UUID) = AsyncAudited() { implicit request =>
    postUserProfileCommon(userId)
  }
  
  def postUserProfileCommon(userId: UUID)(implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]) = {
    val favorites = for {
      user <- Try(ResourceFactory.findById(ResourceIds.User, userId).getOrElse {
        throw new ResourceNotFoundException(s"User with ID '${userId}' not found.")
      })
      org <- Try(ResourceFactory.findById(ResourceIds.Org, user.orgId).getOrElse {
        throw new ResourceNotFoundException(s"Org with ID '${user.orgId}' not found.")
      })
      favs <- UserProfileMethods.readProfileFavorites(request.body)
    } yield (org, user, favs)
    
    favorites match {
      case Failure(e) => HandleExceptionsAsync(e)
      case Success((org, user, favorites)) => {
        execGenericCreate(org, USERPROFILE_TYPE_ID, user, request.body).map { profile =>
          Created(RenderSingle(UserProfile.updateFavoritesProperty(profile, favorites)))
        }.recover {
          case e: Throwable => HandleExceptions(e)
        }
      }
    }    
  }
  
  /*
   * GET /users/self/userprofiles/{id}
   */
  def findUserProfileSelf(profileId: UUID) = Audited() { implicit request =>
    findUserProfileCommon(request.identity.account.id, profileId)
  }
  
  /*
   * GET /top/users/{userid}/userprofiles/{id}
   */
  def findUserProfile(user: UUID, profileId: UUID) = Audited() { implicit request =>
    findUserProfileCommon(user, profileId)
  }
  
  def findUserProfileCommon(userId: UUID, profileId: UUID)(implicit request: SecuredRequest[_,_]): Result = {
    ResourceFactory.findChildOfType(USERPROFILE_TYPE_ID, userId, profileId).map { profile =>
      renderProfileJson(profile, UserProfile.getFavorites(profile))
    } match {
      case None => NotFoundResult(request.uri)
      case Some(profile) => Ok(profile)
    }
  }
  
  /*
   * POST /users/self/userprofiles/{id}/favorites
   */
  def addFavoriteSelf(profileId: UUID) = AsyncAudited() { implicit request =>
    addFavoriteCommon(request.identity.account.id, profileId)  
  }
  
  /*
   * POST /top/users/{userid}/userprofiles/{id}/favorites
   */
  def addFavorite(userId: UUID, profileId: UUID) = AsyncAudited() { implicit request =>
    addFavoriteCommon(userId, profileId)
  }  
  /* POST
   * {
   *   "id": <resource-uuid>,
   *   "nickname": "My Resource"
   * } 
   */
  def addFavoriteCommon(userId: UUID, profileId: UUID)(implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]): Future[Result] = {
    Future {
      (for {
        p <- UserProfileMethods.addFavorite(userId, profileId, request.body)
        updated <- ResourceFactory.update(p, request.identity.account.id)
      } yield updated) match {
        case Failure(e) => HandleExceptions(e)
        case Success(updated) => Ok(RenderSingle(updated))
      }
    }.recover {
      case e: Throwable => HandleExceptions(e)
    }    
  }
  
  import com.galacticfog.gestalt.data._
  
  def deleteUserProfileSelf(profileId: UUID) = Audited() { implicit request =>
    deleteUserProfileCommon(request.identity.account.id, profileId)  
  }
  
  def deleteUserProfileCommon(userId: UUID, profileId: UUID)(implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,_]): Result = {
    ResourceFactory.findChildOfType(USERPROFILE_TYPE_ID, userId, profileId) match {
      case None => NoContent
      case Some(profile) => {
        val manager = new HardDeleteInstanceManager[AuthAccountWithCreds](
            Map.empty[UUID,((Instance, AuthAccountWithCreds) => Try[Unit])])
        manager.delete(profile, request.identity, force = true) match {
          case Failure(e) => HandleExceptions(e)
          case Success(_) => NoContent
        }
      }
    }
  }
  
  /*
   * DELETE /users/self/userprofiles/:id/favorites?id={resource-uuid}
   */
  def deleteFavoriteSelf(profileId: UUID) = Audited() { implicit request =>
    deleteFavoriteCommon(request.identity.account.id, profileId, request.queryString)
  }
  
  /*
   * DELETE /top/users/{userid}/userprofiles/{id}/favorites?id={resource-uuid}
   */
  def deleteFavorite(userId: UUID, profileId: UUID) = Audited() { implicit request =>
    deleteFavoriteCommon(userId, profileId, request.queryString)
  }
  
  def deleteFavoriteCommon(userId: UUID, profileId: UUID, qs: Map[String,Seq[String]])(implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,_]): Result = {
    (for {
      target  <- Try(QueryString.single(qs, "id").getOrElse {
        throw new BadRequestException(s"Must provide query-param 'id' identifying the favorite to remove.")
      })
      profile <- UserProfileMethods.deleteFavorite(userId, profileId, UUID.fromString(target))
      updated <- ResourceFactory.update(profile, request.identity.account.id) 
    } yield updated) match {
      case Failure(e) => HandleExceptions(e)
      case Success(updated) => Ok(RenderSingle(updated))
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
