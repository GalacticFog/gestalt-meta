package controllers


import java.util.UUID

import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.data.{ResourceFactory, TypeFactory, string2uuid, uuid, uuid2string}
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceInfo, ResourceLabel, resourceInfoFormat}
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.api.errors.ForbiddenAPIException
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util.JsonUtil._
import controllers.util._
import javax.inject.Singleton
import play.api.i18n.MessagesApi
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.mvc.{Action, Result}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

@Singleton
class ResourceController @Inject()( 
    messagesApi: MessagesApi,
    env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
    security: Security,
    containerService: ContainerService,
    genericResourceMethods: GenericResourceMethods )
    
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  type TransformFunction = (GestaltResourceInstance, AuthAccountWithCreds, Option[Map[String, Seq[String]]]) => Try[GestaltResourceInstance]
  type FilterFunction    = ((Seq[ResourceLike], Map[String, Seq[String]]) => Seq[ResourceLike])
  
  type Lookup    = (ResourcePath, AuthAccountWithCreds, Option[Map[String, Seq[String]]]) => Option[GestaltResourceInstance]
  type LookupSeq = (ResourcePath, AuthAccountWithCreds, Map[String, Seq[String]]) => Seq[GestaltResourceInstance]
  
  
  private[controllers] val transforms: Map[UUID, TransformFunction] = Map(
      ResourceIds.Group  -> transformGroup,
      ResourceIds.User   -> transformUser,
      ResourceIds.Policy -> transformPolicy,
      ResourceIds.ApiEndpoint -> transformApiEndpoint,
      ResourceIds.Lambda -> embedApiEndpoints,
      ResourceIds.Container -> embedApiEndpoints
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
  
  def lookupProviderActions(path: ResourcePath, user: AuthAccountWithCreds, qs: Map[String, Seq[String]]): Seq[GestaltResourceInstance] ={
    val mapPathData = Resource.mapListPathData(path.path)
    val parent = mapPathData(Resource.ParentId)
    
    if (qs.contains("q") && (qs("q")(0).toLowerCase == "entitlements")) {
      ResourceFactory.rollupActionEntitlements(parent)
    } else {
      ResourceFactory.findChildrenOfType(ResourceIds.ProviderAction, UUID.fromString(parent))
    }
  }
  
  def lookupPolicyRules(path: ResourcePath, user: AuthAccountWithCreds, qs: Map[String, Seq[String]]): Seq[GestaltResourceInstance] ={
    val mapPathData = Resource.mapListPathData(path.path)
    val policy = mapPathData(Resource.ParentId)
    ResourceFactory.findChildrenOfSubType(ResourceIds.Rule, policy)
  }
  
  def lookupProvider(path: ResourcePath, user: AuthAccountWithCreds, qs: Option[Map[String, Seq[String]]]): Option[GestaltResourceInstance] = {
    log.debug("Lookup function : lookupProvider(_,_,_)...")
    
    Resource.toInstance(path) map { res =>
      // Inject actions if this is an ActionProvider
      if (ProviderMethods.isActionProvider(res.typeId)) 
        ProviderMethods.injectProviderActions(res) else res 
    }
  }

  def lookupContainer(path: ResourcePath, user: AuthAccountWithCreds, qs: Option[Map[String, Seq[String]]]): Option[GestaltResourceInstance] = {
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
  
  def lookupContainers(path: ResourcePath, account: AuthAccountWithCreds, qs: Map[String, Seq[String]]): Seq[GestaltResourceInstance] = {
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

  private[this] def fTry[T](t: => T): Future[T] =
    Future.fromTry(Try{t})

  private[this] def findOrgOrFail(fqon: String): Future[GestaltResourceInstance] =
    fTry(orgFqon(fqon) getOrElse {
      throw new InternalErrorException("could not locate org resource after authentication")
    })

    
  def findResourceGlobal(typeName: String, id: UUID) = Audited() { implicit request =>
    GlobalResource.getResource(typeName, id) match {
      case Failure(e) => HandleExceptions(e)
      case Success(res) => Ok(RenderSingle(res))
    }
  }
  
  
  def genericResourcePost(fqon: String, path: String) = AsyncAuditedAny(fqon) { implicit request =>
    log.debug(s"genericResourcePost(${fqon},${path})")
    val rp = new ResourcePath(fqon, path)
    log.debug(rp.info.toString)

    if (rp.isList) {
      // create semantics

      val tryParent = ResourceFactory.findById(
        typeId = rp.parentTypeId getOrElse ResourceIds.Org,
        id = rp.parentId getOrElse fqid(fqon)
      ) match {
        case Some(parent) => Success(parent)
        case None => Failure(new ConflictException("parent not specified/does not exist/does match type"))
      }

      for {
        org <- findOrgOrFail(fqon)
        parent <- Future.fromTry(tryParent)
        jsonRequest <- fTry(request.map(_.asJson.getOrElse(throw new UnsupportedMediaTypeException("Expecting text/json or application/json"))))
        jsonSecuredRequest = SecuredRequest[JsValue](request.identity, request.authenticator, jsonRequest)
        /*
         * Determine if the resource we're creating is backed by a provider.
         */
        result <- getBackingProviderType(rp.targetTypeId) match {
          case None =>
            log.debug("request to create non-provider-backed resource")
            newDefaultResourceResult(org.id, rp.targetTypeId, parent.id, jsonRequest.body)(jsonSecuredRequest)

          case Some(backingProviderType) =>
            log.debug(s"request to create provider-backed resource of type ${ResourceLabel(backingProviderType)}")
            val result = genericResourceMethods.createProviderBackedResource(
              org = org,
              identity = request.identity,
              body = jsonRequest.body,
              parent = parent,
              resourceType = rp.targetTypeId,
              providerType = backingProviderType,
              actionVerb = jsonRequest.getQueryString("action").getOrElse("create")
            )
            result
              .map (r => Created(Output.renderInstance(r, Some(META_URL))))
              .recover { case e => HandleExceptions(e) }
        }
      } yield result
      
    } else {
      
      // perform action
      for {
        org <- findOrgOrFail(fqon)
        
        /*
         *  Lookup action to be performed
         */
        action <- fTry{
          request.getQueryString("action") getOrElse {throwBadRequest("Missing parameter: action")}
        }
        
        /*
         * Get resource target ID from request URL
         */
        targetId <- rp.targetId match {
          case Some(targetId) => Future.successful(UUID.fromString(targetId))
          case None => Future.failed(new ResourceNotFoundException("actions must be performed against a specific resource"))
        }
        
        /*
         * 
         */
        result <- getBackingProviderType(rp.targetTypeId) match {
          case None => 
            Future.successful(BadRequestResult("actions can only be performed against provider-backed resources"))
          case Some(backingProviderType) => 
            genericResourceMethods.performProviderBackedAction(
              org = org,
              identity = request.identity,
              body = request.body,
              resourceType = rp.targetTypeId,
              providerType = backingProviderType,
              actionVerb = action,
              resourceId = targetId)
        }
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
      (implicit request: SecuredRequest[_]): Result = {
    
    log.debug(s"Authorizing lookup : user=${request.identity.account.id}, $action")
    
    Authorize(path.targetId.get, action) {
      
      val resource = lookups.get(path.targetTypeId).fold {
        Resource.toInstance(path)
      }{ f=>
        log.debug(s"Found custom lookup function for Resource. Executing...")
        f(path, request.identity, Option(request.queryString))
      }

      resource.fold(NotFoundResult(request.uri)) { res =>
        transforms.get(res.typeId).fold {
          Ok( RenderSingle(res) )
        }{ f =>

          f(res, request.identity, Option(request.queryString)) match {
            case Failure(err) => HandleExceptions(err)
            case Success(res) => Ok( RenderSingle(res) )
          }
        }          
      }
    }
  }
  
  private[controllers] def AuthorizedResourceList(path: ResourcePath, action: String, qs: Map[String, Seq[String]])
      (implicit request: SecuredRequest[_]): Result = {

    log.debug(s"AuthorizedResourceList(${path.path}, $action)")

    val rss = lookupSeqs.get(path.targetTypeId).fold {
      log.debug(s"Executing standard lookup function for resource list: ${path.path}")
      Resource.listFromPath(path.path, qs)
    }{ f => f(path, request.identity, request.queryString).toList }
    
    AuthorizeList(action) {
      transforms.get(path.targetTypeId).fold {
        rss
        }{ f =>
        rss map { f(_, request.identity, Option(request.queryString)).get }
      }
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
  
  
  import com.galacticfog.gestalt.data.parseUUID
  import com.galacticfog.gestalt.meta.providers._
  

  def getActionUi(fqon: String, actionId: UUID) = Audited(fqon) { implicit request =>

    log.debug("Finding Action...")
    
    ResourceFactory.findById(ResourceIds.ProviderAction, actionId).fold {
      this.ResourceNotFound(ResourceIds.ProviderAction, actionId)
      
    }{ act =>
      
      log.debug("Finding target resource...")
      
      val resource = for {
        a   <- request.queryString.get("resource") orElse {
          throw new BadRequestException("You must supply a `?resource={id}` query parameter")
        }
        b   <- a.headOption
        id  = parseUUID(b) getOrElse {
            throw new BadRequestException(s"Invalid resource UUID. found: '$b'")
        }
        res = ResourceFactory.findById(id) getOrElse {
          throw new ResourceNotFoundException(s"Resource with ID '$id' not found.")
        }
      } yield res
      
      import com.galacticfog.gestalt.meta.providers.ui._

      val output = Assembler.assemble(
          fqon,
          META_URL,
          act, resource.get, request.identity)
      Ok(output).as("text/html")
    }
  }
  
  def getResourceContext(fqon: String, path: String) = Audited(fqon) { implicit request =>
    Ok(Json.toJson(mkPath2(fqon, path)))
  }

  
  def findActionsInScope(org: UUID, target: UUID, prefixFilter: Seq[String] = Seq()): Seq[GestaltResourceInstance] = {
    log.debug("Looking up applicable actions...")
    
    val actions = ResourceFactory.findById(target).fold {
      throw new ResourceNotFoundException(s"Resource with ID '$target' not found.")
    }{ _ => 
      val rs = ResourceFactory.findAncestorsOfSubType(ResourceIds.ActionProvider, target) 
      rs flatMap { p => 
        ResourceFactory.findChildrenOfType(org, p.id, ResourceIds.ProviderAction) 
      }
    }
    
    log.debug("Checking for prefix-filter...")
    log.debug("Prefix-Filter : " + prefixFilter)
    
    if (prefixFilter.isEmpty) actions
    else actions filter { act =>
      val spec = ProviderActionSpec.fromResource(act)
      //(spec.ui_locations.map(_.name.takeWhile { c => c != '.' }) intersect prefixFilter).nonEmpty
      (spec.ui_locations.map(_.name) intersect prefixFilter).nonEmpty
    }
  }

  def getResourceActionsOrg(fqon: String) = Audited(fqon) { implicit request =>
    val targetPrefix = request.queryString.get("filter") getOrElse Seq.empty
    val org = fqid(fqon)
    RenderList {
      findActionsInScope(org, org, targetPrefix)
    }
  }
  
  def getResourceActions(fqon: String, target: UUID) = Audited(fqon) { implicit request =>
    val targetPrefix = request.queryString.get("filter") getOrElse Seq.empty
    RenderList {
      findActionsInScope(fqid(fqon), target, targetPrefix)
    }
  }
  
  /*
   * This function is needed to keep root from showing up in the output of GET /root/orgs.
   * The query that selects the orgs uses the 'owning org' as a filter. root is the only
   * org that is owned by itself.
   */
  def lookupSeqOrgs(path: ResourcePath, account: AuthAccountWithCreds, qs: Map[String, Seq[String]]): List[GestaltResourceInstance] = {
    Resource.listFromPath(path.path, qs) filter { o =>
      o.properties.get("fqon") != path.fqon  
    }
  }
  
  def lookupEntitlement(path: ResourcePath, account: AuthAccountWithCreds, qs: Option[Map[String, Seq[String]]]): Option[GestaltResourceInstance] = {
    Resource.toInstance(path) map { transformEntitlement(_, account).get }
  }
  
  def lookupSeqEntitlements(path: ResourcePath, account: AuthAccountWithCreds, qs: Map[String, Seq[String]]): List[GestaltResourceInstance] = {
    
    val rs = if (Resource.isTopLevel(path.path)) {
      val org = fqid(Resource.getFqon(path.path))
      ResourceFactory.findChildrenOfType(org, org, ResourceIds.Entitlement)
    } else Resource.listFromPath(path.path, qs)

    if (getExpandParam(qs)) rs map { transformEntitlement(_, account).get } else rs
  }
  
  /*
   * Type-Based Lookup Functions
   */ 
  def lookupSeqProviders(path: ResourcePath, account: AuthAccountWithCreds, qs: Map[String, Seq[String]]): List[GestaltResourceInstance] = {
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
        
        // Inject actions if this is an ActionProvider
        if (ProviderMethods.isActionProvider(res.typeId)) 
          ProviderMethods.injectProviderActions(res) else res
      }
    }
  }

  def lookupSeqSecrets(path: ResourcePath, account: AuthAccountWithCreds, qs: Map[String, Seq[String]]): List[GestaltResourceInstance] = {
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

  import com.galacticfog.gestalt.data.ResourceFactory.findTypesWithVariance
  import com.galacticfog.gestalt.data.{CoVariant, Invariant, ResourceType, Variance}
  
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
  
  def filterProvidersByType(rs: List[GestaltResourceInstance], qs: Map[String,Seq[String]]) = {

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

  import com.galacticfog.gestalt.meta.auth._

  private[controllers] def transformEntitlement(res: GestaltResourceInstance, user: AuthAccountWithCreds, qs: Option[Map[String, Seq[String]]] = None) = Try {
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
      qs: Option[Map[String, Seq[String]]] = None) = Try {
    
    val ruleLinks = ResourceFactory.findChildrenOfSubType(ResourceIds.Rule, res.id) map {  toLink(_, None) }
    upsertProperties(res, "rules" -> Json.stringify(Json.toJson(ruleLinks)))
  }

  /**
    * Lookup and inject apiendpoints into upstream object according to implementation_id
    */
  private[controllers] def embedApiEndpoints( res: GestaltResourceInstance,
                                              user: AuthAccountWithCreds,
                                              qs: Option[Map[String, Seq[String]]] = None) = Try {
    val embedEndpoints = for {
      qs <- qs
      embed <- qs.get("embed")
    } yield embed.contains("apiendpoints")

    if (embedEndpoints.contains(true)) {
      val endpoints = {
        val raw = ResourceFactory.findAllByPropertyValue(ResourceIds.ApiEndpoint, "implementation_id", res.id)
        raw.map {
          ep => transformApiEndpoint(ep, user, None) match {
            case Success(xep) => xep
            case Failure(e) =>   ep
          }
        }
      }
      val rendered = Json.stringify(Json.toJson(endpoints.map(Output.renderInstance(_))))
      upsertProperties(res, "apiendpoints" -> rendered)
    } else {
      res
    }
  }

  /**
    * Lookup and inject Kong public_url into ApiEndpoint
    */
  private[controllers] def transformApiEndpoint( res: GestaltResourceInstance,
                                                 user: AuthAccountWithCreds,
                                                 qs: Option[Map[String, Seq[String]]] = None) = Try {

    val maybePublicUrl = GatewayMethods.getPublicUrl(res)

    maybePublicUrl.fold(res) {public_url =>
      upsertProperties(res, "public_url" -> public_url)
    }
  }

  private[controllers] def transformProvider(res: GestaltResourceInstance, user: AuthAccountWithCreds, qs: Option[Map[String, Seq[String]]] = None) = Try {
    val resJson = Json.toJson(res).as[JsObject]
    val renderedLinks: Seq[JsObject] = (resJson \ "properties" \ "linked_providers").asOpt[Seq[JsObject]].map { _.flatMap {
      js => for {
        id <- (js \ "id").asOpt[UUID]
        lp <- ResourceFactory.findById(id)
      } yield (js ++ Json.obj(
        "typeId" -> lp.typeId,
        "type" -> sdk.ResourceName(lp.typeId)
      ))
    } } getOrElse Seq.empty
    val newResJson = replaceJsonProps(resJson, replaceJsonPropValue(resJson, "linked_providers", Json.toJson(renderedLinks)))
    newResJson.validate[GestaltResourceInstance].get
  }
  
  /**
   * Build the dynamic 'groups' property on a User instance.
   */
  private[controllers] def transformUser(res: GestaltResourceInstance, user: AuthAccountWithCreds, qs: Option[Map[String, Seq[String]]] = None) = Try {
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

  /**
   * Add users to the Group's properties collection. Users are looked up dynamically
   * in gestalt-security.
   */
  def transformGroup(r: GestaltResourceInstance, user: AuthAccountWithCreds, qs: Option[Map[String, Seq[String]]] = None): Try[GestaltResourceInstance] = {
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

  import com.galacticfog.gestalt.meta.api.output.toLink
  
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
