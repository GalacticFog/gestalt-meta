package controllers


import java.util.UUID

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.TypeFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.models.ResourceLike
import com.galacticfog.gestalt.data.string2uuid
import com.galacticfog.gestalt.data.uuid
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.ResourceInfo
import com.galacticfog.gestalt.meta.api.sdk.ResourceLabel
import com.galacticfog.gestalt.meta.api.sdk.Resources
import com.galacticfog.gestalt.meta.api.sdk.resourceInfoFormat
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import controllers.util.JsonUtil._
import play.api.i18n.MessagesApi
import play.api.libs.json.{JsObject, JsString, Json}
import play.api.mvc.{Action, Result}
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.language.postfixOps
import javax.inject.Singleton

import com.galacticfog.gestalt.security.api.errors.ForbiddenAPIException


@Singleton
class ResourceController @Inject()( 
    messagesApi: MessagesApi,
    env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
    security: Security,
    containerService: ContainerService)
    
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  type TransformFunction = (GestaltResourceInstance, AuthAccountWithCreds, Option[QueryString]) => Try[GestaltResourceInstance]
  type FilterFunction    = ((Seq[ResourceLike], QueryString) => Seq[ResourceLike])
  
  type Lookup    = (ResourcePath, AuthAccountWithCreds, Option[QueryString]) => Option[GestaltResourceInstance]
  type LookupSeq = (ResourcePath, AuthAccountWithCreds, QueryString) => Seq[GestaltResourceInstance]
  
  
  private[controllers] val transforms: Map[UUID, TransformFunction] = Map(
      ResourceIds.Group  -> transformGroup,
      ResourceIds.User   -> transformUser,
      ResourceIds.Lambda -> transformLambda,
      ResourceIds.Policy -> transformPolicy /*,
      ResourceIds.Provider -> transformProvider */
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
      // Inject actions if this is an ActionProvider
      if (ProviderMethods.isActionProvider(res.typeId)) 
        ProviderMethods.injectProviderActions(res) else res 
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
    } else Resource.listFromPath(path.path)
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
  
  /**
   * Get a Resource or list of Resources by path.
   */
  def getResources(fqon: String, path: String) = Audited(fqon) { implicit request =>
    val rp = new ResourcePath(fqon, path)

    val action = actionInfo(rp.targetTypeId).prefix + ".view"
    
    log.trace(s"getResources(_, $path)")
    log.debug("Action : " + action)

    if (rp.isList) AuthorizedResourceList(rp, action) 
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
  
  private[controllers] def AuthorizedResourceList(path: ResourcePath, action: String)
      (implicit request: SecuredRequest[_]): Result = {

    log.debug(s"AuthorizedResourceList(${path.path}, $action)")

    val rss = lookupSeqs.get(path.targetTypeId).fold {
      log.debug(s"Executing standard lookup function for resource list: ${path.path}")
      Resource.listFromPath(path.path)
    }{ f => f(path, request.identity, request.queryString).toList }
    
    AuthorizeList(action) {
      transforms.get(path.targetTypeId).fold(rss) { f =>
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
  
  
  import com.galacticfog.gestalt.meta.providers._
  import com.galacticfog.gestalt.meta.providers.ui._
  import com.galacticfog.gestalt.data.parseUUID  
  

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

      val output = assembleActionUi(act, resource.get, request.identity)
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
  def lookupSeqOrgs(path: ResourcePath, account: AuthAccountWithCreds, qs: QueryString): List[GestaltResourceInstance] = {
    Resource.listFromPath(path.path) filter { o =>
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
    } else Resource.listFromPath(path.path)

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
        
        // Inject actions if this is an ActionProvider
        if (ProviderMethods.isActionProvider(res.typeId)) 
          ProviderMethods.injectProviderActions(res) else res
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

  import com.galacticfog.gestalt.data.{Variance, Invariant, CoVariant}
  import com.galacticfog.gestalt.data.ResourceType
  import com.galacticfog.gestalt.data.ResourceFactory.findTypesWithVariance 
  
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
  
  /**
   * Lookup and inject associated rules into policy.
   */
  private[controllers] def transformPolicy(
      res: GestaltResourceInstance, 
      user: AuthAccountWithCreds, 
      qs: Option[QueryString] = None) = Try {
    
    def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
      resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
    }
    val ruleLinks = ResourceFactory.findChildrenOfSubType(ResourceIds.Rule, res.id) map {  
      rule => toLink(rule, None) 
    }
    upsertProperties(res, "rules" -> Json.stringify(Json.toJson(ruleLinks)))
  }

  /**
    * Builds lambda.properties.function_mappings object.
    */
  private[controllers] def transformLambda(res: GestaltResourceInstance, user: AuthAccountWithCreds, qs: Option[QueryString] = None) = Try {
    val lmap = ResourceFactory.getLambdaFunctionMap(res.id)
    val resJson = Json.toJson(res).as[JsObject]

    val newjson = if (lmap.isEmpty) resJson else {
      val smap = JsString(Json.toJson(lmap).toString)
      val m2 = replaceJsonPropValue(resJson, "function_mappings", smap)
      replaceJsonProps(resJson, m2)
    }
    newjson.validate[GestaltResourceInstance].get
  }

  private[controllers] def transformProvider(res: GestaltResourceInstance, user: AuthAccountWithCreds, qs: Option[QueryString] = None) = Try {
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
        handleExpansion(ResourceFactory.findAllIn(ResourceIds.User, userids),
            request.queryString, META_URL)
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
          handleExpansion(ResourceFactory.findAllIn(fqid(fqon), ResourceIds.Group, groupids),
              request.queryString, META_URL)
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

  // --------------------------------------------------------------------------
  // API_ENDPOINTS
  // --------------------------------------------------------------------------

  /**
    * Finding Endpoints by Container calls a different factory endpoint, because Endpoints are NOT
    * stored as children of Containers, because Endpoints are stored as children of Apis
    * TODO: there is some duplication here with getEndpointsByContainerFqon, not a big deal for now
    */
  def getEndpointsByContainerFqon(fqon: String, containerId: UUID) = Audited(fqon) { implicit request =>

    val path = new ResourcePath(fqon, s"containers/$containerId/apiendpoints")

    this.AuthorizedResourceList(path, "apiendpoint.view")

    handleExpansion(
      ResourceFactory.findAllByPropertyValue(ResourceIds.ApiEndpoint, "implementation_id", containerId.toString),
      request.queryString)
  }

  /**
   * Finding Endpoints by Lambda calls a different factory endpoint, because Endpoints are NOT
   * stored as children of Lambdas, because Endpoints are stored as children of Apis
   */
  def getEndpointsByLambdaFqon(fqon: String, lambda: UUID) = Audited(fqon) { implicit request =>
    
    val path = new ResourcePath(fqon, s"lambdas/$lambda/apiendpoints")
    
    this.AuthorizedResourceList(path, "apiendpoint.view") 
    
    handleExpansion(
      ResourceFactory.findAllByPropertyValue(ResourceIds.ApiEndpoint, "implementation_id", lambda.toString),
    request.queryString)
  }  

  import com.galacticfog.gestalt.meta.api.output.toLink
  
  def mkPath2(fqon: String, path: String) = {
    
    val rp = new ResourcePath(fqon, path)
    
    println(rp.info)
    
    def tname(typeId: UUID): String = resourceRestName(typeId).dropRight(1).mkString
    
    val org = {
      ResourceFactory.findByPropertyValue(ResourceIds.Org, "fqon", rp.fqon) map { 
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
  
  def mkPath(fqon: String, path: String) = {
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
    
    val org = ResourceFactory.findByPropertyValue(ResourceIds.Org, "fqon", fqon).get
    val keys = List(ResourceIds.Workspace, ResourceIds.Environment)
    val values = path.stripPrefix("/").stripSuffix("/").split("/").toList
    
    resolve(org.id, (keys zip values), Map("org" -> mkinfo(org))) match {
      case Success(m) => Ok(Json.toJson(m))
      case Failure(e) => HandleRepositoryExceptions(e)
    }    
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
    
    val org = ResourceFactory.findByPropertyValue(ResourceIds.Org, "fqon", fqon).get
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
