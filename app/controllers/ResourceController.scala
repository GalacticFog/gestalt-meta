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
import ResourceFactory.{findEndpointsByLambda, findChildrenOfType}


@Singleton
class ResourceController @Inject()( messagesApi: MessagesApi,
                                    env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
                                    containerService: ContainerService )
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  type TransformFunction = (GestaltResourceInstance, AuthAccountWithCreds, Option[QueryString]) => Try[GestaltResourceInstance]
  type FilterFunction    = ((Seq[ResourceLike], QueryString) => Seq[ResourceLike])
  
  type Lookup    = (ResourcePath, AuthAccountWithCreds, Option[QueryString]) => Option[GestaltResourceInstance]
  type LookupSeq = (ResourcePath, AuthAccountWithCreds, QueryString) => Seq[GestaltResourceInstance]
  
  
  private[controllers] val transforms: Map[UUID, TransformFunction] = Map(
      ResourceIds.Group  -> transformGroup,
      ResourceIds.User   -> transformUser,
      ResourceIds.Lambda -> transformLambda,
      ResourceIds.Policy -> transformPolicy
  )
  
  private[controllers] val lookups: Map[UUID, Lookup] = Map(
    ResourceIds.Container   -> lookupContainer,
    ResourceIds.Entitlement -> lookupEntitlement
  )
  
  private[controllers] val lookupSeqs: Map[UUID, LookupSeq] = Map(
    ResourceIds.Provider    -> lookupSeqProviders,
    ResourceIds.Org         -> lookupSeqOrgs,
    ResourceIds.Container   -> lookupContainers,
    ResourceIds.Entitlement -> lookupSeqEntitlements,
    ResourceIds.ApiEndpoint -> lookupApiEndpoints,
    ResourceIds.Rule        -> lookupPolicyRules
  )
  
  def lookupPolicyRules(path: ResourcePath, user: AuthAccountWithCreds, qs: QueryString): Seq[GestaltResourceInstance] ={
    log.debug("Entered => lookupPolicyRules(_,_,_)...")
    val mapPathData = Resource.mapListPathData(path.path)
    val policy = mapPathData(Resource.ParentId)
    
    log.debug("Finding rules for policy : " + policy)
    
    ResourceFactory.findChildrenOfSubType(ResourceIds.Rule, policy)
    
  }
  
  def lookupApiEndpoints(path: ResourcePath, user: AuthAccountWithCreds, qs: QueryString): Seq[GestaltResourceInstance] ={
    log.debug(s"lookupApiEndpoints(${path.path},_,_)...")
    
    val mapPathData = Resource.mapListPathData(path.path)
    val lambda = mapPathData(Resource.ParentId)

    /*
     * TODO: This is dumb - investigate why Resource.mapListPathData doesn't
     * put the actual type ID into the map instead of the REST name for the type.
     */
    val parentTypeId = mapPathData(Resource.ParentType) match {
      case a if a == "lambdas" => ResourceIds.Lambda
      case b if b == "environments" => ResourceIds.Environment
      case c if c == "apis" => ResourceIds.Api
      case _ => throw BadRequestException(s"Invalid parent-type for ApiEndpoint.")
    }
    
    val debugParentLabel = ResourceLabel(parentTypeId)
    log.debug(s"Looking up ApiEndpoints by parent type '$debugParentLabel'")
    
    /*
     * The idea here is that if the request comes in through the /lambdas path we need to
     * execute a special function (findEndpointsByLambda()) since apiendpoints are not
     * children of lambdas currently. This all goes away when we revamp the api-gateway
     * code.
     */
    val lookup: UUID => Seq[GestaltResourceInstance] = parentTypeId match {
      case a if a == ResourceIds.Lambda => findEndpointsByLambda _
      case b if b == ResourceIds.Environment => findChildrenOfType(ResourceIds.ApiEndpoint, _: UUID)
      case c if c == ResourceIds.Api => findChildrenOfType(ResourceIds.ApiEndpoint, _: UUID)
      case _ => throw BadRequestException(s"Invalid parent-type for ApiEndpoint.")
    }
    
    val targetId = UUID.fromString(mapPathData(Resource.ParentId))
    
    log.debug(s"Searching for ApiEndpoints in $debugParentLabel '$targetId'")
    
    lookup(targetId)
  }
  
  def lookupContainer(path: ResourcePath, user: AuthAccountWithCreds, qs: Option[QueryString]): Option[GestaltResourceInstance] = {
    log.debug("Lookup function : lookupContainer(_,_,_)...")
    Resource.fromPath(path.path) flatMap { r =>
      val fqon = Resource.getFqon(path.path)
      val env = ResourceFactory.findParent(r.id) getOrElse throwBadRequest("could not determine environment parent for container")

      log.debug("fqon: %s, env: %s[%s]".format(fqon, env.name, env.id.toString))
      log.debug("Calling external CaaS provider...")

      Await.result(
        containerService.findEnvironmentContainerByName(fqon, env.id, r.name),
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
  
  def getOrgFqon(fqon: String) = Authenticate() { implicit request =>
    orgFqon(fqon).fold( OrgNotFound(fqon) ) { org =>
      Authorize(org.id, "org.view", request.identity) {   
        Ok(RenderSingle(Resource.fromPath(fqon).get))
      }
    }
  }
  
  /**
   * Get a top-level resource list. Pulls resources from across all Orgs (no leading FQON).
   * i.e., GET /users
   */
  def getGlobalResourceList(targetTypeId: String) = Authenticate() { implicit request =>
    val typeId = uuid(targetTypeId)
    //val action = s"${Actions.typePrefix(typeId)}.view"
    val action = actionInfo(typeId).prefix + ".view"
    AuthorizeList(action) {
      ResourceFactory.findAll(typeId)
    }
  }
  
  /**
   * Get a Resource or list of Resources by path.
   */
  def getResources(fqon: String, path: String) = Authenticate(fqon) { implicit request =>
    val rp = new ResourcePath(fqon, path)

    val action = actionInfo(rp.targetTypeId).prefix + ".view"
    
    if (rp.isList) AuthorizedResourceList(rp, action) 
    else AuthorizedResourceSingle(rp, action)
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
  
  private[controllers] def AuthorizedResourceSingle(path: ResourcePath, action: String)
      (implicit request: SecuredRequest[_]): Result = {
    
    log.debug(s"AuthorizedResourceSingle($path, $action)")
    
    Authorize(path.targetId.get, action) {
      
      val resource = lookups.get(path.targetTypeId).fold {
        log.debug("Applying standard lookup function")
        Resource.fromPath(path.path)
      }{ f=>
        log.debug(s"Found custom lookup function for Resource. Executing...")
        f(path, request.identity, Option(request.queryString))
      }
      
      resource.fold(NotFoundResult(request.uri)) { res =>
        transforms.get(res.typeId).fold {
          log.debug("No custom transformer found.")
          Ok( RenderSingle(res) )
        }{ f =>
          log.debug(s"Found custom transformation function for Resource: ${res.id}")
          log.debug(s"type-id: ${res.typeId}, label: ${ResourceLabel(res.typeId)}")
          log.debug("FUNCTION : " + f.getClass.getName)
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
    
    val rss = lookupSeqs.get(path.targetTypeId).fold {
      Resource.listFromPath(path.path)
    }{ f => f(path, request.identity, request.queryString).toList }
    
    AuthorizeList(action) {
      transforms.get(path.targetTypeId).fold(rss) { f =>
        rss map { f(_, request.identity, Option(request.queryString)).get }
      }
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
    Resource.fromPath(path.path) map { transformEntitlement(_, account).get }
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
      filterProvidersByType(rs, qs)
    }
  }
  
  /*
   * 
   * TODO: Need to 're-implement' this. Generalize filtering into resource lookups
   * 
   */
  def filterProvidersByType(rs: List[GestaltResourceInstance], qs: Map[String,Seq[String]]) = {
    if (qs.contains("type")) {

      val typeName = "Gestalt::Configuration::Provider::" + qs("type")(0)
      log.debug("Filtering providers for type : " + typeName)

      import com.galacticfog.gestalt.data.CoVariant
      import com.galacticfog.gestalt.data.ResourceType
      import com.galacticfog.gestalt.data.ResourceFactory.findTypesWithVariance
      
      val validProviderTypes = findTypesWithVariance(CoVariant(ResourceIds.Provider))
      val typeId = Try(ResourceType.id(typeName)) match {
        case Failure(err) => 
          throw new BadRequestException(s"Unknown provider type : '$typeName'")
        case Success(tid) => {
          def validTypes = findTypesWithVariance(CoVariant(ResourceIds.Provider)) map { _.id }
          if(validTypes.contains(tid)) tid
          else throw new BadRequestException(s"Unknown provider type : '$typeName'")
        }
      }
      log.debug(s"Filtering for type-id : $typeId")
      rs filter { _.typeId == typeId }
    } else rs
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
   * Builds lambda.properties.funtion_mappings object.
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
  
  /**
   * Build the dynamic 'groups' property on a User instance.
   */
  private[controllers] def transformUser(res: GestaltResourceInstance, user: AuthAccountWithCreds, qs: Option[QueryString] = None) = Try {
    
    Security.getAccountGroups(res.id, user) match {
      case Failure(er) => {
        throw new RuntimeException(Errors.USER_GROUP_LOOKUP_FAILED(res.id, er.getMessage))
      }
      case Success(gs) => {
        val gids  = gs map { _.id.toString }
        val props = {
          if (gids.isEmpty) res.properties
          else Option(res.properties.get ++ Map("groups" -> gids.mkString(",")))
        }
        res.copy(properties = props)
      }
    }
  }

  /**
   * Add users to the Group's properties collection. Users are looked up dynamically
   * in gestalt-security.
   */
  def transformGroup(r: GestaltResourceInstance, user: AuthAccountWithCreds, qs: Option[QueryString] = None) = {
    
    Security.getGroupAccounts(r.id, user) map { acs =>
      // String list of all users in current group.
      val acids = (acs map { _.id.toString }).mkString(",")
      
      // Inject users into group properties.
      val groupProps  = if (r.properties.isDefined) r.properties.get else Map()
      val outputProps = {
        if (acids.isEmpty) None
        else Some(groupProps ++ Map("users" -> acids))
      }
      r.copy(properties = outputProps)
    }
  }

  // --------------------------------------------------------------------------
  // ENVIRONMENT VARIABLES
  // --------------------------------------------------------------------------  
  
  def getEnvVariablesOrgFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    val org = fqid(fqon)
    Ok(Json.toJson(EnvironmentVars.get(org, org)))
  }
  
  def getEnvVariablesFqon(fqon: String, typeId: String, id: UUID) = Authenticate(fqon) { implicit request =>
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
  def getUserSelf() = Authenticate() { implicit request =>
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
  def getGroupUsersFqon(fqon: String, group: UUID) = Authenticate(fqon) { implicit request =>
    Security.getGroupAccounts(group, request.identity) match {
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
  def getUserGroupsFqon(fqon: String, user: UUID) = Authenticate(fqon) { implicit request =>
    Security.getAccountGroups(request.identity) match {
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
  def getAllResourcesByTypeFqon(fqon: String, typeId: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon).fold(OrgNotFound(fqon)){ org =>
      Ok(Output.renderLinks(ResourceFactory.findAll(typeId, org.id)))
    }
  }
  
  def getAllResourcesFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    orgFqon(fqon).fold(OrgNotFound(fqon)){ org =>
      Ok(Output.renderLinks(ResourceFactory.findAllByOrg(org.id)))
    }
  }
  
  def getResourceByIdFqon(fqon: String, id: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    orgFqon(fqon).fold(OrgNotFound(fqon)) { org =>
      ResourceFactory.findById(id).fold(NotFoundResult(Errors.RESOURCE_NOT_FOUND(id))) { r =>
        Ok(Output.renderInstance(r))
      }
    }
  }
  
  // --------------------------------------------------------------------------
  // API_ENDPOINTS
  // --------------------------------------------------------------------------
  
  /**
   * Finding Endpoints by Lambda calls a different factory endpoint, because Endpoints are NOT
   * stored as children of Lambdas. Why not?
   */
   
  def getEndpointsByLambdaFqon(fqon: String, lambda: UUID) = Authenticate(fqon) { implicit request =>
    
    val path = new ResourcePath(fqon, s"lambdas/$lambda/apiendpoints")
    
    this.AuthorizedResourceList(path, "apiendpoint.view") 
    
//      val org = fqid(fqon)
//      handleExpansion(ResourceFactory.findEndpointsByLambda(lambda), request.queryString)
    
  }  
  
  def mapPath(fqon: String, path: String) = Authenticate(fqon) { implicit request =>
    
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
