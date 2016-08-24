package controllers

import java.util.UUID

import scala.util.{Try,Success,Failure}

import com.galacticfog.gestalt.data.ResourceFactory
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

import com.galacticfog.gestalt.meta.auth.Actions
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import controllers.util._
import controllers.util.JsonUtil._

import play.api.libs.json.{JsObject,JsString,Json}

import play.api.mvc.{Action,Result}


object ResourceController extends Authorization {
  
  type TransformFunction = (GestaltResourceInstance, AuthAccountWithCreds) => Try[GestaltResourceInstance]
  type FilterFunction    = ((Seq[ResourceLike], QueryString) => Seq[ResourceLike])
  
  type Lookup    = (ResourcePath, AuthAccountWithCreds) => Option[GestaltResourceInstance]
  type LookupSeq = (ResourcePath, AuthAccountWithCreds) => Seq[GestaltResourceInstance]
  
  
  private[controllers] val transforms: Map[UUID, TransformFunction] = Map(
      ResourceIds.Group -> transformGroup,
      ResourceIds.User  -> transformUser,
      ResourceIds.Lambda -> transformLambda
  )
  
  private[controllers] val lookups: Map[UUID, Lookup] = Map(
    
  )
  
  private[controllers] val lookupSeqs: Map[UUID, LookupSeq] = Map(
    ResourceIds.Provider -> lookupSeqProviders
  )
  
  def FqonNotFound(fqon: String) = {
    throw new BadRequestException(s"Org with FQON '${fqon}' not found.")
  }
  
  def getOrgFqon(fqon: String) = Authenticate() { implicit request =>
    orgFqon(fqon).fold( OrgNotFound(fqon) ) { org =>
      Authorize(org.id, Actions.Org.View, request.identity) {   
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
    val action = s"${Actions.typePrefix(typeId)}.view"
    
    AuthorizeList(action) {
      ResourceFactory.findAll(typeId)
    }
  }
  
  /**
   * Get a Resource or list of Resources by path.
   */
  def getResources(fqon: String, path: String) = Authenticate(fqon) { implicit request =>
    val rp = new ResourcePath(fqon, path)
    val action = Actions.actionName(rp.targetTypeId, "view")
    
    if (rp.isList) AuthorizedResourceList(rp, action) 
    else AuthorizedResourceSingle(rp, action)
  }  
  
  private[controllers] def AuthorizedResourceSingle(path: ResourcePath, action: String)
      (implicit request: SecuredRequest[_]): Result = {
    
    Authorize(path.targetId.get, action) {
      Resource.fromPath(path.path).fold( NotFoundResult(request.uri) ) { resource =>
        transforms.get(resource.typeId).fold {
          Ok( RenderSingle(resource) )
        }{ 
          log.debug(s"Found transformation function for Resource: ${resource.id}")
          _ (resource, request.identity) match {
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
    }{ f => f(path, request.identity).toList }
    
    AuthorizeList(action) {
      transforms.get(path.targetTypeId).fold(rss) { f =>
        rss map { f(_, request.identity).get }
      }
    }
  }

  /*
   * Type-Based Lookup Functions
   */ 
  def lookupSeqProviders(path: ResourcePath, account: AuthAccountWithCreds): List[GestaltResourceInstance] = {
    log.debug(s"lookupSeqProviders(${path.path}, user = ${account.account.id}")
    
    val parentId = { 
      if (path.parentId.isDefined) path.parentId.get
      else orgFqon(path.fqon).fold(FqonNotFound(path.fqon)){ _.id }
    }
    
    ResourceFactory.findById(parentId).fold {
      throw new ResourceNotFoundException(parentId.toString)
    } { _ => ResourceFactory.findAncestorsOfSubType(ResourceIds.Provider, parentId) }
  }  

  /*
   * 
   * TODO: Need to 're-implement' this. Generalize filtering into resource lookups
   * 
   */
  
//  def filterProvidersByType(rs: List[GestaltResourceInstance], qs: Map[String,Seq[String]]) = {
//    if (qs.contains("type")) {
//
//      val typeName = "Gestalt::Configuration::Provider::" + qs("type")(0)
//      log.debug("Filtering providers for type : " + typeName)
//      //
//      // TODO: No longer necessary to do this - use .findWithVariance()
//      //
//      val typeId = typeName match {
//        case a if a == Resources.ApiGatewayProvider.toString => ResourceIds.ApiGatewayProvider
//        case b if b == Resources.MarathonProvider.toString => ResourceIds.MarathonProvider
//        case c if c == Resources.LambdaProvider.toString => ResourceIds.LambdaProvider
//        case e => throw new BadRequestException(s"Unknown provider type : '$e'")
//      }
//      rs filter { _.typeId == typeId }
//    } else rs
//  }  
  
  private[controllers] def transformLambda(res: GestaltResourceInstance, user: AuthAccountWithCreds) = Try {
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
  private[controllers] def transformUser(res: GestaltResourceInstance, user: AuthAccountWithCreds) = Try {
    
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
  def transformGroup(r: GestaltResourceInstance, user: AuthAccountWithCreds) = {
    
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
          else v.properties.get.get("env") match {
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
    val org = fqid(fqon)
    handleExpansion(ResourceFactory.findEndpointsByLambda(lambda), request.queryString)
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
  
}
