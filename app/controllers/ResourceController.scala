package controllers


import java.util.UUID

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.MethodSymbol
import scala.reflect.runtime.universe.typeOf
import scala.reflect.runtime.universe.TypeTag
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.data.CoVariant
import com.galacticfog.gestalt.data.ReferenceFactory
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.string2uuid
import com.galacticfog.gestalt.data.uuid
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.marathon.ContainerStats
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.output.gestaltResourceInstanceFormat
import com.galacticfog.gestalt.meta.api.resourceRestName
import com.galacticfog.gestalt.meta.api.resourceUUID
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.ResourceInfo
import com.galacticfog.gestalt.meta.api.sdk.ResourceLabel
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import com.galacticfog.gestalt.meta.api.sdk.Resources
import com.galacticfog.gestalt.meta.api.sdk.resourceInfoFormat

import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.GestaltSecurityClient
import com.galacticfog.gestalt.security.api.json.JsonImports.orgFormat

import controllers.util._
import controllers.util.JsonUtil.replaceJsonPropValue
import controllers.util.JsonUtil.replaceJsonProps
import play.api.{Logger => log}
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.Action
import play.api.mvc.AnyContent

import com.galacticfog.gestalt.meta.auth.Actions
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.meta.auth.ActionPrefix

import play.api.mvc.Result
import com.galacticfog.gestalt.meta.api.Resource
import com.galacticfog.gestalt.meta.api.output.Output.{renderInstance => Render}

import com.galacticfog.gestalt.meta.api.ResourcePath
import com.galacticfog.gestalt.data.models.ResourceLike
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds


object ResourceController extends Authorization {
  
  // TODO: Move these to MetaController (or somewhere up the chain)
  private val resources  = ResourceFactory
  private val references = ReferenceFactory

  type TransformFunction = (GestaltResourceInstance, AuthAccountWithCreds) => Try[GestaltResourceInstance]
  type FilterFunction    = ((Seq[ResourceLike], QueryString) => Seq[ResourceLike])

  type Lookup = (ResourcePath, AuthAccountWithCreds) => Option[GestaltResourceInstance]
  type LookupSeq = (ResourcePath, AuthAccountWithCreds) => Seq[GestaltResourceInstance]
  
  private[controllers] val transformers: Map[UUID, TransformFunction] = Map(
      ResourceIds.Group -> transformGroup,
      ResourceIds.User  -> transformUser
  )
  
  private[controllers] val lookupSeqs: Map[UUID, LookupSeq] = Map(
    ResourceIds.Provider -> lookupSeqProviders
  )
  
  def lookupSeqProviders(path: ResourcePath, account: AuthAccountWithCreds): List[GestaltResourceInstance] = {
    log.debug(s"lookupSeqProviders(${path.path}, user = ${account.account.id}")
    
    val parentId = { 
      if (path.parentId.isDefined) path.parentId.get
      else orgFqon(path.fqon).fold(FqonNotFound(path.fqon)){ _.id }
    }
    ResourceFactory.findChildrenOfSubType(ResourceIds.Provider, parentId)  
  }
  
  
  def FqonNotFound(fqon: String) = {
    throw new BadRequestException(s"Org with FQON '${fqon}' not found.")
  }
  
  /*
   * Add ability to override normal lookup function:
   * lookup = (UUID, Account) => Option[GestaltResourceInstance]
   * lookupSeq = (UUID,Account) => Seq[GestaltResourceInstance]
   * 
   */
  
  def getOrgFqon(fqon: String) = Authenticate() { implicit request =>
    orgFqon(fqon).fold( OrgNotFound(fqon) ) { org =>
      Authorize(org.id, Actions.Org.View, request.identity) {   
        Ok(RenderSingle(Resource.fromPath(fqon).get))
      }
    }
  }
  
  /**
   * Get a top-level resource list. Pulls resources from across all Orgs.
   */
  def getGlobalResourceList(targetTypeId: String) = Authenticate() { implicit request =>
    val typeId = uuid(targetTypeId)
    val action = s"${ActionPrefix(typeId)}.view"
    
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
        transformers.get(resource.typeId).fold {
          Ok( RenderSingle(resource) )
        }{ 
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
    }{ f =>
      f(path, request.identity).toList
    }
    
    val transform = transformers.get(path.targetTypeId)
    
    AuthorizeList(action) {
      if (transform.isEmpty) rss else {
        rss map { transform.get(_, request.identity).get }
      }
    }
  }

  
  private[controllers] def RenderSingle(res: GestaltResourceInstance)
      (implicit request: SecuredRequest[_]) = {
    Output.renderInstance(res, META_URL)
  }
  
  private[controllers] def RenderList(rss: Seq[GestaltResourceInstance])
      (implicit request: SecuredRequest[_]) = {
    handleExpansion(rss, request.queryString, META_URL)  
  }
  
//  abstract class ViewHandler(
//      transformers: Map[UUID, ResourceLike => ResourceLike], 
//      filters: Map[UUID, ResourceLike => Boolean]) {
//    
//    def find[T](path: String): T
//  }
//  
//  class ResourceViewHandler (
//      transformers: Map[UUID, ResourceLike => ResourceLike], 
//      filters: Map[UUID, ResourceLike => Boolean]) extends ViewHandler(transformers, filters) {
//    
//    def find[T](path: String) = {
//      val rp = new ResourcePath(path)
//      val typeId = rp.targetTypeId
//      //
//      // perform lookup
//      //
//      val transform = transformers.get(typeId)
//      //
//      // apply transformation function if appropriate.
//      //
//    }
//  }

  
  /**
   * Build the dynamic 'groups' property on a User instance.
   */
  private[controllers] def transformUser(res: GestaltResourceInstance, user: AuthAccountWithCreds) = Try {
    Security.getAccountGroups(res.id, user) match {
      case Success(gs) => {
        val gids = gs map { _.id.toString }
        val props = if (gids.isEmpty) None 
          else Some(res.properties.get ++ Map("groups" -> gids.mkString(",")))
        res.copy(properties = props)
      }
      case Failure(er) => throw new RuntimeException(s"Failed looking up groups for user '${res.id}': ${er.getMessage}")
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
  
  
  protected [controllers] def findSystemResource(typeId: UUID, id: UUID)(implicit request: SecuredRequest[_]) = {
    ResourceFactory.findById(typeId, id).fold {
      ResourceNotFound(typeId, id)
    } { res =>
      Ok(Output.renderInstance(res, META_URL))
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
    ResourceFactory.findById(ResourceIds.User, request.identity.account.id).fold {
      InternalServerError(
        s"User ID ${id.toString} was found in Security but not in Meta - this may be a synchronization error. Contact an administrator.")
    }{ user => Ok(Render(user, META_URL)) }      
  }
  
  def getGroupsFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    handleExpansion(
      ResourceFactory.findAll(ResourceIds.Group, fqid(fqon)),
      request.queryString, 
      META_URL)
  }
  
  

  
  def getGroupByIdFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    
    ResourceFactory.findById(ResourceIds.Group, id).fold(ResourceNotFound(ResourceIds.Group, id)) {
      r => Authorize(fqid(fqon), Actions.Group.View) {    
          
        Security.getGroupAccounts(r.id, request.identity) match {
          case Success(acs) => {
            // String list of all users in current group.
            val acids = (acs map { _.id.toString }).mkString(",")
            
            // Inject users into group properties.
            val groupProps = if (r.properties.isDefined) r.properties.get else Map()
            val outputProps = if (acids.isEmpty) None
              else Some(groupProps ++ Map("users" -> acids))
            
            Ok(Output.renderInstance(r.copy(properties = outputProps), META_URL))
            
          }
          case Failure(err) => HandleExceptions(err)
        }
      } 
    }
  }
  
  /**
   * Find all members of the given Groups. Performs dynamic lookup in gestalt-security.
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
   */
  def getUserGroupsFqon(fqon: String, user: UUID) = Authenticate(fqon) { implicit request =>
    Security.getAccountGroups(request.identity) match {
      case Success(gs) => {
        val groupids = gs map { _.id }
        if (groupids.isEmpty) Ok(Json.parse("[]")) else {
          handleExpansion(ResourceFactory.findAllIn(fqid(fqon), ResourceIds.Group, groupids),
              request.queryString, META_URL)
        }
      }
      case Failure(err) => HandleExceptions(err)
    }
  }
  

  // --------------------------------------------------------------------------
  // CUSTOM-RESOURCES
  // --------------------------------------------------------------------------
  /* TODO: This function implementes `GET /{fqon}/resourcetypes/{type-id}/resources` to return
   * a list of all resources of the give type in the Org. I guess that's useful for custom-resource
   * Get all Resources by Type ID
   */
  def getAllResourcesByTypeOrg(org: UUID, typeId: UUID) = Authenticate(org) { implicit request =>
    Ok(Output.renderLinks(ResourceFactory.findAll(typeId, org)))
  }
  
  def getAllResourcesByTypeFqon(fqon: String, typeId: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case None => OrgNotFound(fqon)  
      case Some(org) => Ok(Output.renderLinks(ResourceFactory.findAll(typeId, org.id)))
    }     
  }
  
  def getAllResources(org: UUID) = Authenticate(org) { implicit request =>
    Ok(Output.renderLinks(ResourceFactory.findAllByOrg(org)))
  }

  def getAllResourcesFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => Ok(Output.renderLinks(ResourceFactory.findAllByOrg(org.id)))
      case None => OrgNotFound(fqon)
    }
  }
  
  def getResourceByIdFqon(fqon: String, id: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => ResourceFactory.findById(id) match {
        case Some(res) => Ok(Output.renderInstance(res))
        case None => NotFoundResult(Errors.RESOURCE_NOT_FOUND(id))
      }
      case None => OrgNotFound(fqon)
    }
  }
  
  def getResourceById(org: UUID, id: UUID) = Authenticate(org) { implicit request =>
    ResourceFactory.findById(id) match {
      case Some(res) => Ok(Output.renderInstance(res))
      case None      => NotFoundResult(Errors.RESOURCE_NOT_FOUND(id))
    }
  }
  
  // --------------------------------------------------------------------------
  // LAMBDAS
  // --------------------------------------------------------------------------
  /*
   * The reason all of these Lambda accessors exist is because lambda.properties.function_mappings
   * is build dynamically on retrieval. injectLambdaFunctionMap() is called on the resource before
   * rendering. 
   */
  
  def getWorkspaceLambdas(org: UUID, workspace: UUID) = Authenticate(org) { implicit request =>
    handleExpansion(
        ResourceFactory.findChildrenOfType(ResourceIds.Lambda, workspace), request.queryString,
        META_URL)
  }
  
  def getWorkspaceLambdasFqon(fqon: String, workspace: UUID) = Authenticate(fqon) { implicit request =>
    val lambdas = ResourceFactory.findChildrenOfType(ResourceIds.Lambda, workspace) map { 
      injectLambdaFunctionMap(_).get
    }
    handleExpansion(lambdas, request.queryString, META_URL)
  }
  
  def getDescendantLambdas(org: UUID) = {
    ResourceFactory.findAll(ResourceIds.Lambda, org) map {
      injectLambdaFunctionMap(_).get
    }  
  }
  
  def getChildLambdas(parent: UUID) = {
    ResourceFactory.findChildrenOfType(ResourceIds.Lambda, parent) map { 
      injectLambdaFunctionMap(_).get
    }    
  }
  
  def getEnvironmentLambdas(org: UUID, environment: UUID) = Authenticate(org) { implicit request =>
    handleExpansion(getChildLambdas(environment), request.queryString, META_URL)    
  }  
  
  def getEnvironmentLambdasFqon(fqon: String, environment: UUID) = Authenticate(fqon) { implicit request =>
    handleExpansion(getChildLambdas(environment), request.queryString)      
  }
  
  def getEnvironmentLambdaByIdFqon(fqon: String, environment: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => getLambdaByIdCommon(id)
      case None => OrgNotFound(fqon)
    }  
  }
  
  def getLambdaByIdFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => getLambdaByIdCommon(id)
      case None => OrgNotFound(fqon)
    }    
  }
  
  def getEnvironmentLambdaById(org: UUID, environment: UUID, id: UUID) = Authenticate(org) { implicit request =>
    getLambdaByIdCommon(id)
  }
  
  def getLambdaByIdCommon(id: UUID)(implicit request: SecuredRequest[AnyContent]) = {
    ResourceFactory.findById(ResourceIds.Lambda, id) match {
      case Some(res) => injectLambdaFunctionMap(res) match {
        case Success(lamb) => Ok(Output.renderInstance(lamb))
        case Failure(err) => HandleRepositoryExceptions(err)
      }
      case None => NotFoundResult(request.uri)
    }    
  }
  
  private[controllers] def injectLambdaFunctionMap(res: GestaltResourceInstance) = Try {
    val lmap = ResourceFactory.getLambdaFunctionMap(res.id)
    val resJson = Json.toJson(res).as[JsObject]
    
    val newjson = if (lmap.isEmpty) resJson else {
        val smap = JsString(Json.toJson(lmap).toString)
        val m2 = replaceJsonPropValue(resJson, "function_mappings", smap)      
        replaceJsonProps(resJson, m2)
    }
    newjson.validate[GestaltResourceInstance].get
  }
  
  // --------------------------------------------------------------------------
  // API_ENDPOINTS
  // --------------------------------------------------------------------------  
  /*
   * Finding Endpoints by Lambda calls a different factory endpoint, because Endpoints are NOT
   * stored as children of Lambdas. Why not?
   */
  def getEndpointsByLambda(org: UUID, lambda: UUID) = Authenticate(org) { implicit request =>
    handleExpansion(ResourceFactory.findEndpointsByLambda(lambda), request.queryString)
  }
  
  def getEndpointsByLambdaFqon(fqon: String, lambda: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => handleExpansion(ResourceFactory.findEndpointsByLambda(lambda), request.queryString)
      case None => OrgNotFound(fqon)
    }
  }  
  

  /*
   * SPECIAL CASE: Providers are merged.
   */
  def getWorkspaceProviders(org: UUID, workspace: UUID) = Authenticate(org) { implicit request =>
    // Get gateway and marathon provider types and merge list.
    /*
     * TODO: Use findWithVariance to get ALL providers of all types and merge.
     */
//    val gateways  = ResourceFactory.findChildrenOfType(ResourceIds.ApiGatewayProvider, workspace)
//    val marathons = ResourceFactory.findChildrenOfType(ResourceIds.MarathonProvider, workspace)
//    val providers = gateways ++ marathons
    
    val providers = ResourceFactory.findChildrenOfSubType(ResourceIds.Provider, workspace)
    handleExpansion(providers, request.queryString, META_URL)
  }
  
  /**
   * Get a list of valid provider-types. That is any resource type that is a sub-type
   * of Provider.
   */
  private[controllers] def providerTypeIds(): Seq[UUID] = {
    ResourceFactory.findTypesWithVariance(CoVariant(ResourceIds.Provider)).map { p =>
     p.id 
    }
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
          ResourceFactory.findChildByName(parent, h._1, h._2) match {
            case None => throw new ResourceNotFoundException(s"${ResourceLabel(h._1)} with name '${h._2}' not found.")
            case Some(res) => 
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
