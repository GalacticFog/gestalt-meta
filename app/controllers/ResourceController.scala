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
import com.galacticfog.gestalt.meta.services.ResourceQueryService
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
  


object ResourceController extends Authorization {
  
  
  // TODO: Get rid of this - obsolete concept.
  private val qs = ResourceQueryService
  
  // TODO: Move these to MetaController (or somewhere up the chain)
  private val resources  = ResourceFactory
  private val references = ReferenceFactory
  

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
  
  def providerTypeIds(): Seq[UUID] = {
    ResourceFactory.findTypesWithVariance(CoVariant(ResourceIds.Provider)).map { p =>
     p.id 
    }
  }
  
  def providerTypes() = {
    ResourceFactory.findTypesWithVariance(CoVariant(ResourceIds.Provider)).map { p =>
      (p.name -> p.id) 
    }.toMap  
  }
  
  def getGenericTopLevel(targetTypeId: String) = Authenticate() { implicit request =>
    val typeId = uuid(targetTypeId)
    val action = s"${ActionPrefix(typeId)}.view"
    
    AuthorizeList(action) {
      ResourceFactory.findAll(typeId)
    }
  }  


  def getResourceByPath(fqon: String, path: String) = Authenticate() { implicit request =>
    orgFqon(fqon).fold( OrgNotFound(fqon) ) { org =>
      Authorize(org.id, Actions.Org.View, request.identity) {
        resourceFromPath(fqon, path)
      }
    }      
  }

  
  def resourceFromPath(fqon: String, path: String)(implicit request: SecuredRequest[_]) = {
    val p = if (path.trim.isEmpty) fqon else "%s/%s".format(fqon, path)
    Resource.fromPath(p).fold(NotFoundResult(request.uri)){ resource =>
      Ok(Render(resource, META_URL))
    }
  }
  
  def getOrgFqon(fqon: String) = Authenticate() { implicit request =>
    
    orgFqon(fqon).fold( OrgNotFound(fqon) ) { org =>
      Authorize(org.id, Actions.Org.View, request.identity) {
        findSystemResource(ResourceIds.Org, org.id)
      }
    }
  }

  /*
   * 
   * TODO: This only handles top-level (after FQON) resources. We need to go two levels deep.
   * We need a function similar to Resource.fromPath that retrieves lists of resources by type.
   * 
   */
  def listSystemResources(fqon: String, restName: String) = Authenticate(fqon) { implicit request =>
    extractByName(fqon, restName) match {
      case Left(result) => result
      case Right((org, typeId)) => {
        //
        // TODO: This is a hack for Lambda - getDescendantLambdas dynamically builds the endpoint function map.
        //
        val resources = typeId match {
          case m if m == ResourceIds.Lambda => getDescendantLambdas(org)
          case _ => ResourceFactory.findAll(typeId, org)
        }
        AuthorizeList(mkActionName(typeId, "view")) {
          ResourceFactory.findAll(typeId, org)
        }
      }
    }
  }  
  
  
  def getAllSystemResourcesByNameFqon(fqon: String, restName: String) = Authenticate(fqon) { implicit request =>
    extractByName(fqon, restName) match {
      case Left(result) => result
      case Right((org, typeId)) => {
        
        // TODO: This is a hack for Lambda - getChildLambdas dynamically builds the endpoint function map.
        val resources = typeId match {
          case m if m == ResourceIds.Lambda => getDescendantLambdas(org)
          case _ => ResourceFactory.findAll(typeId, org)
        }
        
        val action = s"${ActionPrefix(typeId)}.view"
        
        AuthorizeList(mkActionName(typeId, "view")) {
          ResourceFactory.findAll(typeId, org)
        }
      }
    }
  }  

  
//  def FindWithFqon(fqon: String, action: String, typeId: UUID)(block: (UUID,UUID) => Result)(implicit request: SecuredRequest[_]) = {
//    maybeFqid(fqon) match {
//      case None => NotFoundResult(s"Org '$fqon' not found.")
//      case Some(resourceId) => Authorize(resourceId, action, request.identity) { 
//        block(typeId, resourceId) 
//      }
//    }
//  }
  
  def findAuthorized(fqon: String, typeId: UUID, id: UUID)(implicit request: SecuredRequest[_]) = {
    maybeFqid(fqon) match {
      case None => NotFoundResult(s"Org '$fqon' not found.")
      case Some(resourceId) => 
        Authorize(resourceId, Actions.resourceAction(typeId, "view"), request.identity) { 
          findSystemResource(typeId, resourceId) 
        }
    }
  }
  
  def maybeFqid(fqon: String) = {
    orgFqon(fqon) map { _.id }
  }
  

  
  protected [controllers] def findSystemResource(typeId: UUID, id: UUID)(implicit request: SecuredRequest[_]) = {
    ResourceFactory.findById(typeId, id).fold {
      ResourceNotFound(typeId, id)
    } { res =>
      Ok(Output.renderInstance(res, META_URL))
    }
  }

  def getGenericById(targetTypeId: String, org: UUID, id: UUID) = Authenticate(org) { implicit request =>
    getById(org, uuid(targetTypeId), id)  
  }
  
  def getGenericByIdFqon(targetTypeId: String, fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    getById(fqid(fqon), uuid(targetTypeId), id)  
  }
  
  def getGenericChildAll(targetTypeId: String, parentType: String, parentId: UUID, org: UUID) = Authenticate(org) { implicit request =>
    handleExpansion(ResourceFactory.findChildrenOfType(uuid(targetTypeId), parentId),
        request.queryString, META_URL)
  }
  
  def getGenericChildAllFqon(targetTypeId: String, parentType: String, parentId: UUID, fqon: String) = Authenticate(fqon) { implicit request =>  
    handleExpansion(ResourceFactory.findChildrenOfType(uuid(targetTypeId), parentId),
        request.queryString, META_URL)
  }  
  
  
  def getProviders(org: UUID, parentType: String, parent: UUID) = Authenticate(org) { implicit request =>
    getProvidersCommon(org, parentType, parent, request.queryString, META_URL)
  }
  
  def getProvidersOrgFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    val org = fqid(fqon)
    getProvidersCommon(org, ResourceIds.Org, org, request.queryString, META_URL)
  }
  
  def getProvidersFqon(fqon: String, parentType: String, parent: UUID) = Authenticate(fqon) { implicit request =>
    getProvidersCommon(fqid(fqon), parentType, parent, request.queryString, META_URL)
  }
  
  def getProviderByIdOrgFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    getProviderByIdCommon(id)
  }
  
  def getProviderByIdFqon(fqon: String, parentType: String, parentId: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    val parentTypeId = UUID.fromString(parentType)
    ResourceFactory.findById(parentTypeId, parentId) match {
      case None => NotFoundResult(request.uri)
      case Some(_) => getProviderByIdCommon(id)
    }  
  }
  
  def getProviderByIdCommon(id: UUID)(implicit request: SecuredRequest[_]) = {
    ResourceFactory.findById(id) match {
      case None => NotFoundResult(s"Provider with ID '${id}' not found.")
      case Some(p) => {
        if (providerTypeIds.contains(p.typeId)) Ok(Output.renderInstance(p, META_URL))
        else NotFoundResult(s"Provider with ID '${id}' not found.")
      }
    }
  }  
  
  def getProvidersCommon(org: UUID, parentType: String, parent: UUID, qs: Map[String,Seq[String]], baseUrl: Option[String] = None) = {
    ResourceFactory.findById(parentType, parent) match {
      case None => NotFoundResult(s"${ResourceLabel(parentType)} with ID '${parent}' not found.")
      case Some(_) => {
        val allProvidersInScope = ResourceFactory.findAncestorsOfSubType(ResourceIds.Provider, parent)
        val filtered = filterProvidersByType(ResourceFactory.findAncestorProviders(parent), qs)
        handleExpansion(filtered, qs, baseUrl)
      }
    }
  }
  
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
  
  
  def getGroupUsersFqon(fqon: String, group: UUID) = Authenticate(fqon) { implicit request =>
    Security.getGroupAccounts(group, request.identity) match {
      case Success(gs) => {
        val userids = gs map { _.id }
        if (userids.isEmpty) Ok(Json.parse("[]")) else {
        handleExpansion(ResourceFactory.findAllIn(ResourceIds.User, userids),
            request.queryString, META_URL)
        }
      }
      case Failure(er) => HandleExceptions(er)
    }  
  }
  
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
  
  def filterProvidersByType(rs: List[GestaltResourceInstance], qs: Map[String,Seq[String]]) = {
    if (qs.contains("type")) {
      
      val typeName = "Gestalt::Configuration::Provider::" + qs("type")(0)
      log.debug("Filtering providers for type : " + typeName)

      //
      // TODO: No longer necessary to do this - use .findWithVariance()
      //
      val typeId = typeName match {
        case a if a == Resources.ApiGatewayProvider.toString => ResourceIds.ApiGatewayProvider
        case b if b == Resources.MarathonProvider.toString => ResourceIds.MarathonProvider
        case c if c == Resources.LambdaProvider.toString => ResourceIds.LambdaProvider
        case e => throw new BadRequestException(s"Unknown provider type : '$e'")
      }
      rs filter { _.typeId == typeId }
    } else rs
  }
  
  def getGenericChildByIdFqon(targetTypeId: String, parentType: String, parentId: UUID, fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    getGenericChildById(targetTypeId, parentType, parentId, fqid(fqon), id)
  }
  
  def getGenericChildById(targetTypeId: String, parentType: String, parentId: UUID, org: UUID, id: UUID)(implicit request: SecuredRequest[AnyContent]) = {
    def notfound(label: String, id: UUID) = "%s with ID %s not found.".format(label, id)
    
    ResourceFactory.findById(parentType, parentId) match {
      case None => NotFoundResult(notfound(ResourceLabel(parentType), parentId))
      case Some(p) => {
        ResourceFactory.findById(targetTypeId, id) match {
          case None => NotFoundResult(notfound(ResourceLabel(targetTypeId), id))
          case Some(t) => Ok(Output.renderInstance(t, META_URL))
        }
      }
    }    
  }
  
  def getGenericChildByIdOrgFqon(targetTypeId: String, fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    val org = fqid(fqon)
    getGenericChildById(targetTypeId, ResourceIds.Org, org, org, id)
  }
  
  def getGenericChildAllOrgFqon(targetTypeId: String, fqon: String) = Authenticate(fqon) { implicit request =>
    handleExpansion(ResourceFactory.findChildrenOfType(uuid(targetTypeId), fqid(fqon)),
        request.queryString, META_URL)
  }


  def findWorkspaceEnvironment(envId: UUID) = Try {
    val p = ResourceFactory.findParent(ResourceIds.Workspace, envId) getOrElse {
      throw new ResourceNotFoundException(s"Could not find parent Workspace for Environment '$envId'.")
    }
    val c = ResourceFactory.findById(ResourceIds.Environment, envId) getOrElse {
      throw new ResourceNotFoundException(s"Environment with ID '$envId' not found.")
    }
    (p -> c)
  }



  def getEnvironmentContainersIdFqon(fqon: String, environment: UUID, containerId: UUID) = Authenticate(fqon) { implicit request =>
    ResourceFactory.findById(ResourceIds.Container, containerId) match {
      case None => NotFoundResult(s"Container with ID '$containerId' not found.")
      case Some(res) => Ok(Output.renderInstance(res, META_URL))
    }
  }


  /**
   * Get the Meta User corresponding with the caller identity.
   */
  def getUserSelf() = Authenticate() { implicit request =>
    val id = request.identity.account.id
    ResourceFactory.findById(ResourceIds.User, request.identity.account.id).fold {
      InternalServerError(
        s"User ID ${id.toString} was found in Security but not in Meta - this may be a synchronization error. Contact an administrator.")
    }{ user => Ok(Render(user)) }      
  }

  
  /**
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

  def getAllSystemResourcesByName(org: UUID, restName: String) = Authenticate(org)  { implicit request =>
    extractByName(org, restName) match {
      case Left(result) => result
      case Right((org, typeId)) => handleExpansion(ResourceFactory.findAll(typeId, org), request.queryString, META_URL)      
    }
  }
  
  def getAllSystemResourcesByNameId(org: UUID, restName: String, id: UUID) = Authenticate(org) { implicit request =>
    extractByName(org, restName) match {
      case Left(result) => result
      case Right((org, typeId)) => ResourceFactory.findById(typeId, id) match {
        case Some(res) => Ok(Output.renderInstance(res, META_URL))
        case None => NotFoundResult(request.uri)
      }
    }
  }
  
  def getAllSystemResourcesByNameIdFqon(fqon: String, restName: String, id: UUID) = Authenticate(fqon) { implicit request =>
    extractByName(fqon, restName) match {
      case Left(result) => result
      case Right((org, typeId)) => ResourceFactory.findById(typeId, id) match {
        case None      => NotFoundResult(request.uri)
        case Some(res) => {
          
          val action = s"${ActionPrefix(typeId)}.view"

          Authorize(res.id, action) {
            val output = res.typeId match {
              case u if res.typeId == ResourceIds.User => buildUserOutput(res)
              case _ => res
            }
            Ok(Output.renderInstance(output, META_URL))
          }
          
        }
      }
    }
  }  
  
  
  /**
   * Build the dynamic 'groups' property on a User instance.
   */
  private def buildUserOutput(res: GestaltResourceInstance)(implicit request: SecuredRequest[_]) = {
    Security.getAccountGroups(res.id,request.identity) match {
      case Success(gs) => {
        val gids = gs map { _.id.toString }
        val props = if (gids.isEmpty) None 
          else Some(res.properties.get ++ Map("groups" -> gids.mkString(",")))
        res.copy(properties = props)
      }
      case Failure(er) => throw new RuntimeException(s"Failed looking up groups for user '${res.id}': ${er.getMessage}")
    }
  }

  
  /* TODO: This is extremely temporary */
  private def referenceLinks(org: UUID, typeId: UUID): JsValue = {
    val refs = references.findAll(org, typeId) map { r => "{\"id\": \"%s\", \"name\": \"%s\"}".format(r.id, r.name) }
    Json.parse("[%s]".format(refs.mkString(",")))
  }
  
  
  /**
   * Get a List of ResourceLinks by FQON
   * [Implements]: GET /orgs/:fqon/:resources, i.e. /orgs/gf.engineering.core/workspaces
   */
  def getAllByFqon(fqon: String, resource: String) = Authenticate(fqon) { implicit securedRequest =>
    ResourceFactory.findByPropertyValue(ResourceIds.Org, "fqon", fqon) match {
      case Some(org) => {
        resourceUUID(resource) match {
          case Some(typeId) => Ok {
            Output.renderLinks(ResourceFactory.findAll(typeId))
          }
          case None => NotFoundResult(s"Invalid resource name '${resource}'.")
        }
      } // This shouldn't happen since security would send back 'unauthorized'
      case None => NotFound(s"Org FQON '${fqon}' does not exist.")
    }
  }
  
  
  // TODO: Do I use this to get a default 'owning-org' ???
  def getCurrentOrg(implicit client: GestaltSecurityClient): Future[GestaltOrg] = {
    client.get[GestaltOrg]("orgs/current")
  }
  
  
  def getResourcesByPathWithFqon(fqon: String, path: String) = Action.async {
    Future {
      Ok("RESOLVING : " + path)
    }
  }

  /**
   * 
   * With FQON matching String, we have to check if that String is a resource-type.
   * That means we have to prevent registration of an Org with a name that conflicts with our Resource names.
   * Is that acceptable?
   * 
   */

  // --------------------------------------------------------------------------
  // ORGS
  // --------------------------------------------------------------------------

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
  // WORKSPACES
  // --------------------------------------------------------------------------  

  def getAllWorkspaces(org: UUID) = Authenticate(org) { implicit request =>
    handleExpansion(ResourceFactory.findAll(ResourceIds.Workspace, org), request.queryString)  
  }
  
  def getAllWorkspacesFqon(fqon: String) = Authenticate(fqon) { implicit securedRequest =>
    orgFqon(fqon) match {
      case None => OrgNotFound(fqon)  
      case Some(org) => Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.Workspace, org.id)))
    }
  }

  def getWorkspaceById(org: UUID, id: UUID) = Authenticate(org) { implicit request =>
    ResourceFactory.findById(ResourceIds.Workspace, id) match {
      case Some(res) => Ok(Output.renderInstance(res))
      case None       => NotFoundResult(s"Workspace '${id}' not found.")
    }
  }
  
  def getWorkspaceByIdFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => ResourceFactory.findById(ResourceIds.Workspace, id) match {
        case Some(res) => Ok(Output.renderInstance(res))
        case None => NotFoundResult(Errors.RESOURCE_NOT_FOUND(id))
      }
      case None => OrgNotFound(fqon)
    }  
  }
  

  // --------------------------------------------------------------------------
  // APIS
  // --------------------------------------------------------------------------    
  def getApis(org: UUID) = Authenticate(org) { implicit request =>
    Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.Api, org), META_URL))
  }
  
  def getApisFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.Api, org.id), META_URL))
      case None => OrgNotFound(fqon)
    }
  }
  
  def getEnvironmentApis(org: UUID, environment: UUID) = Authenticate(org) { implicit request =>
    Ok(Output.renderLinks(ResourceFactory.findChildrenOfType(ResourceIds.Api, environment)))
  }
  
  def getEnvironmentApisFqon(fqon: String, environment: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => Ok(Output.renderLinks(ResourceFactory.findChildrenOfType(ResourceIds.Api, environment)))
      case None => OrgNotFound(fqon)
    }  
  }

  def getEnvironmentApiById(org: UUID, environment: UUID, id: UUID) = Authenticate(org) { implicit request =>
    ResourceFactory.findById(ResourceIds.Api, id) match {
      case Some(res) => Ok(Output.renderInstance(res))
      case None      => NotFoundResult(request.uri)
    }
  }
  
  def getEnvironmentApiByIdFqon(fqon: String, environment: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => {
        ResourceFactory.findById(ResourceIds.Api, id) match {
          case Some(res) => Ok(Output.renderInstance(res))
          case None => NotFoundResult(request.uri)
        }
      }
      case None => OrgNotFound(fqon)
    }
  }
  

  
  // --------------------------------------------------------------------------
  // LAMBDAS
  // --------------------------------------------------------------------------  
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
  
  def getLambda(id: UUID) = {
    
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
  
  
  def injectLambdaFunctionMap(res: GestaltResourceInstance) = Try {
    val lmap = ResourceFactory.getLambdaFunctionMap(res.id)
    val resJson = Json.toJson(res).as[JsObject]
    
    val newjson = if (lmap.isEmpty) resJson else {
        val smap = JsString(Json.toJson(lmap).toString)
        val m2 = replaceJsonPropValue(resJson, "function_mappings", smap)      
        replaceJsonProps(resJson, m2)
      } 
    
    newjson.validate[GestaltResourceInstance].get
  }
  
  def getEndpointsByLambda(org: UUID, lambda: UUID) = Authenticate(org) { implicit request =>
    trace(s"getEndpointsByLambda($org, $lambda)")
    handleExpansion(ResourceFactory.findEndpointsByLambda(lambda), request.queryString)
  }
  
  def getEndpointsByLambdaFqon(fqon: String, lambda: UUID) = Authenticate(fqon) { implicit request =>
    trace(s"getEndpointsByLambdaFqon($fqon, $lambda)")
    orgFqon(fqon) match {
      case Some(org) => handleExpansion(ResourceFactory.findEndpointsByLambda(lambda), request.queryString)
      case None => OrgNotFound(fqon)
    }
  }  
  
  // --------------------------------------------------------------------------
  // API_ENDPOINTS
  // --------------------------------------------------------------------------  
  def getEndpoints(org: UUID, parentType: UUID, qs: Map[String,Seq[String]]) = {
    handleExpansion(
        ResourceFactory.findChildrenOfType(ResourceIds.ApiEndpoint, parentType), qs)    
  }
  
  def getApiEndpoint(org: UUID, parent: UUID, id: UUID) = Authenticate(org) { implicit request =>
    // TODO: Use parent for validation - ensure exists, ensure is parent of endpoint...
    renderInstance(ResourceIds.ApiEndpoint, id)
  }
  
  def getApiEndpointFqon(fqon: String, parent: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    renderInstance(ResourceIds.ApiEndpoint, id)  
  }
  
  def getWorkspaceApiEndpoints(org: UUID, workspace: UUID) = Authenticate(org) { implicit request =>
    getEndpoints(org, workspace, request.queryString)
  }
  
  def getEnvironmentApiEndpoints(org: UUID, environment: UUID) = Authenticate(org) { implicit request =>
    getEndpoints(org, environment, request.queryString)
  }  
  
  
  def renderInstance(typeId: UUID, id: UUID)(implicit request: SecuredRequest[AnyContent]) = {
    ResourceFactory.findById(typeId, id) match {
      case Some(res) => Ok(Output.renderInstance(res))
      case None => NotFoundResult(request.uri)
    }
  }
  
  
  def getWorkspaceApiEndpointsFqon(fqon: String, workspace: UUID) = Authenticate(fqon) { implicit request =>
    trace(s"getWorkspaceApiEndpointsFqon($fqon, $workspace)")
    orgFqon(fqon) match {
      case Some(org) => getEndpoints(org.id, workspace, request.queryString)
      case None => OrgNotFound(fqon)
    }
  }
  
  
  def getEnvironmentApiEndpointsFqon(fqon: String, environment: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => getEndpoints(org.id, environment, request.queryString)
      case None => OrgNotFound(fqon)
    }
  }
  
  // --------------------------------------------------------------------------
  // ENVIRONMENTS
  // --------------------------------------------------------------------------   

  def getEnvironmentById(org: UUID, id: UUID) = Authenticate(org) { implicit request =>
    FindByIdResult(org, ResourceIds.Environment, id)
  }
  
  
  def FindByIdResult(org: UUID, typeId: UUID, id: UUID) = {
    ResourceFactory.findById(typeId, id) match {
      case Some(res) => Ok(Output.renderInstance(res))
      case None      => NotFoundResult(s"${ResourceLabel(typeId)} '${id}' not found.")
    }    
  }
  
  def getEnvironmentsByWorkspace(org: UUID, workspaceId: UUID) = Authenticate(org) { implicit request =>
    // Ensure workspace exists
    if (ResourceFactory.existsInOrg(org, workspaceId)) {
      handleExpansion(
          ResourceFactory.findAllByPropertyValueOrg(org, ResourceIds.Environment, "workspace", workspaceId.toString), 
          request.queryString, META_URL)
    } else {
      NotFoundResult(s"Workspace ID '$workspaceId' not found.")
    }
  }
  
  def getEnvironmentsByWorkspaceFqon(fqon: String, workspaceId: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case None => OrgNotFound(fqon)
      case Some(org) => handleExpansion(
          ResourceFactory.findAllByPropertyValueOrg(
              org.id, ResourceIds.Environment, "workspace", workspaceId.toString),
                request.queryString)
    }
  }
  
  
  def getWorkspaceProviders(org: UUID, workspace: UUID) = Authenticate(org) { implicit request =>
    // Get gateway and marathon provider types and merge list.
    val gateways = ResourceFactory.findChildrenOfType(ResourceIds.ApiGatewayProvider, workspace)
    val marathons = ResourceFactory.findChildrenOfType(ResourceIds.MarathonProvider, workspace)
    val providers = gateways ++ marathons
    
    handleExpansion(providers, request.queryString, META_URL)
  }
  
  
  def getWorkspaceDomains(org: UUID, workspace: UUID) = Authenticate(org) { implicit request =>
    handleExpansion(ResourceFactory.findChildrenOfType(ResourceIds.Domain, workspace), request.queryString)
  }
  
  
  def getWorkspaceDomainsFqon(fqon: String, workspace: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => handleExpansion(
          ResourceFactory.findChildrenOfType(ResourceIds.Domain, workspace), request.queryString)
      case None => OrgNotFound(fqon)
    }
  }
  
  
//  def getWorkspaceDomainById(org: UUID, workspace: UUID, id: UUID) = Authenticate(org) { implicit request =>
//    ResourceFactory.findById(ResourceIds.Domain, id) match {
//      case Some(res) => Ok(Output.renderInstance(res))
//      case None => NotFoundResult(request.uri)
//    }
//  }
//  
//  def getWorkspaceDomainByIdFqon(fqon: String, workspace: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
//    orgFqon(fqon) match {
//      case Some(org) =>
//        ResourceFactory.findById(ResourceIds.Domain, id) match {
//          case Some(res) => Ok(Output.renderInstance(res))
//          case None => NotFoundResult(request.uri)
//        }
//      case None => OrgNotFound(fqon)
//    } 
//  }  
  
  private def getById(org: UUID, resourceTypeId: UUID, id: UUID) = {
    okNotFound(qs.findById(org, resourceTypeId, id))
  }

  def instance2map[T: ClassTag: TypeTag](inst: T) = {
    val im = currentMirror.reflect( inst )
    @tailrec def loop(acc: Map[String,Any], symbols: List[MethodSymbol]): Map[String,Any] = {
      symbols match {
        case Nil => acc
        case h :: t => loop( acc + ((h.name.toString, im.reflectMethod( h ).apply().toString)), t )
      }
    }
    loop( Map(), getMethods[T] )
  }
  
  def getMethods[T: TypeTag]: List[MethodSymbol] = typeOf[T].members.collect {
    case m: MethodSymbol if m.isCaseAccessor => m
  }.toList  
  
}
