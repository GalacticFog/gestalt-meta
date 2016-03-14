package controllers


import play.api.{ Logger => log }

import play.api.Play.current
import play.api.libs.ws._
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import scala.concurrent.Future
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.mvc.RequestHeader
import play.api.mvc.AnyContent
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{ Try, Success, Failure }
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.sdk.{ ResourceLink => MetaLink }

import com.galacticfog.gestalt.meta.services.ResourceQueryService
import com.galacticfog.gestalt.tasks.io.TaskStatus
import com.galacticfog.gestalt.tasks.play.actors.TaskEventMessage
import com.galacticfog.gestalt.tasks.play.io._
import controllers.util._
import controllers.util.db._
import play.mvc.Result
import java.util.UUID
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltBaseAuthProvider
import com.galacticfog.gestalt.security.play.silhouette.GestaltSecuredController
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.mohiva.play.silhouette.api.services.AuthenticatorService
import com.mohiva.play.silhouette.impl.authenticators.{ DummyAuthenticatorService, DummyAuthenticator }
import com.galacticfog.gestalt.security.api.{GestaltResource => SecurityResource}
import com.galacticfog.gestalt.security.api.{ResourceLink => SecurityLink}

import com.galacticfog.gestalt.security.api._
import com.galacticfog.gestalt.security.api.json.JsonImports
import play.api.libs.json._
import com.galacticfog.gestalt.security.api.json.JsonImports.{ orgFormat, linkFormat, acctFormat }
import com.mohiva.play.silhouette.api.util.Credentials

import com.galacticfog.gestalt.meta.api.output._ //JsonImports._

import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.security.api.{ GestaltResource => SecuredResource }
import com.galacticfog.gestalt.meta.api.errors._

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import controllers.util.JsonUtil._
import com.galacticfog.gestalt.laser._


object ResourceController extends MetaController with NonLoggingTaskEvents {

  private val qs = ResourceQueryService

  /**
   * Get a list of all Orgs in Meta (global)
   */
  def getAllOrgs = Authenticate() { implicit request =>
    Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.Org)))
  }

  def getGroupsFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    ???  
  }
  
  
  import com.galacticfog.gestalt.laser._
  import scala.reflect.runtime.{ universe => ru }
  import scala.reflect.runtime.currentMirror
  import scala.reflect.ClassTag
  import ru.TypeTag
  import ru.MethodSymbol
  import ru.typeOf
  import scala.collection.immutable.ListMap

import scala.annotation.tailrec  
  
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

  def getContainersFqon(fqon: String) = Authenticate().async { implicit request =>
    orgFqon(fqon) match {
      case None => Future { OrgNotFound(fqon) }
      case Some(org) => getContainers(org.id, request.queryString)
    }
  }
  
  def getEnvironmentContainersFqon(fqon: String, environment: UUID) = Authenticate(fqon).async { implicit request =>
    trace(s"getEnvironmentContainersFqon($fqon, $environment)")
    orgFqon(fqon) match {
      case None => Future { OrgNotFound(fqon) }
      case Some(org) => {
        getContainers(org.id, request.queryString)
//        val out = GestaltResourceInstance(
//          id = UUID.randomUUID(),
//          typeId = ResourceIds.Container,
//          orgId = org.id,
//          owner = ResourceOwnerLink(ResourceIds.User, UUID.randomUUID),
//          name = UUID.randomUUID.toString)
//
//        Future { Ok(Output.renderInstance(out, META_URL)) }
//        val marathonClient = MarathonClient(
//          WS.client,
//          current.configuration.getString("marathon.url") getOrElse "http://v2.galacticfog.com:8080" stripSuffix ("/"))
//
//        val outs = marathonClient.listApplications map { s =>
//          s map { i =>
//            out.copy(
//              name = i.service + "-" + UUID.randomUUID.toString, created = None, modified = None,
//              properties = Some(instance2map(i).asInstanceOf[Map[String, String]]))
//          }
//        }
//        outs map { s => handleExpansion(s, request.queryString, META_URL) }
      }
    }
  }
  
  def getContainers(org: UUID, qs: Map[String,Seq[String]])(implicit request: SecuredRequest[AnyContent]) = {
    val out = GestaltResourceInstance(
      id = UUID.randomUUID(),
      typeId = ResourceIds.Container,
      orgId = org,
      owner = ResourceOwnerLink(ResourceIds.User, UUID.randomUUID),
      name = UUID.randomUUID.toString)

    Future { Ok(Output.renderInstance(out, META_URL)) }
    val marathonClient = MarathonClient(
      WS.client,
      current.configuration.getString("marathon.url") getOrElse "http://v2.galacticfog.com:8080" stripSuffix ("/"))

    val outs = marathonClient.listApplications map { s =>
      s map { i =>
        out.copy(
          name = i.service + "-" + UUID.randomUUID.toString, created = None, modified = None,
          properties = Some(instance2map(i).asInstanceOf[Map[String, String]]))
      }
    }
    outs map { s => handleExpansion(s, request.queryString, META_URL) }    
  }
  
  /**
   * Get a single Org
   */
  def getOrgById(id: UUID) = Authenticate(id) { implicit securedRequest =>
    trace(s"getByOrgId($id)")
    getById(org = id, ResourceIds.Org, id)
  }

  def getOrgByFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    trace(s"getOrgByFqon($fqon)")
    orgFqon(fqon) match {
      case Some(org) => getById(org = org.id, ResourceIds.Org, org.id)
      case None => OrgNotFound(fqon)
    }
  }
  
  
  /**
   * Get a list of child Orgs 
   */
  def getChildOrgs(org: UUID) = Authenticate(org) { implicit request =>
    getOrg(org) match {
      case Some(_) => childOrgs(org)
      case None    => OrgNotFound(org)
    }
  }

  def getChildOrgsFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => {
        childOrgs(org.id)
        handleExpansion(ResourceFactory.findChildrenOfType(ResourceIds.Org, org.id), request.queryString)
      }
      case None      => OrgNotFound(fqon)
    }
  }

  /**
   * Get a list of all users in the system (global)
   */
  def getAllUsersGlobal = Authenticate() { implicit request =>
    Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.User)))
  }

  /**
   * Get a list of all Users in the given Org
   */
  def getAllUsers(org: UUID) = Authenticate(org) { implicit request =>
    Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.User, org)))
  }

  def getAllUsersFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => 
        handleExpansion(ResourceFactory.findAll(ResourceIds.User, org.id), request.queryString) //Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.User, org.id)))
      case None => OrgNotFound(fqon)
    }
  }
  
  /**
   * Get the Meta User corresponding with the caller identity.
   * NOTE: The 'empty' call to Autheticate means the caller MUST be able
   * to authenticate against the 'root' Org.
   */
  def getUserSelf() = Authenticate() { implicit request =>
    val id = request.identity.account.id
    ResourceFactory.findById(ResourceIds.User, id) match {
      case Some(user) => Ok(Output.renderInstance(user))
      case None => InternalServerError(
          s"User ID ${id.toString} was found in Security but not in Meta - this may be a synchronization error. Contact an administrator.")
    }  
  }
  
  /**
   * Get a single user by ID
   */
  def getUserById(org: UUID, id: UUID) = Authenticate(org) { implicit request =>
    ResourceFactory.findById(ResourceIds.User, id) match {
      case Some(user) => Ok(Output.renderInstance(user))
      case None       => NotFoundResult(s"User '${id}' not found.")
    }
  }

  def getUserByIdFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => ResourceFactory.findById(ResourceIds.User, id) match {
        case Some(user) => Ok(Output.renderInstance(user))
        case None       => NotFoundResult(s"User '${id}' not found.")
      }
      case None => OrgNotFound(fqon)
    } 
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
  
  val resources = ResourceFactory
  val references = ReferenceFactory
  
  def resourceLinks(org: UUID, typeId: UUID, baseurl: Option[String] = None): JsValue = {
    Output.renderLinks(resources.findAll(typeId, org), baseurl)
  }
  
  def getAllSystemResourcesByName(org: UUID, restName: String) = Authenticate(org)  { implicit request =>
    extractByName(org, restName) match {
      case Left(result) => result
      case Right((org, typeId)) => handleExpansion(ResourceFactory.findAll(typeId, org), request.queryString, META_URL)      
    }
  }
  
  def getAllSystemResourcesByNameFqon(fqon: String, restName: String) = Authenticate(fqon) { implicit request =>
    trace(s"getAllSystemResourceByNameFqon($fqon, $restName)")
    extractByName(fqon, restName) match {
      case Left(result) => result
      case Right((org, typeId)) => {
        handleExpansion(ResourceFactory.findAll(typeId, org), request.queryString, baseUri = META_URL)
      }
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
    trace(s"getAllSystemResourcesByNameIdFqon($fqon, $restName, $id)")

    extractByName(fqon, restName) match {
      case Left(result) => result
      case Right((org, typeId)) => ResourceFactory.findById(typeId, id) match {
        case Some(res) => Ok(Output.renderInstance(res, META_URL))
        case None => NotFoundResult(request.uri)
      }
    }
  }  
  
  private def renderResourceLinks[T](org: UUID, typeId: UUID, url: Option[String]) = {
    if (references.isReferenceType(typeId)) {
      //"REFERENCE_TYPE"
      referenceLinks(org, typeId)
    }
    else {
      resourceLinks(org, typeId, url)
    }
  }
  
  /* TODO: This is extremely temporary */
  private def referenceLinks(org: UUID, typeId: UUID): JsValue = {
    val refs = references.findAll(org, typeId) map { r => "{\"id\": \"%s\", \"name\": \"%s\"}".format(r.id, r.name) }
    Json.parse("[%s]".format(refs.mkString(",")))
  }
  
  
  /**
   * Get a List of ResourceLinks by Org UUID
   * [Implements]: GET /orgs/:uuid/:resources, i.e. /orgs/:uuid/workspaces
   */
//  def getAllByOrgId(org: UUID, resource: String) = GestaltFrameworkAuthAction(Some(org)) { implicit securedRequest =>
//    trace(s"getAllByOrgId($org, $resource)")
//    resourceUUID(resource) match {
//      case Some(id) => getAll(org, id)  
//      case None => NotFound(s"Invalid resource-type: $resource")
//    }
//  }

  
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
    Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.Workspace, org)))  
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
  
  /*
   * TODO: This only handles true | false. Extend to allow for expansion
   * of individual resource attributes and properties.
   */
  def getExpandParam(qs: Map[String,Seq[String]]): Boolean = {
    if (!qs.contains("expand")) false
    else {
      val fp = qs("expand")
      Try {
        fp.mkString.toBoolean
      } match {
        case Success(b) => b == true
        case Failure(_) => throw new BadRequestException(s"Value of 'force' parameter must be true or false. found: $fp")
      }
    }
  }  
  
  /*
   * TODO: This could be much simpler if Output.render* returned JsValue instead of String.
   */
  def handleExpansion(rs: Seq[GestaltResourceInstance], qs: Map[String,Seq[String]], baseUri: Option[String] = None) = {
    if (getExpandParam(qs)) {
      Ok(Json.toJson(rs map { r => Output.renderInstance(r, baseUri) }))
    }
    else Ok(Output.renderLinks(rs, baseUri))
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
    trace(s"getWorkspaceLambdas($org, $workspace)")
    handleExpansion(
        ResourceFactory.findChildrenOfType(ResourceIds.Lambda, workspace), request.queryString,
        META_URL)
  }
  
  def getWorkspaceLambdasFqon(fqon: String, workspace: UUID) = Authenticate(fqon) { implicit request =>
    trace(s"getWorkspaceLambdasFqon($fqon, $workspace)")
    val lambdas = ResourceFactory.findChildrenOfType(ResourceIds.Lambda, workspace) map { 
      injectLambdaFunctionMap(_).get
    }
    handleExpansion(lambdas, request.queryString, META_URL)
  }
  
  
  def getChildLambdas(parent: UUID) = {
    ResourceFactory.findChildrenOfType(ResourceIds.Lambda, parent) map { 
      injectLambdaFunctionMap(_).get
    }    
  }
  
  def getLambda(id: UUID) = {
    
  }
  
  def getEnvironmentLambdas(org: UUID, environment: UUID) = Authenticate(org) { implicit request =>
    trace(s"getEnvironmentLambdas($org, $environment)")
    handleExpansion(getChildLambdas(environment), request.queryString, META_URL)    
  }  
  
  def getEnvironmentLambdasFqon(fqon: String, environment: UUID) = Authenticate(fqon) { implicit request =>
    trace(s"getEnvironmentLambdasFqon($fqon, $environment)")
    handleExpansion(getChildLambdas(environment), request.queryString)      
  }
  
  def getEnvironmentLambdaByIdFqon(fqon: String, environment: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    trace(s"getEnvironmentLambdaByIdFqon($fqon, $environment, $id)")
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
    trace(s"getEnvironmentLambdaById($org, $environment, $id)")
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
    val ps = Json.toJson(res.properties.get).as[JsObject]
    val ljson = Json.toJson(res).as[JsObject]
    val smap = JsString(Json.toJson(lmap).toString)
    val m2 = replaceJsonPropValue(ljson, "function_mappings", smap)

    println("OLD-PROPS :\n" + Json.prettyPrint(ps))
    println("NEW-PROPS :\n" + Json.prettyPrint(m2))
    println("OBJECT : \n" + Json.prettyPrint(replaceJsonProps(ljson, m2)))
    println

    val newjson = replaceJsonProps(ljson, m2)
    val newobj = newjson.validate[GestaltResourceInstance]
    println
    println(newobj)
    println
    
    // TODO: Check for JsError
    newobj.get
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
  def getAllEnvironments(org: UUID) = Authenticate(org) { implicit request =>
    ???
  }
  
  def getAllEnvironmentsFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    ???
  }
  
  def getEnvironmentById(org: UUID, id: UUID) = Authenticate(org) { implicit request =>
    FindByIdResult(org, ResourceIds.Environment, id)
  }
  
  def getEnvironmentByIdFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => FindByIdResult(org.id, ResourceIds.Environment, id)
      case None => OrgNotFound(fqon)
    }
  }
  
  
  def FindByIdResult(org: UUID, typeId: UUID, id: UUID) = {
    ResourceFactory.findById(typeId, id) match {
      case Some(res) => Ok(Output.renderInstance(res))
      case None       => NotFoundResult(s"${ResourceLabel(typeId)} '${id}' not found.")
    }    
  }
  
  def getEnvironmentsByWorkspace(org: UUID, workspaceId: UUID) = Authenticate(org) { implicit request =>
    // Ensure workspace exists
    if (ResourceFactory.existsInOrg(org, workspaceId)) {
      Ok(Output.renderLinks(ResourceFactory.findAllByPropertyValueOrg(org, ResourceIds.Environment, "workspace", workspaceId.toString)))
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
  
  def putEnvironment(org: UUID, id: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    ???
  }
  
  def postEnvironmentFqon(fqon: String, id: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    ???
  }
  
  def postEnvironment(org: UUID, id: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    ???
  }  
  
  def putEnvironmentFqon(fqon: String, id: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    ???
  }
  
  def patchEnvironment(org: UUID, id: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    ???
  }
  
  def patchEnvironmentFqon(fqon: String, id: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    ???
  }
  
  def deleteEnvironment(org: UUID, id: UUID) = Authenticate(org) { implicit request =>
    ???
  }

  def deleteEnvironmentFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    ???
  }  
  
  
  
  // --------------------------------------------------------------------------
  // MACHINE_SPECS
  // --------------------------------------------------------------------------
  
  
  // --------------------------------------------------------------------------
  // SERVICES
  // --------------------------------------------------------------------------  
  

  // --------------------------------------------------------------------------
  // NODE_TEMPLATES
  // --------------------------------------------------------------------------
  
  
  // --------------------------------------------------------------------------
  // CLUSTER_TEMPLATES
  // --------------------------------------------------------------------------
  
  
  // --------------------------------------------------------------------------
  // BLUEPRINTS
  // --------------------------------------------------------------------------  
  
  
  object AuthorizationHandler {
    def getSingle(org: UUID, typeId: UUID, id: UUID, account: AuthAccountWithCreds) = {
      //
      // TODO: Perform Authorization Checking Here! Use AuthMap
      // 
      val identities = account.account +: account.groups map { _.id }
      getById(org, typeId, id)
    }
  }

  //  def createWorkspace() = Action.async(parse.json) { implicit request =>
  //    Future {
  //      val event = newPostEvent(TaskStatus.Pending)
  //            .withActionArgs(Map(
  //                "org_id"   -> "6add206f-58e5-43c4-b958-787879ab2d6a", 
  //                "owner_id" -> "ee7b94e2-5f0c-4b23-8d45-c18de9116c1f") )
  //      raiseTaskEvent {
  //        TaskEventMessage( event ) {
  //          ResourceCommandService.createResource
  //        }
  //      }
  //      Accepted( event.task.toJson )
  //    }  
  //  }
  
  
  def getWorkspaceProviders(org: UUID, workspace: UUID) = Authenticate(org) { implicit request =>
    ResourceFactory.findChildrenOfType(ResourceIds.ApiGatewayProvider, workspace)
    Ok(Output.renderLinks(ResourceFactory.findAll(ResourceIds.ApiGatewayProvider, org)))
  }
  
  
  def getWorkspaceProviderByIdFqon(fqon: String, workspace: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => {
        ResourceFactory.findById(id) match {
          case Some(res) => Ok(Output.renderInstance(res))
          case None => NotFoundResult(request.uri)
        }
      }
    }  
  }
  
  
  def getWorkspaceProvidersFqon(fqon: String, workspace: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => handleExpansion(
            ResourceFactory.findChildrenOfType(ResourceIds.ApiGatewayProvider, workspace), request.queryString)
      case None => OrgNotFound(fqon)
    }
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
  
  
  def getWorkspaceDomainById(org: UUID, workspace: UUID, id: UUID) = Authenticate(org) { implicit request =>
    ResourceFactory.findById(ResourceIds.Domain, id) match {
      case Some(res) => Ok(Output.renderInstance(res))
      case None => NotFoundResult(request.uri)
    }
  }
  
  def getWorkspaceDomainByIdFqon(fqon: String, workspace: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) =>
        ResourceFactory.findById(ResourceIds.Domain, id) match {
          case Some(res) => Ok(Output.renderInstance(res))
          case None => NotFoundResult(request.uri)
        }
      case None => OrgNotFound(fqon)
    } 
  }  
  
  
  def getAllInOrg(typeId: UUID, org: UUID): List[GestaltResourceInstance] = {
    ResourceFactory.findAll(typeId, org)
  }
  
  
  private def getOrg(id: UUID) = {
    ResourceFactory.findById(ResourceIds.Org, id)
  }

  private def childOrgs(org: UUID) = {
    Ok(Output.renderLinks(ResourceFactory.findChildrenOfType(ResourceIds.Org, org)))
  }
  
  private def getById(org: UUID, resourceTypeId: UUID, id: UUID) = {
    okNotFound(qs.findById(org, resourceTypeId, id))
  }

}
