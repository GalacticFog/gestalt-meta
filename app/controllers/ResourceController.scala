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

import com.galacticfog.gestalt.meta.api.BuildInfo


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
  
  
  def getOrg(org: UUID) = Authenticate(org) { implicit request =>
    Authorize(org, Actions.Org.View, request.identity) {
      getById(org, ResourceIds.Org, org)  
    }
  }
  

  def getOrgFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    val org = fqid(fqon)
    
    Authorize(org, Actions.Org.View, request.identity) {    
      getById(org, ResourceIds.Org, org)
    }
  }
  
  def getGenericAll(targetTypeId: String, org: UUID) = Authenticate(org) { implicit request =>
    val allMinusSelf = ResourceFactory.findAll(uuid(targetTypeId), org) filterNot(_.id == org)
    handleExpansion(allMinusSelf, request.queryString, META_URL)
  }
  
  def getGenericAllFqon(targetTypeId: String, fqon: String) = Authenticate(fqon) { implicit request =>
    val id = fqid(fqon)
    AuthorizeList(Actions.Org.View) {
      ResourceFactory.findAll(uuid(targetTypeId), id) filterNot(_.id == id)
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
  
  def getTypeActionsFqon(fqon: String, typeId: UUID) = Authenticate(fqon) { implicit request =>
//    handleExpansion(
//        ResourceFactory.findChildrenOfSubType(ResourceIds.Action, typeId),
//        request.queryString, META_URL)
    ???
  }
  
  def getTypeActionByIdFqon(fqon: String, typeId: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    ???  
  }
  
  def getEnvVariablesOrgFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    val org = fqid(fqon)
    Ok(Json.toJson(getEnvVariablesCommon(org, org)))
  }
  
  def getEnvVariablesFqon(fqon: String, typeId: String, id: UUID) = Authenticate(fqon) { implicit request =>
    ResourceFactory.findById(UUID.fromString(typeId), id) match {
      case None => NotFoundResult(request.uri)
      case Some(_) => Ok(Json.toJson(getEnvVariablesCommon(fqid(fqon), id)))
    }
  }
  
  /*
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
        
        Ok(Json.toJson(mergeBottomUp(all)))        
      }
    }
    
  }
  
  
  
  protected def merge(map1: Map[String,String], map2: Map[String,String]): Map[String,String] = {
    
    def safeAdd(m: Map[String,String], key: String, value: String) = {
      if (!m.contains( key )) m ++ Map(key -> value) else m
    }
    
    def loop(rs: List[(String,String)], acc: Map[String,String]): Map[String,String] = {
      rs match {
        case Nil    => acc
        case h :: t => loop( t, safeAdd( acc, h._1, h._2 ) )
      }      
    }
    if ( map2.isEmpty ) map1 else {
      loop( map2.toList, map1 )
    }
  }
  
    
  def mergeBottomUp(vs: Seq[(Int,Map[String,String])]): Map[String,String] = {

    def go(vars: Seq[(Int,Map[String,String])], acc: Map[String,String]): Map[String,String] = {
      vars match {
        case Nil => acc
        case h :: t => go(t, merge(acc, h._2))
      }    
    }
    go(vs.sortWith((a,b) => a._1 < b._1), Map())
  }
  
  
  def getEnvVariablesCommon(org: UUID, instanceId: UUID) = {
    
    val rs = ResourceFactory.findEnvironmentVariables(instanceId)
    
    val all = rs map { case (k,v) =>
      if (v.properties.isEmpty) None
      else v.properties.get.get("env") match {
          case None => None
          case Some(vars) => {
            Option(k -> Json.parse(vars).validate[Map[String,String]].get)
        }
      }
    } filter { _.isDefined } flatMap { v => v }
    
    mergeBottomUp(all)
  }
  
  
  def getGroupsFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    handleExpansion(
      ResourceFactory.findAll(ResourceIds.Group, fqid(fqon)),
      request.queryString, 
      META_URL)
  }
  
  def getGroupByIdFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    ResourceFactory.findById(ResourceIds.Group, id).fold(ResourceNotFound(ResourceIds.Group, id)) {
      r => 
          Authorize(fqid(fqon), "group.view") {    
            //getById(org, ResourceIds.Org, org)
            //???
          
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
          case Failure(err) => {
            println("ERROR Getting group accounts : " + err)
            HandleExceptions(err)
          }
        }
      
      } // AuthorizeByid
          
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
  


  def getContainersFqon(fqon: String) = Authenticate().async { implicit request =>
    orgFqon(fqon) match {
      case None => Future { OrgNotFound(fqon) }
      case Some(org) => getContainers(org.id, request.queryString)
    }
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

  def futureToFutureTry[T](f: Future[T]): Future[Try[T]] = f.map(Success(_)).recover({case x => Failure(x)})

  def getEnvironmentContainersFqon2(fqon: String, environment: UUID) = Authenticate(fqon).async { implicit request =>
    if (!getExpandParam(request.queryString)) {
      // don't need to expand them, so we don't need to query marathon for status
      Future{Ok(Output.renderLinks(ResourceFactory.findChildrenOfType(ResourceIds.Container, environment), META_URL))}
    } else {
      // make a best effort to get updated stats from the Marathon provider and to update the resource with them
      MarathonController.appComponents(environment) match {
        case Failure(e) => Future.successful(HandleExceptions(e))
        case Success((wrk, env)) =>
          val appGroupPrefix = MarathonClient.metaContextToMarathonGroup(fqon, wrk.name, env.name)
          for {
            metaCons <- Future{ResourceFactory.findChildrenOfType(ResourceIds.Container, environment)}
            // map from meta resource container UUID to a Marathon container guid: (providerId, external_id)
            metaCon2guid = (for {
              metaCon <- metaCons
              props <- metaCon.properties
              providerProp <- props.get("provider")
              pobj <- Try{Json.parse(providerProp)}.toOption
              providerId <- (pobj \ "id").asOpt[UUID]
              eid <- props.get("external_id")
              // MarathonClient.listApplicationsInEnvironment strips env app group prefix
              localEid = eid.stripPrefix(appGroupPrefix).stripPrefix("/")
            } yield (metaCon.id -> (providerId,localEid))).toMap
            // all the providers we care about
            relevantProviders = (metaCon2guid.values.map{_._1} toSeq).distinct.flatMap {
              pid => Try{MarathonController.marathonProvider(pid)}.toOption
            }
            // id is only unique inside a marathon provider, so we have to zip these with the provider ID
            triedContainerListings <- Future.traverse(relevantProviders)({ pid =>
              val marCons = MarathonController.marathonClient(pid).listApplicationsInEnvironment(fqon, wrk.name, env.name)
              val pidsAndMarCons = marCons map {cs => cs.map {c => pid.id -> c}}
              // wrap this in a try so that failures don't crash the whole list in Future.traverse
              futureToFutureTry(pidsAndMarCons)
            })
            successfulContainerListings = triedContainerListings collect {case Success(x) => x}
            marCons = successfulContainerListings.flatten
            _ = log.trace(s"Found ${marCons.size} total containers over ${relevantProviders.size} Marathon providers")
            mapMarCons = marCons.map(p => (p._1, p._2.id.stripPrefix("/")) -> p._2).toMap
            outputMetaContainers = metaCons map { originalMetaCon =>
              val stats = for {
                guid <- metaCon2guid.get(originalMetaCon.id)
                marCon <- mapMarCons.get(guid)
              } yield marCon
              MarathonController.updateMetaContainerWithStats(originalMetaCon, stats, request.identity.account.id)
            }
          } yield Ok(Json.toJson(outputMetaContainers map {Output.renderInstance(_, META_URL).as[JsObject]}))
      }
    }
  }

  def getEnvironmentContainersIdFqon(fqon: String, environment: UUID, containerId: UUID) = Authenticate(fqon) { implicit request =>
    ResourceFactory.findById(ResourceIds.Container, containerId) match {
      case None => NotFoundResult(s"Container with ID '$containerId' not found.")
      case Some(res) => Ok(Output.renderInstance(res, META_URL))
    }
  }


  /**
   * Convert ContainerApp to GestaltResourceInstance
   * TODO: this is some leftover stuff that precedes Marathon container persistence (note the randomly assigned UUID below)
   */
  def toGestaltContainer(fqon: String, c: ContainerStats, provider: Option[GestaltResourceInstance] = None) = {

    // If given, inject properties.provider = providerName
    val providerProps = provider match {
      case None => Map()
      case Some(p) => {
        Map("provider" -> Json.stringify(Json.obj("id" -> p.id.toString, "name" -> p.name)))
      }
    }
    
    val props = Some(instance2map(c).asInstanceOf[Map[String,String]] ++ providerProps)
    
    // drop leading slash from service name, and urlencode the rest in case 
    // there are embedded slashes.
    val containerName = java.net.URLEncoder.encode(c.id.trim.stripPrefix("/"), "utf-8")
    
    GestaltResourceInstance(
      id = UUID.randomUUID(),
      typeId = ResourceIds.Container,
      orgId = fqid(fqon),
      owner = ResourceOwnerLink(ResourceIds.User, UUID.randomUUID),
      name = UUID.randomUUID.toString).copy(
              name = containerName, 
              created = None, modified = None,
              properties = props)
  }
  
  /**
   * TODO: This is still used by /{org}/containers - delete when that endpoint goes away.
   */
  def getContainers(org: UUID, qs: Map[String,Seq[String]])(implicit request: SecuredRequest[AnyContent]) = {
    val out = GestaltResourceInstance(
      id = UUID.randomUUID(),
      typeId = ResourceIds.Container,
      orgId = org,
      owner = ResourceOwnerLink(ResourceIds.User, UUID.randomUUID),
      name = UUID.randomUUID.toString)

    Future { Ok(Output.renderInstance(out, META_URL)) }

    val marathonClient = MarathonClient(
      WS.client, EnvConfig.marathonUrl stripSuffix ("/"))
    
    val outs = marathonClient.listApplications map { s =>
      s map { i =>
        out.copy(
          name = i.id + "-" + UUID.randomUUID.toString, created = None, modified = None,
          properties = Some(instance2map(i).asInstanceOf[Map[String, String]]))
      }
    }
    outs map { s => handleExpansion(s, request.queryString, META_URL) }    
  }
  
  
  /**
   * Get the Meta User corresponding with the caller identity.
   * NOTE: The 'empty' call to Authenticate means the caller MUST be able
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
  
  
  def filterProviders() = {
    ???
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
  
  
  private def getById(org: UUID, resourceTypeId: UUID, id: UUID) = {
    okNotFound(qs.findById(org, resourceTypeId, id))
  }
  
  
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
  
}
