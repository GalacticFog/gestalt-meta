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

object ResourceController extends MetaController with NonLoggingTaskEvents {
  
  
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
  
  def getGenericTopLevel(targetTypeId: String) = Authenticate() { implicit request =>
    handleExpansion(ResourceFactory.findAll(uuid(targetTypeId)), request.queryString, META_URL)  
  }  
  
  def getOrg(org: UUID) = Authenticate(org) { implicit request =>
    getById(org, ResourceIds.Org, org)  
  }
  
  def getOrgFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    val orgId = fqid(fqon)
    getById(orgId, ResourceIds.Org, orgId)
  }  
  
  def getGenericAll(targetTypeId: String, org: UUID) = Authenticate(org) { implicit request =>
    handleExpansion(ResourceFactory.findAll(uuid(targetTypeId), org), request.queryString, META_URL)
  }
  
  def getGenericAllFqon(targetTypeId: String, fqon: String) = Authenticate(fqon) { implicit request =>
    handleExpansion(ResourceFactory.findAll(uuid(targetTypeId), fqid(fqon)), request.queryString, META_URL)
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
  
  def getProvidersFqon(fqon: String, parentType: String, parent: UUID) = Authenticate(fqon) { implicit request =>
    getProvidersCommon(fqid(fqon), parentType, parent, request.queryString, META_URL)
  }
  
  def getProvidersCommon(org: UUID, parentType: String, parent: UUID, qs: Map[String,Seq[String]], baseUrl: Option[String] = None) = {
    ResourceFactory.findById(parentType, parent) match {
      case None => NotFoundResult(s"${ResourceLabel(parentType)} with ID '${parent}' not found.")
      case Some(_) => {
        val filtered = filterProvidersByType(ResourceFactory.findAncestorProviders(parent), qs)
        handleExpansion(filtered, qs, baseUrl)
      }
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
    
//    def notfound(label: String, id: UUID) = "%s with ID %s not found.".format(label, id)
//    
//    ResourceFactory.findById(parentType, parentId) match {
//      case None => NotFoundResult(notfound(ResourceLabel(parentType), parentId))
//      case Some(p) => {
//        ResourceFactory.findById(targetTypeId, id) match {
//          case None => NotFoundResult(notfound(ResourceLabel(targetTypeId), id))
//          case Some(t) => Ok(Output.renderInstance(t, META_URL))
//        }
//      }
//    }
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

    //getGenericChildByIdFqon(targetTypeId, ResourceIds.Org, fqid(fqon), fqon, id)
    val org = fqid(fqon)
    getGenericChildById(targetTypeId, ResourceIds.Org, org, org, id)
    
//    def notfound(label: String, id: UUID) = "%s with ID %s not found.".format(label, id)
//    
//    ResourceFactory.findById(ResourceId, fqid(fqon)) match {
//      case None => NotFoundResult(notfound(ResourceLabel(parentType), parentId))
//      case Some(p) => {
//        ResourceFactory.findById(targetTypeId, id) match {
//          case None => NotFoundResult(notfound(ResourceLabel(targetTypeId), id))
//          case Some(t) => Ok(Output.renderInstance(t, META_URL))
//        }
//      }
//    }
  }   
  
  def getGenericChildAllOrgFqon(targetTypeId: String, fqon: String) = Authenticate(fqon) { implicit request =>
    handleExpansion(ResourceFactory.findChildrenOfType(uuid(targetTypeId), fqid(fqon)),
        request.queryString, META_URL)
  }    
  
  
  //def getGenericChildAll(targetTypeId: String, parentType: String, parentId: UUID, targetId: UUID) = Authenticate(org) { implicit request =>  
  
  private val qs = ResourceQueryService

  implicit lazy val serviceInfoFormat = Json.format[ServiceInfo]
  implicit lazy val aboutMetaFormat = Json.format[AboutMeta]
  
  case class AboutMeta(status: String, url: String, time: String, build_info: JsValue, services: Map[String,ServiceInfo])
  case class ServiceInfo(url: String, status: String)
  
  
  def about() = Authenticate() { implicit request =>
    val result = AboutMeta( 
        status     = "OK",
        url        = META_URL.get,
        time       = org.joda.time.DateTime.now.toString,
        build_info = Json.parse(BuildInfo.toJson), 
        services   = Map(
            "security"       -> ServiceInfo(url = EnvConfig.securityUrl, status = "OK"),
            "gateway"        -> ServiceInfo(url = EnvConfig.gatewayUrl, status = "OK"),
            "gestalt-lambda" -> ServiceInfo(url = EnvConfig.lambdaUrl, status = "OK"),
            "datastore"      -> ServiceInfo(url = EnvConfig.databaseUrl, status = "OK")))
            
    Ok(Json.toJson(result))
  }


  
  import com.galacticfog.gestalt.laser._
  

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

  def getEnvironmentContainersFqon2(fqon: String, environment: UUID) = Authenticate(fqon).async { implicit request =>
    if (!getExpandParam(request.queryString)) {
      // don't need to expand them, so we don't need to query marathon for status
      Future{Ok(Output.renderLinks(ResourceFactory.findChildrenOfType(ResourceIds.Container, environment), META_URL))}
    } else {
      MarathonController.appComponents(environment) match {
        case Failure(e) => Future.successful(HandleExceptions(e))
        case Success((wrk, env)) =>
          val appGroupPrefix = MarathonClient.metaContextToMarathonGroup(fqon, wrk.name, env.name)
          for {
            metaCons <- Future{ResourceFactory.findChildrenOfType(ResourceIds.Container, environment)}
            metaCon2guid = (for {
              con <- metaCons
              props <- con.properties
              providerProp <- props.get("provider")
              pobj <- Try{Json.parse(providerProp)}.toOption
              providerId <- (pobj \ "id").asOpt[UUID]
              eid <- props.get("external_id")
              localEid = eid.stripPrefix(appGroupPrefix).stripPrefix("/")
            } yield (con.id -> (providerId,localEid))).toMap
            relevantProviderIds = (metaCon2guid.values.map{_._1} toSeq).distinct
            relevantProviders = relevantProviderIds flatMap {
              pid => Try{MarathonController.marathonProvider(pid)}.toOption
            }
            _ = log.info(s"Querying ${relevantProviders.size} relevant Marathon providers")
            // id is only unique inside a marathon provider, so we have to zip these with the provider ID
            marCons <- Future.sequence(relevantProviders map { p =>
              MarathonController.marathonClient(p).listApplicationsInEnvironment(fqon, wrk.name, env.name) map {cs => cs.map {c => p.id -> c}}
            }) map {_.flatten}
            _ = log.info(s"Found ${marCons.size} total containers over relevant Marathon")
            mapMarCons = marCons.map(p => (p._1, p._2.id.stripPrefix("/")) -> p._2).toMap
            _ = log.trace(mapMarCons.keys.toString)
            renderedMetaContainers = metaCons map { metaCon =>
              val origRendering = Output.renderInstance(metaCon, META_URL).as[JsObject]
              (for {
                guid <- {
                  log.trace(s"looking for marathon container guid for meta container ${metaCon.id}")
                  metaCon2guid.get(metaCon.id)
                }
                marCon <- {
                  log.trace(s"looking for marathon container corresponding to guid ${guid}")
                  mapMarCons.get(guid)
                }
              } yield marCon) match {
                case Some(correspondingMarCon) =>
                  val updatedRendering = Seq(
                    "age" -> JsString(correspondingMarCon.age.toString),
                    "status" -> JsString(
                      if (metaCon.properties.exists(_.get("status").exists(_ == "MIGRATING"))) "MIGRATING"
                      else correspondingMarCon.status
                    )
                  ).foldLeft(origRendering) { (json, prop) => JsonUtil.withJsonPropValue(json, prop)}
                  log.trace("Updated rendering with marathon status: " + Json.prettyPrint(updatedRendering))
                  updatedRendering
                case None =>
                  log.trace("Original rendering with no marathon status: " + Json.prettyPrint(origRendering))
                  origRendering
              }
            }
          } yield Ok(Json.toJson(renderedMetaContainers))
      }
    }
  }

  def getEnvironmentContainersIdFqon(fqon: String, environment: UUID, containerId: UUID) = Authenticate(fqon) { implicit request =>
    ResourceFactory.findById(ResourceIds.Container, containerId) match {
      case None => NotFoundResult(s"Container with ID '$containerId' not found.")
      case Some(res) => Ok(Output.renderInstance(res, META_URL))
    }
  }

//  def getEnvContainers(fqon: String, environment: UUID, provider: UUID, qs: QueryString)(implicit request: SecuredRequest[AnyContent]) = {
//    log.debug(s"Entered ResourceController::getEnvContainers($fqon, $environment, $provider)")
//
//    val targets = Try {
//      val prv = ResourceFactory.findById(ResourceIds.MarathonProvider, provider) getOrElse {
//        throw new ResourceNotFoundException(s"MarathonProvider with ID '$provider' not found.")
//      }
//      val we = ResourceController.findWorkspaceEnvironment(environment).get
//      (we._1, we._2, prv)
//    }
//
//    targets match {
//      case Failure(e) => Future { HandleRepositoryExceptions(e) }
//      case Success((wrk, env, prv)) => {
//
//        log.debug("workspace   : %s - %s".format(wrk.id, wrk.name))
//        log.debug("environment : %s - %s".format(env.id, env.name))
//        log.debug("provider    : %s - %s".format(prv.id, prv.name))
//
//        val providerUrl = {
//          (Json.parse(prv.properties.get("config")) \ "url").as[String]
//        }
//        log.debug("provider.url: %s".format(providerUrl))
//
//        val marathonClient = MarathonClient(WS.client, providerUrl)
//        marathonClient.listApplicationsInEnvironment(fqon, wrk.name, env.name).map { cs =>
//          cs.map { toGestaltContainer(fqon, _, Some(prv)) }
//        }
//        .map { handleExpansion(_, request.queryString, META_URL) }
//        .recover { case e: Throwable => BadRequest(e.getMessage) }
//      }
//    }
//  }

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
    extractByName(fqon, restName) match {
      case Left(result) => result
      case Right((org, typeId)) => {
        
        // TODO: This is a hack for Lambda - getChildLambdas dynamically builds the endpoint function map.
        val resources = typeId match {
          case m if m == ResourceIds.Lambda => getDescendantLambdas(org)
          case _ => ResourceFactory.findAll(typeId, org)
        }
        handleExpansion(resources, request.queryString, baseUri = META_URL)
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

  def getEnvironmentById(org: UUID, id: UUID) = Authenticate(org) { implicit request =>
    FindByIdResult(org, ResourceIds.Environment, id)
  }
  
//  def getEnvironmentByIdFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
//    orgFqon(fqon) match {
//      case Some(org) => FindByIdResult(org.id, ResourceIds.Environment, id)
//      case None => OrgNotFound(fqon)
//    }
//  }
  
  
  def FindByIdResult(org: UUID, typeId: UUID, id: UUID) = {
    ResourceFactory.findById(typeId, id) match {
      case Some(res) => Ok(Output.renderInstance(res))
      case None       => NotFoundResult(s"${ResourceLabel(typeId)} '${id}' not found.")
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
  
  
  def getWorkspaceProviderByIdFqon(fqon: String, workspace: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case None => OrgNotFound(fqon)
      case Some(org) => {
        ResourceFactory.findById(id) match {
          case Some(res) => Ok(Output.renderInstance(res))
          case None => NotFoundResult(request.uri)
        }
      }
    }  
  }
  
  
  def filterProviders() = {
    
  }
  
  def getWorkspaceProvidersFqon(fqon: String, workspace: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => {
        val gateways = ResourceFactory.findChildrenOfType(ResourceIds.ApiGatewayProvider, workspace)
        val marathons = ResourceFactory.findChildrenOfType(ResourceIds.MarathonProvider, workspace)
        val qs = request.queryString
        
        val providers = if (qs.contains("type")) {
          val typeId = "Gestalt::Configuration::Provider::" + qs("type")(0) match {
            case a if a == Resources.ApiGatewayProvider.toString => ResourceIds.ApiGatewayProvider
            case b if b == Resources.MarathonProvider.toString   => ResourceIds.MarathonProvider
            case e => throw new BadRequestException(s"Unknown provider type : '$e'")
          }
          (gateways ++ marathons) filter { _.typeId == typeId }
        } else (gateways ++ marathons)
        
        handleExpansion(providers, request.queryString, META_URL)
      }
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
