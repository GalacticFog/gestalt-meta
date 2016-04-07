package controllers

import java.util.UUID

import com.galacticfog.gestalt.marathon._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.laser.MarathonClient
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator

import controllers.util._
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.ws.WS
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.laser._
import play.api.{ Logger => log }

object MarathonController extends GestaltFrameworkSecuredController[DummyAuthenticator]
  with MetaController with SecurityResources {

  type ProxyAppFunction = (String,String,String) => Future[JsValue]

  /**
    * GET /{fqon}/environments/{eid}/providers/{pid}/v2/deployments
    */
  def getDeployments(fqon: String, parentType: String, environment: UUID, providerId: UUID) = Authenticate(fqon).async { implicit request =>
    val provider = marathon(providerId)

    execAppFunction(fqon, parentType, environment, provider, "v2/deployments") {
      client(provider).listDeploymentsAffectingEnvironment_marathon_v2
    } map { Ok(_) } recover {
      case e: Throwable => BadRequest(e.getMessage)
    }
  }

  /**
   * GET /{fqon}/environments/{eid}/providers/{pid}/v2/info
   */
  def getInfo(fqon: String, environment: UUID, provider: UUID) = Authenticate(fqon).async { implicit request =>
    client(marathon(provider)).getInfo.map { Ok( _ ) } recover {
      case e: Throwable => HandleExceptions(e)
    }  
  }
  
  /**
    * DELETE /{fqon}/environments/{eid}/providers/{pid}/v2/apps
    */
  def delMarathonApps(fqon: String, parentType: String, environment: UUID, providerId: UUID, proxyUri: String) = Authenticate(fqon).async { implicit request =>

    val provider = marathon(providerId)

    // if this isn't formatted like v2/apps/app-id/..., allow it to fail below in execAppFunction
    val (newProxyUri,appId) = if (proxyUri.startsWith("v2/apps"))
      ("v2/apps",proxyUri.stripPrefix("v2/apps"))
    else
      (proxyUri,"")

    execAppFunction(fqon, parentType, environment, provider, newProxyUri) {
      client(provider).deleteApplication(_,_,_,appId)
    } map { Ok(_) } recover {
      case e: Throwable => BadRequest(e.getMessage)
    }
  }

  
  /**
   * GET /{fqon}/environments/{eid}/containers
   */
  def getMarathonAppsAll(fqon: String, parentType: String, environment: UUID) = Authenticate(fqon).async { implicit request =>
    
    def go(
        ps: Seq[GestaltResourceInstance], wrk: String, env: String,
        acc: Seq[Future[Seq[GestaltResourceInstance]]]): Seq[Future[Seq[GestaltResourceInstance]]] = {

      ps match {
        case Nil => acc
        case h #:: t => {
          val containers = client(h).listApplicationsInEnvironment(fqon, wrk, env).map { cs =>
            cs.map { ResourceController.toGestaltContainer(fqon, _, Some(h)) }  
          }
          go(t, wrk, env, acc :+ containers)
        }
      }
    }
    
    appComponents(environment) match {
      case Failure(e) => Future( HandleExceptions(e) )
      case Success((wrk, env)) => {
        
        val providers  = findAllMarathonProvidersInScope(environment)
        val containers = go(providers.values.toSeq, wrk.name, env.name, Seq())
        
        for {
          a <- (Future sequence containers)
          b = a.flatten
          c = handleExpansion(b, request.queryString, META_URL)
        } yield c
        
      }
    }
  }
  
  
  /**
   * GET /{fqon}/environments/{eid}/providers/{pid}/v2/apps
   */
  def getMarathonApps(fqon: String, parentType: String, environment: UUID, providerId: UUID, proxyUri: String) = Authenticate(fqon).async { implicit request =>

    val provider = marathon(providerId)

    execAppFunction(fqon, parentType, environment, provider, proxyUri) {
      client(provider).listApplicationsInEnvironment_marathon_v2
    } map { Ok(_) } recover {
      case e: Throwable => BadRequest(e.getMessage)
    }
  }
  
  
  /**
   * POST /{fqon}/environments/{eid}/providers/{pid}/v2/apps
   */  
  def postMarathonApps(fqon: String, parentType: String, environment: UUID, providerId: UUID, proxyUri: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    
    val provider = marathon(providerId)
    
    execAppFunction(fqon, parentType, environment, provider, proxyUri) {
      client(provider).launchContainer_marathon_v2(_,_,_,request.request.body.as[JsObject])
    } map { Created( _ ) } recover { 
      case e: Throwable => BadRequest(e.getMessage) 
    }
  }
  
  import scala.concurrent.{ ExecutionContext, ExecutionContext$, Future, Promise, Await }
  import scala.concurrent.duration._
  
  
  //
  // POST /{fqon}/environments/{eid}/providers/{pid}/v2/apps
  //
  def postMarathonApp(fqon: String, environment: UUID, providerId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    
    log.debug("RECEIVED :\n" + Json.prettyPrint(request.body))
    
    appComponents(environment) match {
      case Failure(e) => Future { HandleExceptions(e) }
      case Success((wrk,env)) => {
        
        // This initializes all required properties if missing.
        val inputJson = normalizeInputContainer(request.body)
        val name = requiredJsString("name", (inputJson \ "name"))
        val provider = marathon(providerId)
        
        // Create app in Marathon
        createMarathonApp(fqon, name, wrk.name, env.name, inputJson, provider) match {
          case Failure(e) => Future { HandleExceptions(e) }
          case Success(r) => {
            
            log.debug("Marathon App created:\n" + Json.prettyPrint(r))
            
            // Inject external_id property
            val marathonGroupId = groupId(fqon, wrk.name, env.name)
            val resourceJson = JsonUtil.withJsonPropValue(inputJson, "external_id", JsString(marathonGroupId))
            
            // Create app in Meta
            log.debug("Marathon-Group-ID : " + marathonGroupId)
            log.debug("Creating Container in Meta:\n" + Json.prettyPrint(resourceJson))
            //createResourceD(fqid(fqon), resourceJson, Some(ResourceIds.Container), Some(environment))
            Future { Ok("Testing create Container") }
          }
        }
      }
    
    }
  }
  
  def groupId(fqon: String, workspaceName: String, environmentName: String) = {
    MarathonClient.metaContextToMarathonGroup(fqon, workspaceName, environmentName)
  }
  
  def createMarathonApp(
      fqon: String, 
      appName: String, 
      workspaceName: String, 
      environmentName: String, 
      inputJson: JsObject, 
      provider: GestaltResourceInstance) = Try {
        
    // Create app in Marathon
    val app = toMarathonApp(appName, inputJson)
    val marathonPayload = Json.toJson(app).as[JsObject]
    
    log.debug("Creating App in Marathon:\n" + Json.prettyPrint(marathonPayload))
    Await.result(client(provider).launchContainer_marathon_v2(
      fqon, workspaceName, environmentName, marathonPayload), 5 seconds)    
  }
  
  private def execAppFunction(
      fqon: String, 
      parentType: String, 
      environment: UUID, 
      provider: GestaltResourceInstance, proxyUri: String)(fn: ProxyAppFunction) = {
    
    appComponents(environment) match {
      case Failure(e) => throw e
      case Success((parent, child)) => proxyUri match {
        case "v2/apps" =>        { fn(fqon, parent.name, child.name) }
        case "v2/deployments" => { fn(fqon, parent.name, child.name) }
        case e         => throw new BadRequestException(s"Unsupported Marathon URL : " + e)
      }
    }
  }
  
  private def marathon(provider: UUID) = {
    ResourceFactory.findById(ResourceIds.MarathonProvider, provider) getOrElse {
      throw new ResourceNotFoundException(s"MarathonProvider with ID '$provider' not found.")
    }
  }  
  
  private def appComponents(environment: UUID /*, provider: UUID*/) = Try {
    val we = ResourceController.findWorkspaceEnvironment(environment).get
    (we._1, we._2)  
  }
  
  private def client(provider: GestaltResourceInstance) = {
    val providerUrl = (Json.parse(provider.properties.get("config")) \ "url").as[String]
    MarathonClient(WS.client, providerUrl)
  }
  
  /**
   * Get a unique list (Set) of marathon providers in the given environment's scope.
   * Uniqueness is determined by provider URL.
   */
  private def findAllMarathonProvidersInScope(environment: UUID): Map[String,GestaltResourceInstance] = {
    
    def go(ps: Seq[GestaltResourceInstance], acc: Map[String, GestaltResourceInstance]): Map[String,GestaltResourceInstance] = {
      ps match {
        case Nil => acc
        case h :: t => {
          val url = (Json.parse(h.properties.get("config")) \ "url").as[String]
          val m = if (acc.contains(url)) acc else acc ++ Map(url -> h)
          go(t, m)
        }
      }
    }
    val providers = ResourceFactory.findAncestorProviders(environment) filter { p =>
      p.typeId == ResourceIds.MarathonProvider  
    }
    go(providers, Map())
  }  
  
  def normalizeInputContainer(inputJson: JsValue): JsObject = {
    val defaults = containerWithDefaults(inputJson)
    val newprops = (inputJson \ "properties").as[JsObject] ++ (Json.toJson(defaults).as[JsObject])
    inputJson.as[JsObject] ++ Json.obj("properties" -> newprops)
  }
  
  def normalizeContainerInput(json: JsValue, props: Option[InputContainerProperties] = None): Try[GestaltResourceInput] = {
    val defaults = props getOrElse containerWithDefaults(json)
    val newprops = (json \ "properties").as[JsObject] ++ (Json.toJson(defaults).as[JsObject])
    safeGetInputJson {  
      json.as[JsObject] ++ Json.obj("properties" -> newprops)
    }    
  }
  
  def containerIntake(json: JsValue): Try[GestaltResourceInput] = {
    val defaults = containerWithDefaults(json)
    val name = requiredJsString("name", (json \ "name"))

    val newprops = (json \ "properties").as[JsObject] ++ (Json.toJson(defaults).as[JsObject])
    safeGetInputJson {  
      json.as[JsObject] ++ Json.obj("properties" -> newprops)
    }
  }  


//    targets match {
//      case Failure(e) => Future { HandleRepositoryExceptions(e) } 
//      case Success((parent, child, provider)) => {
//        val providerUrl = (provider.properties.get("config") \ "url").as[String]
//        val marathonClient = MarathonClient(WS.client, providerUrl)
//        
//        (request.method, proxyUri) match {
//          case (HttpVerbs.GET, "v2/apps") =>
//            marathonClient.listApplicationsInEnvironment_marathon_v2(fqon, wrkName = parent.name, envName = child.name)
//              .map {Ok(_)}
//              .recover {case e: Throwable => BadRequest(e.getMessage)}
//          case (HttpVerbs.POST,"v2/apps") =>
//            marathonClient.launchContainer_marathon_v2(fqon, wrkName = parent.name, envName = child.name, marPayload = request.request.body.as[JsObject])
//              .map {Created(_)}
//              .recover {case e: Throwable => BadRequest(e.getMessage)}
//          case _ => Future { BadRequest(s"Unsupported provider URL: " + proxyUri) }
//        }        
//      }
//    }
//  }
  
}



