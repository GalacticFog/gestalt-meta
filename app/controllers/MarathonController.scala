package controllers

import java.util.UUID

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

import controllers.util.MetaController
import play.api.Play.current
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.ws.WS


object MarathonController extends GestaltFrameworkSecuredController[DummyAuthenticator]
  with MetaController with SecurityResources {

  type ProxyAppFunction = (String,String,String) => Future[JsValue]

  
  /**
   * GET /{fqon}/environments/{eid}/providers/{pid}/v2/info`
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

  def findAllMarathonProvidersInScope(environment: UUID): Map[String,GestaltResourceInstance] = {
    
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
    go(ResourceFactory.findAncestorProviders(environment), Map())
  }
  
  def getMarathonAppsAll(fqon: String, parentType: String, environment: UUID, providerId: UUID, proxyUri: String) = Authenticate(fqon) { implicit request =>
    ???
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
  
  def execAppFunction(
      fqon: String, 
      parentType: String, 
      environment: UUID, 
      provider: GestaltResourceInstance, proxyUri: String)(fn: ProxyAppFunction) = {
    
    appComponents(environment) match {
      case Failure(e) => throw e
      case Success((parent, child)) => proxyUri match {
        case "v2/apps" => { fn(fqon, parent.name, child.name) }
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



