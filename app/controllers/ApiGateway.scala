package controllers


import java.util.UUID
import java.net.URL
import play.api.http.HttpVerbs
import play.api.libs.ws.WS
import play.api.Play.current

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.PropertyValidator
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.ResourceType
import com.galacticfog.gestalt.data.illegal
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.{ PatchOp, PatchDocument, PatchHandler }
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.{ GestaltResource => SecurityResource }
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController

import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import controllers.util.JsonUtil._
import controllers.util.db._
import controllers.util.MetaController
import controllers.util.Security
import play.api.{ Logger => log }
import play.api.libs.json._
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import controllers.util.stringmap
import controllers.util.trace
import com.galacticfog.gestalt.meta.api._
import play.api.mvc.Result
import play.api.mvc.Action
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.security.api.json.JsonImports.linkFormat
import com.galacticfog.gestalt.laser.ApiResponse
import com.galacticfog.gestalt.meta.auth.Actions 
import com.galacticfog.gestalt.meta.auth.Authorization

object ApiGateway {

 /*
   * 
   * TODO: Move classess and formatters to package
   * Move rest of gateway provider code to some utility class
   * Maybe an ApiGatewayService object makes sense???
   * 
   */
  def client(gatewayUrl: String) = {
    val gateway = HostConfig.make(new URL(gatewayUrl))
    new GatewayClient(gateway, Option(EnvConfig.securityKey), Option(EnvConfig.securitySecret))
  }    
  
  case class GatewayInputLocation(name: String, enabled: Boolean = true)
  case class GatewayInputAuth(scheme: String, username: String, password: String)
  case class GatewayInputConfig(auth: GatewayInputAuth, url: String, extra: Option[String] = None)
  case class GatewayInputProperties(config: GatewayInputConfig, locations: Seq[GatewayInputLocation])
  case class GatewayInput(name: String, description: Option[String], resource_type: String, properties: GatewayInputProperties)

  implicit lazy val gatewayInputLocationFormat = Json.format[GatewayInputLocation]
  implicit lazy val gatewayInputAuth = Json.format[GatewayInputAuth]
  implicit lazy val gatewayInputConfig = Json.format[GatewayInputConfig]
  implicit lazy val gatewayInputProperties = Json.format[GatewayInputProperties]
  implicit lazy val gatewayInput = Json.format[GatewayInput]

  
  import java.net.URL
  import java.net.MalformedURLException

//  lazy val gatewayConfig = HostConfig.make(new URL(EnvConfig.gatewayUrl))
//  lazy val lambdaConfig  = HostConfig.make(new URL(EnvConfig.lambdaUrl))
//  lazy val laser = new Laser(
//      gatewayConfig, lambdaConfig, 
//      Option(EnvConfig.securityKey), 
//      Option(EnvConfig.securitySecret))
  
  
  /*
   * 
   * TODO: This function needs transaction enforcement. The workflow entails creating three resources on the
   * gateway service: 1) Provider, 2) Location, 3) Gateway.  If any of them fail, nothing is to be created
   * in Meta, and any gateway resources that *were* successfully created should deleted (or otherwise scrubbed).
   * 
   */

  
  
  private[controllers] def setMetaGatewayProps(obj: JsValue, id: UUID, externalId: UUID, parent: JsValue): Try[JsObject] = {
    val json = (obj.as[JsObject] ++ Json.obj("id" -> JsString(id.toString))) ++ Json.obj("resource_type" -> ResourceIds.ApiGatewayProvider)
    upsertProperties(json ++ Json.obj("id" -> JsString(id.toString)),
      ("parent" -> parent),
      ("external_id" -> JsString(externalId)))
  }
  
  
  private def parseJsonId(json: JsValue) = {
    (json \ "id") match {
      case u: JsUndefined => throw new RuntimeException(
        "Could not find 'id' in JSON returned from gateway server.")
      case v => UUID.fromString(v.as[String]) // TODO: Test for FormatException
    }
  }  
  
  private[controllers] def parseLaserResponseId(response: Try[ApiResponse]) = {
    response match {
      case Failure(err) => throw err
      case Success(res) => {
        //log.debug("Successfully created Resource in gateway service. Response:\n" + res)
        parseJsonId(res.output.get)
      }
    }
  }
  
  
  def getGatewayLocation(input: GatewayInput) = {
    val numLocations = input.properties.locations.size
    numLocations match {
      case z if z <= 0 => throw new BadRequestException("You must provide a location.")
      case n if n > 1  => throw new ConflictException("Multiple locations found. The current implementation only supports a SINGLE location.")
      case _ => input.properties.locations(0)
    }     
  }
  
  /**
   * Build the 'gatewayInfo' JSON object for the POST /gateways payload in apigateway service.
   */
  def buildGatewayInfo(input: GatewayInput) = {

    def normalizeUrl(url: String) = {
       if (url.trim.toLowerCase.startsWith("http")) url else s"http://$url"
    }
     
    def getKongPublicInfo(serviceAddress: String, publicAddress: Option[String]) = {
      val out = publicAddress match {
        case Some(address) => new URL(normalizeUrl(address))
        case None => {
          val u = new URL(normalizeUrl(serviceAddress))
          new URL("%s://%s:%d".format(u.getProtocol, "admin." + u.getHost, u.getPort))
        }
      }
      (out.getHost, if (out.getPort == -1) 80 else out.getPort)
    }
    
    val kongServiceUrl = new URL(normalizeUrl(input.properties.config.url))
    val kongServiceAddress = kongServiceUrl.getHost
    val kongServicePort = if (kongServiceUrl.getPort == -1) 80 else kongServiceUrl.getPort
    
    val username = input.properties.config.auth.username
    val password = input.properties.config.auth.password
    
    val (kongPublicHost,kongPublicPort) = getKongPublicInfo(kongServiceAddress, input.properties.config.extra)
    
    Json.obj(
      "host" -> kongServiceAddress,
      "port" -> kongServicePort,
      "username" -> username,
      "password" -> password,
      "gatewayHost" -> kongPublicHost,
      "gatewayPort" -> kongPublicPort)

  }    
  

  /*
   * END API_GATEWAY_PROVIDER code.
   */
    
  
}