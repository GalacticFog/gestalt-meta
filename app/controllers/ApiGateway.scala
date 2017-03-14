//package controllers
//
//import java.util.UUID
//
//import scala.util.Failure
//import scala.util.Success
//import scala.util.Try
//import com.galacticfog.gestalt.data.uuid2string
//import controllers.util.JsonUtil._
//import play.api.{ Logger => log }
//import play.api.libs.json._
//import com.galacticfog.gestalt.meta.api.sdk._
//import com.galacticfog.gestalt.meta.api.errors._
//import com.galacticfog.gestalt.meta.api.sdk.ApiResponse
//
//
//object ApiGateway {
//
// /*
//   *
//   * TODO: Move classess and formatters to package
//   * Move rest of gateway provider code to some utility class
//   * Maybe an ApiGatewayService object makes sense???
//   *
//   */
//
//  case class GatewayInputLocation(name: String, enabled: Boolean = true)
//  case class GatewayInputAuth(scheme: String, username: String, password: String)
//  case class GatewayInputConfig(auth: GatewayInputAuth, url: String, extra: Option[String] = None)
//  case class GatewayInputProperties(config: GatewayInputConfig, locations: Seq[GatewayInputLocation])
//  case class GatewayInput(name: String, description: Option[String], resource_type: String, properties: GatewayInputProperties)
//
//  implicit lazy val gatewayInputLocationFormat = Json.format[GatewayInputLocation]
//  implicit lazy val gatewayInputAuth = Json.format[GatewayInputAuth]
//  implicit lazy val gatewayInputConfig = Json.format[GatewayInputConfig]
//  implicit lazy val gatewayInputProperties = Json.format[GatewayInputProperties]
//  implicit lazy val gatewayInput = Json.format[GatewayInput]
//
//  import java.net.URL
//
//  
//  private[controllers] def setMetaGatewayProps(obj: JsValue, id: UUID, externalId: UUID, parent: JsValue): Try[JsObject] = {
//    val json = (obj.as[JsObject] ++ Json.obj("id" -> JsString(id.toString))) ++ Json.obj("resource_type" -> ResourceIds.ApiGatewayProvider)
//    upsertProperties(json ++ Json.obj("id" -> JsString(id.toString)),
//      ("parent" -> parent),
//      ("external_id" -> JsString(externalId)))
//  }
//  
//  
//  private def parseJsonId(json: JsValue) = {
//    (json \ "id") match {
//      case u: JsUndefined => throw new RuntimeException(
//        "Could not find 'id' in JSON returned from gateway server.")
//      case v => UUID.fromString(v.as[String]) // TODO: Test for FormatException
//    }
//  }  
//  
//  private[controllers] def parseLaserResponseId(response: Try[ApiResponse]) = {
//    response match {
//      case Failure(err) => throw err
//      case Success(res) => {
//        //log.debug("Successfully created Resource in gateway service. Response:\n" + res)
//        parseJsonId(res.output.get)
//      }
//    }
//  }
//  
//  
//  def getGatewayLocation(input: GatewayInput) = {
//    val numLocations = input.properties.locations.size
//    numLocations match {
//      case z if z <= 0 => throw new BadRequestException("You must provide a location.")
//      case n if n > 1  => throw new ConflictException("Multiple locations found. The current implementation only supports a SINGLE location.")
//      case _ => input.properties.locations(0)
//    }     
//  }
//  
//  /**
//   * Build the 'gatewayInfo' JSON object for the POST /gateways payload in apigateway service.
//   */
//  def buildGatewayInfo(input: GatewayInput) = {
//
//    def normalizeUrl(url: String) = {
//       if (url.trim.toLowerCase.startsWith("http")) url else s"http://$url"
//    }
//     
//    def getKongPublicInfo(serviceUrl: URL, publicAddress: Option[String]) = {
//      publicAddress map {addr => new URL(normalizeUrl(addr))} getOrElse {
//        log.warn("Gateway provider did not include public address info; using service address")
//        serviceUrl
//      }
//    }
//    
//    val kongServiceUrl = new URL(normalizeUrl(input.properties.config.url))
//    val kongServiceProtocol = kongServiceUrl.getProtocol
//    val kongServiceAddress = kongServiceUrl.getHost
//    val kongServicePort = Some(kongServiceUrl.getPort).filter(_ > 0)
//    
//    val username = input.properties.config.auth.username
//    val password = input.properties.config.auth.password
//    
//    val kongPublicUrl = getKongPublicInfo(kongServiceUrl, input.properties.config.extra)
//    val kongPublicProtocol = kongPublicUrl.getProtocol
//    val kongPublicHost = kongPublicUrl.getHost
//    val kongPublicPort = Some(kongPublicUrl.getPort).filter(_ > 0)
//
//    val p1 = kongServicePort.map(p => (Json.obj("port"        -> p)))
//    val p2 = kongPublicPort.map(p =>  (Json.obj("gatewayPort" -> p)))
//    Seq(p1,p2).flatten.fold(Json.obj(
//      "protocol" -> kongServiceProtocol,
//      "host" -> kongServiceAddress,
//      "username" -> username,
//      "password" -> password,
//      "gatewayProtocol" -> kongPublicProtocol,
//      "gatewayHost" -> kongPublicHost
//    ))(_ ++ _)
//  }
//
//  /*
//   * END API_GATEWAY_PROVIDER code.
//   */
//    
//  
//}