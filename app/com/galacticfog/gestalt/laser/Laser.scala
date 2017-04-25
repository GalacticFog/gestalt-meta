//package com.galacticfog.gestalt.laser
//
//import play.api.libs.json._
//import scala.util.{Try,Success,Failure}
//
//import com.galacticfog.gestalt.meta.api.sdk._
//
//import play.api.Logger
//
//import java.nio.charset.StandardCharsets;
//import java.nio.charset.Charset
//import java.util.Base64;
//import com.galacticfog.gestalt.json.Js
//
//class Laser(gatewayConfig: HostConfig, lambdaConfig: HostConfig, key: Option[String] = None, secret: Option[String] = None) {
//
//  private val log = Logger(this.getClass)
//  private val gatewayClient = configureClient(gatewayConfig)
//  private val lambdaClient = configureClient(lambdaConfig)
//
//  // --------------------------------------------------------------------------
//  // GATEWAYS
//  // --------------------------------------------------------------------------
//  def gateways(): Seq[LaserGateway] = {
//    getSeq[LaserGateway](gatewayClient, "/gateways", Seq(200))
//  }
//
//  def gateways(id: String): Option[LaserGateway] = {
//    get[LaserGateway](gatewayClient, s"/gateways/$id", Seq(200))
//  }
//
//  import JsonWebClient.apiResponse
//
//  def createGateway(gateway: LaserGateway): Try[ApiResponse] = {
//    val json = Json.toJson(gateway)
//    apiResponse(gatewayClient.post("/gateways", Some(json)), expected = Seq(200,201,202))
//  }
//
//  def deleteGateway(id: String) = {
//    apiResponse(gatewayClient.delete(s"/providers/$id"), expected = Seq(200,204,404))
//  }
//
//  // --------------------------------------------------------------------------
//  // APIS
//  // --------------------------------------------------------------------------
//  def apis() = getSeq[LaserApi](gatewayClient, "/apis", Seq(200))
//
//  def apis(id: String) = get[LaserApi](gatewayClient, s"/apis/$id", Seq(200))
//
//  def createApi(api: LaserApi): Try[ApiResponse] = {
//    val json = Json.toJson(api)
//    log.debug("POSTING NEW API TO GATEWAY:")
//    log.debug(Json.prettyPrint(json))
//    apiResponse(gatewayClient.post("/apis", Option(json)), expected = Seq(200,201,202))
//  }
//
//  def deleteApi(id: String) = {
//    apiResponse(gatewayClient.delete(s"/apis/$id"), expected = Seq(200,204,404))
//  }
//
//  // --------------------------------------------------------------------------
//  // ENDPOINTS
//  // --------------------------------------------------------------------------
//
//  def endpoints(apiId: String) = ???
//  def endpoints(apiId: String, id: String) = ???
//
//  def createEndpoint(endpoint: LaserEndpoint, apiId: String) = {
//    val json = Json.toJson(endpoint)
//    log.debug("Creating ApiEndpoint in Laser:\n" + Json.prettyPrint(json))
//    apiResponse(gatewayClient.post(s"/apis/$apiId/endpoints", Option(json)))
//  }
//
//  def deleteEndpoint(apiId: String, id: String) = {
//    val uri = s"/apis/$apiId/endpoints/$id"
//    apiResponse(gatewayClient.delete(uri), expected = Seq(200,204,404))
//  }
//
//  // --------------------------------------------------------------------------
//  // PROVIDERS
//  // --------------------------------------------------------------------------
//
//  def providers(): Seq[LaserProvider] = {
//    getSeq[LaserProvider](gatewayClient, "/providers", Seq(200))
//  }
//
//  def providers(id: String): Try[ApiResponse] = {
//    apiResponse(gatewayClient.get(s"/providers/$id"))
//  }
//
//  def providerLocations(providerId: String): Try[ApiResponse] = {
//    apiResponse(gatewayClient.get(s"/providers/$providerId/locations"))
//  }
//
//  def createProvider(provider: LaserProvider): Try[ApiResponse] = {
//    val json = Json.toJson(provider)
//    log.debug("Laser::createProvider(...):\n" + Json.prettyPrint(json))
//    apiResponse(gatewayClient.post("/providers", Option(json)))
//  }
//
//  // --------------------------------------------------------------------------
//  // LOCATIONS
//  // --------------------------------------------------------------------------
//  def locations(): Try[ApiResponse] = {
//    apiResponse(gatewayClient.get("/locations"))
//  }
//
//  def locations(id: String): Try[ApiResponse] = {
//    apiResponse(gatewayClient.get(s"/locations/$id"))
//  }
//
//  def createLocation(location: LaserLocation): Try[ApiResponse] = {
//    val json = Json.toJson(location)
//    apiResponse(gatewayClient.post("/locations", Option(json)))
//  }
//
//  private[laser] def base64(s: String, charset: Charset = StandardCharsets.UTF_8): String = {
//    Base64.getEncoder().encodeToString(s.getBytes(charset))
//  }
//
//  private[laser] def configureClient(config: HostConfig) = {
//
//	val authconfig = if (key.isDefined && secret.isDefined) {
//       HostConfig(config.protocol, config.host, config.port, config.timeout, Some(BasicCredential(key.get, secret.get)))
//    } else {
//       config
//    }
//    new JsonWebClient(authconfig)
//  }
//
//  private[laser] def get[T](client: JsonWebClient, resource: String, expected: Seq[Int])
//      (implicit fmt: Format[T]): Option[T] = {
//
//    apiResponse(client.get(resource), expected = expected) match {
//      case Failure(err) => {
//        log.error(err.getMessage)
//        throw err
//      }
//      case Success(res) => res.output match {
//        case Some(out) => out.validate[T] match {
//          case s: JsSuccess[T] => Some(s.get)
//          case e: JsError =>
//            throw new RuntimeException(Js.errorString(e))
//        }
//        case None => None
//      }
//    }
//  }
//
//  // --------------------------------------------------------------------------
//  // LAMBDAS
//  // --------------------------------------------------------------------------
//
//  def lambdas(): Seq[LaserLambda] = {
//    getSeq[LaserLambda](lambdaClient, "/lambdas", Seq(200))
//  }
//
//  def lambdas(id: String): Option[LaserLambda] = {
//    get[LaserLambda](lambdaClient, s"/lambdas/$id", Seq(200))
//  }
//
//  def createLambda(lambda: LaserLambda): Try[ApiResponse] = {
//    val json = Json.toJson(lambda)
//    log.debug("Creating new Lambda in gestalt-lambda:\n" + Json.prettyPrint(json))
//    apiResponse(lambdaClient.post("/lambdas", Option(json)), expected = Seq(200,201,202))
//  }
//
//  def updateLambda(lambda: LaserLambda): Try[ApiResponse] = {
//    log.debug("Entered Laser.updateLambda()")
//    val id = lambda.id getOrElse {
//      throw new RuntimeException("Cannot update Lambda without ID. found: " + lambda)
//    }
//    val json = Json.toJson(lambda)
//    log.debug("Updating Lambda in gestalt-lambda:\n" + Json.prettyPrint(json))
//    apiResponse(lambdaClient.put(s"/lambdas/${id}", Option(json)))
//  }
//
//  def deleteLambda(id: String): Try[ApiResponse] = {
//    apiResponse(lambdaClient.delete(s"/lambdas/${id}"), expected = Seq(200,204,404))
//  }
//
//  private[laser] def getSeq[T](client: JsonWebClient, resource: String, expected: Seq[Int])(implicit fmt: Format[T]): Seq[T] = {
//    apiResponse(client.get(resource), expected = expected) match {
//      case Failure(err) => throw err
//      case Success(res) => res.output match {
//        case None => Seq()
//        case Some(out) => out.validate[Seq[T]] match {
//          case s: JsSuccess[Seq[T]] => s.get
//          case e: JsError =>
//            throw new RuntimeException(Js.errorString(e))
//        }
//      }
//    }
//  }
//
//}
