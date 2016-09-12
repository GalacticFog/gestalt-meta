package com.galacticfog.gestalt.laser

import play.api.libs.json._
import scala.util.{Try,Success,Failure}

import com.galacticfog.gestalt.meta.api.sdk._

import play.api.{ Logger => log }

import java.nio.charset.StandardCharsets;
import java.nio.charset.Charset
import java.util.Base64;

class Laser(gatewayConfig: HostConfig, lambdaConfig: HostConfig, key: Option[String] = None, secret: Option[String] = None) {

  private val gatewayClient = configureClient(gatewayConfig)
  private val lambdaClient = configureClient(lambdaConfig)
  
  // --------------------------------------------------------------------------
  // GATEWAYS
  // --------------------------------------------------------------------------  
  def gateways(): Seq[LaserGateway] = {
    getSeq[LaserGateway](gatewayClient, "/gateways", Seq(200))
  }
  
  def gateways(id: String): Option[LaserGateway] = {
    get[LaserGateway](gatewayClient, s"/gateways/$id", Seq(200))
  }
  
  def createGateway(gateway: LaserGateway): Try[ApiResponse] = {
    val json = Json.toJson(gateway)
    gatewayClient.post("/gateways", json, Seq(200,201,202))
  }
  
  def deleteGateway(id: String) = {
    gatewayClient.delete(s"/providers/$id", Seq(200,204,404))
  }

  // --------------------------------------------------------------------------
  // APIS
  // --------------------------------------------------------------------------  
  def apis() = getSeq[LaserApi](gatewayClient, "/apis", Seq(200))
  
  def apis(id: String) = get[LaserApi](gatewayClient, s"/apis/$id", Seq(200))
  
  def createApi(api: LaserApi): Try[ApiResponse] = {
    val json = Json.toJson(api)
    gatewayClient.post("/apis", json, Seq(200,201,202))
  }
  
  def deleteApi(id: String) = {
    gatewayClient.delete(s"/apis/$id", Seq(200,204,404))
  }

  // --------------------------------------------------------------------------
  // ENDPOINTS
  // --------------------------------------------------------------------------  
  
  def endpoints(apiId: String) = ???
  def endpoints(apiId: String, id: String) = ???
  
  def createEndpoint(endpoint: LaserEndpoint, apiId: String) = {
    val json = Json.toJson(endpoint)
    log.debug("Creating ApiEndpoint in Laser:\n" + Json.prettyPrint(json))
    gatewayClient.post(s"/apis/$apiId/endpoints", json, Seq(200,201,202))
  }
  
  def deleteEndpoint(apiId: String, id: String) = {
    val uri = s"/apis/$apiId/endpoints/$id"
    gatewayClient.delete(uri, Seq(200,204,404))
  }
  
  // --------------------------------------------------------------------------
  // PROVIDERS
  // --------------------------------------------------------------------------  
  
  def providers(): Seq[LaserProvider] = {
    getSeq[LaserProvider](gatewayClient, "/providers", Seq(200))
  }
  
  def providers(id: String): Try[ApiResponse] = {
    gatewayClient.get(s"/providers/$id", Seq(200))
  }
  
  def providerLocations(providerId: String): Try[ApiResponse] = {
    gatewayClient.get(s"/providers/$providerId/locations", Seq(200))
  }
  
  def createProvider(provider: LaserProvider): Try[ApiResponse] = {
    val json = Json.toJson(provider)
    log.debug("Laser::createProvider(...):\n" + Json.prettyPrint(json))
    gatewayClient.post("/providers", json, Seq(200,201,202))
  }

  // --------------------------------------------------------------------------
  // LOCATIONS
  // --------------------------------------------------------------------------  
  def locations(): Try[ApiResponse] = {
    gatewayClient.get("/locations", Seq(200))
  }
  
  def locations(id: String): Try[ApiResponse] = {
    gatewayClient.get(s"/locations/$id", Seq(200))
  }
  
  def createLocation(location: LaserLocation): Try[ApiResponse] = {
    val json = Json.toJson(location)
    gatewayClient.post("/locations", json, Seq(200,201,202))
  }
  
  private[laser] def base64(s: String, charset: Charset = StandardCharsets.UTF_8): String = {
    Base64.getEncoder().encodeToString(s.getBytes(charset))
  }
  
  private[laser] def configureClient(config: HostConfig) = {
    val authHeader = if (key.isDefined && secret.isDefined) {
      Option("Authorization" -> base64("%s:%s".format(key.get, secret.get)))
    } else None
    new JsonWebClient(config, authHeader)
  }  
  
  private[laser] def get[T](client: JsonWebClient, resource: String, expected: Seq[Int])
      (implicit fmt: Format[T]): Option[T] = {
    
    client.get(resource, expected) match {
      case Failure(err) => throw err
      case Success(res) => res.output match {
        case Some(out) => out.validate[T] match {
          case s: JsSuccess[T] => Some(s.get)
          case e: JsError => 
            throw new RuntimeException(JsError.toFlatJson(e).toString)
        }
        case None => None
      }
    }    
  }

  // --------------------------------------------------------------------------
  // LAMBDAS
  // --------------------------------------------------------------------------  
  
  def lambdas(): Seq[LaserLambda] = {
    getSeq[LaserLambda](lambdaClient, "/lambdas", Seq(200))
  }
  
  def lambdas(id: String): Option[LaserLambda] = {
    get[LaserLambda](lambdaClient, s"/lambdas/$id", Seq(200))  
  }
  
  def createLambda(lambda: LaserLambda): Try[ApiResponse] = {
    val json = Json.toJson(lambda)
    lambdaClient.post("/lambdas", json, Seq(200,201,202))
  }
  
  def deleteLambda(id: String): Try[ApiResponse] = {
    lambdaClient.delete(s"/lambdas/${id}", Seq(200,204,404))
  }

  private[laser] def getSeq[T](client: JsonWebClient, resource: String, expected: Seq[Int])(implicit fmt: Format[T]): Seq[T] = {
    client.get(resource, expected) match {
      case Failure(err) => throw err
      case Success(res) => res.output match {
        case Some(out) => out.validate[Seq[T]] match {
          case s: JsSuccess[Seq[T]] => s.get
          case e: JsError =>
            throw new RuntimeException(JsError.toFlatJson(e).toString)
        }
        case None => Seq()
      }
    }
    
  }
  
}
