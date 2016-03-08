package com.galacticfog.gestalt.laser

import play.api.libs.json._
import scala.util.{Try,Success,Failure}


class Laser(gatewayConfig: HostConfig, lambdaConfig: HostConfig) {

  private val client = new JsonWebClient(gatewayConfig)
  private val lambdaClient = new JsonWebClient(lambdaConfig)
  
  // --------------------------------------------------------------------------
  // GATEWAYS
  // --------------------------------------------------------------------------  
  def gateways(): Seq[LaserGateway] = {
    getSeq[LaserGateway](client, "/gateways", Seq(200))
  }
  
  def gateways(id: String): Option[LaserGateway] = {
    get[LaserGateway](client, s"/gateways/$id", Seq(200))
  }
  
  def createGateway(gateway: LaserGateway) = ???
  
  def deleteGateway(id: String) = ???
  
  
  // --------------------------------------------------------------------------
  // APIS
  // --------------------------------------------------------------------------  
  def apis() = getSeq[LaserApi](client, "/apis", Seq(200))
  
  def apis(id: String) = get[LaserApi](client, s"/apis/$id", Seq(200))
  
  def createApi(api: LaserApi) = {
    val json = Json.toJson(api)
    println("CREATING LASER API - POSTING:\n" + Json.prettyPrint(json))
    client.post("/apis", json, Seq(200,201,202))
  }
  
  def deleteApi(id: String) = {
    client.delete(s"/apis/$id", Seq(200,204))
  }
  
  def deleteApis(ids: Seq[String]) = {
    ids map { i => deleteApi(i) }
  }
  
  
  // --------------------------------------------------------------------------
  // ENDPOINTS
  // --------------------------------------------------------------------------  
  
  def endpoints(apiId: String) = ???
  def endpoints(apiId: String, id: String) = ???
  
  def createEndpoint(endpoint: LaserEndpoint, apiId: String) = {
    val json = Json.toJson(endpoint)
    println("CREATING LASER ENDPOINT:\n" + Json.prettyPrint(json))
    client.post(s"/apis/$apiId/endpoints", json, Seq(200,201,202))
  }
  
  def deleteEndpoint(apiId: String, id: String) = {
    val uri = s"/apis/$apiId/endpoints/$id"
    client.delete(uri, Seq(200,204))
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
  
  def createLambda(lambda: LaserLambda) = {
    val json = Json.toJson(lambda)
    lambdaClient.post("/lambdas", json, Seq(200,201,202))
  }
  
  def deleteLambda(id: String) = {
    lambdaClient.delete(s"/lambdas/${id}", Seq(200,204))
  }
  
  def deleteLambdas(ids: Seq[String]) = {
    ids map { i => deleteLambda(i) }
  }
  
  
  // --------------------------------------------------------------------------
  // PROVIDERS
  // --------------------------------------------------------------------------  
  def providers() = {
    client.get("/providers", Seq(200))
  }
  
  def providers(id: String) = {
    client.get(s"/providers/$id", Seq(200))
  }
  
  def providerLocations(providerId: String) = {
    client.get(s"/providers/$providerId/locations", Seq(200))
  }
  
  def createProvider(provider: LaserProvider) = ???
  
  // --------------------------------------------------------------------------
  // LOCATIONS
  // --------------------------------------------------------------------------  
  
  
  
  protected def get[T](client: JsonWebClient, resource: String, expected: Seq[Int])
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
  
  protected def getSeq[T](client: JsonWebClient, resource: String, expected: Seq[Int])
      (implicit fmt: Format[T]): Seq[T] = {
    
    client.get(resource, expected) match {
      case Failure(err) => throw err
      case Success(res) => {
        println("GET-SEQ:\n" + res)
        res.output match {
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
  
}