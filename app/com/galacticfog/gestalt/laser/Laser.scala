package com.galacticfog.gestalt.laser

import play.api.libs.json._
import scala.util.{Try,Success,Failure}


class Laser(config: HostConfig) {

  val client = new JsonWebClient(config)
  
  def gateways(): Seq[LaserGateway] = {
    client.get("/gateways") match {
      case Success(gs) => {
        gs.output.get.validate[Seq[LaserGateway]] match {
          case s: JsSuccess[Seq[LaserGateway]] => s.get
          case e: JsError => Seq() 
        }
      }
      case Failure(er) => throw er
    }
  }
  
  
  def gateways(id: String): Option[LaserGateway] = {
    client.get("/gateways/" + id) match {
      case Success(g) => {
        g.output.get.validate[LaserGateway] match {
          case s: JsSuccess[LaserGateway] => Some(s.get)
          case e: JsError => None
        }
      }
      case Failure(e) => throw e
    }
  }
  
  def createGateway(gateway: LaserGateway) = ???
  def deleteGateway(id: String) = ???
  
  def apis() = ???
  def apis(id: String) = ???
  def createApi(api: LaserApi) = ???
  def deleteApi(id: String) = ???
  
  def endpoints(apiId: String) = ???
  def endpoints(apiId: String, id: String) = ???
  def createEndpoint(endpoint: LaserEndpoint) = ???
  def deleteEndpoint(apiId: String, id: String) = ???
  
  def createLambda(lambda: LaserLambda) = ???
  def deleteLambda(id: String) = ???
  
}