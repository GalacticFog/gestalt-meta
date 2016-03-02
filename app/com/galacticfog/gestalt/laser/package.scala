package com.galacticfog.gestalt

import com.galacticfog.gestalt.data.models._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.sdk._

package object laser {

  implicit lazy val laserApiFormat = Json.format[LaserApi]
  implicit lazy val laserGatewayInfoFormat = Json.format[LaserGatewayInfo]
  implicit lazy val laserGatewayFormat = Json.format[LaserGateway]
  implicit lazy val laserEndpointFormat = Json.format[LaserEndpoint]
  implicit lazy val laserArtifactDescriptionFormat = Json.format[LaserArtifactDescription]
  implicit lazy val laserLambdaFormat = Json.format[LaserLambda]
  
  case class LaserGatewayInfo(host: String, port: Int, username: String, password: String)
  
  case class LaserGateway(
      id: Option[String], 
      name: String, 
      gatewayInfo: JsValue)  
  
  case class LaserApi(
      id: Option[String], 
      name: String, 
      provider: JsValue, 
      description: Option[String] = None)

  case class LaserEndpoint(
      id: Option[String], 
      apiId: String, 
      uri: String, 
      path: String, 
      domain: JsValue,
      endpointInfo: Option[JsValue] = None, 
      authentication: Option[JsValue] = None)

  case class LaserArtifactDescription(
      artifactUri: String,
      runtime: String,
      functionName: String,
      handler: String,
      memorySize: Int,
      cpus: Double,
      description: Option[String] = None,
      publish: Boolean = false,
      role: String = "none",
      timeoutSecs: Int = 180)
  
  case class LaserLambda(
      id: Option[String], 
      eventFilter: Option[String],
      provider: JsValue,
      artifactDescription: LaserArtifactDescription)


/*
{
  "name": "GFI-Kong-4",
  "properties": {
    "locations": ["us-east-1a", "us-east-1b", "us-west-1a", "us-west-1b", "us-west-1c"],
    "gateway_info": {
        "host" : "192.168.200.20",
        "port" : 8000,
        "username" : "testUser",
        "password" : "password"        
    }
  }
}
*/

  def toLaserGateway(input: GestaltResourceInput) = {
    LaserGateway(id = None, name = input.name, gatewayInfo = input.properties.get("gateway_info"))
  }

  /*
   * 
   * I need to create Api in order to map api_name to laser's api_id
   * 
   */
  

  def getApiId(apiName: String) = ???
  
  // lambdaId is the Laser string ID
  def buildEndpointUri(lambdaHost: String, lambdaId: String) = {
    "%s/lambdas/%s/invoke".format(lambdaHost, lambdaId)
  }
  
  // uri is implementation.uri
  def getEndpointPath(uri: String) = uri.drop(uri.lastIndexOf("/"))
  
  /*
   * TODO: May translate into severl LaserApis
   */
  def toLaserApi(api: GestaltResourceInput): Seq[LaserApi] = {
    // One LaserApi for each given provider.
    ???
  }
  
  def toLaserEndpoint(lambda: GestaltResourceInput): Seq[LaserEndpoint] = {
    // One LaserEndpoint for each given provider.
    /*
     * 1.) Get ApiId
     * 2.) uri == implementation.uri
     * 3.) path ???
     * 4.) endpointInfo ???
     * 5.) authentication ???
     */
    ???
  }
  
  /*
   * TODO: This may translate into multiple LaserLambdas
   */
  def toLaserLambda(lambda: GestaltResourceInput) = {
    
    // One LaserLambda for each given provider.
    
    println("toLaserLambda(...)")
    
    val props = lambda.properties.get
    val (handler,function) = parseHandlerFunction(lambda)
    
    LaserLambda(
      id = None, 
      eventFilter = Some("placeholder.event"),
      provider = props("provider"),
      LaserArtifactDescription(
          artifactUri = props("package_url").as[String],
          description = None,
          functionName = function,
          handler = handler,
          memorySize = props("memory").as[Int],
          cpus = props("cpus").as[Double],
          publish = false,             // <- currently not used
          role = "placeholder.role",   // <- currently not used
          runtime = props("runtime").as[String],
          timeoutSecs = props("timeout").as[Int]) )
  }

  def parseHandlerFunction(lambda: GestaltResourceInput) = {
    val props = lambda.properties.get
    val runtime = props("runtime")
    val mapJson = lambda.properties.get("function_mappings")
    val fmap = mapJson.validate[Map[String, JsValue]]
    //
    // TODO: Validation parsing.
    //
    val function = fmap.get.keys.toSeq(0)

    runtime.as[String] match {
      case "dotnet" => ("", function)
      case _        => splitEntrypoint(function)
    }
  }
  
  def splitEntrypoint(ep: String) = { 
    val ps = (for {
      m <- "^(.*)\\.(.*)$".r.findAllIn(ep.trim).matchData
      e <- m.subgroups
    } yield e).toList
    
    ps(0) -> ps(1)
  }          
}

