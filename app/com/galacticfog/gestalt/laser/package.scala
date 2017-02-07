package com.galacticfog.gestalt


import com.galacticfog.gestalt.data.models._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.sdk._
import java.util.UUID
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import play.api.Logger

package object laser {

  private val log = Logger(this.getClass)
  
  implicit lazy val laserApiFormat = Json.format[LaserApi]
  implicit lazy val laserGatewayInfoFormat = Json.format[LaserGatewayInfo]
  implicit lazy val laserGatewayFormat = Json.format[LaserGateway]
  implicit lazy val laserEndpointFormat = Json.format[LaserEndpoint]
  implicit lazy val laserArtifactDescriptionFormat = Json.format[LaserArtifactDescription]
  implicit lazy val laserLambdaFormat = Json.format[LaserLambda]
  implicit lazy val laserProviderFormat = Json.format[LaserProvider]
  implicit lazy val laserLocationFormat = Json.format[LaserLocation]
  
  case class LaserGatewayInfo(host: String, port: Int, username: String, password: String)
  
  case class LaserGateway(
      id: Option[String], 
      name: String, 
      locationId: String,
      gatewayInfo: JsValue)  
  
  case class LaserApi(
      id: Option[String], 
      name: String, 
      gatewayId: Option[String] = None,
      provider: Option[JsValue] = None, 
      description: Option[String] = None)

  case class LaserProvider(id: Option[String], name: String, href: Option[String] = None, providerInfo: Option[JsValue] = None)
  case class LaserLocation(id: Option[String], name: String, providerId: String)
  
  case class LaserEndpoint(
      id: Option[String], 
      apiId: String, 
      upstreamUrl: String,
      path: String, 
      domain: Option[JsValue] = None,
      url: Option[String] = None,
      provider: Option[JsValue],
      endpointInfo: Option[JsValue] = None, 
      authentication: Option[JsValue] = None)

  case class LaserArtifactDescription(
      artifactUri: Option[String],
      runtime: String,
      handler: String,
      memorySize: Int,
      cpus: Double,
      description: Option[String] = None,
      compressed: Boolean = false,
      publish: Boolean = false,
      role: String = "none",
      timeoutSecs: Int = 180,
      code: Option[String] = None,
      synchronous: Boolean = false,
      headers : Map[String,String] = Map.empty)
  
  case class LaserLambda(
      id: Option[String], 
      eventFilter: Option[String],
      public: Boolean,
      provider: Option[JsValue],
      artifactDescription: LaserArtifactDescription)


//  def toLaserGateway(input: GestaltResourceInput) = {
//    LaserGateway(id = None, name = input.name, gatewayInfo = input.properties.get("gateway_info"))
//  }

  def getApiId(apiName: String) = ???
  
  // lambdaId is the Laser string ID
  def buildEndpointUri(lambdaHost: String, lambdaId: String) = {
    "%s/lambdas/%s/invoke".format(lambdaHost, lambdaId)
  }
  
  // uri is implementation.uri
  def getEndpointPath(uri: String) = uri.drop(uri.lastIndexOf("/"))
  

  def toLaserApi(api: GestaltResourceInput, provider: UUID, location: String) = {
    // One LaserApi for each given provider.
    val input = Json.toJson(api)
    println("API-INPUT:\n" + Json.prettyPrint(input))
    val id = api.id.getOrElse(UUID.randomUUID).toString
    LaserApi(
        id = Some(id),
        name = api.name,
        description = api.description,
        provider = Some(Json.obj("id" -> provider.toString, "location" -> location)))
  }
  
  
  def toLaserEndpoint(input: GestaltResourceInput, apiId: UUID, upstreamUrl: String, provider: JsValue) = {
    val id = Some(input.id.getOrElse(UUID.randomUUID).toString)
    val props = input.properties.get
    LaserEndpoint(
        id = id,  
        apiId = apiId.toString,           // argument
        upstreamUrl = upstreamUrl,        // argument
        path = props("resource").as[String],        // from properties
        domain = if (props.contains("domain")) Some(props("domain")) else None,      // from properties
        provider = Some(provider))    // argument
  }
  
  
  /*
   * TODO: This may translate into multiple LaserLambdas
   */
  def toLaserLambda(lambda: GestaltResourceInput, providerId: String, location: String) = {
    
    log.debug("toLaserLambda(...)")
    
    val props = lambda.properties.get
    
    val handler = props("handler").as[String]
    val isPublic = if (props.contains("public")) props("public").as[Boolean] else false
    val compressed = if (props.contains("compressed")) props("compressed").as[Boolean] else false
    val artifactUri = if (props.contains("package_url")) Some(props("package_url").as[String]) else None
    
    LaserLambda(
      id          = Some(lambda.id.get.toString), 
      eventFilter = Some(UUID.randomUUID.toString),
      public      = isPublic,
      provider    = Some(Json.parse(s"""{ "id": "${providerId.toString}", "location": "$location", "href": "/foo/bar" }""")),
      
      LaserArtifactDescription(
          artifactUri = artifactUri,
          description = if (props.contains("description")) props("description").asOpt[String] else None,
          handler     = handler,
          memorySize  = props("memory").as[Int],
          cpus        = props("cpus").as[Double],
          publish     = false,     // <- currently not used
          role        = "none",    // <- currently not used
          runtime     = props("runtime").as[String],
          timeoutSecs = props("timeout").as[Int],
          compressed  = compressed,
          code        = if (props.contains("code")) props("code").asOpt[String] else None,
          headers     = if( props.contains("headers")) props("headers").as[Map[String,String]] else Map.empty
    ))
  }
  
  def requiredProperty(props: Map[String,JsValue], key: String) = {
    if (props.contains(key)) props(key) else
      throw new BadRequestException(s"Missing property '$key'.")
  }

  def splitEntrypoint(ep: String): (String,String) = { 
    val ps = (for {
      m <- "^(.*)\\.(.*)$".r.findAllIn(ep.trim).matchData
      e <- m.subgroups
    } yield e).toList
    
    ps(0) -> ps(1)
  }
}

