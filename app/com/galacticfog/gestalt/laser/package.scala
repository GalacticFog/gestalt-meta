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
      provider: Option[JsValue] = None,
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
      periodicInfo : Option[JsValue] = None,
      headers : Map[String,String] = Map.empty)
  
  case class LaserLambda(
      id: Option[String], 
      eventFilter: Option[String],
      public: Boolean,
      provider: Option[JsValue],
      artifactDescription: LaserArtifactDescription)


  /*
   * TODO: This may translate into multiple LaserLambdas
   */
  def toLaserLambda(lambda: GestaltResourceInput, providerId: String): LaserLambda = {

    log.debug("toLaserLambda(...)")

    val props = lambda.properties.get

    val handler = props("handler").as[String]
    val isPublic = if (props.contains("public")) props("public").as[Boolean] else false
    val compressed = if (props.contains("compressed")) props("compressed").as[Boolean] else false
    val artifactUri = if (props.contains("package_url")) Some(props("package_url").as[String]) else None
    val periodic = if( props.contains("periodic_info" )) props("periodic_info").asOpt[JsValue] else None

    LaserLambda(
      id          = Some(lambda.id.get.toString),
      eventFilter = Some(UUID.randomUUID.toString),
      public      = isPublic,
      provider    = Some(Json.obj(
        "id" -> providerId.toString,
        "location" -> "",
        "href" -> "/foo/bar"
      )),
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
        periodicInfo= periodic,
        code        = if (props.contains("code")) props("code").asOpt[String] else None,
        headers     = if( props.contains("headers")) props("headers").as[Map[String,String]] else Map.empty
      ))
  }

}

