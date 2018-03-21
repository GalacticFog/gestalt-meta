package com.galacticfog.gestalt


import com.galacticfog.gestalt.data.models._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.sdk._
import java.util.UUID

import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import play.api.Logger

import scala.util.Try

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

  case class LaserEndpoint( id: Option[String],
                            apiId: String,
                            upstreamUrl: String,
                            path: String,
                            domain: Option[JsValue] = None,
                            url: Option[String] = None,
                            provider: Option[JsValue] = None,
                            endpointInfo: Option[JsValue] = None,
                            authentication: Option[JsValue] = None,
                            methods: Option[Seq[String]] = None,
                            plugins: Option[JsValue] = None )

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
   *       What does that mean? ^^^^
   */
  def toLaserLambda(lambda: ResourceLike, providerId: String): LaserLambda = {

    def maybeToBool(s: String): Option[Boolean] = Try{s.toBoolean}.toOption
    def maybeToJson(s: String): Option[JsValue] = Try{Json.parse(s)}.toOption

    log.debug("toLaserLambda(...)")

    val props = lambda.properties.get

    val handler = props("handler")
    val isPublic = props.get("public").flatMap(maybeToBool) getOrElse false
    val compressed = props.get("compressed").flatMap(maybeToBool) getOrElse false
    val artifactUri = props.get("package_url")
    val periodic = props.get("periodic_info").flatMap(maybeToJson)

    LaserLambda(
      id          = Some(lambda.id.toString),
      eventFilter = Some(UUID.randomUUID.toString),
      public      = isPublic,
      provider    = Some(Json.obj(
        "id" -> providerId.toString,
        "location" -> "",
        "href" -> "/foo/bar"
      )),
      LaserArtifactDescription(
        artifactUri = artifactUri,
        description = props.get("description"),
        handler     = handler,
        memorySize  = props("memory").toInt,
        cpus        = props("cpus").toDouble,
        publish     = false,     // <- currently not used
        role        = "none",    // <- currently not used
        runtime     = props("runtime"),
        timeoutSecs = props("timeout").toInt,
        compressed  = compressed,
        periodicInfo= periodic,
        code        = props.get("code"),
        headers     = props.get("headers").flatMap(maybeToJson).map(_.as[Map[String,String]]) getOrElse Map.empty
      ))
  }

}

