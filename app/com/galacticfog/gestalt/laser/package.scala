package com.galacticfog.gestalt


import com.galacticfog.gestalt.data.models._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.sdk._
import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.auth.Entitlement
import play.api.Logger

import scala.util.Try

package object laser {

  private val log = Logger(this.getClass)
  
  implicit lazy val laserApiFormat = Json.format[LaserApi]
  implicit lazy val laserEndpointFormat = Json.format[LaserEndpoint]
  implicit lazy val laserArtifactDescriptionFormat = Json.format[LaserArtifactDescription]
  implicit lazy val laserLambdaFormat = Json.format[LaserLambda]

  case class LaserApi(
      id: Option[String],
      name: String, 
      gatewayId: Option[String] = None,
      provider: Option[JsValue] = None, 
      description: Option[String] = None)

  case class LaserEndpoint( id: Option[String],
                            apiId: String,
                            upstreamUrl: String,
                            path: Option[String],
                            domain: Option[JsValue] = None,
                            url: Option[String] = None,
                            provider: Option[JsValue] = None,
                            endpointInfo: Option[JsValue] = None,
                            authentication: Option[JsValue] = None,
                            methods: Option[Seq[String]] = None,
                            plugins: Option[JsValue] = None,
                            hosts: Option[Seq[String]] = None) {


    def updateWithAuthorization(users: Seq[UUID], groups: Seq[UUID]): LaserEndpoint = {
      val updated = for {
        plugins <- this.plugins.flatMap(_.asOpt[JsObject])
        secPlugin <- (plugins \ "gestaltSecurity").asOpt[JsObject]
        if (secPlugin \ "enabled").asOpt[Boolean].contains(true)
        updatedSecPlugin = secPlugin ++ Json.obj(
          "users" -> users,
          "groups" -> groups
        )
        updatedPlugins = plugins ++ Json.obj(
          "gestaltSecurity" -> updatedSecPlugin
        )
      } yield this.copy(
        plugins = Some(updatedPlugins)
      )
      updated getOrElse this
    }

  }

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
      artifactDescription: LaserArtifactDescription,
      computePathOverride: Option[String] = None,
      secrets: Option[Seq[JsObject]] = None)

  def toLaserLambda(lambda: ResourceLike, providerId: UUID): Try[LaserLambda] = Try {

    def maybeToBool(s: String): Option[Boolean] = Try{s.toBoolean}.toOption
    def maybeToJson(s: String): Option[JsValue] = Try{Json.parse(s)}.toOption

    log.debug("toLaserLambda(...)")

    val props = lambda.properties.getOrElse(Map.empty)

    val handler = props("handler")
    val isPublic = props.get("public").flatMap(maybeToBool) getOrElse false
    val compressed = props.get("compressed").flatMap(maybeToBool) getOrElse false
    val artifactUri = props.get("package_url")
    val periodic = props.get("periodic_info").flatMap(maybeToJson)

    val secretMounts = (for {
      secrets <- props.get("secrets")
      mounts = Json.parse(secrets).as[Seq[ContainerSpec.SecretMount]]
    } yield mounts).getOrElse(Seq.empty)

    val secrets = secretMounts.map {
      sm => ResourceFactory.findById(ResourceIds.Secret, sm.secret_id).getOrElse(throw new BadRequestException(s"Secret '${sm.secret_id}' does not exist'"))
    }

    val secretProviders = secrets.map({
      s => Try{(Json.parse(s.properties.get("provider")) \ "id").as[UUID]}.getOrElse(
        throw new BadRequestException(s"Secret '${s.id}' did not have valid 'provider' block")
      )
    }).distinct

    val lambdaProviderId = Try{(Json.parse(props("provider")) \ "id").as[UUID]}.getOrElse(
      throw new BadRequestException(s"Lambda '${lambda.id}' did not have valid 'provider' block")
    )
    val lambdaProvider = ResourceFactory.findById(ResourceIds.LambdaProvider, lambdaProviderId).getOrElse(
      throw new BadRequestException(s"Lambda '${lambda.id}' provider did not exist")
    )
    val lambdaCaasProvider = Try{(Json.parse(lambdaProvider.properties.get("config")) \ "env" \ "public" \ "META_COMPUTE_PROVIDER_ID").as[UUID]}.getOrElse(
      throw new BadRequestException((s"Lambda '${lambda.id}' provider '${lambdaProviderId}' did not have '.properties.config.env.public.META_COMPUTE_PROVIDER_ID'"))
    )

    if (secretProviders.length > 1) throw new BadRequestException("Secrets must have the same CaaS provider")
    else if (secretProviders.headOption.exists(_ != lambdaCaasProvider)) throw new BadRequestException(s"Lambda '${lambda.id}' provider '${lambdaProviderId}' did not have same CaaS provider as mounted secrets")

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
      ),
      secrets = Some(secretMounts.map(Json.toJson(_).as[JsObject]))
    )
  }

}

