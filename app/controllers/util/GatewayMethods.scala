package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.ContainerSpec.ServiceAddress

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.meta.api.errors._

import play.api.Logger
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper

import com.google.inject.Inject
import com.galacticfog.gestalt.json.Js

import com.galacticfog.gestalt.meta.api.sdk._

class GatewayMethods @Inject() () {
  
  private[this] val log = Logger(this.getClass)

  def toGatewayEndpoint(js: JsValue, api: UUID): Try[LaserEndpoint] = {

    // TODO: error handling here should be better, a lot of Try{_.get}
    def mkUpstreamUrl(implType: String, implId: UUID, maybePortName: Option[String], sync: Boolean): Try[String] = {
      (implType,maybePortName.map(_.trim).filter(_.nonEmpty)) match {
        case ("lambda",_) =>
          ResourceFactory.findById(ResourceIds.Lambda, implId) match {
            case None => Failure(new BadRequestException("no lambda with id matching ApiEndpoint \"implementation_id\""))
            case Some(l) =>
              val invoke = if (sync) "invokeSync" else "invoke"
              for {
                providerId <- Try{ UUID.fromString((Json.parse(l.properties.get("provider")) \ "id").as[String]) }
                provider <- Try{ ResourceFactory.findById(providerId).get }
                config <- Try{Json.parse(provider.properties.get("config")).as[JsObject]}
                svcHost <- Try{ Js.find(config, "/env/public/SERVICE_HOST").map(_.as[String]).get }
                svcPort <- Try{ Js.find(config, "/env/public/SERVICE_PORT").map(_.as[String].toInt).get }
              } yield s"http://${svcHost}:${svcPort}/lambdas/${l.id}/${invoke}"
          }
        case ("container",None) =>
          Failure(new BadRequestException("""ApiEndpoint with "implementation_type" == "container" must also provide non-empty "container_port_name""""))
        case ("container",Some(portName)) =>
          ResourceFactory.findById(ResourceIds.Container, implId) match {
            case None => Failure(new BadRequestException("no container with id matching ApiEndpoint \"implementation_id\""))
            case Some(c) => for {
              spec <- ContainerSpec.fromResourceInstance(c)
              (svcHost,svcPort) <- spec.port_mappings.find(pm => pm.name.contains(portName)) match {
                case None =>
                  Failure(new BadRequestException("no port mapping with the specified name on the specified container"))
                case Some(ContainerSpec.PortMapping(_,_,_,_,_,_,Some(true),Some(ServiceAddress(svcHost,svcPort,_,_)),_)) =>
                  Success((svcHost,svcPort))
                case Some(_) =>
                  Failure(new BadRequestException("port mapping with the specified name was not exposed or did not contain service address"))
              }
            } yield s"http://${svcHost}:${svcPort}"
          }
        case _ => Failure(new BadRequestException("ApiEndpoint \"implementation_type\" must be \"container\" or \"lambda\""))
      }
    }

    for {
      json  <- Try{js.as[JsObject]}
      apiId <- Try{Js.find(json, "/id").flatMap(_.asOpt[String]).getOrElse(
        throw BadRequestException("ApiEndpoint did not contain \"id\"")
      )}
      path  <- Try{ Js.find(json, "/properties/resource").flatMap(_.asOpt[String]).getOrElse(
        throw BadRequestException("ApiEndpoint did not contain \"/properties/resource\"")
      )}
      implId <- Try{ Js.find(json, "/properties/implementation_id").flatMap(_.asOpt[UUID]).getOrElse(
        throw BadRequestException("ApiEndpoint did not contain properly-formatted \"/properties/implementation_id\"")
      )}
      sync = Js.find(json, "/properties/synchronous").flatMap(_.asOpt[Boolean]).getOrElse(true)
      implType <- Try{ Js.find(json, "/properties/implementation_type")
        .flatMap(_.asOpt[String])
        .orElse(ResourceFactory.findById(implId).flatMap(_.typeId match {
          case ResourceIds.Lambda => Some("lambda")
          case ResourceIds.Container => Some("container")
          case _ => None
        }))
        .getOrElse(
          throw BadRequestException("ApiEndpoint did not contain valid \"/properties/implementation_type\"")
        )
      }
      portName = Js.find(json, "/properties/container_port_name").flatMap(_.asOpt[String])
      upstreamUrl <- mkUpstreamUrl(implType, implId, portName, sync)
    } yield LaserEndpoint(
      id = Some(apiId),
      apiId       = api.toString,
      upstreamUrl = upstreamUrl,
      path        = path
    )
  }
  
  def findGatewayProvider(api: GestaltResourceInstance): Try[GestaltResourceInstance] = Try {
      val prvder = Json.parse(api.properties.get("provider")).as[JsObject]

      val pid = Js.find(prvder, "/id").fold {
        unprocessable("Missing required property [properties.provider.id]")
      }{ js => UUID.fromString(js.as[String]) }

      log.debug("Parsed provider ID from API for endpoint: " + pid)

      ResourceFactory.findById(ResourceIds.GatewayManager, pid).fold {
        unprocessable(s"GatewayManager provider with ID '$pid' not found")
      }{ gateway => gateway }    
  }

  def toGatewayApi(api: JsObject, location: UUID) = {
    val name = Js.find(api.as[JsObject], "/name") match {
      case None => unprocessable("Could not find /name property in payload")
      case Some(n) => n.as[String]
    }

    // Extract 'id' from payload, use to create backend system API payload
    val id = Js.find(api.as[JsObject], "/id").get.as[String]
    LaserApi(Some(UUID.fromString(id)), name,
      provider = Some(Json.obj(
        "id"       -> location.toString,
        "location" -> location.toString)))
  }

  private[controllers] def unprocessable(message: String) =
    throw new UnprocessableEntityException(message)

}
