package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.ContainerSpec.ServiceAddress
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.meta.api.errors._
import play.api.Logger
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import com.google.inject.Inject
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.meta.api.patch.PatchInstance
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import play.api.libs.ws.WSClient

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps

class GatewayMethods @Inject() ( ws: WSClient,
                                 providerMethods: ProviderMethods ) {

  val GATEWAY_PROVIDER_TIMEOUT_MS = 5000

  private[this] val log = Logger(this.getClass)

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
              svcHost <- Js.find(config, "/env/public/SERVICE_HOST").map(_.as[String]) match {
                case Some(host) => Success(host)
                case None => Failure(new RuntimeException("Could not determine service host from env SERVICE_HOST lambda provider when creating upstream URL for lambda-bound ApiEndpoint"))
              }
              svcPort <- Js.find(config, "/env/public/SERVICE_PORT").map(_.as[String].toInt) match {
                case Some(port) => Success(port)
                case None => Failure(new RuntimeException("Could not determine service port from env SERVICE_PORT for lambda provider when creating upstream URL for lambda-bound ApiEndpoint"))
              }
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

  def toGatewayEndpoint(js: JsValue, api: UUID): Try[LaserEndpoint] = {


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

  def deleteApiHandler( r: ResourceLike, user: AuthAccountWithCreds ): Try[Unit] = ???

  def deleteEndpointHandler( r: ResourceLike, user: AuthAccountWithCreds ): Try[Unit] = ???

  def patchEndpointHandler( r: GestaltResourceInstance,
                            patch: PatchDocument,
                            user: AuthAccountWithCreds): Try[GestaltResourceInstance] = Try {

    // Strip path to last component to get field name.
    val strippedOps = patch.ops collect {
      case PatchOp("replace" | "add",path,Some(value)) =>
        val fieldName = path.stripPrefix("/properties/")
        (fieldName -> value)
    } toMap

    // extra components necessary for upstream_url
    val curProps = r.properties getOrElse Map.empty
    val implementation_type: String = strippedOps.get("implementation_type").flatMap(_.asOpt[String]) orElse curProps.get("implementation_type") getOrElse(throw new RuntimeException(s"could not locate current 'implementation_type' for ApiEndpoint ${r.id}"))
    val implementation_id_str: String = strippedOps.get("implementation_id").flatMap(_.asOpt[String]) orElse curProps.get("implementation_id")   getOrElse(throw new RuntimeException(s"could not locate current 'implementation_id' for ApiEndpoint ${r.id}"))
    val port_name: Option[String]   = strippedOps.get("container_port_name").flatMap(_.asOpt[String]) orElse curProps.get("container_port_name")
    val synchronous: Boolean        = strippedOps.get("synchronous").flatMap(_.asOpt[Boolean])        orElse curProps.get("synchronous").flatMap(s => Try{s.toBoolean}.toOption) getOrElse true
    val implementation_id     = Try{UUID.fromString(implementation_id_str)} getOrElse(throw new RuntimeException(s"'implementation_id' == '${implementation_id_str}' for ApiEndpoint ${r.id} could not be parsed as a UUID"))
    val newUpstreamUrl = mkUpstreamUrl(implementation_type, implementation_id, port_name, synchronous).get

    val fullOps = PatchDocument(
      (patch.ops :+ PatchOp("replace", "/properties/upstream_url", Some(JsString(newUpstreamUrl)))):_*
    )

    log.debug("Finding endpoint in gateway provider system...")
    val parentApi = ResourceFactory.findParent(ResourceIds.Api, r.id) getOrElse(throw new RuntimeException(s"Could not find API parent of ApiEndpoint ${r.id}"))
    val provider = findGatewayProvider(parentApi) getOrElse(throw new RuntimeException(s"Could not locate ApiGateway provider for parent API ${parentApi.id} of ApiEndpoint ${r.id}"))
    val client = providerMethods.configureWebClient(provider, Some(ws))

    val f = for {
      // Get api-endpoint from gestalt-api-gateway
      getReq <- client.get(s"/endpoints/${r.id}") flatMap { response => response.status match {
        case 200 => Future.successful(response)
        case 404 => Future.failed(new ResourceNotFoundException(s"No ApiEndpoint with ID '${r.id}' was found in gestalt-api-gateway"))
        case _   => Future.failed(new RuntimeException(s"received $response response from ApiGateway provider on endpoint GET"))
      } }
      gotEndpoint <- getReq.json.validate[LaserEndpoint] match {
        case JsSuccess(ep, _) => Future.successful(ep)
        case e: JsError => Future.failed(new RuntimeException(
          "could not parse ApiEndpoint GET response from ApiGateway provider: " + e.toString
        ))
      }
      _ = log.debug("Endpoint found in ApiGateway provider.")
      patchedEndpoint = gotEndpoint.copy(
        path = strippedOps.get("resource").flatMap(_.asOpt[String]) getOrElse gotEndpoint.path,
        upstreamUrl = newUpstreamUrl
      )
      _ = log.debug("Patched endpoint resource before PUT: " + Json.toJson(patchedEndpoint))
      updatedEndpointReq = client.put(s"/endpoints/${r.id}", Some(Json.toJson(patchedEndpoint)))
      _ <- updatedEndpointReq flatMap { response => response.status match {
        case 200 =>
          log.info(s"Successfully PUT ApiEndpoint to ApiGateway provider.")
          Future.successful(response)
        case _   =>
          log.error(s"Error PUTting Lambda in ApiGateway provider: ${response}")
          Future.failed(new RuntimeException(s"Error updating Lambda in ApiGateway provider: ${response}"))
      }}
      updatedMetaEndpoint = PatchInstance.applyPatch(r, fullOps).get.asInstanceOf[GestaltResourceInstance]
    } yield updatedMetaEndpoint // we don't actually use the result from api-gateway, though we probably should

    Await.result(f, GATEWAY_PROVIDER_TIMEOUT_MS millis)
  }

  private[controllers] def unprocessable(message: String) =
    throw new UnprocessableEntityException(message)

}
