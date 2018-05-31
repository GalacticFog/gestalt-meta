package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data
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
import com.galacticfog.gestalt.meta.auth.Entitlement
import com.galacticfog.gestalt.patch.PatchDocument
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import play.api.libs.ws.WSClient
import play.api.mvc.RequestHeader

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps

class GatewayMethods @Inject() ( ws: WSClient,
                                 providerMethods: ProviderMethods ) {

  import GatewayMethods._

  private[this] val log = Logger(this.getClass)

  val GATEWAY_PROVIDER_TIMEOUT_MS = 5000

  def createApi(provider: GestaltResourceInstance,
                resource: GestaltResourceInstance,
                lapi: LaserApi): Future[GestaltResourceInstance] = {
    val client = providerMethods.configureWebClient(provider, Some(ws))
    client.post("/apis", Option(Json.toJson(lapi))) flatMap { result =>
      if (Seq(200, 201).contains(result.status)) {
        log.info("Successfully created API in GatewayManager.")
        Future.successful(resource)
      } else {
        log.error("Error creating API in GatewayManager.")
        Future.failed(ApiError(result.status, result.body).throwable)
      }
    }
  }

  def createEndpoint( api: GestaltResourceInstance,
                      metaEndpoint: GestaltResourceInstance,
                      gatewayEndpoint: LaserEndpoint ): Future[GestaltResourceInstance] = {
    val uri = s"/apis/${api.id}/endpoints"
    val gatewayProvider = findGatewayProvider(api) getOrElse {
      throw new UnprocessableEntityException(s"Cannot find gateway provider for API '${api.id} (${api.name})'")
    }
    val client = providerMethods.configureWebClient(gatewayProvider, Some(ws))
    client.post(uri, Option(Json.toJson(gatewayEndpoint))) flatMap {
      result =>
        if (Seq(200, 201, 202).contains(result.status)) {
          log.info("Successfully created Endpoint in GatewayManager.")
          // we don't currently do anything with the data that comes back; a generic wrapper would potentially do something
          Future.successful(metaEndpoint)
        } else {
          Future.failed(
            ApiError(result.status, result.body).throwable
          )
        }
    }
  }

  def deleteApiHandler( r: ResourceLike ): Try[Unit] = {
    val fdelete = (for {
      provider <- Future.fromTry(findGatewayProvider(r))
      client = providerMethods.configureWebClient(provider, Some(ws))
      _ = log.info("Deleting API from GatewayManager...")
      delResult <-  client.delete(s"/apis/${r.id}")
    } yield delResult) recover {
      case e: Throwable => {
        log.error(s"Error deleting API from Gateway Manager: " + e.getMessage)
        throw e
      }
    }
    Try {
      val res = Await.result(fdelete, GATEWAY_PROVIDER_TIMEOUT_MS millis)
      log.debug("Response from GatewayManager: " + res.body)
      ()
    }
  }

  def deleteEndpointHandler( r: ResourceLike ): Try[Unit] = {
    val fdelete = (for {
      provider <- Future.fromTry(findGatewayProvider(r))
      client = providerMethods.configureWebClient(provider, Some(ws))
      _ = log.info("Deleting Endpoint from GatewayManager...")
      fdelete <- client.delete(s"/endpoints/${r.id}")
    } yield fdelete) recover {
      case e: Throwable => {
        log.error(s"Error deleting Endpoint from Gateway Manager: " + e.getMessage)
        throw e
      }
    }
    Try {
      val res = Await.result(fdelete, GATEWAY_PROVIDER_TIMEOUT_MS millis)
      log.debug("Response from GatewayManager: " + res.body)
      ()
    }
  }

  def updateEndpoint( endpoint: GestaltResourceInstance ): Future[GestaltResourceInstance] = {

    val (patchedEndpoint,updatedUrl) = {
      val newProps = endpoint.properties getOrElse Map.empty
      val implementation_type: String   = newProps.get("implementation_type") getOrElse(throw new RuntimeException(s"could not locate current 'implementation_type' for ApiEndpoint ${endpoint.id}"))
      val implementation_id_str: String = newProps.get("implementation_id")   getOrElse(throw new RuntimeException(s"could not locate current 'implementation_id' for ApiEndpoint ${endpoint.id}"))
      val port_name: Option[String]     = newProps.get("container_port_name")
      val synchronous: Boolean          = newProps.get("synchronous").flatMap(s => Try{s.toBoolean}.toOption) getOrElse true
      val implementation_id             = Try{UUID.fromString(implementation_id_str)} getOrElse(throw new RuntimeException(s"'implementation_id' == '${implementation_id_str}' for ApiEndpoint ${endpoint.id} could not be parsed as a UUID"))
      val newUpstreamUrl = mkUpstreamUrl(implementation_type, implementation_id, port_name, synchronous).get
      (
        endpoint.copy(
          properties = Some(newProps ++ Map(
            "upstream_url" -> newUpstreamUrl
          ))
        ),
        newUpstreamUrl
      )
    }

    val updatedProps = patchedEndpoint.properties.getOrElse(Map.empty)
    val updatedPlugins = for {
      str <- updatedProps.get("plugins")
      js = Try{Json.parse(str)} match {
        case Success(js) => js
        case Failure(t) => throw new RuntimeException("unable to parse patched json for .properties.plugins", t)
      }
    } yield js
    val updatedMethods = for {
      str <- updatedProps.get("methods")
      js = Try{Json.parse(str)} match {
        case Success(js) => js
        case Failure(t) => throw new RuntimeException("unable to parse patched json for .properties.methods", t)
      }
      methods <- js.asOpt[Seq[String]]
    } yield methods
    val updatedHosts = for {
      str <- updatedProps.get("hosts")
      js = Try{Json.parse(str)} match {
        case Success(js) => js
        case Failure(t) => throw new RuntimeException("unable to parse patched json for .properties.hosts", t)
      }
      hosts <- js.asOpt[Seq[String]].filter(_.nonEmpty)
    } yield hosts

    log.debug("Finding endpoint in gateway provider system...")
    val parentApi = ResourceFactory.findParent(ResourceIds.Api, endpoint.id) getOrElse(throw new RuntimeException(s"Could not find API parent of ApiEndpoint ${endpoint.id}"))
    val provider = findGatewayProvider(parentApi) getOrElse(throw new RuntimeException(s"Could not locate ApiGateway provider for parent API ${parentApi.id} of ApiEndpoint ${endpoint.id}"))
    val client = providerMethods.configureWebClient(provider, Some(ws))

    for {
      // Get api-endpoint from gestalt-api-gateway
      getReq <- client.get(s"/endpoints/${endpoint.id}") flatMap { response => response.status match {
        case 200 => Future.successful(response)
        case 404 => Future.failed(new ResourceNotFoundException(s"No ApiEndpoint with ID '${endpoint.id}' was found in gestalt-api-gateway"))
        case _   => Future.failed(new RuntimeException(s"received $response response from ApiGateway provider on endpoint GET"))
      } }
      gotEndpoint <- getReq.json.validate[LaserEndpoint] match {
        case JsSuccess(ep, _) => Future.successful(ep)
        case e: JsError => Future.failed(new RuntimeException(
          "could not parse ApiEndpoint GET response from ApiGateway provider: " + e.toString
        ))
      }
      _ = log.debug("Endpoint found in ApiGateway provider.")
      newPlugins = updatedPlugins.flatMap(_.asOpt[JsObject]) orElse gotEndpoint.plugins.flatMap(_.asOpt[JsObject]) getOrElse Json.obj() match {
        case ps if (ps \ "gestaltSecurity" \ "enabled").asOpt[Boolean].contains(true) =>
          val gestaltPlugin = (ps \ "gestaltSecurity").as[JsObject]
          val ids = ResourceFactory.findDescendantEntitlements(endpoint.id, "apiendpoint.invoke").headOption map Entitlement.make flatMap (_.properties.identities) getOrElse Seq.empty
          val (userIds,groupIds) = ids.partition(id => ResourceFactory.findById(ResourceIds.User, id).isDefined)
          val updatedGestaltPlugin = gestaltPlugin ++ Json.obj(
            "users" -> userIds,
            "groups" -> groupIds
          )
          ps ++ Json.obj("gestaltSecurity" -> updatedGestaltPlugin)
        case ps =>
          ps
      }
      patchedEP = gotEndpoint.copy(
        path = updatedProps.get("resource"),
        hosts = updatedHosts,
        methods = updatedMethods orElse gotEndpoint.methods,
        plugins = Some(newPlugins),
        upstreamUrl = updatedUrl
      )
      _ = log.debug("Patched endpoint resource before PUT: " + Json.toJson(patchedEP))
      updatedEndpointReq = client.put(s"/endpoints/${endpoint.id}", Some(Json.toJson(patchedEP)))
      _ <- updatedEndpointReq flatMap { response => response.status match {
        case 200 =>
          log.info(s"Successfully PUT ApiEndpoint to ApiGateway provider.")
          Future.successful(response)
        case _   =>
          log.error(s"Error PUTting Lambda in ApiGateway provider: ${response}")
          Future.failed(new RuntimeException(s"Error updating Lambda in ApiGateway provider: ${response}"))
      }}
    } yield patchedEndpoint // we don't actually use the result from api-gateway, though we probably should
  }

}

object GatewayMethods {

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
              case Some(ContainerSpec.PortMapping(_,_,_,_,_,_,Some(true),Some(ServiceAddress(svcHost,svcPort,_,_)),_,_)) =>
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
      path = Js.find(json, "/properties/resource").flatMap(_.asOpt[String])
      hosts = Js.find(json, "/properties/hosts").flatMap(_.asOpt[Seq[String]]).filter(_.nonEmpty)
      _ <- Try {
        if (path.isEmpty && !hosts.exists(_.nonEmpty)) throw new BadRequestException("ApiEndpoint must contain exactly one of '/properties/resource' or non-empty '/properties/hosts'")
      }
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
      methods = Js.find(json, "/properties/methods").flatMap(_.asOpt[Seq[String]])
      plugins = Js.find(json, "/properties/plugins")
    } yield LaserEndpoint(
      id = Some(apiId),
      apiId       = api.toString,
      upstreamUrl = upstreamUrl,
      path        = path,
      methods     = methods,
      plugins     = plugins,
      hosts       = hosts
    )
  }

  def findGatewayProvider(r: ResourceLike): Try[GestaltResourceInstance] = for {
    apiProps <- r.properties.fold[Try[data.Hstore]] {
      Failure{unprocessable("API/endpoint resource missing properties Hstore")}
    }(Success(_))

    providerProps <- apiProps.get("provider").fold[Try[JsObject]] {
      Failure{unprocessable("API/endpoint resource missing required property [properties.provider]")}
    }{
      jsonStr => Try{ Json.parse(jsonStr).as[JsObject]}
    }

    pid <- Js.find(providerProps, "/id").fold[Try[UUID]] {
      Failure{unprocessable("Missing required property [properties.provider.id]")}
    }{
      js => Try{ UUID.fromString(js.as[String]) }
    }

    _ = log.debug(s"Parsed provider ID from API/endpoint: " + pid)

    provider <- ResourceFactory.findById(ResourceIds.GatewayManager, pid).fold[Try[GestaltResourceInstance]] {
      Failure{unprocessable(s"GatewayManager provider with ID '$pid' not found")}
    }(Success(_))
  } yield provider

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

  def getPublicUrl(endpointResource: GestaltResourceInstance): Option[String] = {
    val byHost = for {
      api <- ResourceFactory.findParent(ResourceIds.Api, endpointResource.id)
      props <- endpointResource.properties
      hostsStr <- props.get("hosts")
      maybePath = props.get("resource").map("/" + api.name + _).getOrElse("")
      firstHost <- Try{Json.parse(hostsStr)}.toOption.flatMap(_.asOpt[Seq[String]]).flatMap(_.headOption)
      provider <- props.get("provider")
      providerJson <- Try{Json.parse(provider)}.toOption
      locations <- (providerJson \ "locations").asOpt[Seq[String]]
      location <- locations.headOption
      kongId <- Try{UUID.fromString(location)}.toOption
      kongProvider <- ResourceFactory.findById(ResourceIds.KongGateway, kongId)
      kpp <- kongProvider.properties
      kpc <- kpp.get("config")
      kpcJson <- Try{Json.parse(kpc)}.toOption
      kongProto <- (kpcJson \ "external_protocol").asOpt[String]
      publicUrl = s"$kongProto://$firstHost$maybePath"
    } yield publicUrl

    lazy val byPath = for {
      api <- ResourceFactory.findParent(ResourceIds.Api, endpointResource.id)
      props <- endpointResource.properties
      resourcePath <- props.get("resource")
      provider <- props.get("provider")
      providerJson <- Try{Json.parse(provider)}.toOption
      locations <- (providerJson \ "locations").asOpt[Seq[String]]
      location <- locations.headOption
      kongId <- Try{UUID.fromString(location)}.toOption
      kongProvider <- ResourceFactory.findById(ResourceIds.KongGateway, kongId)
      kpp <- kongProvider.properties
      kpc <- kpp.get("config")
      kpcJson <- Try{Json.parse(kpc)}.toOption
      kongVhost <- (kpcJson \ "env" \ "public" \ "PUBLIC_URL_VHOST_0").asOpt[String]
      kongProto <- (kpcJson \ "external_protocol").asOpt[String]
      publicUrl = s"${kongProto}://${kongVhost}/${api.name}${resourcePath}"
    } yield publicUrl

    byHost orElse byPath
  }

  private[controllers] def unprocessable(message: String) =
    throw new UnprocessableEntityException(message)

}
