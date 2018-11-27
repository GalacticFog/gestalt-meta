package com.galacticfog.gestalt.meta.providers.gwm

import java.util.UUID
import scala.concurrent.Future
import scala.util.Try
import com.google.inject.Inject
import play.api.libs.ws.WSClient
import play.api.Logger
import play.api.libs.json._
import cats.syntax.either._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.auth.Entitlement
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.patch.PatchDocument
import com.galacticfog.gestalt.util.Either._
import com.galacticfog.gestalt.util.ResourceSerde
import controllers.util.ProviderMethods

class GatewayManagerProvider @Inject()(ws: WSClient, providerMethods: ProviderMethods) extends GwmProviderImplementation[Future] {
  import play.api.libs.concurrent.Execution.Implicits.defaultContext
  import GwmSpec.Implicits._

  private[this] val log = Logger(this.getClass)

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

  implicit val laserApiFormat: Format[LaserApi] = Json.format[LaserApi]
  implicit val laserEndpointFormat: Format[LaserEndpoint] = Json.format[LaserEndpoint]

  def getPublicUrl(resource: GestaltResourceInstance): Option[String] = {
    def byHost(endpointProperties: ApiEndpointProperties): Either[String,String] = {
      val res = for {
        api <- Either.fromOption(ResourceFactory.findParent(ResourceIds.Api, resource.id),
         s"Api parent for ${resource.id} not found")
        maybePath = endpointProperties.resource.map { path => s"/${api.name}${path}" } getOrElse("")
        firstHost <- Either.fromOption(endpointProperties.hosts.flatMap(_.headOption), "hosts array must not be empty")
        location <- Either.fromOption(endpointProperties.provider.locations.headOption, "locations array must not be empty")
        kongProvider <- Either.fromOption(ResourceFactory.findById(ResourceIds.KongGateway, UUID.fromString(location)),
          s"Kong Provider with id ${location} not found")
        kpp <- Either.fromOption(kongProvider.properties, "Aborting due to kong provider configuration: empty properties")
        kpc <- Either.fromOption(kpp.get("config"), "Aborting due to kong provider configuration: empty config")
        kpcJson <- eitherFromTry(Try{Json.parse(kpc)})
        kongProto <- Either.fromOption((kpcJson \ "external_protocol").asOpt[String],
         "Aborting due to kong provider configuration: no external_protocol")
        publicUrl = s"$kongProto://$firstHost$maybePath"
      } yield publicUrl

      res.left.foreach { errorMessage =>
        log.warn(s"Failed to get public url by host: ${errorMessage}")
      }
      res
    }

    def byPath(endpointProperties: ApiEndpointProperties): Either[String,String] = {
      val res = for {
        api <- Either.fromOption(ResourceFactory.findParent(ResourceIds.Api, resource.id),
         s"Api parent for ${resource.id} not found")
        resourcePath <- Either.fromOption(endpointProperties.resource, "resource must be present")
        location <- Either.fromOption(endpointProperties.provider.locations.headOption, "locations array must not be empty")
        kongProvider <- Either.fromOption(ResourceFactory.findById(ResourceIds.KongGateway, UUID.fromString(location)),
          s"Kong Provider with id ${location} not found")
        kpp <- Either.fromOption(kongProvider.properties, "Aborting due to kong provider configuration: empty properties")
        kpc <- Either.fromOption(kpp.get("config"), "Aborting due to kong provider configuration: empty config")
        kpcJson <- eitherFromTry(Try{Json.parse(kpc)})
        kongVhost <- Either.fromOption((kpcJson \ "env" \ "public" \ "PUBLIC_URL_VHOST_0").asOpt[String],
         "Aborting due to kong provider configuration: no public env var PUBLIC_URL_VHOST_0")
        kongProto <- Either.fromOption((kpcJson \ "external_protocol").asOpt[String],
         "Aborting due to kong provider configuration: no external_protocol")
        publicUrl = s"${kongProto}://${kongVhost}/${api.name}${resourcePath}"
      } yield publicUrl

      res.left.foreach { errorMessage =>
        log.warn(s"Failed to get public url by path: ${errorMessage}")
      }
      res
    }

    (ResourceSerde.deserialize[ApiEndpointProperties](resource) flatMap { ep =>
      if(ep.hosts.getOrElse(Seq()).size == 0) {
        byPath(ep) orElse byHost(ep)
      }else {
        byHost(ep) orElse byPath(ep)
      }
    }).toOption
  }

  def createApi(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[GestaltResourceInstance] = {
    val client = providerMethods.configureWebClient(provider, Some(ws))
    for(
      apiProperties <- ResourceSerde.deserialize[ApiProperties](resource).liftTo[Future];
      locationId <- Either.fromOption(apiProperties.provider.locations.headOption, "Empty locations array").liftTo[Future];
      lapi = LaserApi(Some(resource.id.toString),
        resource.name,
        provider = Some(Json.obj(
          "id" -> locationId,
          "location" -> locationId
        ))
      );
      result <- client.post("/apis", Some(Json.toJson(lapi)))
    ) yield {
      if(Seq(200, 201).contains(result.status)) {
        log.info("Successfully created API in GatewayManager.")
        resource
      } else {
        throw new ApiError(result.status, result.body).throwable
      }
    }
  }
  def deleteApi(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[Unit] = {
    val client = providerMethods.configureWebClient(provider, Some(ws))
    client.delete(s"/apis/${resource.id}") map { result =>
      log.debug("Response from GatewayManager: " + result.body)
      ()
    }
  }

  private def mkUpstreamUrl(endpointProperties: ApiEndpointProperties): String = {
    import com.galacticfog.gestalt.json.Js
    import com.galacticfog.gestalt.meta.api.ContainerSpec
    import com.galacticfog.gestalt.meta.api.ContainerSpec.ServiceAddress

    val implType = endpointProperties.implementation_type
    val implId = endpointProperties.implementation_id
    val maybePortName = endpointProperties.container_port_name
    val sync = endpointProperties.synchronous.getOrElse(true)
    val isHttpAware = endpointProperties.is_http_aware.getOrElse(false)
    (implType,maybePortName.map(_.trim).filter(_.nonEmpty)) match {
      case ("lambda",_) => {
        log.debug("ENTERED LAMBDA CASE...")
        log.debug("impltType : " + implType)
        log.debug("implId: " + implId)
        
        ResourceFactory.findById(ResourceIds.Lambda, implId) match {
          case None => throw new BadRequestException("no lambda with id matching ApiEndpoint \"implementation_id\"")
          case Some(l) =>
            val invoke = if(sync) {
              if(isHttpAware) { "invokeSyncHA" }else { "invokeSync" }
            }else {
              "invoke"
            }
            val providerId = UUID.fromString((Json.parse(l.properties.get("provider")) \ "id").as[String])
            val provider = ResourceFactory.findById(providerId).get
            val config = Json.parse(provider.properties.get("config")).as[JsObject]
            val svcHost = Js.find(config, "/env/public/SERVICE_HOST").map(_.as[String]) match {
              case Some(host) => host
              case None => throw new RuntimeException("Could not determine service host from env SERVICE_HOST lambda provider when creating upstream URL for lambda-bound ApiEndpoint")
            }
            val svcPort = Js.find(config, "/env/public/SERVICE_PORT").map(_.as[String].toInt) match {
              case Some(port) => port
              case None => throw new RuntimeException("Could not determine service port from env SERVICE_PORT for lambda provider when creating upstream URL for lambda-bound ApiEndpoint")
            }
            s"http://${svcHost}:${svcPort}/lambdas/${l.id}/${invoke}"
        }
      }
      case ("container",None) =>
        throw new BadRequestException("""ApiEndpoint with "implementation_type" == "container" must also provide non-empty "container_port_name"""")
      case ("container",Some(portName)) =>
        ResourceFactory.findById(ResourceIds.Container, implId) match {
          case None => throw new BadRequestException("no container with id matching ApiEndpoint \"implementation_id\"")
          case Some(c) => {
            val spec = ContainerSpec.fromResourceInstance(c).get
            val (svcHost,svcPort) = spec.port_mappings.find(pm => pm.name.contains(portName)) match {
              case None =>
                throw new BadRequestException("no port mapping with the specified name on the specified container")
              case Some(ContainerSpec.PortMapping(_,_,_,_,_,_,Some(true),Some(ServiceAddress(svcHost,svcPort,_,_)),_,_,_,_)) =>
                (svcHost,svcPort)
              case Some(_) =>
                throw new BadRequestException("port mapping with the specified name was not exposed or did not contain service address")
            }
            s"http://${svcHost}:${svcPort}"
          }
        }
      case _ => throw new BadRequestException("ApiEndpoint \"implementation_type\" must be \"container\" or \"lambda\"")
    }
  }
  
  def createEndpoint(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[GestaltResourceInstance] = {
    val client = providerMethods.configureWebClient(provider, Some(ws))
    for(
      endpointProperties <- ResourceSerde.deserialize[ApiEndpointProperties](resource).liftTo[Future];
      _ <- if(endpointProperties.hosts.getOrElse(Seq()) == Seq() && endpointProperties.resource == None) {
        Future.failed(new RuntimeException(s"Either `hosts` or `resource` field must be specified on resource ${resource.id}"))
      }else {
        Future.successful(())
      };
      upstreamUrl <- eitherFromTry(Try(mkUpstreamUrl(endpointProperties))).liftTo[Future];
      updatedEndpointProperties = endpointProperties.copy(upstream_url=Some(upstreamUrl));
      parent <- Either.fromOption(endpointProperties.parent, "Parent must be set").liftTo[Future];
      updatedResource <- ResourceSerde.serialize[ApiEndpointProperties](resource, updatedEndpointProperties).liftTo[Future];
      le = LaserEndpoint(
        id = Some(resource.id.toString),
        apiId = parent,
        upstreamUrl = upstreamUrl,
        path = endpointProperties.resource,
        methods = endpointProperties.methods,
        plugins = endpointProperties.plugins,
        hosts = endpointProperties.hosts.filter(_.nonEmpty)
      );
      endpointEnts = ResourceFactory.findDescendantEntitlements(resource.id, "apiendpoint.invoke").headOption map Entitlement.make;
      usersAndGroups = endpointEnts.flatMap(_.properties.identities).getOrElse(Seq.empty).partition(
        id => ResourceFactory.findById(ResourceIds.User, id).isDefined
      );
      endpointWithAuth = le.updateWithAuthorization(usersAndGroups._1, usersAndGroups._2);
      result <- client.post(s"/apis/${parent}/endpoints", Some(Json.toJson(endpointWithAuth)))
    ) yield {
      if (Seq(200, 201, 202).contains(result.status)) {
        log.info("Successfully created Endpoint in GatewayManager.")
        updatedResource
      } else {
        throw ApiError(result.status, result.body).throwable
      }
    }
  }
  def updateEndpoint(provider: GestaltResourceInstance, resource: GestaltResourceInstance, patch: PatchDocument): Future[GestaltResourceInstance] = {
    
    def processPlugins(resource: GestaltResourceInstance, ps: JsObject): JsObject = {
      if((ps \ "gestaltSecurity" \ "enabled").asOpt[Boolean].contains(true)) {
        val gestaltPlugin = (ps \ "gestaltSecurity").as[JsObject]
        val ids = ResourceFactory.findDescendantEntitlements(resource.id, "apiendpoint.invoke").headOption map Entitlement.make flatMap (_.properties.identities) getOrElse Seq.empty
        val (userIds,groupIds) = ids.partition(id => ResourceFactory.findById(ResourceIds.User, id).isDefined)
        val updatedGestaltPlugin = gestaltPlugin ++ Json.obj(
          "users" -> userIds,
          "groups" -> groupIds
        )
        ps ++ Json.obj("gestaltSecurity" -> updatedGestaltPlugin)
      }else {
        ps
      }
    }

    val client = providerMethods.configureWebClient(provider, Some(ws))
    for(
      endpointProperties <- ResourceSerde.deserialize[ApiEndpointProperties](resource).liftTo[Future];
      upstreamUrl <- eitherFromTry(Try(mkUpstreamUrl(endpointProperties))).liftTo[Future];
      response <- client.get(s"/endpoints/${resource.id}");
      _ <- if(response.status == 200) {
        Future.successful(())
      }else if(response.status == 404) {
        Future.failed(new ResourceNotFoundException(s"No ApiEndpoint with ID '${resource.id}' was found in gestalt-api-gateway"))
      }else {
        Future.failed(ApiError(response.status, response.body).throwable)
      };
      le <- eitherFromJsResult(response.json.validate[LaserEndpoint]).liftTo[Future];
      _ = log.debug("Endpoint found in ApiGateway provider.");
      plugins = endpointProperties.plugins orElse le.plugins.map(_.as[JsObject]) getOrElse Json.obj();
      newPlugins = processPlugins(resource, plugins);
      newLe = le.copy(
        path = endpointProperties.resource,
        hosts = endpointProperties.hosts,
        methods = endpointProperties.methods orElse le.methods,
        plugins = Some(newPlugins),
        upstreamUrl = upstreamUrl
      );
      updatedEndpointProperties = endpointProperties.copy(
        methods = endpointProperties.methods orElse le.methods,
        upstream_url = Some(upstreamUrl),
        plugins=Some(newPlugins)
      );
      updatedResource <- ResourceSerde.serialize[ApiEndpointProperties](resource, updatedEndpointProperties).liftTo[Future];
      response <- client.put(s"/endpoints/${resource.id}", Some(Json.toJson(newLe)));
      _ <- if(response.status == 200) {
        log.info(s"Successfully PUT ApiEndpoint to ApiGateway provider.")
        Future.successful(())
      }else {
        Future.failed(ApiError(response.status, response.body).throwable)
      }
    ) yield updatedResource
  }
  def deleteEndpoint(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[Unit] = {
    val client = providerMethods.configureWebClient(provider, Some(ws))
    client.delete(s"/endpoints/${resource.id}") map { result =>
      log.debug("Response from GatewayManager: " + result.body)
      ()
    }
  }
}