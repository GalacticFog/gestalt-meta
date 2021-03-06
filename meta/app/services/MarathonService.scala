package services

import java.util.UUID

import akka.actor.ActorRef
import play.api.libs.ws.WSClient

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import scala.concurrent.{ExecutionContext, Future}
import com.galacticfog.gestalt.data.{Instance, ResourceFactory, ResourceState}
import com.galacticfog.gestalt.data.models.{ResourceLike, GestaltResourceInstance}
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.errors.{BadRequestException, InternalErrorException, UnprocessableEntityException}
import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.api.{ContainerSpec, ContainerStats, SecretSpec, VolumeSpec}
import com.google.inject.Inject
import play.api.libs.json._
import controllers.util._
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.meta.api.ContainerSpec._
import com.google.inject.name.Named
import akka.pattern.ask
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceStates}
import play.api.Logger

import scala.concurrent.duration._
import scala.language.postfixOps

trait MarathonClientFactory {
  def getClient(provider: Instance): Future[MarathonClient]
}

class DefaultMarathonClientFactory @Inject() ( defaultClient: WSClient,
                                               @Named("permissive-wsclient") permissiveClient: WSClient,
                                               @Named(DCOSAuthTokenActor.name) dcosTokenActor: ActorRef )
  extends MarathonClientFactory {

  import MarathonService._

  private[this] val log = Logger(this.getClass)

  override def getClient(provider: Instance): Future[MarathonClient] = {
    val providerConfig = ContainerService.getProviderConfig(provider) getOrElse {throw new RuntimeException("provider 'properties.config' missing or not parsable")}

    val dcosUrl = (providerConfig \ Properties.DCOS_BASE_URL).asOpt[String]

    val marathonBaseUrl = (providerConfig \ Properties.MARATHON_BASE_URL).asOpt[String]
      .orElse(dcosUrl.map(_.stripSuffix("/") + "/service/marathon"))
      .getOrElse {throw new RuntimeException("DCOS provider must have one of 'properties.config.dcos_url' or 'properties.config.url'")}

    val secretSupport = (providerConfig \ Properties.SECRET_SUPPORT).asOpt[Boolean] getOrElse false
    log.debug("Secret support: " + secretSupport)

    val secretStore = (providerConfig \ Properties.SECRET_STORE).asOpt[String] getOrElse MarathonClient.DEFAULT_SECRET_STORE

    val secretBaseUrl = (providerConfig \ Properties.SECRET_BASE_URL).asOpt[String]
      .orElse(dcosUrl.map(_.stripSuffix("/") + "/secrets/v1"))
      .filter(_ => secretSupport)

    log.debug("Marathon URL: " + marathonBaseUrl)
    log.debug("Secret URL: " + secretBaseUrl)

    val acceptAnyCert = (providerConfig \ Properties.ACCEPT_ANY_CERT).asOpt[Boolean].getOrElse(false)
    val wsclient = if (acceptAnyCert) {
      permissiveClient
    } else {
      defaultClient
    }
    val auth = (providerConfig \ Properties.AUTH_CONFIG).asOpt[JsObject] getOrElse Json.obj()

    val baseClient = MarathonClient(wsclient, marathonBaseUrl, None, secretBaseUrl, secretStore)

    val acsTokenRequestTimeout = (providerConfig \ Properties.ACS_TOKEN_REQUEST_TIMEOUT)
      .asOpt[String]
      .flatMap(s => Try(Duration(s)).toOption)
      .collect({case fd: FiniteDuration => fd})
      .getOrElse(DEFAULT_ACS_TOKEN_REQUEST_TIMEOUT)

    (auth \ "scheme").asOpt[String] match {
      case Some("acs") =>
        val tokReq = for {
          id <- (auth \ "service_account_id").asOpt[String]
          key <- (auth \ "private_key").asOpt[String]
          url <- (auth \ "dcos_base_url").asOpt[String]
        } yield DCOSAuthTokenActor.DCOSAuthTokenRequest(provider.id, acceptAnyCert, id, key, url)
        tokReq.fold[Future[MarathonClient]] {
          Future.failed(BadRequestException("provider with 'acs' authentication was missing required fields"))
        } {
          req =>
            val fTokenResp = dcosTokenActor.ask(req)(acsTokenRequestTimeout)
            fTokenResp flatMap {
              case DCOSAuthTokenActor.DCOSAuthTokenResponse(authToken) =>
                log.debug("provisioning MarathonClient with acs auth token")
                Future.successful(baseClient.copy(
                  acsToken = Some(authToken)
                ))
              case DCOSAuthTokenActor.DCOSAuthTokenError(msg) =>
                Future.failed(BadRequestException(s"error from DCOSAuthTokenActor: ${msg}"))
              case _ =>
                Future.failed(InternalErrorException("unexpected response from DCOSAuthTokenActor"))
            }
        }
      case _ =>
        log.debug("provisioning MarathonClient without authentication credentials")
        Future.successful(baseClient)
    }
  }

}

class MarathonService @Inject() ( marathonClientFactory: MarathonClientFactory )
  extends CaasService with JsonInput with MetaControllerUtils {

  import MarathonService.Properties
  import scala.language.implicitConversions
  implicit def jsval2obj(jsv: JsValue): JsObject = jsv.as[JsObject]

  def create( context: ProviderContext, container: Instance )
            ( implicit ec: ExecutionContext ): Future[Instance] = {
    log.debug("Entered create(...)")

    def updateSuccessfulLaunch(resource: Instance)(marathonResponse: JsValue): Instance = {
      log.debug("Entered updateSuccessfulLaunch(...)")
      val marathonAppId = (marathonResponse \ "id").as[String]
      // This parses the marathon VIP labels into meta port_mapping.service_address
      val updated = updateServiceAddresses( context.provider, marathonResponse, resource)
      upsertProperties(updated,
        "external_id" -> marathonAppId,
        "status" -> "LAUNCHED"
      )
    }

    def updateFailedLaunch(resource: Instance)(t: Throwable): Throwable = {
      BadRequestException(s"launch failed: ${t.getMessage}")
    }

    val fMarClient = marathonClientFactory.getClient(context.provider)
    ContainerSpec.fromResourceInstance(container) match {
      case Failure(e) => Future.failed(e)
      case Success(spec) =>
        val marathonApp = toMarathonLaunchPayload(
          uncheckedFQON = context.fqon,
          namespace = siblingMarathonNamespace(context).getOrElse(defaultMarathonNamespace(context).get),
          environment = context.environment,
          props = spec,
          provider = context.provider
        )
        val marathonAppCreatePayload = Json.toJson(marathonApp)
        log.debug("Marathon v2 application payload: ")
        log.debug(marathonAppCreatePayload.toString)
        for {
          marClient <- fMarClient
          resp <- marClient.launchApp(
            marPayload = marathonAppCreatePayload
          ).transform( updateSuccessfulLaunch(container), updateFailedLaunch(container) )
        } yield resp
    }
  }

  def destroy(container: GestaltResourceInstance): Future[Unit] = {
    val providerId = Json.parse(container.properties.get("provider")) \ "id"
    val provider   = ResourceFactory.findById(UUID.fromString(providerId.as[String])) getOrElse {
      throw new RuntimeException("Could not find Provider : " + providerId)
    }
    val fMarClient = marathonClientFactory.getClient(provider)
    // TODO: what to do if there is no external_id ? delete the resource? attempt to reconstruct external_id from resource?
    val maybeExternalId = ContainerService.resourceExternalId(container)

    maybeExternalId match {
      case Some(eid) =>
        for {
          marClient <- fMarClient
          resp <- marClient.deleteApplication(eid)
          _ = log.debug(s"response from MarathonClient.deleteApplication:\n$resp")
        } yield ()
      case None =>
        log.warn(s"no external_id property in container ${container.id}, will not attempt delete against provider")
        Future.successful(())
    }
  }

  def find(context: ProviderContext, container: Instance): Future[Option[ContainerStats]] = {
    // Lookup container in marathon, convert to ContainerStats
    ContainerService.resourceExternalId(container) match {
      case None => Future.successful(None)
      case Some(eid) =>
        for {
          client <- marathonClientFactory.getClient(context.provider)
          js <- client.getApplicationByAppId(eid)
          stats <- Future.fromTry(Try {MarathonClient.marathon2Container(js).get})
        } yield Some(stats)
    }
  }

  def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]] = {
    val externalIds = for {
      cts <- ResourceFactory.findChildrenOfType(typeId = ResourceIds.Container, parentId = context.environmentId)
      eid <- cts.properties.flatMap(_.get("external_id"))
    } yield eid
    val sharedPrefix = externalIds.map({
      eid => eid.stripPrefix("/").split("/").dropRight(1).scanLeft("/")(_ + _ + "/").toSet
    }) match {
      case Nil =>
        val prefix = MarathonService.getAppGroupPrefix(context.provider)
        metaContextToMarathonAppGroup(prefix, context.fqon, context.workspace.name, context.environment.name)
      case nonempty =>
        nonempty.reduce(_ intersect _ ).max.stripSuffix("/")
    }
    for {
      marClient <- marathonClientFactory.getClient(context.provider)
      list <- marClient.listApplicationsInEnvironment(sharedPrefix)
    } yield list
  }

  private[services] def upsertProperties(resource: Instance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }

  /**
    * Look at the Marathon payload and extract VIP_{n} labels from portDefinitions
    *
    * A port from a Mesos container can have multiple VIPs assigned, but the Meta Container API only allows a single service address. In this case,
    * we will save only the first VIP. It should be noted that Mesos containers created by the Meta Container API and unmodified will therefore only
    * ever have a single VIP per port.
    *
    * @param marApp the marathon application definition, returned from a GET/LIST
    * @param origResource the resource for the container being updated
    * @return a resource for the container, potentially updated with ServiceAddress info for the port mappings
    */
  private[services] def updateServiceAddresses(provider: Instance, marApp: JsValue, origResource: Instance): Instance = {
    val providerConfig = ContainerService.getProviderConfig(provider) getOrElse Json.obj()
    val marathonFrameworkName = (providerConfig \ Properties.MARATHON_FRAMEWORK_NAME).asOpt[String].getOrElse("marathon")
    val dcosClusterName = (providerConfig \ Properties.DCOS_CLUSTER_NAME).asOpt[String].getOrElse("thisdcos")
    val clusterid = s".${marathonFrameworkName}.l4lb.${dcosClusterName}.directory"
    val VIPLabel = "VIP_([0-9]+)".r
    val VIPValue = "/([-a-z0-9.]+):(\\d+)".r

    val serviceAddresses = (
      (marApp \ "container" \ "docker" \ "portMappings").asOpt[Seq[JsValue]] orElse
        (marApp \ "container" \ "portMappings").asOpt[Seq[JsValue]] orElse
        (marApp \ "portDefinitions").asOpt[Seq[JsValue]]
      ).map {
        /*
         * port names are required by Meta Container API, but not by Marathon,
         * therefore we will map service addresses using port index
         */
      _.zipWithIndex flatMap { case (portDef, portIndex) =>
        // 'protocol' is optional in marathon API, default is "tcp"
        val protocol = Js.find(portDef, "/protocol").map(_.as[String]) orElse Some("tcp")
        for {
          labels   <- Js.find(portDef.as[JsObject], "/labels")
          labelMap <- labels.asOpt[Map[String, String]]
          serviceAddress <- labelMap.collectFirst { // collectFirst, because one VIP is as good as another
            case (VIPLabel(_), VIPValue(address, port)) =>
              ServiceAddress(address + clusterid, port.toInt, protocol)
          }
        } yield (portIndex -> serviceAddress)
      } toMap
    } getOrElse Map.empty

    // This loads Meta PortMappings from the resource
    val metaPortMaps = origResource.properties.flatMap(_.get("port_mappings")) map {
      p => Js.parse[Seq[PortMapping]](Json.parse(p)) getOrElse {
        throw new RuntimeException("Could not parse portMappings to JSON.")
      }
    } getOrElse Seq.empty

    val updatedPortMappings = metaPortMaps.zipWithIndex map {
      case (pm,portIndex) => pm.copy(
        service_address = serviceAddresses.get(portIndex),
        lb_port = (pm.lb_port orElse pm.container_port).filter(_ => pm.expose_endpoint.contains(true))
      )
    }

    val newproperties = origResource.properties.getOrElse(Map.empty) ++ Map(
      "port_mappings" -> Json.toJson(updatedPortMappings).toString
    )
    origResource.copy(properties = Some(newproperties))
  }

  private[services] def vhostVariableName(portMappingName: String, index: Int) = {
    val prefix = portMappingName.trim.replaceAll("-","_").toUpperCase
    "%s_VHOST_%d".format(prefix, index)
  }

  def scale(context: ProviderContext, container: Instance, numInstances: Int): Future[Instance] = {
    ContainerService.resourceExternalId(container) match {
      case None => Future.failed(new RuntimeException("container.properties.external_id not found."))
      case Some(external_id) =>
        val provider = ContainerService.caasProvider(ContainerService.containerProviderId(container))
        for {
          marClient <- marathonClientFactory.getClient(provider)
          _ <- marClient.scaleApplication(
            appId = external_id,
            numInstances = numInstances
          )
        } yield upsertProperties(
          container,
          "num_instances" -> numInstances.toString
        )
    }
  }

  def update(context: ProviderContext, container: Instance)
                     (implicit ec: ExecutionContext): Future[Instance] = {
    ContainerService.resourceExternalId(container) match {
      case None =>
        Future.failed(new RuntimeException("container.properties.external_id not found."))
      case Some(external_id) =>
        val provider = ContainerService.caasProvider(ContainerService.containerProviderId(container))
        val fMarClient = marathonClientFactory.getClient(provider)
        ContainerSpec.fromResourceInstance(container) match {
          case Failure(e) =>
            Future.failed(e)
          case Success(spec) =>
            val marathonApp = toMarathonLaunchPayload(
              uncheckedFQON = context.fqon,
              namespace = "/ignore",
              environment = context.environment,
              props = spec,
              provider = context.provider
            ).copy(
              id = Some(external_id)
            )
            val putPayload = Json.toJson(marathonApp)
            log.debug("Marathon PUT payload: " + putPayload)
            for {
              marClient <- fMarClient
              resp <- marClient.updateApplication(
                appId = external_id,
                marPayload = putPayload
              )
              _ = log.debug(s"Marathon PUT response: $resp")
              updated = updateServiceAddresses(context.provider, putPayload, container)
              _ = log.debug("Meta resource with updated service addresses: " + Json.toJson(Output.renderInstance(updated)))
            } yield updated
        }
    }
  }

  def createSecret(context: ProviderContext, metaResource: Instance, items: Seq[SecretSpec.Item])
                           (implicit ec: ExecutionContext): Future[Instance] = {
    log.debug("Entered createSecret(...)")

    def updateSuccessfulLaunch(id: String, resource: Instance)(marathonResponse: Unit): Instance = {
      upsertProperties(resource.copy(
        state = ResourceState.id(ResourceStates.Active)
      ), "external_id" -> id)
    }

    def createFailed(resource: Instance)(t: Throwable): Throwable = {
      ResourceFactory.hardDeleteResource(resource.id)
      UnprocessableEntityException(s"secret creation failed: ${t.getMessage}")
    }

    val fMarClient = marathonClientFactory.getClient(context.provider)
    SecretSpec.fromResourceInstance(metaResource) match {
      case Failure(e) => Future.failed(e)
      case Success(spec) =>
        val (secretId, secretPayload) = toDcosSecretPayload( context, spec, items )
        log.debug(s"DCOS v1 secret payload: $secretPayload")
        for {
          marClient <- fMarClient
          resp <- marClient.createSecret(
            secretId, secretPayload
          ).transform( updateSuccessfulLaunch(secretId, metaResource), createFailed(metaResource) )
        } yield resp
    }
  }

  def destroySecret(secret: GestaltResourceInstance): Future[Unit] = {
    val providerId = Json.parse(secret.properties.get("provider")) \ "id"
    val provider   = ResourceFactory.findById(UUID.fromString(providerId.as[String])) getOrElse {
      throw new RuntimeException("Could not find Provider : " + providerId)
    }
    val fMarClient = marathonClientFactory.getClient(provider)
    val maybeExternalId = ContainerService.resourceExternalId(secret)

    maybeExternalId match {
      case Some(eid) =>
        for {
          marClient <- fMarClient
          _ <- marClient.deleteSecret(eid)
        } yield ()
      case None =>
        log.debug(s"no external_id property in container ${secret.id}, will not attempt delete against provider")
        Future.successful(())
    }
  }

  def createVolume(context: ProviderContext, metaResource: Instance)(implicit ec: ExecutionContext): Future[Instance] = {
    Future.fromTry(for {
      spec <- VolumeSpec.fromResourceInstance(metaResource)
      v <- spec.`type` match {
        case VolumeSpec.HostPath | VolumeSpec.Persistent => Success(metaResource)
        case _ => Failure(new BadRequestException("DC/OS only supports volumes of type 'host_path' and 'persistent'"))
      }
    } yield v)
  }

  def updateVolume(context: ProviderContext, metaResource: Instance)(implicit ec: ExecutionContext): Future[Instance] = {
    log.warn("MarathonService::updateVolume is currently a no-op and is not expected to be called")
    Future.successful(metaResource)
  }

  def destroyVolume(secret: GestaltResourceInstance): Future[Unit] = Future.successful(())

  def createJob(context: ProviderContext, metaResource: Instance)
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = create(context, metaResource)
  
  def destroyJob(job: GestaltResourceInstance): Future[Unit] = destroy(job)
}

object MarathonService {

  def getAppGroupPrefix(provider: ResourceLike): Option[String] = {
    ContainerService.getProviderProperty[String](provider, APP_GROUP_PREFIX_PROP) orElse
      ContainerService.getProviderProperty[String](provider, APP_GROUP_PREFIX_PROP_LEGACY)
  }

  val DEFAULT_ACS_TOKEN_REQUEST_TIMEOUT: FiniteDuration = 10 seconds
  object Properties {
    val MARATHON_FRAMEWORK_NAME = "marathon_framework_name"
    val DCOS_CLUSTER_NAME = "dcos_cluster_name"
    val ACCEPT_ANY_CERT = "accept_any_cert"
    val MARATHON_BASE_URL = "url"
    val DCOS_BASE_URL = "dcos_url"
    val SECRET_BASE_URL = "secret_url"
    val SECRET_SUPPORT = "secret_support"
    val SECRET_STORE = "secret_store"
    val AUTH_CONFIG = "auth"
    val ACS_TOKEN_REQUEST_TIMEOUT = "acs_token_request_timeout"
  }
}
