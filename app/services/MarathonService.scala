package services

import scala.language.implicitConversions
import java.util.UUID

import play.api.libs.ws.WS
import play.api.Play.current

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import scala.concurrent.{ExecutionContext, Future}
import com.galacticfog.gestalt.data.{Instance, ResourceFactory}
import com.galacticfog.gestalt.data.models.{GestaltResourceInstance}
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.google.inject.Inject
import skuber.api.client._
import play.api.libs.json._
import controllers.util._
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.meta.api.ContainerSpec._
import scala.language.postfixOps

trait MarathonClientFactory {
  def getClient(provider: GestaltResourceInstance): MarathonClient
}

class DefaultMarathonClientFactory extends MarathonClientFactory {
  override def getClient(provider: Instance): MarathonClient = {
    val providerUrl = (Json.parse(provider.properties.get("config")) \ "url").as[String]
    log.debug("Marathon URL: " + providerUrl)
    MarathonClient(WS.client, providerUrl)
  }
}

class MarathonService @Inject() ( marathonClientFactory: MarathonClientFactory )
  extends CaasService with JsonInput with MetaControllerUtils {

  import scala.language.implicitConversions
  implicit def jsval2obj(jsv: JsValue) = jsv.as[JsObject]

  def create( context: ProviderContext, container: GestaltResourceInstance )
            ( implicit ec: ExecutionContext ): Future[GestaltResourceInstance] = {
    log.debug("Entered create(...)")

    def updateSuccessfulLaunch(resource: GestaltResourceInstance)(marathonResponse: JsValue): GestaltResourceInstance = {
      log.debug("Entered updateSuccessfulLaunch(...)")
      val marathonAppId = (marathonResponse \ "id").as[String]
      // This parses the marathon VIP labels into meta port_mapping.service_address
      val updated = updateServiceAddresses(marathonResponse, resource)
      upsertProperties(updated,
        "external_id" -> marathonAppId,
        "status" -> "LAUNCHED"
      )
    }
    
    def updateFailedLaunch(resource: GestaltResourceInstance)(t: Throwable): Throwable = {
      // TODO: this has no side-effect
      val updatedResource = upsertProperties(resource,
        "status" -> "LAUNCH_FAILED"
      )
      new BadRequestException(s"launch failed: ${t.getMessage}")
    }
    
    ContainerSpec.fromResourceInstance(container) match {
      case Failure(e) => Future.failed(e)
      case Success(spec) =>
        val marathonApp = toMarathonLaunchPayload(
          uncheckedFQON = context.fqon,
          uncheckedWrkName = context.workspace.name,
          uncheckedEnvName = context.environment.name,
          uncheckedCntrName = container.name,
          props = spec,
          provider = context.provider
        )
        val marathonAppCreatePayload = Json.toJson(marathonApp)
        log.debug("Marathon v2 application payload: ")
        log.debug(Json.prettyPrint(marathonAppCreatePayload))
        val output = marathonClientFactory.getClient(context.provider).launchApp(
          marPayload = marathonAppCreatePayload
        ).transform( updateSuccessfulLaunch(container), updateFailedLaunch(container) )
        
        output
    }
  }

  def destroy(container: GestaltResourceInstance): Future[Unit] = {
    val providerId = Json.parse(container.properties.get("provider")) \ "id"
    val provider   = ResourceFactory.findById(UUID.fromString(providerId.as[String])) getOrElse {
      throw new RuntimeException("Could not find Provider : " + providerId)
    }
    // TODO: what to do if there is no external_id ? delete the resource? attempt to reconstruct external_id from resource?
    val maybeExternalId = for {
      props <- container.properties
      eid <- props.get("external_id")
    } yield eid
    
    maybeExternalId match {
      case Some(eid) => marathonClientFactory.getClient(provider).deleteApplication(eid) map { js =>
        log.debug(s"response from MarathonClient.deleteApplication:\n${Json.prettyPrint(js)}")
      }
      case None =>
        log.debug(s"no external_id property in container ${container.id}, will not attempt delete against provider")
        Future.successful(())
    }
  }

  override def find(context: ProviderContext, container: GestaltResourceInstance): Future[Option[ContainerStats]] = {
    // Lookup container in marathon, convert to ContainerStats
    container.properties.flatMap(_.get("external_id")) match {
      case None => Future.successful(None)
      case Some(eid) =>
        for {
          client <- Future(marathonClientFactory.getClient(context.provider))
          js <- client.getApplicationByAppId(eid)
          stats <- Future.fromTry(Try {MarathonClient.marathon2Container(js).get})
        } yield Some(stats)
    }
  }

  override def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]] = {
    val prefix = getProviderProperty[String](context.provider, APP_GROUP_PREFIX_PROP)
    marathonClientFactory.getClient(context.provider).listApplicationsInEnvironment(prefix, context.fqon, context.workspace.name, context.environment.name)
  }

  private[services] def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }

  private[services] def futureToFutureTry[T](f: Future[T]): Future[Try[T]] = f.map(Success(_)).recover({case x => Failure(x)})

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
  private[services] def updateServiceAddresses(marApp: JsValue, origResource: GestaltResourceInstance): GestaltResourceInstance = {
    val clusterid = ".marathon.l4lb.thisdcos.directory"
    val VIPLabel = "VIP_([0-9]+)".r
    val VIPValue = "/([-a-z0-9.]+):(\\d+)".r

    val serviceAddresses = ((marApp \ "container" \ "docker" \ "network").asOpt[String] match {
      case Some("BRIDGE") =>
        log.debug("Detected BRIDGE networking, parsing portMappings...")
        Js.find(marApp, "/container/docker/portMappings") filterNot( _ == JsNull )
      case _ =>
        log.debug("Did not detect BRIDGE networking, parsing portDefinitions...")
        Js.find(marApp, "/portDefinitions") filterNot( _ == JsNull )
    }) map {
      /*
       * port names are required by Meta Container API, but not by Marathon, 
       * therefore we will map service addresses using port index
       */
      _.as[JsArray].value.zipWithIndex flatMap { case (portDef, portIndex) =>
        // 'protocol' is optional in marathon API, default is "tcp"
        val protocol = Js.find(portDef, "/protocol").map(_.as[String]) orElse Some("tcp") 
        for {
          labels   <- Js.find(portDef.as[JsObject], "/labels")
          labelMap <- Try(labels.as[Map[String, String]]).toOption
          serviceAddress <- labelMap.collectFirst {
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
      case (pm,portIndex) => pm.copy(service_address = serviceAddresses.get(portIndex))
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

  override def scale(context: ProviderContext, container: Instance, numInstances: Int): Future[Instance] = {
    container.properties.flatMap(_.get("external_id")) match {
      case None => Future.failed(new RuntimeException("container.properties.external_id not found."))
      case Some(external_id) =>
        val provider = ContainerService.caasProvider(ContainerService.containerProviderId(container))
        val marClient = marathonClientFactory.getClient(provider)
        marClient.scaleApplication(
          appId = external_id,
          numInstances = numInstances
        ) map { _ =>
          upsertProperties(
            container,
            "num_instances" -> numInstances.toString
          )
        }
    }
  }

  override def update(context: ProviderContext, container: Instance)
                     (implicit ec: ExecutionContext): Future[Instance] = {
    container.properties.flatMap(_.get("external_id")) match {
      case None => Future.failed(new RuntimeException("container.properties.external_id not found."))
      case Some(external_id) =>
        val provider = ContainerService.caasProvider(ContainerService.containerProviderId(container))
        val marClient = marathonClientFactory.getClient(provider)
        ContainerSpec.fromResourceInstance(container) match {
          case Failure(e) => Future.failed(e)
          case Success(spec) =>
            val marathonApp = toMarathonLaunchPayload(
              uncheckedFQON = context.fqon,
              uncheckedWrkName = context.workspace.name,
              uncheckedEnvName = context.environment.name,
              uncheckedCntrName = container.name,
              props = spec,
              provider = context.provider
            ).copy(
              id = Some(external_id)
            )
            marClient.updateApplication(
              appId = external_id,
              marPayload = Json.toJson(marathonApp)
            ) map { updateServiceAddresses(_, container) }
        }
    }
  }

}