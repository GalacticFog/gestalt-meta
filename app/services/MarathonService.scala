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

class MarathonService @Inject() ( marathonClientFactory: MarathonClientFactory ) extends CaasService with JsonInput with MetaControllerUtils {

  import scala.language.implicitConversions
  implicit def jsval2obj(jsv: JsValue) = jsv.as[JsObject]

  def create( context: ProviderContext, container: GestaltResourceInstance )
            ( implicit ec: ExecutionContext ): Future[GestaltResourceInstance] = {
    log.debug("Entered create(...)")

    def updateSuccessfulLaunch(resource: GestaltResourceInstance)(marathonResponse: JsValue): GestaltResourceInstance = {
      log.debug("Entered updateSuccessfulLaunch(...)")
      val marathonAppId = (marathonResponse \ "id").as[String]
      // This parses the marathon VIP labels into meta port_mapping.service_address
      val updated = updateServiceAddresses(marathonResponse, container)
      upsertProperties(updated,
        "external_id" -> marathonAppId,
        "status" -> "LAUNCHED"
      )
    }
    
    def updateFailedLaunch(resource: GestaltResourceInstance)(t: Throwable): Throwable = {
      val updatedResource = upsertProperties(resource,
        "status" -> "LAUNCH_FAILED"
      )
      new BadRequestException(s"launch failed: ${t.getMessage}")
    }

    ContainerSpec.fromResourceInstance(container) match {
      case Failure(e) => Future.failed(e)
      case Success(spec) =>          
        val marathonApp = toMarathonLaunchPayload(
          fqon = context.fqon,
          workspaceName = context.workspace.name,
          environmentName = context.environment.name,
          name = container.name,
          props = spec,
          provider = context.provider
        )
        log.debug("About to launch container...")
        val marathonAppCreatePayload = Json.toJson(marathonApp).as[JsObject]
        
        log.debug("APP-PAYLOAD: ")
        log.debug(Json.prettyPrint(marathonAppCreatePayload))

        marathonClientFactory.getClient(context.provider).launchApp(
          fqon = context.fqon,
          wrkName = context.workspace.name,
          envName = context.environment.name,
          name = spec.name,
          marPayload = marathonAppCreatePayload
        ).transform( updateSuccessfulLaunch(container), updateFailedLaunch(container) )
    }
  }


  def destroyContainer(container: GestaltResourceInstance): Future[Unit] = {
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
    marathonClientFactory.getClient(context.provider).listApplicationsInEnvironment(context.fqon, context.workspace.name, context.environment.name)
  }

  private[services] def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }

  private[services] def futureToFutureTry[T](f: Future[T]): Future[Try[T]] = f.map(Success(_)).recover({case x => Failure(x)})

  private[services] def updateServiceAddresses(marApp: JsValue, r: GestaltResourceInstance): GestaltResourceInstance = {
    val clusterid = ".marathon.l4lb.thisdcos.directory"

    log.debug("Parsing portMappings...")
    val portMappings = Js.find(marApp, "/container/docker/portMappings") map { portmaps =>
      /*
       * This guard is to get around a bug in the dcos API where, if you pass in
       * an empty array (or omit the array) for container.docker.portMappings, the
       * API returns `portMappings: null` (instead of the expected nothing or empty array).
       */
      if (portmaps == JsNull) {
        log.warn("[container.docker.portMappings] == null")
        Seq.empty
      }
      else {
        portmaps.as[JsArray].value map { pm =>
          val name = Js.find(pm, "/name") map { _.as[String] } getOrElse {
            throw new RuntimeException(s"Could not parse 'name' from portMapping")
          }
          val port = Js.find(pm, "/containerPort") map { _.as[Int] } getOrElse {
            throw new RuntimeException(s"Could not parse 'port' from portMapping")
          }
          val protocol = Js.find(pm, "/protocol").map(_.as[String]) orElse Some("tcp")
          Js.find(pm.as[JsObject], "/labels").foldLeft(Map[String,ServiceAddress]()) { (acc, next) =>
            val labelvalue = next.as[Map[String, String]] collect {
              case (k,v) if k.matches("VIP_[0-9]+") => v.split(":").head.stripPrefix("/") + clusterid
            }
            acc + (name -> ServiceAddress(labelvalue.headOption.get, port, protocol))
          }
        }
      }
    }

    log.debug("Parsing portDefinitions...")

    val portDefinitions =
      if (portMappings.isDefined && portMappings.get.nonEmpty) {
        log.debug("portMappings were found - ignoring portDefinitions")
        Option(Seq[Map[String,ContainerSpec.ServiceAddress]]())
      } else {
        Js.find(marApp, "/portDefinitions") map { portdefs =>
          if (portdefs == JsNull) {
            log.info("[container.portDefinitions] is null")
            Seq.empty
          } else {
            portdefs.as[JsArray].value map { pd =>

              val name = Js.find(pd, "/name") map { _.as[String] } getOrElse {
                throw new RuntimeException(s"Could not parse 'name' from portDefinition")
              }

              val port = Js.find(pd, "/port") map { _.as[Int] } getOrElse {
                throw new RuntimeException(s"Could not parse 'port' from portDefinition")
              }

              val protocol = Js.find(pd, "/protocol").map(_.as[String]) orElse Some("tcp")

              Js.find(pd.as[JsObject], "/labels").foldLeft(Map[String,ServiceAddress]()) { (acc, next) =>
                val labelvalue = next.as[Map[String, String]] collect {
                  case (k,v) if k.matches("VIP_[0-9]+") => v.split(":").head.stripPrefix("/") + clusterid
                }
                acc + (name -> ServiceAddress(labelvalue.headOption.get, port, protocol))
              }
            }
          }
        }
      }

    val ports = {
      (portMappings getOrElse Seq.empty) ++ (portDefinitions getOrElse Seq.empty)
    }

    val resultResource = {
      if (portMappings.isEmpty) {
        log.debug("No ServiceAddresses found - returning original resource.")
        r
      }
      else {
        val portList = ports.flatten.toMap

        // This loads Meta PortMappings from the resource
        val metaPortMaps = r.properties.get.get("port_mappings") map {
          p => Js.parse[Seq[PortMapping]](Json.parse(p)) getOrElse {
            throw new RuntimeException("Could not parse portMappings to JSON.")
          }
        }

        def isExposed(p: PortMapping): Boolean =
          p.expose_endpoint getOrElse false

        // Get PortMappings where `expose_endpoint == true`
        val exposedPorts = metaPortMaps.get collect { case p if isExposed(p) =>
          p.copy(service_address = Some(portList(p.name.get)))
        }

        if (exposedPorts.isEmpty) {
          log.info("No exposed ports found. Returning.")
          r
        }
        else {
          log.info(s"${exposedPorts.size} exposed ports found.")

          // Replace r.properties.port_mappings
          val newproperties = r.properties.get ++ Map(
            "port_mappings" -> Json.toJson(exposedPorts).toString)

          r.copy(properties = Some(newproperties))
        }
      }
    }
    resultResource
  }

}