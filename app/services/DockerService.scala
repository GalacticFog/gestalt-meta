package services
import java.util.UUID
import javax.inject.Inject

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.marathon.ContainerStats
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.ContainerSpec.{PortMapping, ServiceAddress}
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.spotify.docker.client.exceptions.ServiceNotFoundException
import com.spotify.docker.client.messages.swarm._
import com.spotify.docker.client.{DefaultDockerClient, DockerClient, messages => docker}
import controllers.util.ContainerService
import org.joda.time.DateTime
import org.slf4j.LoggerFactory
import play.api.libs.json.Json
import services.util.CommandParser

import collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object DockerService {
  val META_CONTAINER_KEY = "gestalt-meta/container"
  val META_ENVIRONMENT_KEY = "gestalt-meta/environment"
  val META_WORKSPACE_KEY = "gestalt-meta/workspace"
  val META_FQON_KEY = "gestalt-meta/fqon"
  val META_PROVIDER_KEY = "gestalt-meta/provider"

  type DockerClient = com.spotify.docker.client.DockerClient
}

trait DockerClientFactory {
  def getDockerClient(providerId: UUID): Try[DockerClient]
}

class DefaultDockerClientFactory @Inject() () extends DockerClientFactory {
  override def getDockerClient(providerId: UUID): Try[DockerClient] = {
    for {
      provider <- Try{ContainerService.caasProvider(providerId)}
      url <- ContainerService.getProviderProperty[String](provider, "url") match {
        case Some(c) => Success(c)
        case None => Failure(new RuntimeException("provider 'properties.config' missing or not parsable"))
      }
      client <- Try{DefaultDockerClient.builder().uri(url).build()}
    } yield client
  }
}

class DockerService @Inject() ( dockerClientFactory: DockerClientFactory ) extends CaasService {

  import DockerService._

  private[this] val log = LoggerFactory.getLogger(this.getClass)

  private[services] def mkServiceSpec(id: UUID, containerSpec: ContainerSpec, providerId: UUID, fqon: String, workspaceId: UUID, environmentId: UUID): Try[docker.swarm.ServiceSpec] = {
    val allLabels = containerSpec.labels ++ Map[String,String](
      META_CONTAINER_KEY -> id.toString,
      META_ENVIRONMENT_KEY -> environmentId.toString,
      META_FQON_KEY -> fqon,
      META_WORKSPACE_KEY -> workspaceId.toString,
      META_PROVIDER_KEY -> providerId.toString
    )

    /** for port mappings, docker has to modes for publishing a service:
      * vip mode (use the routing mesh): each swarm node will publish the service on the same (service) port
      *    we will allow use service_port semantics for this, and allow use to optionally specify the service port
      *    this is equivalent to Kube Service of type NodePort (what we do in the Kube adapter)
      * host mode: only swarm modes running a container will publish the port
      *    we will NOT support this right now; later on, we can support it, perhaps according to host_port:
      *    host_port match {
      *      case None => use vip mode
      *      case Some(0) => use host mode with random host port
      *      case Some(p) => use host mode with user-specified host port
      *    }
      */

    val maybeCmdArray = containerSpec.cmd map CommandParser.translate map (_.asJava)
    val env = containerSpec.env.map({
      case (k, v) => (k + "=" + v)
    }).toSeq

    Try {
      docker.swarm.ServiceSpec.builder()
        .name(s"${environmentId}-${containerSpec.name}")
        .mode(ServiceMode.withReplicas(containerSpec.num_instances))
        .taskTemplate(docker.swarm.TaskSpec.builder()
          .resources(ResourceRequirements.builder().limits(Resources.builder()
              .nanoCpus( (containerSpec.cpus * 1e9).toLong )
              .memoryBytes( (containerSpec.memory * 1024.0 * 1024.0).toLong )
              .build()
          ).build())
          .containerSpec(
            docker.swarm.ContainerSpec.builder()
              .image(containerSpec.image)
              .args(containerSpec.args.map(_.asJava).getOrElse(null))
              .command(maybeCmdArray.getOrElse(null))
              .env(env:_*)
              .build()
          )
          .build()
        )
        .endpointSpec(
          EndpointSpec.builder()
            .mode(EndpointSpec.Mode.RESOLUTION_MODE_VIP)
            .ports(
              containerSpec.port_mappings.collect({
                case PortMapping(protocol, Some(containerPort), _, maybeServicePort, Some(name), _, Some(true), _, _) =>
                  PortConfig.builder()
                    .name(name)
                    .protocol(protocol)
                    .publishMode(PortConfig.PortConfigPublishMode.INGRESS)
                    .targetPort(containerPort)
                    .publishedPort(maybeServicePort.map(int2Integer).getOrElse(null))
                    .build()
              }).asJava
            )
            .build()
        )
        .labels(allLabels.asJava)
        .build()
    }
  }

  private[services] def createService( docker: DockerClient,
                                       containerId: UUID,
                                       spec: ContainerSpec,
                                       providerId: UUID,
                                       fqon: String,
                                       workspaceId: UUID,
                                       environmentId: UUID )
                                     ( implicit ec: ExecutionContext ): Future[ContainerSpec] = {
    for {
      config <- Future.fromTry(mkServiceSpec(containerId, spec, providerId, fqon, workspaceId, environmentId))
      response <- Future{docker.createService(config)}
      newPortMappings = spec.port_mappings map {
        case pm @ PortMapping(proto, Some(cp), _, maybeSP, _, _, Some(true), _, maybeVHosts) =>
          pm.copy(service_address = Some(ServiceAddress(
            host = "",
            port = maybeSP getOrElse cp,
            protocol = Some(proto),
            virtual_hosts = maybeVHosts
          )))
        case pm => pm.copy(
          service_address = None
        )
      }
    } yield spec.copy(
      port_mappings = newPortMappings
    )
  }

  override def create(context: ProviderContext,
                      container: GestaltResourceInstance )
                     ( implicit ec: ExecutionContext ): Future[GestaltResourceInstance] = {
    log.debug("DockerService::create(...)")
    ContainerSpec.fromResourceInstance(container) match {
      case Failure(e) => Future.failed(e)
      case Success(spec) => for {
        docker     <- Future.fromTry(dockerClientFactory.getDockerClient(context.providerId))
        updatedContainerSpec <- createService(docker, container.id, spec, context.providerId, context.fqon, context.workspace.id, context.environmentId)
      } yield upsertProperties(
        container,
        "external_id" -> s"${context.environmentId}-${container.name}",
        "status" -> "LAUNCHED",
        "port_mappings" -> Json.toJson(updatedContainerSpec.port_mappings).toString()
      )
    }
  }

  override def destroy(container: GestaltResourceInstance): Future[Unit] = {
    import play.api.libs.concurrent.Execution.Implicits.defaultContext
    log.debug("DockerService::create(...)")
    val provider = ContainerService.containerProvider(container)
    val externalId = ContainerService.containerExternalId(container) orElse {
      for {
        envId <- ResourceFactory.findParent(ResourceIds.Environment, container.id).map(_.id.toString)
      } yield envId + "-" + container.name
    } getOrElse(throw new BadRequestException("Could not determine 'external_id' for container. Container resource may be corrupt."))
    for {
      docker <- Future.fromTry(dockerClientFactory.getDockerClient(provider.id))
      _      <- Future(docker.removeService(externalId))
    } yield ()
  }


  override def find(context: ProviderContext, container: GestaltResourceInstance): Future[Option[ContainerStats]] = ???


  override def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]] = {
    val inThisEnvironment: Service.Criteria = Service.Criteria.builder().addLabel(META_ENVIRONMENT_KEY, context.environmentId.toString).build()
    import play.api.libs.concurrent.Execution.Implicits.defaultContext
    log.debug("DockerService::listInEnvironment(...)")
    for {
      docker    <- Future.fromTry(dockerClientFactory.getDockerClient(context.providerId))
      svcs      <- Future{docker.listServices(inThisEnvironment).asScala}
    } yield svcs.map { svc =>
      println(svc.toString)
      ContainerStats(
        external_id = svc.spec().name(),
        containerType = "DOCKER",
        status = "RUNNING",
        cpus = Try{svc.spec().taskTemplate().resources().limits().nanoCpus().toDouble / 1e9}.getOrElse(0),
        memory = Try{svc.spec().taskTemplate().resources().limits().memoryBytes().toDouble / (1024*1024)}.getOrElse(0),
        image = svc.spec().taskTemplate().containerSpec().image(),
        age = new DateTime(svc.createdAt()),
        numInstances = Try{svc.spec().mode().replicated().replicas().toInt}.getOrElse(0),
        tasksStaged = 0,
        tasksRunning = 0,
        tasksHealthy = 0,
        tasksUnhealthy = 0,
        taskStats = None
      )
    }
  }

  override def update(context: ProviderContext, container: GestaltResourceInstance)(implicit ec: ExecutionContext): Future[GestaltResourceInstance] = ???

  override def scale(context: ProviderContext, container: GestaltResourceInstance, numInstances: Int): Future[GestaltResourceInstance] = {
    ???
//    import play.api.libs.concurrent.Execution.Implicits.defaultContext
//    log.debug("DockerService::scale(...)")
//    val provider = ContainerService.containerProvider(container)
//    val externalId = ContainerService.containerExternalId(container) orElse {
//      for {
//        envId <- ResourceFactory.findParent(ResourceIds.Environment, container.id).map(_.id.toString)
//      } yield envId + "-" + container.name
//    } getOrElse(throw new BadRequestException("Could not determine 'external_id' for container. Container resource may be corrupt."))
//    for {
//      docker    <- Future.fromTry(dockerClientFactory.getDockerClient(provider.id))
//      extantSvc <- getServiceOption(docker, externalId) flatMap {
//        case Some(svc) => Future.successful(svc)
//        case None => Future.failed(new RuntimeException(
//          s"could not locate associated Service in Docker Swarm provider for container ${container.id}"
//        ))
//      }
//      updatedSvc <- Future{
//        val newSvc = extantSvc.spec().mode().replicated().replicas()
//      }
//    } yield ()
  }

  private[services] def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }

  private[this] def getServiceOption(client: DockerClient, serviceName: String)(implicit ec: ExecutionContext): Future[Option[docker.swarm.Service]] = {
    Future{client.inspectService(serviceName)} map (Some(_)) recover {
      case snf: ServiceNotFoundException => None
    }
  }

}
