package services
import java.util.UUID
import javax.inject.Inject

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.marathon.ContainerStats
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.spotify.docker.client.DockerClient
import com.spotify.docker.client.{messages => docker}
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

class DockerService @Inject() ( dockerClientFactory: DockerClientFactory ) extends CaasService {

  import DockerService._

  private[this] val log = LoggerFactory.getLogger(this.getClass)

  override def find(context: ProviderContext, container: GestaltResourceInstance): Future[Option[ContainerStats]] = ???

  override def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]] = ???

  private[services] def mkServiceSpec(id: UUID, containerSpec: ContainerSpec, providerId: UUID, fqon: String, workspaceId: UUID, environmentId: UUID): Try[docker.swarm.ServiceSpec] = {
    val allLabels = containerSpec.labels ++ Map[String,String](
      META_CONTAINER_KEY -> id.toString,
      META_ENVIRONMENT_KEY -> environmentId.toString,
      META_FQON_KEY -> fqon,
      META_WORKSPACE_KEY -> workspaceId.toString,
      META_PROVIDER_KEY -> providerId.toString
    )

    val maybeCmdArray = containerSpec.cmd map CommandParser.translate map (_.asJava)

    Try {
      docker.swarm.ServiceSpec.builder()
        .name(s"${environmentId}-${containerSpec.name}")
        .taskTemplate(docker.swarm.TaskSpec.builder()
          .containerSpec(
            docker.swarm.ContainerSpec.builder()
              .image(containerSpec.image)
              .args(containerSpec.args.map(_.asJava).getOrElse(null))
              .command(maybeCmdArray.getOrElse(null))
              .build()
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
    } yield spec
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

  override def destroy(container: GestaltResourceInstance): Future[Unit] = ???

  override def update(context: ProviderContext, container: GestaltResourceInstance)(implicit ec: ExecutionContext): Future[GestaltResourceInstance] = ???

  override def scale(context: ProviderContext, container: GestaltResourceInstance, numInstances: Int): Future[GestaltResourceInstance] = ???

  private[services] def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }
}
