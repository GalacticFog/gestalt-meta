package services
import java.util.UUID
import javax.inject.Inject

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.marathon.ContainerStats
import com.spotify.docker.client.DockerClient

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object DockerService {
  val META_CONTAINER_KEY = "meta/container"

  type DockerClient = com.spotify.docker.client.DockerClient
}

trait DockerClientFactory {
  def getDockerClient(providerId: UUID): Try[DockerClient]
}

class DockerService @Inject() ( dockerClientFactory: DockerClientFactory ) extends CaasService {

  import DockerService._

  override def find(context: ProviderContext, container: GestaltResourceInstance): Future[Option[ContainerStats]] = ???

  override def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]] = ???

  override def create(context: ProviderContext, container: GestaltResourceInstance)(implicit ec: ExecutionContext): Future[GestaltResourceInstance] = ???

  override def destroy(container: GestaltResourceInstance): Future[Unit] = ???

  override def update(context: ProviderContext, container: GestaltResourceInstance)(implicit ec: ExecutionContext): Future[GestaltResourceInstance] = ???

  override def scale(context: ProviderContext, container: GestaltResourceInstance, numInstances: Int): Future[GestaltResourceInstance] = ???
}
