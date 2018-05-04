package services

import com.galacticfog.gestalt.data.Instance
import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.meta.api.{ContainerStats, SecretSpec}

import scala.concurrent.{ExecutionContext, Future}

trait GestaltProviderService

trait CaasService extends GestaltProviderService {

  type *** = Any

  type CaasContainer = ***
  type CaasPod = ***
  type CaasService = ***
  type CaasDeployment = ***

  def find(context: ProviderContext, container: GestaltResourceInstance): Future[Option[ContainerStats]]

  def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]]

  def create(context: ProviderContext, container: GestaltResourceInstance)
            (implicit ec: ExecutionContext): Future[GestaltResourceInstance]

  def createSecret(context: ProviderContext, metaResource: Instance, items: Seq[SecretSpec.Item])
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance]

  def destroy(container: ResourceLike): Future[Unit]

  def destroySecret(secret: ResourceLike): Future[Unit]

  def update(context: ProviderContext, container: GestaltResourceInstance)
            (implicit ec: ExecutionContext): Future[GestaltResourceInstance]

  def scale(context: ProviderContext, container: GestaltResourceInstance, numInstances: Int): Future[GestaltResourceInstance]

}