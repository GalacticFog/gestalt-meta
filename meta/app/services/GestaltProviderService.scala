package services

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.{ContainerStats, SecretSpec}

import scala.concurrent.{ExecutionContext, Future}

trait GestaltProviderService

trait CaasService extends GestaltProviderService {

  def find(context: ProviderContext, container: GestaltResourceInstance): Future[Option[ContainerStats]]

  def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]]

  def create(context: ProviderContext, container: GestaltResourceInstance)
            (implicit ec: ExecutionContext): Future[GestaltResourceInstance]

  def createSecret(context: ProviderContext, metaResource: GestaltResourceInstance, items: Seq[SecretSpec.Item])
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance]

  def createVolume(context: ProviderContext, metaResource: GestaltResourceInstance)
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance]

  def createJob(context: ProviderContext, metaResource: GestaltResourceInstance)
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance]

  def destroy(container: GestaltResourceInstance): Future[Unit]

  def destroySecret(secret: GestaltResourceInstance): Future[Unit]

  def destroyVolume(secret: GestaltResourceInstance): Future[Unit]
  
  def destroyJob(job: GestaltResourceInstance): Future[Unit]

  def update(context: ProviderContext, container: GestaltResourceInstance)
            (implicit ec: ExecutionContext): Future[GestaltResourceInstance]

  def updateVolume(context: ProviderContext, metaResource: GestaltResourceInstance)
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance]

  def scale(context: ProviderContext, container: GestaltResourceInstance, numInstances: Int): Future[GestaltResourceInstance]

}