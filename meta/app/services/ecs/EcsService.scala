package services.ecs

import com.galacticfog.gestalt.data.{Instance, ResourceFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.{ContainerSpec,ContainerStats,SecretSpec}
import com.galacticfog.gestalt.util.ResourceSerde
import com.galacticfog.gestalt.util.EitherWithErrors._
import com.galacticfog.gestalt.util.FutureFromTryST._
import com.galacticfog.gestalt.integrations.ecs.EcsClient
import controllers.util.ContainerService
import java.util.UUID
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import cats.syntax.either._
import cats.syntax.traverse._
import cats.instances.vector._
import cats.instances.either._
import com.google.inject.Inject
import play.api.Logger
import play.api.libs.json.Json
import services.{CaasService, ProviderContext}

class EcsService @Inject() (awsSdkFactory: AwsSdkFactory) extends CaasService with ECSOps with EC2Ops {

  private val log = Logger(this.getClass)

  private def composableCleanly(providerId: UUID)(implicit ec: ExecutionContext) = {
    new {
      def map[T](f: EcsClient => T): Future[T] = {
        awsSdkFactory.getEcsClient(providerId) map { client =>
          val t = f(client)
          client.client.shutdown()
          t
        }
      }
      def flatMap[T](f: EcsClient => Future[T]): Future[T] = {
        awsSdkFactory.getEcsClient(providerId) flatMap { client =>
          val fT = f(client)
          fT.onComplete(_ => client.client.shutdown())
          fT
        }
      }
    }
  }

  def find(context: ProviderContext, container: GestaltResourceInstance): Future[Option[ContainerStats]] = {
    // the method signature doesn't support passing an implicit here – probably should be updated
    import play.api.libs.concurrent.Execution.Implicits.defaultContext

    ContainerService.resourceExternalId(container).fold(Future.successful[Option[ContainerStats]](None)) { externalId =>
      for(
        client <- composableCleanly(context.provider.id);
        tss <- describeServices(client, context, Seq(externalId)).liftTo[Future];
        (nextToken, services) = tss;
        updatedServices <- services.toVector.traverse(populateContainerStatsWithPrivateAddresses(client, _)).liftTo[Future];
        _ <- if(nextToken == None && services.size == 1) {
          Future.successful(())
        }else {
          Future.failed(new RuntimeException(s"Unexpected value returned from describeServices: `${(nextToken, services)}`; this is a bug"))
        }
      ) yield Some(services(0))
    }
  }

  def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]] = {
    import play.api.libs.concurrent.Execution.Implicits.defaultContext

    @tailrec
    def describeAllServices(client: EcsClient, paginationToken: Option[String], stats: Seq[ContainerStats]): EitherError[Seq[ContainerStats]] = {
      val described = for(
        tss <- describeServices(client, context, Seq(), paginationToken);
        (nextToken, services) = tss;
        updatedServices <- services.toVector.traverse(populateContainerStatsWithPrivateAddresses(client, _))
      ) yield (nextToken, updatedServices)
      described match {
        case Right((Some(token), moreStats)) => describeAllServices(client, Some(token), stats ++ moreStats)
        case Right((None, moreStats)) => Right(stats ++ moreStats)
        case Left(error) => Left(error)
      }
    }

    for(
      client <- composableCleanly(context.provider.id);
      stats <- describeAllServices(client, None, Seq()).liftTo[Future]
    ) yield stats
  }

  def create(context: ProviderContext, container: GestaltResourceInstance)
   (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    for(
      client <- composableCleanly(context.provider.id);
      specProperties0 <- ResourceSerde.deserialize[ContainerSpec](container).liftTo[Future];
      specProperties = specProperties0.copy(name=container.name);
      _ <- createTaskDefinition(client, container.id, specProperties, context).liftTo[Future];
      serviceArn <- createService(client, container.id, specProperties, context).liftTo[Future]
    ) yield {
      val portMappings = specProperties.port_mappings map { pm =>
        pm.copy(
          expose_endpoint=Some(true),
          service_address=Some(ContainerSpec.ServiceAddress(serviceArn, pm.container_port.getOrElse(0), None, None))
        )
      }
      val values = Map(
        "port_mappings" -> Json.toJson(portMappings).toString,
        "external_id" -> serviceArn,
        "status" -> "RUNNING"
      )
      container.copy(properties = Some((container.properties getOrElse Map()) ++ values.toMap))
    }
  }

  def createSecret(context: ProviderContext, metaResource: Instance, items: Seq[SecretSpec.Item])
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = Future.failed(new RuntimeException("Secrets are not supported by ECS"))

  def createVolume(context: ProviderContext, metaResource: Instance)
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    //     GestaltResourceInstance(b1628891-749c-46a9-940d-ea19156d4803,24361db7-0fc0-4be5-9973-d88d5602956f,a4f47f53-0e3e-4796-99a9-eeca7a9216cc,fa1ad369-d571-479b-9e91-8a510b24f997,ResourceOwnerLink(
    // 58b6775d-37a5-44bc-9115-7331fc4964b7,1b514f76-d8d1-4ac9-8571-dc70ea39927e,Some(gestalt-admin),Some(/root/users/1b514f76-d8d1-4ac9-8571-dc70ea39927e),None),test,None,Some(Map(typeId -> 58b6775d-37a5-44bc-9115-7331fc4964b7, id -> 1b514f76-d
    // 8d1-4ac9-8571-dc70ea39927e, timestamp -> 2018-10-09T15:59:38.218Z)),Some(Map(typeId -> 58b6775d-37a5-44bc-9115-7331fc4964b7, id -> 1b514f76-d8d1-4ac9-8571-dc70ea39927e, timestamp -> 2018-10-09T15:59:38.218Z, action -> created)),Some(Map(p
    // rovider -> {"name":"default-ecs-ec2","id":"44c1bb08-943c-491f-86c6-8afd880a0563","resource_type":"resourceType[8406d8e0-4b7e-49f1-acf7-04c519ca9dca]"}, size -> 1, config -> {"host_path":"/tmp"}, access_mode -> ReadWriteMany, type -> host_
    // path)),None,None,None)

    // only HostPath volumes are supported atm, they don't require any special initialisation
    Future.successful(metaResource)
  }

  def destroy(container: GestaltResourceInstance): Future[Unit] = {
    import play.api.libs.concurrent.Execution.Implicits.defaultContext
    
    val provider = ContainerService.containerProvider(container)

    ContainerService.resourceExternalId(container).fold(Future.successful(())) { externalId =>
      for(
        client <- composableCleanly(provider.id);
        specProperties0 <- ResourceSerde.deserialize[ContainerSpec](container).liftTo[Future];
        specProperties = specProperties0.copy(name=container.name);
        deletedBackend = deleteService(client, externalId);
        _ = deletedBackend.left foreach { errorMessage =>
          log.warn(s"Failed to destroy ECS container on backend: ${errorMessage}")
        };
        deletedVolumes = specProperties.volumes.toVector traverse { volumeMount =>
          val ee: EitherError[Unit] = volumeMount match {
            case ContainerSpec.ExistingVolumeMountSpec(_, volumeId) => {
              // hardDeleteResource returns Try[Int]
              eitherFromTry(ResourceFactory.hardDeleteResource(volumeId) map { _ => () })
            }
            case _ => Right(())
          }
          ee
        };
        _ <- deletedVolumes.liftTo[Future]
      ) yield ()
    }
  }

  def destroySecret(secret: GestaltResourceInstance): Future[Unit] = Future.failed(new RuntimeException("Secrets are not supported by ECS"))

  def destroyVolume(volume: GestaltResourceInstance): Future[Unit] = {
    val volumeType = for(
      properties <- volume.properties;
      `type` <- properties.get("type")
    ) yield `type`
    volumeType match {
      case Some("host_path") => Future.successful(())
      case Some(_) => Future.fromTryST(Try(???))
      case None => {
        log.warn(s"`type` field was missing on volume resource ${volume.id}")
        Future.successful(())
      }
    }
  }

  def update(context: ProviderContext, container: GestaltResourceInstance)
            (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = Future.failed(new RuntimeException("Updates are not supported for ECS containers"))

  def updateVolume(context: ProviderContext, metaResource: GestaltResourceInstance)
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = Future.failed(new RuntimeException("Updates are not supported for ECS volumes"))

  def scale(context: ProviderContext, container: GestaltResourceInstance, numInstances: Int): Future[GestaltResourceInstance] = {
    import play.api.libs.concurrent.Execution.Implicits.defaultContext

    for(
      client <- composableCleanly(context.provider.id);
      specProperties0 <- ResourceSerde.deserialize[ContainerSpec](container).liftTo[Future];
      specProperties = specProperties0.copy(name=container.name);
      newNumInstances <- if(specProperties.external_id.getOrElse("") == "") {     // pending or lost container, doing nothing
        Future.successful(specProperties.num_instances)
      }else {
        scaleService(client, specProperties, context, numInstances).liftTo[Future] map { _ => numInstances}
      }
    ) yield {
      val values = Map(
        "num_instances" -> s"${newNumInstances}"
      )
      container.copy(properties = Some((container.properties getOrElse Map()) ++ values.toMap))
    }
  }

  def createJob(context: ProviderContext, metaResource: Instance)
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = create(context, metaResource)
  
  def destroyJob(job: GestaltResourceInstance): Future[Unit] = destroy(job)
}