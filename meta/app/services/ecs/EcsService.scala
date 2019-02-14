package services.ecs

import com.galacticfog.gestalt.data.{Instance, ResourceFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.{ContainerSpec,ContainerStats,SecretSpec}
import com.galacticfog.gestalt.util.EitherWithErrors._
import com.galacticfog.gestalt.util.FutureFromTryST._
import com.galacticfog.gestalt.integrations.ecs.EcsClient
import controllers.util.ContainerService
import java.util.UUID
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Try,Success,Failure}
import cats.syntax.either._
import cats.syntax.traverse._
import cats.instances.vector._
import cats.instances.either._
import com.google.inject.Inject
import play.api.Logger
import play.api.libs.json.Json
import com.amazonaws.services.ecs.model.{ServiceNotActiveException,ServiceNotFoundException}
import services.{CaasService, ProviderContext}

class EcsService @Inject() (awsSdkFactory: AwsSdkFactory) extends CaasService with ECSOps with EC2Ops {

  private val log = Logger(this.getClass)

  private def cleanly[T](providerId: UUID)(f: EcsClient => Future[T]): Future[T] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    awsSdkFactory.getEcsClient(providerId) flatMap { client =>
      val fT = f(client)
      fT.onComplete(_ => client.client.shutdown())
      fT
    }
  }

  private def composableCleanly(providerId: UUID) = {
    import scala.concurrent.ExecutionContext.Implicits.global

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


  def find(context: ProviderContext, container: GestaltResourceInstance): Future[Option[ContainerStats]] = {     // the method signature doesn't support passing an implicit here – probably should be updated
    cleanly(context.provider.id) { client =>
      ContainerService.resourceExternalId(container) match {
        case Some(externalId) => {
          val described = for(
            (nextToken, services) <- describeServices(client, context, Seq(externalId)).liftTo[Try];
            updatedServices <- services.toVector.traverse(populateContainerStatsWithPrivateAddresses(client, _)).liftTo[Try]
          ) yield (nextToken, updatedServices)
          Future.fromTryST(described flatMap {
            case (None, Seq(stats)) => Success(Option(stats))
            case other => Failure(new RuntimeException(s"Unexpected value returned from describeServices: `${other}`; this is a bug"))
          })
        }
        case None => Future.successful(None)
      }
    }
  }

  def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]] = {
    @tailrec
    def describeAllServices(client: EcsClient, paginationToken: Option[String], stats: Seq[ContainerStats]): Try[Seq[ContainerStats]] = {
      val described = for(
        (nextToken, services) <- describeServices(client, context, Seq(), paginationToken).liftTo[Try];
        updatedServices <- services.toVector.traverse(populateContainerStatsWithPrivateAddresses(client, _)).liftTo[Try]
      ) yield (nextToken, updatedServices)
      described match {
        case Success((Some(token), moreStats)) => describeAllServices(client, Some(token), stats ++ moreStats)
        case Success((None, moreStats)) => Success(stats ++ moreStats)
        case Failure(throwable) => Failure(throwable)
      }
    }

    cleanly(context.provider.id) { client =>
      Future.fromTryST(describeAllServices(client, None, Seq()))
    }
  }

  def create(context: ProviderContext, container: GestaltResourceInstance)
   (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    cleanly(context.provider.id) { client =>
      Future.fromTryST(for(
        spec <- ContainerSpec.fromResourceInstance(container);
        _ <- createTaskDefinition(client, container.id, spec, context).liftTo[Try];
        serviceArn <- createService(client, container.id, spec, context).liftTo[Try]
      ) yield {
        val portMappings = spec.port_mappings map { pm =>
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
      })
    }
  }

  def createSecret(context: ProviderContext, metaResource: Instance, items: Seq[SecretSpec.Item])
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = Future.fromTryST(Try(???))

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
    
    def destroyRunningContainer(externalId: String): Future[Unit] = {
      cleanly(provider.id) { client =>
        Future.fromTryST(deleteService(client, externalId).liftTo[Try] recoverWith {
          case _: ServiceNotActiveException => Success(())
          case _: ServiceNotFoundException => Success(())
        })
      }
    }
    
    ContainerService.resourceExternalId(container) match {
      case Some(externalId) => {
        // destroy() signature ought to be changed
        val mounts = ContainerSpec.fromResourceInstance(container.asInstanceOf[GestaltResourceInstance]) map { spec =>
          spec.volumes
        } recoverWith { case e: Throwable =>
          e.printStackTrace()
          log.warn(s"Failed to deserialize container ${container.id}: ${e.getMessage}")
          Failure(e)
        } getOrElse {
          Seq()
        }
        for(
          _ <- destroyRunningContainer(externalId);
          _ <- Future.traverse(mounts) {
            case ContainerSpec.ExistingVolumeMountSpec(_, volumeId) => {
              // destroyVolume(volume)    // this is practically no-op now
              Future.fromTryST(ResourceFactory.hardDeleteResource(volumeId))
            }
            case _ => Future.successful(())
          }
        ) yield ()
      }
      case None => Future.successful(())    // the container wasn't created properly – can be safely deleted
    }
  }

  def destroySecret(secret: GestaltResourceInstance): Future[Unit] = Future.fromTryST(Try(???))

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
            (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = Future.fromTryST(Try(???))

  def updateVolume(context: ProviderContext, metaResource: GestaltResourceInstance)
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = Future.fromTryST(Try(???))

  def scale(context: ProviderContext, container: GestaltResourceInstance, numInstances: Int): Future[GestaltResourceInstance] = {
    cleanly(context.provider.id) { client =>
      val scaled = for(
        spec <- ContainerSpec.fromResourceInstance(container);
        newNumInstances <- if(spec.external_id.getOrElse("") == "") {     // pending or lost container, doing nothing
          Success(spec.num_instances)
        }else {
          scaleService(client, spec, context, numInstances).liftTo[Try] map { _ => numInstances}
        }
      ) yield {
        val values = Map(
          "num_instances" -> s"${newNumInstances}"
        )
        container.copy(properties = Some((container.properties getOrElse Map()) ++ values.toMap))
      }
      Future.fromTryST(scaled)
    }
  }

  def createJob(context: ProviderContext, metaResource: Instance)
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = Future.fromTryST(Try(???))
  
  def destroyJob(job: GestaltResourceInstance): Future[Unit] = Future.fromTryST(Try(???))
}