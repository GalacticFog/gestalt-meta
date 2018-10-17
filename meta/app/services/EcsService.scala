package services

import com.galacticfog.gestalt.data.Instance
import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.meta.api.{ContainerSpec,ContainerStats,SecretSpec}
import com.galacticfog.gestalt.util.FutureFromTryST._
import com.galacticfog.gestalt.integrations.ecs.EcsClient
import controllers.util.ContainerService
import java.util.UUID
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Try,Success,Failure}
import cats.instances.vector._
import cats.instances.try_._
import cats.syntax.traverse._
import akka.actor._
import com.google.inject.Inject
import com.google.inject.name.Named
import play.api.Logger
import play.api.libs.json._
import com.amazonaws.services.ecs.model.{ServiceNotActiveException,ServiceNotFoundException}
import com.amazonaws.services.elasticloadbalancingv2.model.LoadBalancerNotFoundException

class EcsService @Inject() (awsSdkFactory: AwsSdkFactory, @Named("ecs-actor") actor: ActorRef) extends CaasService with ECSOps with ELBOps with EC2Ops {

  private[this] val log = Logger(this.getClass)

  def cleanly[T](providerId: UUID)(f: EcsClient => Future[T]): Future[T] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    awsSdkFactory.getEcsClient(providerId) flatMap { client =>
      val fT = f(client)
      fT.onComplete(_ => client.client.shutdown())
      fT
    }
  }

  private[services] def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }

  override def find(context: ProviderContext, container: GestaltResourceInstance): Future[Option[ContainerStats]] = {
    import scala.concurrent.ExecutionContext.Implicits.global     // the method signature doesn't support passing an implicit here – probably should be updated
    cleanly(context.provider.id) { client =>
      ContainerService.resourceExternalId(container) match {
        case Some("") => {    // pending container
          for(
            spec <- Future.fromTryST(ContainerSpec.fromResourceInstance(container));
            stats <- Future.fromTryST(mkPlaceholderContainerStats(spec))
          ) yield Some(stats)
        }
        case Some(externalId) => {
          Future.fromTryST(describeServices(client, context, Seq(externalId)) flatMap {
            case (None, Seq(stats)) => Success(Option(stats))
            case other => Failure(throw new RuntimeException(s"Unexpected value returned from describeServices: `${other}`; this is a bug"))
          })
        }
        case None => Future.successful(None)
      }
    }
  }

  override def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]] = {
    @tailrec
    def describeAllServices(client: EcsClient, paginationToken: Option[String], stats: Seq[ContainerStats]): Try[Seq[ContainerStats]] = {
      describeServices(client, context, Seq(), paginationToken) match {
        case Success((Some(token), moreStats)) => describeAllServices(client, Some(token), stats ++ moreStats)
        case Success((None, moreStats)) => Success(stats ++ moreStats)
        case Failure(throwable) => Failure(throwable)
      }
    }

    cleanly(context.provider.id) { client =>
      Future.fromTryST(describeAllServices(client, None, Seq()))
    }
  }

  override def create(context: ProviderContext, container: GestaltResourceInstance)
   (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    // apparently there is no way to get correct request.identity instance here (without changing method signature)
    def getUserId(): Try[UUID] = container.created.flatMap(_.get("id")) match {
      case Some(userId) => Try(UUID.fromString(userId))
      case None => Failure(new RuntimeException("Failed to obtain user id for create operation"))
    }
    val res = for(
      userId <- getUserId()
    ) yield {
      actor ! ECSActor.CreateService(container.id, userId, container, context)
      val values = Map(
        "external_id" -> "",
        "status" -> "PENDING"
      )
      container.copy(properties = Some((container.properties getOrElse Map()) ++ values.toMap))
    }
    Future.fromTryST(res)
  }

  override def createSecret(context: ProviderContext, metaResource: Instance, items: Seq[SecretSpec.Item])
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = Future.fromTryST(Try(???))

  override def createVolume(context: ProviderContext, metaResource: Instance)
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    //     GestaltResourceInstance(b1628891-749c-46a9-940d-ea19156d4803,24361db7-0fc0-4be5-9973-d88d5602956f,a4f47f53-0e3e-4796-99a9-eeca7a9216cc,fa1ad369-d571-479b-9e91-8a510b24f997,ResourceOwnerLink(
    // 58b6775d-37a5-44bc-9115-7331fc4964b7,1b514f76-d8d1-4ac9-8571-dc70ea39927e,Some(gestalt-admin),Some(/root/users/1b514f76-d8d1-4ac9-8571-dc70ea39927e),None),test,None,Some(Map(typeId -> 58b6775d-37a5-44bc-9115-7331fc4964b7, id -> 1b514f76-d
    // 8d1-4ac9-8571-dc70ea39927e, timestamp -> 2018-10-09T15:59:38.218Z)),Some(Map(typeId -> 58b6775d-37a5-44bc-9115-7331fc4964b7, id -> 1b514f76-d8d1-4ac9-8571-dc70ea39927e, timestamp -> 2018-10-09T15:59:38.218Z, action -> created)),Some(Map(p
    // rovider -> {"name":"default-ecs-ec2","id":"44c1bb08-943c-491f-86c6-8afd880a0563","resource_type":"resourceType[8406d8e0-4b7e-49f1-acf7-04c519ca9dca]"}, size -> 1, config -> {"host_path":"/tmp"}, access_mode -> ReadWriteMany, type -> host_
    // path)),None,None,None)

    // only HostPath volumes are supported atm, they don't require any special initialisation
    Future.successful(metaResource)
  }

  override def destroy(container: ResourceLike): Future[Unit] = {
    val provider = ContainerService.containerProvider(container)
    
    def destroyRunningContainer(externalId: String): Future[Unit] = {
      cleanly(provider.id) { client =>
        val rawPms = container.properties.get("port_mappings")
        val res = for(
          jsPms <- Try(Json.parse(rawPms));
          pms <- Try(jsPms.as[Seq[ContainerSpec.PortMapping]]);
          _ <- if(pms.isEmpty) { Success(()) } else {     // not for FARGATE - can't even check because I have no access to container spec
            (for(
              _ <- pms.toVector traverse { pm => deleteLoadBalancer(client, pm) };
              vpc <- getDefaultVpc(client);
              _ <- deleteSecurityGroup(client, container.name, vpc)
            ) yield ()) recoverWith {
              case _: LoadBalancerNotFoundException => Success(())
            }
          };
          _ <- deleteService(client, externalId) recoverWith {
            case _: ServiceNotActiveException => Success(())
            case _: ServiceNotFoundException => Success(())
          }
        ) yield ()
        Future.fromTryST(res)
      }
    }
    
    ContainerService.resourceExternalId(container) match {
      case Some("") => Future.successful(())    // pending container
      case Some(externalId) => destroyRunningContainer(externalId)
      case None => Future.successful(())    // the container wasn't created properly – can be safely deleted
    }
  }

  override def destroySecret(secret: ResourceLike): Future[Unit] = Future.fromTryST(Try(???))

  override def destroyVolume(secret: ResourceLike): Future[Unit] = Future.fromTryST(Try(???))

  override def update(context: ProviderContext, container: GestaltResourceInstance)
            (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = Future.fromTryST(Try(???))

  override def updateVolume(context: ProviderContext, metaResource: GestaltResourceInstance)
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = Future.fromTryST(Try(???))

  override def scale(context: ProviderContext, container: GestaltResourceInstance, numInstances: Int): Future[GestaltResourceInstance] = {
    cleanly(context.provider.id) { client =>
      val scaled = for(
        spec <- ContainerSpec.fromResourceInstance(container);
        newNumInstances <- if(spec.external_id.getOrElse("") == "") {     // pending or lost container, doing nothing
          Success(spec.num_instances)
        }else {
          scaleService(client, spec, context, numInstances) map { _ => numInstances}
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
}