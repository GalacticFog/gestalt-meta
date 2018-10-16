package services

import com.galacticfog.gestalt.meta.api.{ContainerSpec,ContainerStats}
import com.galacticfog.gestalt.util.FutureFromTryST._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.integrations.ecs.EcsClient
import java.util.UUID
import akka.actor._
import com.google.inject.Inject
import cats.instances.vector._
import cats.instances.try_._
import cats.syntax.traverse._
import scala.concurrent.Future
import scala.util.{Try,Success,Failure}
import play.api.Logger
import play.api.libs.json._

class ECSActor @Inject() (awsSdkFactory: AwsSdkFactory) extends Actor with ECSOps with ELBOps with EC2Ops {
  import scala.collection.JavaConversions._
  import ECSActor._

  private[this] val log = Logger(this.getClass)

  implicit val executionContext = context.system.dispatcher

  def cleanly[T](providerId: UUID)(f: EcsClient => Future[T]): Future[T] = {
    awsSdkFactory.getEcsClient(providerId) flatMap { client =>
      val fT = f(client)
      fT.onComplete(_ => client.client.shutdown())
      fT
    }
  }

  def create(containerId: UUID, ownerId: UUID, container: GestaltResourceInstance, context: ProviderContext): Unit = {
    def createServiceEtAl(client: EcsClient): Try[GestaltResourceInstance] = {
      for(
        spec <- ContainerSpec.fromResourceInstance(container);
        _ <- createTaskDefinition(client, containerId, spec, context);
        targetGroups <- if(client.launchType == "EC2" && !spec.port_mappings.isEmpty) {
          for(
            vpc <- getDefaultVpc(client);
            subnets <- getAllSubnetsFromVpc(client, vpc);
            securityGroup <- createSecurityGroup(client, spec, vpc);    // only for Application Load Balancer
            lbAndTgArns <- spec.port_mappings.toVector.traverse(createLoadBalancer(client, _, vpc, subnets, securityGroup));
            _ <- pollForActiveLoadBalancers(client, lbAndTgArns.map(_._2))
          ) yield {
            (lbAndTgArns.map { case(name, lb, tg) => (name, tg) }).toMap
          }
        }else if(client.launchType == "FARGATE" && !spec.port_mappings.isEmpty) {
          // Not supported for FARGATE
          // val subnets: Seq[String] = spec.network.getOrElse("").split(";")
          // getVpcFromSubnets(client, subnets) map { vpc => (vpc, subnets) }
          Success(Map.empty[String,String])
        }else {
          Success(Map.empty[String,String])
        };
        serviceArn <- createService(client, containerId, spec, context, targetGroups)
      ) yield {
        val values = Map(
          "external_id" -> serviceArn,
          "status" -> "RUNNING"
        )
        container.copy(properties = Some((container.properties getOrElse Map()) ++ values.toMap))
      }
    }
    cleanly(context.provider.id) { client =>
      val res = (for(
        instanceWithUpdates <- createServiceEtAl(client);
        _ <- ResourceFactory.update(instanceWithUpdates, ownerId)
      ) yield ()) recoverWith { case throwable =>
        throwable.printStackTrace()
        log.info(s"Container creation failed with message: ${throwable}")
        // val stats = ContainerStats.ContainerStateStat(
        //   objectName = "",
        //   objectType = "",
        //   // stateId: String = "unknown",
        //   // reason: Option[String] = None,
        //   reason = Some("Failed to create container"),
        //   message = Some(s"${throwable}")
        //   // message: Option[String] = None,
        //   // finishedAt: Option[DateTime] = None,
        //   // priority: Int = 0
        // )
        val values = Map(
          // "status_detail" -> Json.toJson(stats).toString,
          "status" -> "FAILED"
        )
        val instanceWithUpdates = container.copy(properties = Some((container.properties getOrElse Map()) ++ values.toMap))
        ResourceFactory.update(instanceWithUpdates, ownerId)
      }
      Future.fromTryST(res)
    } onComplete {
      case Success(_) => ()
      case Failure(throwable) => {
        throwable.printStackTrace()
        log.error(s"Unhandled container creation failure: ${throwable}; this is a bug")
      }
    }
  }
  
  def receive = {
    case CreateService(containerId: UUID, ownerId: UUID, container: GestaltResourceInstance, context: ProviderContext) => {
      create(containerId, ownerId, container, context)
    }
  }
}

object ECSActor {
  def props = Props[ECSActor]
  
  case class CreateService(containerId: UUID, ownerId: UUID, container: GestaltResourceInstance, context: ProviderContext)
}