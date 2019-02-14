package services.ecs

import com.galacticfog.gestalt.meta.api.ContainerStats
import com.galacticfog.gestalt.integrations.ecs.EcsClient
import com.galacticfog.gestalt.util.EitherWithErrors._
import com.galacticfog.gestalt.util.Error
import scala.util.Try
import cats.syntax.traverse._
import cats.syntax.either._
import cats.instances.vector._
import cats.instances.either._
import play.api.Logger
import com.amazonaws.services.ec2.model._

trait EC2Ops {
  import scala.collection.JavaConversions._

  private[this] val log = Logger(this.getClass)

  def getInstancePrivateAddresses(client: EcsClient, instanceIds: Seq[String]): EitherError[Seq[(String,String)]] = {
    val dir = new DescribeInstancesRequest()
      .withInstanceIds(instanceIds)
    for(
      reservations <- eitherFromTry(Try(client.ec2.describeInstances(dir)).map(_.getReservations()));
      instances = reservations.map(_.getInstances()).flatten;
      addresses <- instances.toVector traverse { instance =>
        val ee: EitherError[(String,String)] = Option(instance.getPrivateIpAddress()) match {
          case Some(ipAddr) => Right((instance.getInstanceId(), ipAddr))
          case None => Left(Error.Default(s"Private IP address not set on instance ${instance.getInstanceId()}"))
        }
        ee
      }
    ) yield addresses
  }

  def populateContainerStatsWithPrivateAddresses(client: EcsClient, stats: ContainerStats): EitherError[ContainerStats] = {
    if(client.launchType == "EC2" && !stats.taskStats.isEmpty) {
      val instanceIds = stats.taskStats.get.collect { case ts if ts.host != "" => ts.host }
      for(
        addressMapping <- getInstancePrivateAddresses(client, instanceIds).map(_.toMap);
        updatedTaskStats = stats.taskStats.get.map { ts =>
          (addressMapping.get(ts.host), ts.ipAddresses) match {
            case (Some(ipAddr), None) => ts.copy(ipAddresses=Some(Seq(ContainerStats.TaskStat.IPAddress(ipAddr, "tcp"))))
            case (Some(ipAddr), Some(seq)) => ts.copy(ipAddresses=Some(seq :+ ContainerStats.TaskStat.IPAddress(ipAddr, "tcp")))
            case (None, _) => ts
          }
        }
      ) yield stats.copy(taskStats=Some(updatedTaskStats))
    }else {
      Right(stats)
    }
  }
}