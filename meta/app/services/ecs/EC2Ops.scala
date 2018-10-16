package services

import com.galacticfog.gestalt.integrations.ecs.EcsClient
import com.galacticfog.gestalt.meta.api.ContainerSpec
import scala.util.{Try,Success,Failure}
import cats.instances.vector._
import cats.instances.try_._
import cats.syntax.traverse._
import com.amazonaws.services.ec2.model._

trait EC2Ops {
  import scala.collection.JavaConversions._

  // def getVpcFromSubnets(client: EcsClient, subnets: Seq[String]): Try[String] = {
  //   def getVpcs(subnets: Seq[String]): Try[Seq[String]] = {
  //     val dsr = new DescribeSubnetsRequest()
  //       .withSubnetIds(subnets)
  //     Try(client.ec2.describeSubnets(dsr)) map { res =>
  //       res.getSubnets().map(_.getVpcId()).toSeq
  //     }
  //   }

  //   for(
  //     vpcs <- subnets.toVector.sliding(10, 10).toVector.traverse(getVpcs(_)).map(_.flatten.toSet);
  //     vpc <- if(vpcs.size == 1) {
  //       Success(vpcs.head)
  //     }else {
  //       Failure(new RuntimeException(s"Subnets ${subnets} must belong to the same vpc"))
  //     }
  //   ) yield vpc
  // }

  def getAllSubnetsFromVpc(client: EcsClient, vpc: String): Try[Seq[String]] = {
    val dsr = new DescribeSubnetsRequest()
      .withFilters(new Filter("vpc-id", Seq(vpc)))
    Try(client.ec2.describeSubnets(dsr)) map { res =>
      res.getSubnets().map(_.getSubnetId()).toSeq
    }
  }

  def createSecurityGroup(client: EcsClient, spec: ContainerSpec, vpc: String): Try[String] = {
    val openPorts = spec.port_mappings.map(_.lb_port).collect { case Some(port) => port };
    val csgr = new CreateSecurityGroupRequest()
      .withGroupName(s"${spec.name}-sg")
      .withDescription("Managed by Gestalt")
      .withVpcId(vpc);
    for(
      groupId <- Try(client.ec2.createSecurityGroup(csgr)).map(_.getGroupId());
      range = new IpRange().withCidrIp("0.0.0.0/0");
      perms = openPorts map { port =>
        new IpPermission() 
          .withIpProtocol("tcp")
          .withToPort(port)
          .withFromPort(port)
          .withIpv4Ranges(range)
      };
      asgir = new AuthorizeSecurityGroupIngressRequest()
        .withGroupId(groupId)
        .withIpPermissions(perms);
      _ <- Try(client.ec2.authorizeSecurityGroupIngress(asgir))//;
      // asger = new AuthorizeSecurityGroupEgressRequest()
      //   .withGroupId(groupId);
        // .withIpPermissions(new IpPermission().withIpProtocol("-1").withIpv4Ranges(range));
      // _ <- Try(client.ec2.authorizeSecurityGroupEgress(asger))
    ) yield groupId
  }

  def deleteSecurityGroup(client: EcsClient, containerName: String, vpc: String): Try[Unit] = {
    val sgName = s"${containerName}-sg"
    val describeSgr = new DescribeSecurityGroupsRequest()
      .withFilters(new Filter("group-name").withValues(sgName))
      .withFilters(new Filter("vpc-id").withValues(vpc))
    for(
      sgs <- Try(client.ec2.describeSecurityGroups(describeSgr)).map(_.getSecurityGroups());
      sgOpt = sgs map { sg =>
        (sg.getGroupName(), sg.getGroupId())
      } find { case(name, id) =>
        name == sgName
      };
      groupId <- sgOpt match {
        case Some((name, groupId)) => Success(groupId)
        case None => Failure(new RuntimeException(s"No matching security group found: $sgName"))
      };
      deleteSgr = new DeleteSecurityGroupRequest()
        .withGroupId(groupId);
      _ <- Try(client.ec2.deleteSecurityGroup(deleteSgr))
    ) yield ()
  }
}