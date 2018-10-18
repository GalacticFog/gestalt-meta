package services

import com.galacticfog.gestalt.integrations.ecs.EcsClient
import com.galacticfog.gestalt.meta.api.ContainerSpec
import scala.util.{Try,Success,Failure}
import scala.annotation.tailrec
import cats.instances.vector._
import cats.instances.try_._
import cats.syntax.traverse._
import play.api.Logger
import com.amazonaws.services.elasticloadbalancingv2.model._

trait ELBOps {
  import scala.collection.JavaConversions._

  private[this] val log = Logger(this.getClass)

  def createLoadBalancer(client: EcsClient, portMapping: ContainerSpec.PortMapping, vpc: String, subnets: Seq[String],
   securityGroup: String): Try[(String,String,String)] = {

    def failIfNot(condition: Boolean)(message: String): Try[Unit] = if(condition) {
      Success(())
    }else {
      Failure(new RuntimeException(message))
    }

    val allowedProtocols = Seq("udp", "tcp", "http", "https")    // tcp for Network Load Balancer, http and https for Application Load Balancer
    for(
      _ <- failIfNot(allowedProtocols.contains(portMapping.protocol)) {
        s"Invalid port mapping protocol ${portMapping.protocol}; valid options: ${allowedProtocols}"
      };
      name = portMapping.name.getOrElse("");
      _ <- failIfNot(name != "") { "Port mapping name must not be empty" };
      lbPort = portMapping.lb_port.getOrElse(-1);
      _ <- failIfNot(lbPort != -1) { s"Port mapping lb port must not be empty (${name})" };
      // lbType = if(portMapping.protocol == "tcp") { "network" }else { "application" };
      lbType = "application";
      clbr = new CreateLoadBalancerRequest()
        .withName(s"${name}-lb")
        .withSubnets(subnets)
        .withType(lbType)
        .withSecurityGroups(Seq(securityGroup));
      // _ <- failIfNot(portMapping.protocol == "http") { "Only http protocol is supported atm" };
      lbArn <- Try(client.elb.createLoadBalancer(clbr)).map(_.getLoadBalancers().toSeq(0)).map(_.getLoadBalancerArn());
      ctgr = new CreateTargetGroupRequest()
        .withName(s"${name}-tg")
        .withPort(lbPort)
        // .withProtocol(portMapping.protocol)
        .withProtocol("HTTP")
        .withVpcId(vpc);
      tgArn <- Try(client.elb.createTargetGroup(ctgr)).map(_.getTargetGroups().toSeq(0)).map(_.getTargetGroupArn());
      // rtr = new RegisterTargetsRequest()
      //   .withTargetGroupArn(tgArn)
      //   .withTargets(containerInstances.map(new TargetDescription().withId(_)));
      // _ <- Try(client.elb.registerTargets(rtr));
      clr = new CreateListenerRequest()
        .withLoadBalancerArn(lbArn)
        .withProtocol("HTTP")
        .withPort(lbPort)
        .withDefaultActions(new Action().withType("forward").withTargetGroupArn(tgArn));
      listenerArn <- Try(client.elb.createListener(clr)).map(_.getListeners().toSeq(0)).map(_.getListenerArn())
    ) yield (name, lbArn, tgArn)
  }

  def pollForActiveLoadBalancers(client: EcsClient, lbArns: Seq[String]): Try[Unit] = {
    val defaultAttempts = 300
    val interval = 1000
    @tailrec
    def poll(lbArns: Set[String], attempts: Int = defaultAttempts): Try[Unit] = {
      val dlbr = new DescribeLoadBalancersRequest()
        .withLoadBalancerArns(lbArns.toSeq)
      val res = Try(client.elb.describeLoadBalancers(dlbr)) map { res =>
        val activeLbs = res.getLoadBalancers() collect {
          case lb if lb.getState().getCode() == "active" => {
            lb.getLoadBalancerArn()
          }
        }
        lbArns -- activeLbs.toSet
      }
      res match {
        case Success(lbs) if lbs.size == 0 => Success(())
        case Success(lbs) if attempts == 1 => {
          Failure(new RuntimeException(s"Load balancers ${lbs} failed to transition to active state after ${defaultAttempts * interval / 1000} seconds"))
        }
        case Success(lbs) => {
          Thread.sleep(interval)
          poll(lbs, attempts - 1)
        }
        case Failure(throwable) => Failure(throwable)
      }
    }
    poll(lbArns.toSet)
  }

  def deleteLoadBalancer(client: EcsClient, portMapping: ContainerSpec.PortMapping): Try[Unit] = {
    def deleteListener(listenerArn: String): Try[Unit] = {
      val deleteListenerR = new DeleteListenerRequest()
        .withListenerArn(listenerArn)
      Try(client.elb.deleteListener(deleteListenerR))
    }

    val name = portMapping.name.getOrElse("")
    val lbName = s"${name}-lb"
    val tgName = s"${name}-tg"
    val describeLbr = new DescribeLoadBalancersRequest()
      .withNames(lbName)
    for(
      lbs <- Try(client.elb.describeLoadBalancers(describeLbr)).map(_.getLoadBalancers());
      lbOpt = lbs map { lb =>
        (lb.getLoadBalancerName(), lb.getLoadBalancerArn())
      } find { case(name, arn) =>
        name == lbName
      };
      lbArn <- lbOpt match {
        case Some((name, arn)) => Success(arn)
        case None => Failure(new RuntimeException(s"No matching load balancer found: $lbName"))
      };
      describeListernersR = new DescribeListenersRequest()
        .withLoadBalancerArn(lbArn);
      listeners <- Try(client.elb.describeListeners(describeListernersR)).map(_.getListeners());
      _ <- listeners.map(_.getListenerArn()).toVector.traverse(deleteListener(_));
      deleteLbr = new DeleteLoadBalancerRequest()
        .withLoadBalancerArn(lbArn);
      _ <- Try(client.elb.deleteLoadBalancer(deleteLbr));
      describeTgr = new DescribeTargetGroupsRequest()
        .withNames(tgName);
      tgs <- Try(client.elb.describeTargetGroups(describeTgr)).map(_.getTargetGroups());
      tgOpt = tgs map { tg =>
        (tg.getTargetGroupName(), tg.getTargetGroupArn())
      } find { case(name, arn) =>
        name == tgName
      };
      tgArn <- tgOpt match {
        case Some((name, arn)) => Success(arn)
        case None => Failure(new RuntimeException(s"No matching target group found: $tgName"))
      };
      deleteTgr = new DeleteTargetGroupRequest()
        .withTargetGroupArn(tgArn);
      _ <- Try(client.elb.deleteTargetGroup(deleteTgr))
    ) yield ()
  }
}