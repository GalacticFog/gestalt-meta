package services

import java.util.UUID
import javax.inject.Inject
import com.galacticfog.gestalt.data.{Instance, ResourceFactory}
import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.meta.api.{ContainerSpec, ContainerStats, SecretSpec}
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import controllers.util.ContainerService
import org.joda.time.DateTime
import play.api.Logger
import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Try,Success,Failure}
import scala.collection.JavaConversions._
import com.amazonaws.services.ecs.model._

class EcsService @Inject() (awsSdkFactory: AwsSdkFactory) extends CaasService {
  private[this] val log = Logger(this.getClass)

  def cleanly[T](providerId: UUID)(f: AwsEcsClient => Future[T]): Future[T] = {
    awsSdkFactory.getEcsClient(providerId) flatMap { ecs =>
      val fT = f(ecs)
      fT.onComplete(_ => ecs.client.shutdown())
      fT
    }
  }

  private[services] def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }

  private[this] def createTaskDefinition(ecs: AwsEcsClient, containerId: UUID, spec: ContainerSpec, context: ProviderContext)
   (implicit ec: ExecutionContext): Try[String] = {
    // If using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the memory parameter:
    // 256 (.25 vCPU) - Available memory values: 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB)
    // 512 (.5 vCPU) - Available memory values: 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB)
    // 1024 (1 vCPU) - Available memory values: 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB)
    // 2048 (2 vCPU) - Available memory values: Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB)
    // 4096 (4 vCPU) - Available memory values: Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB)
    val cpus = scala.math.min(4096, (spec.cpus * 1024 - 1).toInt / 256 * 256 + 256)

    val env = spec.env.toSeq map { case(key, value) =>
      new KeyValuePair()
        .withName(key)
        .withValue(value)
    }

    val healthChecks = spec.health_checks collect {
      case hc if hc.protocol == "COMMAND" && !hc.command.isEmpty => {
        new HealthCheck()
          .withCommand(hc.command.getOrElse(""))
          .withInterval(hc.interval_seconds)
          .withRetries(hc.max_consecutive_failures)
          .withStartPeriod(hc.grace_period_seconds)
          .withTimeout(hc.timeout_seconds)
      }
    }

    val volumes = spec.volumes collect {
      // probably type java.util.UUID is too restrictive in this case
      case ContainerSpec.ExistingVolumeMountSpec(path, uuid) => {
        new MountPoint().withContainerPath(path).withSourceVolume(uuid.toString)
      }
    }

    val portMappings = spec.port_mappings collect {
      // "If using containers in a task with the awsvpc or host network mode, exposed ports should be specified using containerPort"
      // "If using containers in a task with the awsvpc or host network mode, the hostPort can either be left blank or set to the same value as the containerPort."
      case pm if pm.protocol.toLowerCase() == "tcp" && !pm.container_port.isEmpty => {
        new PortMapping().withProtocol(TransportProtocol.Tcp).withContainerPort(pm.container_port.get)
      }
      case pm if pm.protocol.toLowerCase() == "udp" && !pm.container_port.isEmpty => {
        new PortMapping().withProtocol(TransportProtocol.Udp).withContainerPort(pm.container_port.get)
      }
    }

    val cd = new ContainerDefinition()
      .withImage(spec.image)
      .withName(spec.name)
      .withMemory(spec.memory.toInt)
      .withCpu(cpus)

    if(!env.isEmpty) { cd.setEnvironment(env) }
    if(!spec.labels.isEmpty) { cd.setDockerLabels(spec.labels) }
    if(!volumes.isEmpty) { cd.setMountPoints(volumes) }
    if(!portMappings.isEmpty) { cd.setPortMappings(portMappings) }

    // https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.11/#container-v1-core
    // kubernetes command = docker entrypoint
    // kubernetes arguments = docker command
    // I assume ContainerSpec cmd and args have the same meaning as Kubernetes cmd and args
    spec.cmd.foreach { cmd => cd.setEntryPoint(cmd.split(" ").toSeq) }
    spec.args.foreach { args => cd.setCommand(args.toSeq) }
    
    if(healthChecks.size > 0) {
      if(healthChecks.size > 1) {
        log.error("At most one health check is supported with Amazon ECS; the rest are ignored")
      }
      cd.setHealthCheck(healthChecks(0))
    }

    spec.user.foreach(cd.setUser(_))

    val rtdr = new RegisterTaskDefinitionRequest()
      .withContainerDefinitions(cd)
      .withFamily(s"${context.environmentId}-${spec.name}")
      .withNetworkMode("awsvpc")
      .withMemory(spec.memory.toInt.toString)
      .withCpu(cpus.toString)
      .withRequiresCompatibilities("FARGATE")
      .withTaskRoleArn(ecs.taskRoleArn)
      .withExecutionRoleArn(ecs.taskRoleArn)
    Try(ecs.client.registerTaskDefinition(rtdr)) map { result =>
      val taskDefn = result.getTaskDefinition()
      s"${taskDefn.getFamily()}:${taskDefn.getRevision()}"
    }
  }

  private[this] def createService(ecs: AwsEcsClient, containerId: UUID, spec: ContainerSpec, context: ProviderContext)
   (implicit ec: ExecutionContext): Try[String] = {

    val avc = new AwsVpcConfiguration().withSubnets(spec.network.getOrElse(""))    // subnet id; required
    val nc = new NetworkConfiguration().withAwsvpcConfiguration(avc)

    val name = s"${context.environmentId}-${spec.name}"

    val csr = new CreateServiceRequest()
      .withCluster(ecs.cluster)
      .withTaskDefinition(name)
      .withServiceName(name)
      .withDesiredCount(spec.num_instances)
      .withLaunchType(LaunchType.FARGATE)
      .withNetworkConfiguration(nc)
    Try(ecs.client.createService(csr)) map { result =>
      result.getService().getServiceArn()
    }
  }

  private[this] def describeServices(ecs: AwsEcsClient, context: ProviderContext, lookupServiceArns: Seq[String] = Seq(),
   paginationToken: Option[String] = None): Try[(Option[String],Seq[ContainerStats])] = {
    def listServices(): Try[(Option[String],Seq[String])] = {
      if(lookupServiceArns.isEmpty) {
        val lsr = new ListServicesRequest()
          .withCluster(ecs.cluster)
          .withLaunchType(LaunchType.FARGATE)
        val lsrWithToken = paginationToken match {
          case None => lsr
          case Some(token) => lsr.withNextToken(token)
        }
        Try(ecs.client.listServices(lsr)) map { response =>
          (Option(response.getNextToken()), response.getServiceArns())
        }
      }else if(lookupServiceArns.size > 10) {
        Failure(throw new RuntimeException("Please pass no more than 10 service ARNs to describeServices"))
      }else {
        Success((None, lookupServiceArns))
      }
    }
    val tryListAndDescribeServices = for(
      (nextToken, serviceArns) <- listServices();
      // eligibleServiceArns = serviceArns.filter { serviceArn =>
      //   val Array(_, name) = serviceArn.split("/", 2)
      //   name.startsWith(context.environmentId.toString)
      // };
      dsr = new DescribeServicesRequest()
        .withCluster(ecs.cluster)
        .withServices(serviceArns);
      describeResponse <- Try(ecs.client.describeServices(dsr))
    ) yield {
      describeResponse.getFailures() foreach { failure =>
        log.error(s"Failure during a DescribeServicesRequest: `failure.getArn()` (`failure.getReason()`)")
      }
      (nextToken, describeResponse.getServices())
    }

    tryListAndDescribeServices flatMap { case(token, services) =>
      val init: Try[Seq[ContainerStats]] = Success(Seq())
      services.foldLeft(init) { case(boxedStats, service) =>
        for(
          stats <- boxedStats;
          dtdr = new DescribeTaskDefinitionRequest()
            .withTaskDefinition(service.getTaskDefinition());
          describeResponse <- Try(ecs.client.describeTaskDefinition(dtdr));
          taskDefn = describeResponse.getTaskDefinition();
          containerDefns: Seq[ContainerDefinition] = taskDefn.getContainerDefinitions();
          containerDefn <- containerDefns match {
            case Seq(containerDefn: ContainerDefinition) => Success(containerDefn)
            case containerDefns if containerDefns.isEmpty => {
              Failure(throw new RuntimeException(s"Zero containers present on task definition `${taskDefn.getTaskDefinitionArn()}`; this is not supported"))
            }
            case _ => {
              Failure(throw new RuntimeException(s"More than one container present on task definition `${taskDefn.getTaskDefinitionArn()}`; this is not supported"))
            }
          }
        ) yield {
          val tasks = service.getDeployments().toSeq map { deployment =>
            ContainerStats.TaskStat(
              id = deployment.getId(),
              host = "",
              ipAddresses = None,
              ports = Seq(),
              startedAt = Some(deployment.getCreatedAt().toString)
            )
          }
          val moreStats = ContainerStats(
            external_id = service.getServiceArn(),
            containerType = "DOCKER",
            status = service.getStatus(),
            cpus = containerDefn.getCpu().toDouble / 1024,
            memory = containerDefn.getMemory().toDouble,
            image = containerDefn.getImage(),
            age = new DateTime(service.getCreatedAt()),
            numInstances = service.getDesiredCount(),
            tasksStaged = service.getPendingCount(),
            tasksRunning = service.getRunningCount(),
            tasksHealthy = 0,
            tasksUnhealthy = 0,
            taskStats = Some(tasks),
            lb_address = None
          )
          stats :+ moreStats
        }
      } map { stats =>
        (token, stats)
      }
    }
  }

  private[this] def deleteService(ecs: AwsEcsClient, serviceArn: String): Try[Unit] = {
    val decribesr = new DescribeServicesRequest()
      .withCluster(ecs.cluster)
      .withServices(serviceArn)
    val usr = new UpdateServiceRequest()
      .withCluster(ecs.cluster)
      .withService(serviceArn)
      .withDesiredCount(0)
    val deletesr = new DeleteServiceRequest()
      .withCluster(ecs.cluster)
      .withService(serviceArn)
    for(
      response <- Try(ecs.client.describeServices(decribesr));
      _ = response.getFailures() foreach { failure =>
        log.error(s"Failure during a DescribeServicesRequest: `failure.getArn()` (`failure.getReason()`)")
      };
      Seq(taskDefnArn) = response.getServices().toSeq map { service => service.getTaskDefinition() };
      _ <- Try(ecs.client.updateService(usr));
      _ <- Try(ecs.client.deleteService(deletesr));
      dtdr = new DeregisterTaskDefinitionRequest().withTaskDefinition(taskDefnArn);
      _ <- Try(ecs.client.deregisterTaskDefinition(dtdr))
    ) yield ()
  }

  override def find(context: ProviderContext, container: GestaltResourceInstance): Future[Option[ContainerStats]] = {
    cleanly(context.provider.id) { ecs =>
      ContainerService.resourceExternalId(container) match {
        case Some(externalId) => Future.fromTry {
          describeServices(ecs, context, Seq(externalId)) flatMap {
            case (None, Seq(stats)) => Success(Option(stats))
            case other => Failure(throw new RuntimeException(s"Unexpected value returned from describeServices: `${other}`; this is a bug"))
          }
        }
        case None => Future.successful(None)
      }
    }
  }

  override def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]] = {
    @tailrec
    def describeAllServices(ecs: AwsEcsClient, paginationToken: Option[String], stats: Seq[ContainerStats]): Try[Seq[ContainerStats]] = {
      describeServices(ecs, context, Seq(), paginationToken) match {
        case Success((Some(token), moreStats)) => describeAllServices(ecs, Some(token), stats ++ moreStats)
        case Success((None, moreStats)) => Success(stats ++ moreStats)
        case Failure(throwable) => Failure(throwable)
      }
    }

    cleanly(context.provider.id) { ecs =>
      Future.fromTry(describeAllServices(ecs, None, Seq()))
    }
  }

  override def create(context: ProviderContext, container: GestaltResourceInstance)
   (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    for(
      spec <- Future.fromTry(ContainerSpec.fromResourceInstance(container));
      serviceArn <- cleanly(context.provider.id) { ecs =>
        Future.fromTry {
          for(
            _ <- createTaskDefinition(ecs, container.id, spec, context);
            serviceArn <- createService(ecs, container.id, spec, context)
          ) yield serviceArn
        }
      }
    ) yield upsertProperties(
      container,
      "external_id" -> serviceArn,
      "status" -> "LAUNCHED"
    )
  }

  override def createSecret(context: ProviderContext, metaResource: Instance, items: Seq[SecretSpec.Item])
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = ???

  override def createVolume(context: ProviderContext, metaResource: Instance)
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = ???

  override def destroy(container: ResourceLike): Future[Unit] = {
    val provider = ContainerService.containerProvider(container)
    val tryExternalId = ContainerService.resourceExternalId(container) match {
      case Some(externalId) => Success(externalId)
      case None => Failure(new RuntimeException("Could not determine 'external_id' for container"))
    }
    cleanly(provider.id) { ecs =>
      Future.fromTry {
        for(
          externalId <- tryExternalId;
          res <- deleteService(ecs, externalId)
        ) yield res
      }
    }
  }

  override def destroySecret(secret: ResourceLike): Future[Unit] = ???

  override def destroyVolume(secret: ResourceLike): Future[Unit] = ???

  override def update(context: ProviderContext, container: GestaltResourceInstance)
            (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = ???

  override def updateVolume(context: ProviderContext, metaResource: GestaltResourceInstance)
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = ???

  override def scale(context: ProviderContext, container: GestaltResourceInstance, numInstances: Int): Future[GestaltResourceInstance] = ???
}