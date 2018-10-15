package services

import java.util.UUID
import javax.inject.Inject
import com.galacticfog.gestalt.data.{Instance, ResourceFactory}
import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.meta.api.{ContainerSpec, ContainerStats, SecretSpec, VolumeSpec}
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.util.FutureFromTryST._
import controllers.util.ContainerService
import org.joda.time.DateTime
import play.api.Logger
import play.api.libs.json._
import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Try,Success,Failure}
import scala.collection.JavaConversions._
import cats.instances.vector._
import cats.instances.try_._
import cats.syntax.traverse._
import com.amazonaws.services.ecs.model._
import com.galacticfog.gestalt.integrations.ecs.EcsClient

class EcsService @Inject() (awsSdkFactory: AwsSdkFactory) extends CaasService {
  private[this] val log = Logger(this.getClass)

  def cleanly[T](providerId: UUID)(f: EcsClient => Future[T]): Future[T] = {
    awsSdkFactory.getEcsClient(providerId) flatMap { ecs =>
      val fT = f(ecs)
      fT.onComplete(_ => ecs.client.shutdown())
      fT
    }
  }

  private[services] def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }

  private[this] def createTaskDefinition(ecs: EcsClient, containerId: UUID, spec: ContainerSpec, context: ProviderContext)
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

    def failIfNot(condition: Boolean)(message: String): Try[Unit] = if(condition) {
      Success(())
    }else {
      Failure(new RuntimeException(message))
    }

    def processHealthCheck(hc: ContainerSpec.HealthCheck): Try[HealthCheck] = {
      for(
        _ <- failIfNot(hc.protocol == "COMMAND") { s"Invalid health check: unsupported protocol ${hc.protocol}; only COMMAND is supported" };
        command = hc.command.getOrElse("");
        _ <- failIfNot(command != "") { s"Invalid health check: command must not be empty" }
      ) yield {
        new HealthCheck()
          .withCommand(command)
          .withInterval(hc.interval_seconds)
          .withRetries(hc.max_consecutive_failures)
          .withStartPeriod(hc.grace_period_seconds)
          .withTimeout(hc.timeout_seconds)
      }
    }

    val tryHealthCheck = for(
      hcs <- spec.health_checks.toVector.traverse(processHealthCheck);
      _ <- failIfNot(hcs.size <= 1)("At most one health check is supported with Amazon ECS")
    ) yield {
      if(hcs.size == 0) {
        None
      }else {
        Some(hcs(0))
      }
    }

    def processVolumeMount(mountSpec: ContainerSpec.VolumeMountSpec): Try[(MountPoint,Volume)] = {
      for(
        pathAndVolumeId <- mountSpec match {
          case ContainerSpec.ExistingVolumeMountSpec(path, volumeId) => Success((path, volumeId))
          case other => Failure(new RuntimeException(s"Expecting an instance of ContainerSpec.ExistingVolumeMountSpec; got $other"))
        };
        resource <- ResourceFactory.findById(migrations.V13.VOLUME_TYPE_ID, pathAndVolumeId._2) match {
          case Some(resource) => Success(resource)
          case None => Failure(throw new RuntimeException(s"Unknown volume ${pathAndVolumeId._2}"))
        };
        volumeSpec <- VolumeSpec.fromResourceInstance(resource);
        _ <- failIfNot(volumeSpec.`type` == VolumeSpec.HostPath) { s"Unsupported volume type: ${volumeSpec.`type`}. Only hostPath is supported" };
        _ <- failIfNot(volumeSpec.name != "") { s"Volume name must not be empty" };
        hostPath <- Try((volumeSpec.config \ "host_path").as[String])
      ) yield {
        val volume = new Volume()
          .withHost(new HostVolumeProperties().withSourcePath(hostPath))
          .withName(volumeSpec.name)
        val mountPoint = new MountPoint()
          .withContainerPath(pathAndVolumeId._1)
          .withSourceVolume(volumeSpec.name)
        (mountPoint, volume)
      }
    }

    val tryVolumes = spec.volumes.toVector.traverse(processVolumeMount)

    def processPortMapping(pm: ContainerSpec.PortMapping): Try[PortMapping] = {
      // "If using containers in a task with the awsvpc or host network mode, exposed ports should be specified using containerPort"
      // "If using containers in a task with the awsvpc or host network mode, the hostPort can either be left blank or set to the same value as the containerPort."
      val protocol = pm.protocol.toLowerCase()
      for(
        _ <- failIfNot(Seq("tcp", "udp").contains(protocol)) { s"Invalid port mapping protocol: ${pm.protocol}; only tcp or udp are supported" };
        _ <- failIfNot(!pm.container_port.isEmpty) { "Invalid port mapping: container port cannot be empty" }
      ) yield {
        new PortMapping().withProtocol(protocol).withContainerPort(pm.container_port.get)
      }
    }

    val tryPortMappings = spec.port_mappings.toVector.traverse(processPortMapping)

    val cd = new ContainerDefinition()
      .withImage(spec.image)
      .withName(spec.name)
      .withMemory(spec.memory.toInt)
      .withCpu(cpus)

    if(!env.isEmpty) { cd.setEnvironment(env) }
    if(!spec.labels.isEmpty) { cd.setDockerLabels(spec.labels) }
    tryPortMappings.foreach { portMappings => cd.setPortMappings(portMappings) }

    tryVolumes.foreach { pairs =>
      val mountPoints = pairs.map(_._1)
      cd.setMountPoints(mountPoints)
    }

    // https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.11/#container-v1-core
    // kubernetes command = docker entrypoint
    // kubernetes arguments = docker command
    // I assume ContainerSpec cmd and args have the same meaning as Kubernetes cmd and args
    spec.cmd.foreach { cmd => cd.setEntryPoint(cmd.split(" ").toSeq) }
    spec.args.foreach { args => cd.setCommand(args.toSeq) }
    
    for(
      hcOpt <- tryHealthCheck;
      hc <- hcOpt
    ) { cd.setHealthCheck(hc) }

    spec.user.foreach(cd.setUser(_))

    val rtdr = new RegisterTaskDefinitionRequest()
      .withContainerDefinitions(cd)
      .withFamily(s"${context.environmentId}-${spec.name}")
      .withMemory(spec.memory.toInt.toString)
      .withCpu(cpus.toString)
      .withRequiresCompatibilities(ecs.launchType)

    tryVolumes.foreach { pairs =>
      val volumes = pairs.map(_._2)
      rtdr.setVolumes(volumes)
    }

    if(ecs.launchType == "EC2") {
      rtdr.setNetworkMode(spec.network.getOrElse("none"))
    }else if(ecs.launchType == "FARGATE") {
      rtdr.setNetworkMode("awsvpc")
    }

    ecs.taskRoleArn foreach { taskRoleArn =>
      rtdr.setTaskRoleArn(taskRoleArn)
      rtdr.setExecutionRoleArn(taskRoleArn)
    }
    for(
      _ <- tryPortMappings;
      _ <- tryHealthCheck;
      _ <- tryVolumes;
      result <- Try(ecs.client.registerTaskDefinition(rtdr))
    ) yield {
      val taskDefn = result.getTaskDefinition()
      s"${taskDefn.getFamily()}:${taskDefn.getRevision()}"
    }
  }

  private[this] def createService(ecs: EcsClient, containerId: UUID, spec: ContainerSpec, context: ProviderContext)
   (implicit ec: ExecutionContext): Try[String] = {

    val name = s"${context.environmentId}-${spec.name}"

    val csr = new CreateServiceRequest()
      .withCluster(ecs.cluster)
      .withTaskDefinition(name)
      .withServiceName(name)
      .withDesiredCount(spec.num_instances)
      .withLaunchType(LaunchType.valueOf(ecs.launchType))

    if(ecs.launchType == "FARGATE") {
      val avc = new AwsVpcConfiguration()
        .withSubnets(spec.network.getOrElse("").split(";").toSeq)    // subnet id; required
        .withAssignPublicIp("ENABLED")
      val nc = new NetworkConfiguration().withAwsvpcConfiguration(avc)
      csr.setNetworkConfiguration(nc)
    }

    Try(ecs.client.createService(csr)) map { result =>
      result.getService().getServiceArn()
    }
  }

  private[this] def describeServices(ecs: EcsClient, context: ProviderContext, lookupServiceArns: Seq[String] = Seq(),
   paginationToken: Option[String] = None): Try[(Option[String],Seq[ContainerStats])] = {
    def listServices(): Try[(Option[String],Seq[String])] = {
      if(lookupServiceArns.isEmpty) {
        val lsr = new ListServicesRequest()
          .withCluster(ecs.cluster)
          .withLaunchType(LaunchType.valueOf(ecs.launchType))
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

    def getStats(service: Service): Try[ContainerStats] = {
      val dtdr = new DescribeTaskDefinitionRequest()
          .withTaskDefinition(service.getTaskDefinition())
      for(
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
        };
        ltr = new ListTasksRequest()
          .withCluster(ecs.cluster)
          .withLaunchType(LaunchType.valueOf(ecs.launchType))
          .withServiceName(service.getServiceArn());
        taskArns <- Try(ecs.client.listTasks(ltr)).map(_.getTaskArns().toSeq);
        tasks <- if(!taskArns.isEmpty) {
          val dtr = new DescribeTasksRequest()
            .withCluster(ecs.cluster)
            .withTasks(taskArns);
          Try(ecs.client.describeTasks(dtr)).map(_.getTasks().toSeq);
        }else { Success(Seq.empty[Task]) };    // tasks not yet spawned
        containerInstanceArns = tasks map { r => Option(r.getContainerInstanceArn()) } collect { case Some(v) => v };
        containerInstances <- if(!containerInstanceArns.isEmpty) {
          val dcir = new DescribeContainerInstancesRequest()
            .withCluster(ecs.cluster)
            .withContainerInstances(containerInstanceArns)
          Try(ecs.client.describeContainerInstances(dcir)).map(_.getContainerInstances().toSeq) map { containerInstances =>
            Map(containerInstances map { ci =>
              ci.getContainerInstanceArn() -> ci
            }: _*)
          }
        }else { Success(Map.empty[String,ContainerInstance]) }
      ) yield {
        val taskStats = for(
          task <- tasks;
          container <- task.getContainers().toSeq
        ) yield {
          val host = Option(task.getContainerInstanceArn()) flatMap { ciArn =>
            containerInstances.get(ciArn).map(_.getEc2InstanceId())
          }
          val ports = Option(container.getNetworkBindings()).map(_.toSeq).getOrElse(Seq.empty[NetworkBinding]) map { networkBinding =>
            networkBinding.getHostPort().toInt
          }
          val ipAddresses = Option(container.getNetworkInterfaces()).map(_.toSeq).getOrElse(Seq.empty[NetworkInterface]) map { networkInterface =>
            ContainerStats.TaskStat.IPAddress(networkInterface.getPrivateIpv4Address(), "")
          }
          ContainerStats.TaskStat(
            id = task.getTaskArn(),
            host = host.getOrElse("FARGATE"),
            ipAddresses = Some(ipAddresses),
            ports = ports,
            startedAt = Option(task.getStartedAt()).map(_.toString)
          )
        }
        val cpus = Option(taskDefn.getCpu()) orElse Option(containerDefn.getCpu()).map(_.toString) getOrElse("0")
        val memory = Option(taskDefn.getMemory()) orElse Option(containerDefn.getMemory()).map(_.toString) getOrElse("0")
        ContainerStats(
          external_id = service.getServiceArn(),
          containerType = "DOCKER",
          status = service.getStatus(),
          cpus = cpus.toDouble / 1024,
          memory = memory.toDouble,
          image = containerDefn.getImage(),
          age = new DateTime(service.getCreatedAt()),
          numInstances = service.getDesiredCount(),
          tasksStaged = service.getPendingCount(),
          tasksRunning = service.getRunningCount(),
          tasksHealthy = 0,
          tasksUnhealthy = 0,
          taskStats = Some(taskStats),
          lb_address = None
        )
      }
    }

    for(
      (nextToken, serviceArns) <- listServices();
      dsr = new DescribeServicesRequest()
        .withCluster(ecs.cluster)
        .withServices(serviceArns);
      describeResponse <- Try(ecs.client.describeServices(dsr));
      _ = describeResponse.getFailures() foreach { failure =>
        log.error(s"Failure during a DescribeServicesRequest: `failure.getArn()` (`failure.getReason()`)")
      };
      stats <- describeResponse.getServices().toVector.traverse(getStats)
    ) yield {
      (nextToken, stats)
    }
  }

  private[this] def deleteService(ecs: EcsClient, serviceArn: String): Try[Unit] = {
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
      Seq(service) = response.getServices().toSeq;
      _ <- if(service.getLaunchType() == ecs.launchType) { Success(()) }else {
        Failure(throw new RuntimeException(s"The service `${serviceArn}` belongs to `${service.getLaunchType()}` launch type; this provider is configured to use `${ecs.launchType}`"))
      };
      taskDefnArn = service.getTaskDefinition();
      _ <- Try(ecs.client.updateService(usr));
      _ <- Try(ecs.client.deleteService(deletesr));
      dtdr = new DeregisterTaskDefinitionRequest().withTaskDefinition(taskDefnArn);
      _ <- Try(ecs.client.deregisterTaskDefinition(dtdr))
    ) yield ()
  }

  override def find(context: ProviderContext, container: GestaltResourceInstance): Future[Option[ContainerStats]] = {
    cleanly(context.provider.id) { ecs =>
      ContainerService.resourceExternalId(container) match {
        case Some(externalId) => {
          Future.fromTryST(describeServices(ecs, context, Seq(externalId)) flatMap {
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
    def describeAllServices(ecs: EcsClient, paginationToken: Option[String], stats: Seq[ContainerStats]): Try[Seq[ContainerStats]] = {
      describeServices(ecs, context, Seq(), paginationToken) match {
        case Success((Some(token), moreStats)) => describeAllServices(ecs, Some(token), stats ++ moreStats)
        case Success((None, moreStats)) => Success(stats ++ moreStats)
        case Failure(throwable) => Failure(throwable)
      }
    }

    cleanly(context.provider.id) { ecs =>
      Future.fromTryST(describeAllServices(ecs, None, Seq()))
    }
  }

  override def create(context: ProviderContext, container: GestaltResourceInstance)
   (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    for(
      spec <- Future.fromTryST(ContainerSpec.fromResourceInstance(container));
      serviceArn <- cleanly(context.provider.id) { ecs =>
        Future.fromTryST(for(
          _ <- createTaskDefinition(ecs, container.id, spec, context);
          serviceArn <- createService(ecs, container.id, spec, context)
        ) yield serviceArn)
      }
    ) yield upsertProperties(
      container,
      "external_id" -> serviceArn,
      "status" -> "LAUNCHED"
    )
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
      cleanly(provider.id) { ecs =>
        Future.fromTryST(deleteService(ecs, externalId) recoverWith {
          case _: ServiceNotActiveException => Success(())
          case _: ServiceNotFoundException => Success(())
        })
      }
    }
    
    ContainerService.resourceExternalId(container) match {
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

  override def scale(context: ProviderContext, container: GestaltResourceInstance, numInstances: Int): Future[GestaltResourceInstance] = Future.fromTryST(Try(???))
}