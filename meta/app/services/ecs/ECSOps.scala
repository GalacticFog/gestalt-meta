package services.ecs

import com.galacticfog.gestalt.meta.api.{ContainerSpec,ContainerStats,VolumeSpec}
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.integrations.ecs.{EcsClient,AwslogsConfiguration}
import com.galacticfog.gestalt.util.ResourceSerde
import com.galacticfog.gestalt.util.EitherWithErrors._
import com.galacticfog.gestalt.util.Error
import java.util.UUID
import scala.util.Try
import scala.annotation.tailrec
import cats.syntax.traverse._
import cats.syntax.either._
import cats.instances.vector._
import cats.instances.either._
import play.api.Logger
import play.api.libs.json._
import org.joda.time.DateTime
import com.amazonaws.services.ecs.model._
import services.ProviderContext

trait ECSOps {
  import scala.collection.JavaConversions._

  private[this] val log = Logger(this.getClass)

  private[this] val sidecarContainer = "gesalt-ecs-sidecar-lb-agent"

  def listContainerInstances(client: EcsClient): EitherError[Seq[String]] = {
    @tailrec
    def iter(nextToken: Option[String] = None, containerInstances: Seq[String] = Seq()): EitherError[Seq[String]] = {
      val lcir = new ListContainerInstancesRequest()
        .withCluster(client.cluster)

      nextToken.foreach(lcir.setNextToken(_))

      val res = eitherFromTry(Try(client.client.listContainerInstances(lcir))) map { res =>
        (Option(res.getNextToken()), res.getContainerInstanceArns())
      }
      res match {
        case Right((None, moreContainerInstances)) => Right(containerInstances ++ moreContainerInstances)
        case Right((nextToken, moreContainerInstances)) => iter(nextToken, containerInstances ++ moreContainerInstances)
        case Left(error) => Left(error)
      }
    }
    iter()
  }

  def describeContainerInstances(client: EcsClient): EitherError[Seq[ContainerInstance]] = {
    def describe(cis: Seq[String]): EitherError[Seq[ContainerInstance]] = {
      val dcir = new DescribeContainerInstancesRequest()
        .withCluster(client.cluster)
        .withContainerInstances(cis)
      eitherFromTry(Try(client.client.describeContainerInstances(dcir)).map(_.getContainerInstances().toSeq))
    }
    for(
      containerInstances <- listContainerInstances(client);
      cis <- containerInstances.sliding(10, 10).toVector.traverse(describe(_)).map(_.flatten)
    ) yield cis
  }

  def createTaskDefinition(ecs: EcsClient, containerId: UUID, spec: ContainerSpec, context: ProviderContext): EitherError[String] = {
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

    def processHealthCheck(hc: ContainerSpec.HealthCheck): EitherError[HealthCheck] = {
      for(
        _ <- if(hc.protocol == "COMMAND") {
          Right(())
        }else {
          Left(Error.Default(s"Invalid health check: unsupported protocol ${hc.protocol}; only COMMAND is supported"))
        };
        command = hc.command.getOrElse("");
        _ <- if(command != "") {
          Right(())
        }else {
          Left(Error.Default(s"Invalid health check: command must not be empty"))
        }
      ) yield {
        new HealthCheck()
          .withCommand(command)
          .withInterval(hc.interval_seconds)
          .withRetries(hc.max_consecutive_failures)
          .withStartPeriod(hc.grace_period_seconds)
          .withTimeout(hc.timeout_seconds)
      }
    }

    val eitherHealthCheck = for(
      hcs <- spec.health_checks.toVector.traverse(processHealthCheck);
      _ <- if(hcs.size <= 1) {
        Right(())
      }else {
        Left(Error.Default("At most one health check is supported with Amazon ECS"))
      }
    ) yield {
      if(hcs.size == 0) {
        None
      }else {
        Some(hcs(0))
      }
    }

    def processVolumeMount(mountSpec: ContainerSpec.VolumeMountSpec): EitherError[(MountPoint,Volume)] = {
      for(
        pathAndVolumeId <- mountSpec match {
          case ContainerSpec.ExistingVolumeMountSpec(path, volumeId) => Right((path, volumeId))
          case other => Left(Error.Default(s"Expecting an instance of ContainerSpec.ExistingVolumeMountSpec; got $other"))
        };
        resource <- eitherFrom[Error.Default].option(ResourceFactory.findById(migrations.V13.VOLUME_TYPE_ID,
         pathAndVolumeId._2), s"Unknown volume ${pathAndVolumeId._2}");
        volumeSpec0 <- ResourceSerde.deserialize[VolumeSpec](resource);
        volumeSpec = volumeSpec0.copy(name=resource.name);
        _ <- if(volumeSpec.`type` == VolumeSpec.HostPath) {
          Right(())
        }else {
          Left(Error.Default(s"Unsupported volume type: ${volumeSpec.`type`}. Only hostPath is supported"))
        };
        _ <- if(volumeSpec.name != "") {
          Right(())
        }else {
          Left(Error.Default("Volume name must not be empty"))
        };
        hostPath <- eitherFromJsResult((volumeSpec.config \ "host_path").validate[String])
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

    val eitherVolumes = spec.volumes.toVector.traverse(processVolumeMount)

    def processPortMapping(pm: ContainerSpec.PortMapping): EitherError[PortMapping] = {
      // "If using containers in a task with the awsvpc or host network mode, exposed ports should be specified using containerPort"
      // "If using containers in a task with the awsvpc or host network mode, the hostPort can either be left blank or set to the same value as the containerPort."
      // val protocol = pm.protocol.toLowerCase()
      for(
        // _ <- failIfNot(Seq("tcp", "udp").contains(protocol)) { s"Invalid port mapping protocol: ${pm.protocol}; only tcp or udp are supported" };
        _ <- if(!pm.container_port.isEmpty) {
          Right(())
        }else {
          Left(Error.Default("Invalid port mapping: container port cannot be empty"))
        }
      ) yield {
        new PortMapping().withProtocol("tcp").withContainerPort(pm.container_port.get)
      }
    }

    val eitherPortMappings = spec.port_mappings.toVector.traverse(processPortMapping)

    val logConfiguration = ecs.loggingConfiguration match {
      case Some(AwslogsConfiguration(groupName, region)) => {
        Some(new LogConfiguration()
          .withLogDriver(LogDriver.Awslogs)
          .withOptions(Map(
            // "awslogs-create-group" -> "true",
            "awslogs-region" -> region,
            "awslogs-group" -> groupName,
            "awslogs-stream-prefix" -> s"${context.environmentId}-${spec.name}"
          )))
      }
      case _ => None
    }

    val cd = new ContainerDefinition()
      .withImage(spec.image)
      .withName(spec.name)
      .withMemory(spec.memory.toInt)
      .withCpu(cpus)

    val sidecarCd = for(
      configureUrl <- ecs.kongConfigureUrl;
      managementUrl <- ecs.kongManagementUrl
    ) yield {
      new ContainerDefinition()
        .withImage(ecs.sidecarContainerImage)
        .withName(sidecarContainer)
        // .withMemory(64)
        // .withCpu(256)
        .withCommand(Seq("/bin/app", "-url", configureUrl, "-region", ecs.region, "-management-url", managementUrl))
    }

    if(ecs.launchType == "EC2") {
      cd.setHostname(spec.name)
    }

    logConfiguration foreach { lc =>
      cd.withLogConfiguration(lc)
      sidecarCd.foreach(_.withLogConfiguration(lc))
    }

    if(!env.isEmpty) { cd.setEnvironment(env) }
    if(!spec.labels.isEmpty) { cd.setDockerLabels(spec.labels) }
    eitherPortMappings.foreach { portMappings => cd.setPortMappings(portMappings) }

    eitherVolumes.foreach { pairs =>
      val mountPoints = pairs.map(_._1)
      cd.setMountPoints(mountPoints)
    }

    // https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.11/#container-v1-core
    // kubernetes command = docker entrypoint
    // kubernetes arguments = docker command
    // I assume ContainerSpec cmd and args have the same meaning as Kubernetes cmd and args
    // spec.cmd.foreach { cmd => cd.setEntryPoint(cmd.split(" ").toSeq) }
    // spec.args.foreach { args => cd.setCommand(args.toSeq) }

    // however the default ui only allows to specify `command` (in docker sense)
    // I expect this format: ["nginx", "-g", "daemon off;"]    (can't see any other viable option tbh)
    val eitherCmd = spec.cmd match {
      case Some(args) => {
        (for(
          argsJs <- eitherFromTry(Try(Json.parse(args)));
          splitArgs <- eitherFromJsResult(argsJs.validate[Seq[String]])
        ) yield splitArgs) map { sargs =>
          cd.setCommand(sargs.toSeq)
        } match {
          case Right(v) => Right(v)
          case Left(_) => Left(Error.Default(s"""Failed to parse command, please use this format: `["nginx", "-g", "daemon off;"]`"""))
        }
      }
      case None => Right(Seq())
    }
    
    for(
      hcOpt <- eitherHealthCheck;
      hc <- hcOpt
    ) { cd.setHealthCheck(hc) }

    spec.user.foreach(cd.setUser(_))

    val containerDefinitions = Seq(cd) ++ sidecarCd.map(Seq(_)).getOrElse(Seq())

    val rtdr = new RegisterTaskDefinitionRequest()
      .withContainerDefinitions(containerDefinitions)
      .withFamily(s"${context.environmentId}-${spec.name}")
      .withMemory(spec.memory.toInt.toString)
      .withCpu(cpus.toString)
      .withRequiresCompatibilities(ecs.launchType)

    eitherVolumes.foreach { pairs =>
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
      _ <- eitherPortMappings;
      _ <- eitherHealthCheck;
      _ <- eitherVolumes;
      _ <- eitherCmd;
      result <- eitherFromTry(Try(ecs.client.registerTaskDefinition(rtdr)))
    ) yield {
      val taskDefn = result.getTaskDefinition()
      s"${taskDefn.getFamily()}:${taskDefn.getRevision()}"
    }
  }

  def createService(ecs: EcsClient, containerId: UUID, spec: ContainerSpec, context: ProviderContext): EitherError[String] = {

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

    eitherFromTry(Try(ecs.client.createService(csr))).map(_.getService().getServiceArn())
  }

  def scaleService(client: EcsClient, spec: ContainerSpec, context: ProviderContext, numInstances: Int): EitherError[Unit] = {
    val externalId = spec.external_id.getOrElse("")
    val usr = new UpdateServiceRequest()
      .withCluster(client.cluster)
      .withService(externalId)
      .withDesiredCount(numInstances)
    for(
      _ <- if(externalId == "") {
        Left(Error.Default(s"Cannot scale container with external_id=${externalId}"))
      }else {
        Right(())
      };
      _ <- eitherFromTry(Try(client.client.updateService(usr)))
    ) yield ()
  }

  def describeServices(ecs: EcsClient, context: ProviderContext, lookupServiceArns: Seq[String] = Seq(),
   paginationToken: Option[String] = None): EitherError[(Option[String],Seq[ContainerStats])] = {
    def listServices(): EitherError[(Option[String],Seq[String])] = {
      if(lookupServiceArns.isEmpty) {
        val lsr = new ListServicesRequest()
          .withCluster(ecs.cluster)
          .withLaunchType(LaunchType.valueOf(ecs.launchType))
        val lsrWithToken = paginationToken match {
          case None => lsr
          case Some(token) => lsr.withNextToken(token)
        }
        eitherFromTry(Try(ecs.client.listServices(lsr))) map { response =>
          (Option(response.getNextToken()), response.getServiceArns())
        }
      }else if(lookupServiceArns.size > 10) {
        Left(Error.Default("Please pass no more than 10 service ARNs to describeServices"))
      }else {
        Right((None, lookupServiceArns))
      }
    }

    def getStats(service: Service): EitherError[ContainerStats] = {
      val dtdr = new DescribeTaskDefinitionRequest()
          .withTaskDefinition(service.getTaskDefinition())
      for(
        describeResponse <- eitherFromTry(Try(ecs.client.describeTaskDefinition(dtdr)));
        taskDefn = describeResponse.getTaskDefinition();
        containerDefns: Seq[ContainerDefinition] = taskDefn.getContainerDefinitions();
        containerDefn <- (containerDefns filter { cd =>
          cd.getName() != sidecarContainer
        }) match {
          case Seq(containerDefn: ContainerDefinition) => Right(containerDefn)
          case containerDefns if containerDefns.isEmpty => {
            Left(Error.Default(s"Zero containers present on task definition `${taskDefn.getTaskDefinitionArn()}`; this is not supported"))
          }
          case _ => {
            Left(Error.Default(s"More than one container present on task definition `${taskDefn.getTaskDefinitionArn()}`; this is not supported"))
          }
        };
        ltr = new ListTasksRequest()
          .withCluster(ecs.cluster)
          .withLaunchType(LaunchType.valueOf(ecs.launchType))
          .withServiceName(service.getServiceArn());
        taskArns <- eitherFromTry(Try(ecs.client.listTasks(ltr)).map(_.getTaskArns().toSeq));
        tasks <- if(!taskArns.isEmpty) {
          val dtr = new DescribeTasksRequest()
            .withCluster(ecs.cluster)
            .withTasks(taskArns);
          eitherFromTry(Try(ecs.client.describeTasks(dtr))).map(_.getTasks().toSeq);
        }else { Right(Seq.empty[Task]) };    // tasks not yet spawned
        containerInstanceArns = tasks map { r => Option(r.getContainerInstanceArn()) } collect { case Some(v) => v };
        containerInstances <- if(!containerInstanceArns.isEmpty) {
          val dcir = new DescribeContainerInstancesRequest()
            .withCluster(ecs.cluster)
            .withContainerInstances(containerInstanceArns)
          eitherFromTry(Try(ecs.client.describeContainerInstances(dcir))).map(_.getContainerInstances().toSeq) map { containerInstances =>
            Map(containerInstances map { ci =>
              ci.getContainerInstanceArn() -> ci
            }: _*)
          }
        }else { Right(Map.empty[String,ContainerInstance]) }
      ) yield {
        val taskStats = for(
          task <- tasks;
          container <- task.getContainers().toSeq filter { container =>
            container.getName() != sidecarContainer
          }
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
      services <- listServices();
      (nextToken, serviceArns) = services;
      dsr = new DescribeServicesRequest()
        .withCluster(ecs.cluster)
        .withServices(serviceArns);
      describeResponse <- eitherFromTry(Try(ecs.client.describeServices(dsr)));
      _ = describeResponse.getFailures() foreach { failure =>
        log.error(s"Failure during a DescribeServicesRequest: `failure.getArn()` (`failure.getReason()`)")
      };
      stats <- describeResponse.getServices().toVector.traverse(getStats)
    ) yield {
      (nextToken, stats)
    }
  }

  def deleteService(ecs: EcsClient, serviceArn: String): EitherError[Unit] = {
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
      response <- eitherFromTry(Try(ecs.client.describeServices(decribesr)));
      _ = response.getFailures() foreach { failure =>
        log.error(s"Failure during a DescribeServicesRequest: `failure.getArn()` (`failure.getReason()`)")
      };
      Seq(service) = response.getServices().toSeq;
      _ <- if(service.getLaunchType() == ecs.launchType) { Right(()) }else {
        Left(Error.Default(s"The service `${serviceArn}` belongs to `${service.getLaunchType()}` launch type; this provider is configured to use `${ecs.launchType}`"))
      };
      taskDefnArn = service.getTaskDefinition();
      _ <- eitherFromTry(Try(ecs.client.updateService(usr)));
      _ <- eitherFromTry(Try(ecs.client.deleteService(deletesr)));
      dtdr = new DeregisterTaskDefinitionRequest().withTaskDefinition(taskDefnArn);
      _ <- eitherFromTry(Try(ecs.client.deregisterTaskDefinition(dtdr)))
    ) yield ()
  }
}