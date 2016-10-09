package com.galacticfog.gestalt
 
import java.util.UUID
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.{ContainerInstance, ContainerSpec}
import org.joda.time.{DateTimeZone, DateTime}
import play.api.data.validation.ValidationError

import scala.Option
import scala.collection.generic
import scala.concurrent.duration.FiniteDuration
import scala.util.{Try,Success,Failure}
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import play.api.libs.json._
import play.api.{ Logger => log }
import play.api.libs.json.Reads._        // Custom validation helpers
import play.api.libs.functional.syntax._ // Combinator syntax

package object marathon {

  def getOrJsNull[A](maybeA: Option[A])(implicit tjs: play.api.libs.json.Writes[A]): JsValue = maybeA.fold[JsValue](JsNull)(Json.toJson(_))

  val DEFAULT_UPGRADE_STRATEGY_WITH_PERSISTENT_VOLUMES = UpgradeStrategy(
    minimumHealthCapacity = 0.5,
    maximumOverCapacity = 0.0
  )

  implicit lazy val inputProviderFmt = Json.format[ContainerSpec.InputProvider]

  implicit lazy val metaHealthCheckFmt = Json.format[ContainerSpec.HealthCheck]

  implicit lazy val metaPersistentVolumeFmt = Json.format[ContainerSpec.Volume.PersistentVolumeInfo]

  implicit lazy val metaPortMappingSpecFmt = Json.format[ContainerSpec.PortMapping]

  implicit lazy val metaVolumeSpecFmt = Json.format[ContainerSpec.Volume]

  implicit lazy val marathonVolumePersistenceFmt = Json.format[Container.PersistentVolumeInfo]

  implicit lazy val marathonUpgradeStrategyFmt = Json.format[UpgradeStrategy]

  implicit lazy val marathonFetchUriFmt = Json.format[FetchUri]

  implicit lazy val marathonReadinessCheckFmt = Json.format[ReadinessCheck]

  lazy val marathonVolumeReads =  (
    (__ \ "containerPath").read[String] and
      (__ \ "hostPath").readNullable[String] and
      (__ \ "persistent").readNullable[Container.PersistentVolumeInfo] and
      (__ \ "mode").read[String]
    )(Container.Volume.apply _)

  lazy val marathonVolumeWrites = (
    (__ \ "containerPath").write[String] and
      (__ \ "hostPath").writeNullable[String] and
      (__ \ "persistent").writeNullable[Container.PersistentVolumeInfo] and
      (__ \ "mode").write[String]
    )(unlift(Container.Volume.unapply))


//  implicit lazy val metaVolumeWrites = Json.writes[ContainerSpec.VolumeSpec]
//
//  implicit lazy val metaVolumeReads = new Reads[ContainerSpec.VolumeSpec] {
//    lazy val simpleVolumeReads = Json.reads[VolumeSpec]
//    override def reads(json: JsValue): JsResult[VolumeSpec] = {
//      json.validate[VolumeSpec](simpleVolumeReads) match {
//        case s @ JsSuccess(VolumeSpec(cPath, Some(hPath), None,         mode), _) => s
//        case s @ JsSuccess(VolumeSpec(cPath, None,        Some(pvInfo), mode), _) => s
//        case s @ JsSuccess(VolumeSpec(_, None,    None,    _), _) => JsError("container volume must contain one of host_path or persistent")
//        case s @ JsSuccess(VolumeSpec(_, Some(_), Some(_), _), _) => JsError("container volume must contain one of host_path or persistent")
//        case e: JsError => e
//      }
//    }
//  }

  lazy val containerSpecReads = Json.writes[ContainerSpec]

  lazy val containerSpecWrites: Reads[ContainerSpec] = (
    (__ \ "container_type").read[String] and
      (__ \ "image").read[String] and
      (__ \ "provider").read[ContainerSpec.InputProvider] and
      ((__ \ "port_mappings").read[Seq[ContainerSpec.PortMapping]] orElse Reads.pure(Seq())) and
      ((__ \ "cpus").read[Double] orElse Reads.pure(0.2)) and
      ((__ \ "memory").read[Double] orElse Reads.pure(128.0)) and
      ((__ \ "disk").read[Double] orElse Reads.pure(0.0)) and
      ((__ \ "num_instances").read[Int] orElse Reads.pure(1)) and
      (__ \ "network").readNullable[String] and
      (__ \ "cmd").readNullable[String] and
      ((__ \ "constraints").read[Seq[String]] orElse Reads.pure(Seq())) and
      (__ \ "accepted_resource_roles").readNullable[Seq[String]] and
      (__ \ "args").readNullable[Seq[String]] and
      ((__ \ "force_pull").read[Boolean] orElse Reads.pure(false)) and
      ((__ \ "health_checks").read[Seq[ContainerSpec.HealthCheck]] orElse Reads.pure(Seq())) and
      ((__ \ "volumes").read[Seq[ContainerSpec.Volume]] orElse Reads.pure(Seq())) and
      ((__ \ "labels").read[Map[String,String]] orElse Reads.pure(Map())) and
      ((__ \ "env").read[Map[String,String]] orElse Reads.pure(Map())) and
      (__ \ "user").readNullable[String]
    )(ContainerSpec.apply _)

  implicit lazy val metaContainerSpec = Format(containerSpecWrites, containerSpecReads)

  implicit lazy val healthCheckFmt = Json.format[AppUpdate.HealthCheck]

  implicit lazy val marathonPortDefintionFmt = Json.format[AppUpdate.PortDefinition]

  implicit lazy val portDiscoveryFmt = Json.format[AppUpdate.PortDiscovery]

  implicit lazy val discoveryInfoFmt = Json.format[AppUpdate.DiscoveryInfo]

  implicit lazy val ipPerTaskInfoFmt = Json.format[AppUpdate.IPPerTaskInfo]

  implicit lazy val marathonAppUpdateFmt = Json.format[AppUpdate]

  implicit lazy val marathonContainerDockerParameterFmt = Json.format[Container.Docker.Parameter]

  implicit lazy val marathonContainerVolumeFmt = Json.format[Container.Volume]

  implicit lazy val PortMappingReads: Reads[Container.Docker.PortMapping] = (
    (__ \ "containerPort").formatNullable[Int].map(_ getOrElse AppDefinition.RandomPortValue) ~
      (__ \ "hostPort").formatNullable[Int] ~
      (__ \ "servicePort").formatNullable[Int].map(_ getOrElse AppDefinition.RandomPortValue) ~
      (__ \ "protocol").formatNullable[String].map(_ getOrElse "tcp") ~
      (__ \ "name").formatNullable[String] ~
      (__ \ "labels").formatNullable[Map[String, String]].map(_ getOrElse Map.empty[String, String])
    )(Container.Docker.PortMapping(_, _, _, _, _, _))

  implicit lazy val PortMappingWrites = Json.writes[Container.Docker.PortMapping]

  implicit lazy val marathonContainerDockerFmt = Json.format[Container.Docker]

  implicit lazy val marathonContainerResidencyFmt = Json.format[Residency]

  implicit lazy val marathonContainerReads: Reads[Container] = (
    (__ \ "docker").readNullable[Container.Docker] and
      ((__ \ "type").read[String] orElse Reads.pure("DOCKER")) and
      (__ \ "volumes").readNullable[Seq[Container.Volume]].map(_.getOrElse(Seq.empty[Container.Volume]))
    )( Container.apply _ ).filterNot(ValidationError("container volume must contain one of hostPath or persistent")) {
      container =>
    container.volumes.exists { vol =>
      !(vol.hostPath.isDefined ^ vol.persistent.isDefined)
    }
  }

  private[this] lazy val ValidPortName: Reads[String] = {
    implicitly[Reads[String]]
      .filter(ValidationError(s"Port name must fully match regular expression ${PortAssignment.PortNamePattern}"))(
        PortAssignment.PortNamePattern.pattern.matcher(_).matches()
      )
  }

  private[this] lazy val ValidPorts: Reads[Seq[DiscoveryInfo.Port]] = {
    def hasUniquePortNames(ports: Seq[DiscoveryInfo.Port]): Boolean = {
      ports.map(_.name).toSet.size == ports.size
    }

    def hasUniquePortNumberProtocol(ports: Seq[DiscoveryInfo.Port]): Boolean = {
      ports.map(port => (port.number, port.protocol)).toSet.size == ports.size
    }

    implicitly[Reads[Seq[DiscoveryInfo.Port]]]
      .filter(ValidationError("Port names are not unique."))(hasUniquePortNames)
      .filter(ValidationError("There may be only one port with a particular port number/protocol combination."))(
        hasUniquePortNumberProtocol
      )
  }

  lazy val ValidPortProtocol: Reads[String] = {
    implicitly[Reads[String]]
      .filter(ValidationError("Invalid protocol. Only 'udp' or 'tcp' are allowed."))(
        DiscoveryInfo.Port.AllowedProtocols
      )
  }

  implicit lazy val PortReads: Reads[DiscoveryInfo.Port] = (
    (__ \ "number").read[Int] ~
      (__ \ "name").read[String](ValidPortName) ~
      (__ \ "protocol").read[String](ValidPortProtocol) ~
      (__ \ "labels").readNullable[Map[String, String]].map(_ getOrElse Map.empty[String,String])
    )(DiscoveryInfo.Port(_, _, _, _))

  implicit lazy val PortWrites = Json.writes[DiscoveryInfo.Port]

  implicit lazy val DiscoveryInfoFormat: Format[DiscoveryInfo] = Format(
    (__ \ "ports").read[Seq[DiscoveryInfo.Port]](ValidPorts).map(DiscoveryInfo(_)),
    Writes[DiscoveryInfo] { discoveryInfo =>
      Json.obj("ports" -> discoveryInfo.ports.map(PortWrites.writes))
    }
  )

  implicit lazy val IpAddressReads: Reads[IpAddress] = (
    (__ \ "groups").readNullable[Seq[String]].map(_ getOrElse Nil) ~
      (__ \ "labels").readNullable[Map[String, String]].map(_ getOrElse Map.empty[String, String]) ~
      (__ \ "discovery").readNullable[DiscoveryInfo].map(_ getOrElse DiscoveryInfo.empty) ~
      (__ \ "networkName").readNullable[String]
    )(IpAddress(_, _, _, _))

  implicit lazy val IpAddressWrites = Json.writes[IpAddress]

  implicit lazy val marathonContainerWrites: Writes[Container] = (
    (__ \ "docker").writeNullable[Container.Docker] and
      (__ \ "type").write[String] and
      (__ \ "volumes").write[Seq[Container.Volume]]
    )( (mc: Container) => (mc.docker, mc.`type`, mc.volumes))

  implicit lazy val appDefinitionWrites: Writes[AppDefinition] = {
    implicit lazy val durationWrites = Writes[FiniteDuration] { d =>
      JsNumber(d.toSeconds)
    }

    Writes[AppDefinition] { app =>
      var appJson: JsObject = Json.obj(
        "id" -> app.id,
        "cmd" -> app.cmd,
        "args" -> app.args,
        "user" -> app.user,
        "env" -> app.env,
        "instances" -> app.instances,
        "cpus" -> app.cpus,
        "mem" -> app.mem,
        "disk" -> app.disk,
        "gpus" -> app.gpus,
        "executor" -> app.executor,
        "constraints" -> app.constraints,
        "uris" -> app.fetch.map(_.uri),
        "fetch" -> app.fetch,
        "storeUrls" -> app.storeUrls,
        "backoffSeconds" -> app.backoff,
        "backoffFactor" -> app.backoffFactor,
        "maxLaunchDelaySeconds" -> app.maxLaunchDelay,
        "container" -> app.container,
        "healthChecks" -> app.healthChecks,
        "readinessChecks" -> app.readinessChecks,
        "dependencies" -> app.dependencies,
        "upgradeStrategy" -> app.upgradeStrategy,
        "labels" -> app.labels,
        "acceptedResourceRoles" -> app.acceptedResourceRoles,
        "ipAddress" -> app.ipAddress,
        "version" -> app.version,
        "residency" -> app.residency,
        "secrets" -> app.secrets,
        "taskKillGracePeriodSeconds" -> app.taskKillGracePeriod
      )
      // top-level ports fields are incompatible with IP/CT
      if (app.ipAddress.isEmpty) {
        appJson = appJson ++ Json.obj(
          "ports" -> app.servicePorts,
          "portDefinitions" -> {
            if (app.servicePorts.nonEmpty) {
              app.portDefinitions.zip(app.servicePorts).map {
                case (portDefinition, servicePort) => portDefinition.copy(port = servicePort)
              }
            } else {
              app.portDefinitions
            }
          },
          // requirePorts only makes sense when allocating hostPorts, which you can't do in IP/CT mode
          "requirePorts" -> app.requirePorts
        )
      }
//      Json.toJson(app.versionInfo) match {
//        case JsNull => appJson
//        case v: JsValue => appJson + ("versionInfo" -> v)
//      }
      appJson
    }
  }

  implicit lazy val taskCountsWrites: Writes[TaskCounts] =
    Writes { counts =>
      Json.obj(
        "tasksStaged" -> counts.tasksStaged,
        "tasksRunning" -> counts.tasksRunning,
        "tasksHealthy" -> counts.tasksHealthy,
        "tasksUnhealthy" -> counts.tasksUnhealthy
      )
    }

  implicit lazy val ExtendedAppInfoWrites: Writes[AppInfo] =
    Writes { info =>
      val appJson = appDefinitionWrites.writes(info.app).as[JsObject]

      val maybeJson = Seq[Option[JsObject]](
        info.maybeCounts.map(taskCountsWrites.writes(_).as[JsObject]) /*,
        info.maybeDeployments.map(deployments => Json.obj("deployments" -> deployments)),
        info.maybeReadinessCheckResults.map(readiness => Json.obj("readinessCheckResults" -> readiness)),
        info.maybeTasks.map(tasks => Json.obj("tasks" -> tasks)),
        info.maybeLastTaskFailure.map(lastFailure => Json.obj("lastTaskFailure" -> lastFailure)),
        info.maybeTaskStats.map(taskStats => Json.obj("taskStats" -> taskStats)) */,
        Some(Json.obj("tasks" -> Json.arr())),
        Some(Json.obj("deployments" -> Json.arr()))
      ).flatten

      maybeJson.foldLeft(appJson)((result, obj) => result ++ obj)
    }



  /**
   * Convert Marathon App JSON to Meta ContainerSpec
   */
  def marAppPayloadToMetaContainerSpec(inputJson: JsValue, provider: GestaltResourceInstance): Try[(Option[String],ContainerSpec)] = Try {
    log.debug("Entered marathonApp2MetaContainer...")
    log.debug("Received:\n" + Json.prettyPrint(inputJson))
    log.debug("Deserializing Marathon App JSON...")

    val app = inputJson.validate[AppUpdate].recoverTotal { e =>
      throw new BadRequestException("Could not parse Marathon App JSON: " + JsError.toFlatJson(e).toString)
    }

    log.debug("Marathon App:\n" + app)

    val name = app.id
    val props = ContainerSpec(
      container_type = "DOCKER",
      image = app.container flatMap {_.docker map {_.image}} getOrElse "",
      provider = ContainerSpec.InputProvider(id = provider.id, name = Some(provider.name)),
      port_mappings = app.container flatMap {_.docker flatMap {_.portMappings.map {_.zipWithIndex.map { case (pm,index) => ContainerSpec.PortMapping(
        name = None,
        labels = pm.labels,
        protocol = pm.protocol,
        container_port = pm.containerPort,
        host_port = pm.hostPort getOrElse 0,
        service_port = pm.servicePort
      )}}}} getOrElse Seq(),
      cpus = app.cpus getOrElse AppDefinition.DefaultCpus,
      memory = app.mem getOrElse AppDefinition.DefaultMem,
      num_instances = app.instances getOrElse AppDefinition.DefaultInstances,
      network = app.container flatMap( _.docker flatMap (_.network)),
      cmd = app.cmd,
      constraints = app.constraints.map(_.map(_.foldLeft("")(_ + ":" + _))) getOrElse Seq(),
      accepted_resource_roles = app.acceptedResourceRoles,
      args = app.args.map(_.toSeq),
      force_pull = app.container flatMap (_.docker flatMap (_.forcePullImage)) getOrElse false,
      health_checks = app.healthChecks.map(_.map { check => ContainerSpec.HealthCheck(
        protocol = check.protocol getOrElse AppUpdate.HealthCheck.DefaultProtocol,
        path = check.path getOrElse AppUpdate.HealthCheck.DefaultPath,
        grace_period_seconds = check.gracePeriodSeconds getOrElse AppUpdate.HealthCheck.DefaultGracePeriod.toSeconds.toInt,
        interval_seconds = check.intervalSeconds getOrElse AppUpdate.HealthCheck.DefaultInterval.toSeconds.toInt,
        timeout_seconds = check.timeoutSeconds getOrElse AppUpdate.HealthCheck.DefaultTimeout.toSeconds.toInt,
        max_consecutive_failures = check.maxConsecutiveFailures getOrElse AppUpdate.HealthCheck.DefaultMaxConsecutiveFailures
      )}) getOrElse Seq(),
      volumes = app.container.map(_.volumes.map(v =>
        ContainerSpec.Volume(
          container_path = v.containerPath,
          host_path = v.hostPath,
          persistent = v.persistent.map(p => ContainerSpec.Volume.PersistentVolumeInfo(size = p.size)),
          mode = v.mode
        )
      )) getOrElse Seq(),
      labels = app.labels getOrElse Map(),
      env = app.env getOrElse Map()
    )
    (name,props)
  }

  /**
    * Convert Meta container resource to MarathonApp object
    *
    * @param metaContainerSpec
    * @return
    */
  def metaToMarathonAppInfo(metaContainerSpec: GestaltResourceInstance,
                            instances: Option[Seq[ContainerInstance]] = None,
                            deploymentIDs: Option[Seq[String]] = None): Try[AppInfo] = {

    def portmap(ps: Seq[ContainerSpec.PortMapping]): Seq[Container.Docker.PortMapping] = {
      ps map {pm => Container.Docker.PortMapping(
        protocol = pm.protocol,
        containerPort = pm.container_port,
        hostPort = Some(pm.host_port),
        servicePort = pm.service_port
      )}
    }

    val maybeCounts = instances.map {
      cis => TaskCounts(
        tasksStaged = cis.count(_.isStaged),
        tasksRunning = cis.count(_.isRunning),
        tasksHealthy = cis.count(_.isHealthy),
        tasksUnhealthy = cis.count(_.isUnhealthy)
      )
    }

    val appInfo = for {
      props <- Try{metaContainerSpec.properties.get}
      ctype <- Try{props("container_type")}
      provider <- Try{props("provider")} map {json => Json.parse(json).as[ContainerSpec.InputProvider]}
      cpus <- Try{props("cpus").toDouble}
      memory <- Try{props("memory").toDouble}
      num_instances <- Try{props("num_instances").toInt}
      cmd = props.get("cmd")
      constraints = props.get("constraints") map {json => Json.parse(json).as[Seq[String]].map(_.split(":") match {
        case Array(f1,f2) =>
          Seq(f1,f2.toUpperCase)
        case Array(f1,f2,f3) =>
          Seq(f1,f2.toUpperCase,f3)
        case e => e.toSeq
      })}
      acceptedResourceRoles = props.get("accepted_resource_roles") map {json => Json.parse(json).as[Seq[String]]}
      args = props.get("args") map {json => Json.parse(json).as[Seq[String]]}
      health_checks = props.get("health_checks") map {json => Json.parse(json).as[Seq[ContainerSpec.HealthCheck]]}
      volumes = props.get("volumes") map {json => Json.parse(json).as[Seq[ContainerSpec.Volume]]}
      labels = props.get("labels") map {json => Json.parse(json).as[Map[String,String]]}
      env = props.get("env") map {json => Json.parse(json).as[Map[String,String]]}
      port_mappings = props.get("port_mappings") map {json => Json.parse(json).as[Seq[ContainerSpec.PortMapping]]}
      user = props.get("user")
      docker = for {
        image <- props.get("image")
        network = props.get("network")
        force_pull = props.get("force_pull") map {_.toBoolean}
      } yield Container.Docker(
        image = image,
        network = network,
        forcePullImage = force_pull orElse Some(false),
        portMappings = port_mappings map portmap,
        parameters = Some(Seq()),
        privileged = Some(false)
      )
      createdUTC = for {
        created <-  metaContainerSpec.created
        timestamp <- created.get("timestamp")
        dt <- Try{DateTime.parse(timestamp)}.toOption
        utc = dt.toDateTime(DateTimeZone.UTC)
      } yield utc.toString
      container = Container(
        docker = if (ctype.equalsIgnoreCase("DOCKER")) docker else None,
        `type` = ctype,
        volumes = volumes.map(_.map(v => Container.Volume(
          containerPath = v.container_path,
          hostPath = v.host_path,
          persistent = v.persistent.map(p => Container.PersistentVolumeInfo(size = p.size)),
          mode = v.mode
        ))) getOrElse Seq()
      )
    } yield AppInfo(
      app = AppDefinition(
        id = "/" + metaContainerSpec.name,
        cmd = cmd,
        args = args,
        user = user,
        env = env getOrElse Map(),
        instances = num_instances,
        container = Some(container),
        cpus = cpus,
        mem = memory,
        constraints = constraints getOrElse Seq.empty[Seq[String]],
        portDefinitions = port_mappings.getOrElse(Seq()) map { pm => AppUpdate.PortDefinition(
          port = pm.host_port,
          protocol = pm.protocol,
          name = pm.name,
          labels = pm.labels
        )},
        // TODO: ipAddress = None,
        // TODO: upgradeStrategy = None,
        acceptedResourceRoles = acceptedResourceRoles,
        labels = labels getOrElse Map(),
        healthChecks = health_checks map {_.map { hc => AppUpdate.HealthCheck(
          protocol = Some(hc.protocol),
          path = Some(hc.path),
          portIndex = None, // TODO: figure out how to define this
          gracePeriodSeconds = Some(hc.grace_period_seconds),
          intervalSeconds = Some(hc.interval_seconds),
          timeoutSeconds = Some(hc.timeout_seconds),
          maxConsecutiveFailures = Some(hc.max_consecutive_failures)
        )}} getOrElse Seq(),
        version = createdUTC
      ),
      maybeCounts = maybeCounts,
      maybeDeployments = deploymentIDs
    )
    appInfo recoverWith {
      case e: Throwable => throw new IllegalArgumentException("Could not parse container properties",e)
    }
  }


  /**
   * Convert Meta Container JSON to Marathon App object.
   * TODO: convert this to a Future[MarathonApp]
   */
  def toMarathonApp(name: String, props: ContainerSpec, provider: GestaltResourceInstance): AppUpdate = {

    val isDocker = props.container_type.equalsIgnoreCase("DOCKER")

    val providerNetworkNames = (for {
      providerProps <- provider.properties
      configStr <- providerProps.get("config")
      config <- Try{Json.parse(configStr)}.toOption
      networks <- (config \ "networks").validate[Seq[JsObject]] match {
        case JsSuccess(list,_) => Some(list)
        case JsError(_) =>
          log.warn("error parsing network list to Seq[JsObject]")
          None
      }
      names = networks flatMap {n => (n \ "name").asOpt[String]}
    } yield names) getOrElse Seq.empty
    log.debug("found provider networks" + providerNetworkNames)


    def portmap(ps: Seq[ContainerSpec.PortMapping]): Seq[Container.Docker.PortMapping] = {
      ps map {pm => Container.Docker.PortMapping(
        protocol = pm.protocol,
        containerPort = pm.container_port,
        hostPort = Some(pm.host_port),
        servicePort = pm.service_port
      )}
    }

    def toDocker(props: ContainerSpec): (Option[Container.Docker], Option[AppUpdate.IPPerTaskInfo]) = {
      val requestedNetwork = props.network getOrElse ""
      val dockerParams = props.user.filter(_.trim.nonEmpty).map(u => Seq(Container.Docker.Parameter("user",u)))
      if (providerNetworkNames.isEmpty || props.network.isEmpty ) {
        (Some(Container.Docker(
          image = props.image,
          network = if (requestedNetwork.nonEmpty) Some(requestedNetwork) else None,
          forcePullImage = Some(props.force_pull),
          portMappings = if (requestedNetwork.equalsIgnoreCase("BRIDGE")) Some(portmap(props.port_mappings)) else None,
          parameters = dockerParams,
          privileged = Some(false)
        )), None)
      } else {
        providerNetworkNames.find(_.equalsIgnoreCase(requestedNetwork)) match {
          case None => throw new BadRequestException(
            message = "invalid network name: container network was not among list of provider networks",
            payload = Some(Json.toJson(props))
          )
          case Some(stdNet) if stdNet.equalsIgnoreCase("HOST") || stdNet.equalsIgnoreCase("BRIDGE") =>
            (Some(Container.Docker(
              image = props.image,
              network = Some(stdNet),
              forcePullImage = Some(props.force_pull),
              portMappings = if (stdNet.equalsIgnoreCase("BRIDGE")) Some(portmap(props.port_mappings)) else None,
              parameters = dockerParams,
              privileged = Some(false)
            )), None)
          case Some(calicoNet) =>
            val docker = Container.Docker(
              image = props.image,
              network = Some("USER"),
              forcePullImage = Some(props.force_pull),
              portMappings = None,
              parameters = Some(Seq(
                Container.Docker.Parameter("net", calicoNet)
              ) ++ dockerParams.getOrElse(Seq())),
              privileged = Some(false)
            )
            val ippertask = AppUpdate.IPPerTaskInfo(
                discovery = Some(AppUpdate.DiscoveryInfo(
                  ports = Some(props.port_mappings.map(pm => AppUpdate.PortDiscovery(
                    number = pm.container_port,
                    name = pm.name getOrElse pm.container_port.toString,
                    protocol = pm.protocol
                  )).toSeq)
                ))
              )
            (Some(docker), Some(ippertask))
        }
      }
    }

    val (docker,ipPerTask) =
      if (isDocker) toDocker(props)
      else (None,None) // no support for non-docker ipPerTask right now

    val container = Container(
        docker = docker,
        `type` = props.container_type,
        volumes = props.volumes.map(v => Container.Volume(
          containerPath = v.container_path,
          hostPath = v.host_path,
          mode = v.mode,
          persistent = v.persistent.map(p => Container.PersistentVolumeInfo(size = p.size))
        )))

    val upgradeStrategy = if( container.volumes.exists(_.isPersistent) ) Some(DEFAULT_UPGRADE_STRATEGY_WITH_PERSISTENT_VOLUMES) else None

    AppUpdate(
      id = Some("/" + name.stripPrefix("/")),
      container = Some(container),
      constraints = if (props.constraints.nonEmpty) Some(props.constraints.map(
        _.split(":") match {
          case Array(f1,f2) =>
            Seq(f1,f2.toUpperCase)
          case Array(f1,f2,f3) =>
            Seq(f1,f2.toUpperCase,f3)
          case e => e.toSeq
        }
      )) else None,
      cpus = Some(props.cpus),
      mem = Some(props.memory),
      disk = None,
      instances = Some(props.num_instances),
      cmd = props.cmd,
      acceptedResourceRoles = props.accepted_resource_roles flatMap {rs => if (rs.isEmpty) None else Some(rs)},
      args = props.args,
      portDefinitions = if (ipPerTask.isEmpty) Some(props.port_mappings map {
        pm => AppUpdate.PortDefinition(port = pm.container_port, protocol = pm.protocol, name = pm.name, pm.labels)
      }) else None,
      labels = Some(props.labels),
      healthChecks = Some(props.health_checks map { hc => AppUpdate.HealthCheck(
        protocol = Some(hc.protocol),
        path = Some(hc.path),
        portIndex = None, // TODO: don't know how to define this
        gracePeriodSeconds = Some(hc.grace_period_seconds),
        intervalSeconds = Some(hc.interval_seconds),
        timeoutSeconds = Some(hc.timeout_seconds),
        maxConsecutiveFailures = Some(hc.max_consecutive_failures)
      )}),
      env = Some(props.env),
      ipAddress = ipPerTask,
      upgradeStrategy = upgradeStrategy,
      user = None
    )
  }

  def containerWithDefaults(json: JsValue) = {
    val ctype = (json \ "properties" \ "container_type").asOpt[String] match {
      case Some(t) if ! t.trim.isEmpty => t
      case _ => throw new IllegalArgumentException(s"'container_type' is missing or empty.")
    }
    val image = (json \ "properties" \ "image") match {
      case u: JsUndefined => throw new IllegalArgumentException(s"'image' is missing.")
      case v => v.as[String]
    }
    val prv   = (json \ "properties" \ "provider") match {
      case u: JsUndefined => throw new IllegalArgumentException(s"'provider' is missing.")
      case v => v.validate[ContainerSpec.InputProvider].map {
        case p: ContainerSpec.InputProvider => p
      }.recoverTotal { e =>
        throw new IllegalArgumentException("Invalid provider JSON: " + JsError.toFlatJson(e).toString)
      }
    }
    ContainerSpec(
      container_type = ctype,
      image = image,
      provider = prv
    )
  }
  
  def requiredJsString(name: String, value: JsValue) = value match {
    case u: JsUndefined => throw new IllegalArgumentException(s"'$name' is missing.")
    case v => v.as[String]
  }


}
