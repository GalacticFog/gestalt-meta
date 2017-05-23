package com.galacticfog.gestalt
 
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.{ContainerInstance, ContainerSpec}
import org.joda.time.DateTimeZone
import play.api.data.validation.ValidationError

import scala.concurrent.duration.FiniteDuration
import scala.util.Try
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import play.api.libs.json._
import play.api.{ Logger => log }
import play.api.libs.json.Reads._        // Custom validation helpers
import play.api.libs.functional.syntax._ // Combinator syntax
import com.galacticfog.gestalt.json.Js

package object marathon {

  val DEFAULT_UPGRADE_STRATEGY_WITH_PERSISTENT_VOLUMES = UpgradeStrategy(
    minimumHealthCapacity = 0.5,
    maximumOverCapacity = 0.0
  )

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

  implicit lazy val healthCheckFmt = Json.format[AppUpdate.HealthCheck]

  implicit lazy val marathonPortDefintionFmt = Json.format[AppUpdate.PortDefinition]

  implicit lazy val portDiscoveryFmt = Json.format[AppUpdate.IPPerTaskInfo.DiscoveryInfo.PortDiscovery]

  implicit lazy val discoveryInfoFmt = Json.format[AppUpdate.IPPerTaskInfo.DiscoveryInfo]

  implicit lazy val ipPerTaskInfoFmt = Json.format[AppUpdate.IPPerTaskInfo]

  implicit lazy val marathonAppUpdateFmt = Json.format[AppUpdate]

  implicit lazy val marathonContainerDockerParameterFmt = Json.format[Container.Docker.Parameter]

  implicit lazy val marathonContainerVolumeFmt = Json.format[Container.Volume]

  implicit lazy val portMappingFormat = Json.format[Container.Docker.PortMapping]

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
      appJson
    }
  }

  implicit lazy val taskCountsWrites: Writes[TaskCounts] = Writes { counts =>
    Json.obj(
      "tasksStaged" -> counts.tasksStaged,
      "tasksRunning" -> counts.tasksRunning,
      "tasksHealthy" -> counts.tasksHealthy,
      "tasksUnhealthy" -> counts.tasksUnhealthy
    )
  }

  implicit lazy val ExtendedAppInfoWrites: Writes[AppInfo] = Writes { info =>
    val appJson = appDefinitionWrites.writes(info.app).as[JsObject]

    val maybeJson = Seq[Option[JsObject]](
      info.maybeCounts.map(taskCountsWrites.writes(_).as[JsObject]) /*,
        info.maybeReadinessCheckResults.map(readiness => Json.obj("readinessCheckResults" -> readiness)),
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
  def marathonToMetaContainerSpec(app: AppUpdate, provider: GestaltResourceInstance): Try[(Option[String],ContainerSpec)] = Try {
    log.debug("Entered marathonToMetaContainerSpec...")
    log.debug("Received:\n" + app)

    def portMapping(pm: Container.Docker.PortMapping) = ContainerSpec.PortMapping(
      name = pm.name,
      labels = pm.labels,
      protocol = pm.protocol getOrElse "tcp",
      container_port = pm.containerPort,
      host_port = pm.hostPort,
      service_port = pm.servicePort
    )

    val name = app.id
    val props = ContainerSpec(
      name = name.map(_.stripPrefix("/")) getOrElse "",
      container_type = "DOCKER",
      image = app.container flatMap {_.docker map {_.image}} getOrElse "",
      provider = ContainerSpec.InputProvider(id = provider.id, name = Some(provider.name)),
      port_mappings = app.container.flatMap(_.docker).flatMap(_.portMappings).map(_.map(portMapping)) getOrElse Seq.empty,
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
    * @param spec
    * @return
    */
  def metaToMarathonAppInfo(spec: ContainerSpec,
                            instances: Option[Seq[ContainerInstance]] = None,
                            deploymentIDs: Option[Seq[String]] = None): Try[AppInfo] = {

    def portMapping(pm: ContainerSpec.PortMapping): Container.Docker.PortMapping = {
      Container.Docker.PortMapping(
        protocol = Some(pm.protocol),
        containerPort = pm.container_port,
        hostPort = pm.host_port,
        servicePort = pm.service_port,
        name = pm.name,
        labels = pm.labels
      )
    }

    val maybeCounts = instances.map {
      cis => TaskCounts(
        tasksStaged = cis.count(_.isStaged),
        tasksRunning = cis.count(_.isRunning),
        tasksHealthy = cis.count(_.isHealthy),
        tasksUnhealthy = cis.count(_.isUnhealthy)
      )
    }

    val constraints = spec.constraints.map(_.split(":") match {
      case Array(f1,f2) =>
        Seq(f1,f2.toUpperCase)
      case Array(f1,f2,f3) =>
        Seq(f1,f2.toUpperCase,f3)
      case e => e.toSeq
    })

    val docker = Container.Docker(
      image = spec.image,
      network = spec.network,
      forcePullImage = Some(spec.force_pull),
      portMappings = Some(spec.port_mappings map portMapping),
      parameters = Some(Seq()),
      privileged = Some(false)
    )

    val container = Container(
      docker = if (spec.container_type.equalsIgnoreCase("DOCKER")) Some(docker) else None,
      `type` = spec.container_type,
      volumes = spec.volumes.map(v => Container.Volume(
        containerPath = v.container_path,
        hostPath = v.host_path,
        persistent = v.persistent.map(p => Container.PersistentVolumeInfo(size = p.size)),
        mode = v.mode
      ))
    )

    val createdUTC = spec.created.map(_.toDateTime(DateTimeZone.UTC).toString)

    Try(AppInfo(
      app = AppDefinition(
        id = "/" + spec.name,
        cmd = spec.cmd,
        args = spec.args,
        user = spec.user,
        env = spec.env,
        instances = spec.num_instances,
        container = Some(container),
        cpus = spec.cpus,
        mem = spec.memory,
        disk = spec.disk,
        constraints = constraints,
        portDefinitions = spec.port_mappings map { pm => AppUpdate.PortDefinition(
          port = pm.host_port getOrElse 0,
          protocol = pm.protocol,
          name = pm.name,
          labels = pm.labels getOrElse Map.empty
        )},
        // TODO: ipAddress = None,
        // TODO: upgradeStrategy = None,
        acceptedResourceRoles = spec.accepted_resource_roles,
        labels = spec.labels,
        healthChecks = spec.health_checks map { hc => AppUpdate.HealthCheck(
          protocol = Some(hc.protocol),
          path = Some(hc.path),
          portIndex = None, // TODO: figure out how to define this
          gracePeriodSeconds = Some(hc.grace_period_seconds),
          intervalSeconds = Some(hc.interval_seconds),
          timeoutSeconds = Some(hc.timeout_seconds),
          maxConsecutiveFailures = Some(hc.max_consecutive_failures)
        )},
        version = createdUTC
      ),
      maybeCounts = maybeCounts,
      maybeDeployments = deploymentIDs
    ))
  }


  /**
   * Convert Meta Container JSON to Marathon App object.
   */
  def toMarathonLaunchPayload(fqon: String, workspaceName: String, environmentName: String, name: String, props: ContainerSpec, provider: GestaltResourceInstance): AppUpdate = {

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

    def portMapping(vip: String, exposeHost: Boolean, pm: ContainerSpec.PortMapping) = Container.Docker.PortMapping(
      protocol = Some(pm.protocol),
      containerPort = pm.container_port,
      hostPort = pm.host_port,
      servicePort = None,  //  TODO: need to revisit this: https://gitlab.com/galacticfog/gestalt-meta/issues/226
      name = pm.name,
      labels = for {
        cp <- pm.service_port orElse pm.container_port
        ep <- pm.expose_endpoint if ep == true
      } yield Map("VIP_0" -> s"${vip}:${cp}")
    )

    // TODO: marathon supports requesting a specific host port with host networking (set .portDefinitions.port and .requirePorts == true), however meta has not defined any semantics to activate that use case
    def portDefinition(vip: String, pm: ContainerSpec.PortMapping) = AppUpdate.PortDefinition(
      port = 0, //  TODO: need to revisit this: https://gitlab.com/galacticfog/gestalt-meta/issues/226
      protocol = pm.protocol,
      name = pm.name,
      labels = (for {
        sp <- pm.service_port
        ep <- pm.expose_endpoint if ep == true
      } yield Map("VIP_0" -> s"${vip}:${sp}")) getOrElse Map.empty
    )

    val namedVIP = "/" + (Array(name.stripPrefix("/").stripSuffix("/"),environmentName,workspaceName) ++ fqon.split('.').reverse).mkString(".")

    def toDocker(props: ContainerSpec): (Option[Container.Docker], Option[AppUpdate.IPPerTaskInfo], Option[Seq[AppUpdate.PortDefinition]]) = {
      val requestedNetwork = props.network.map(_.trim) getOrElse ""
      val dockerParams = props.user.filter(_.trim.nonEmpty).map(u => Seq(Container.Docker.Parameter("user",u)))
      // TODO: (cgbaker) DRY-clean this
      if (providerNetworkNames.isEmpty || requestedNetwork.isEmpty ) {
        (Some(Container.Docker(
          image = props.image,
          network = if (requestedNetwork.nonEmpty) Some(requestedNetwork) else None,
          forcePullImage = Some(props.force_pull),
          portMappings = if (requestedNetwork.equalsIgnoreCase("BRIDGE")) Some(props.port_mappings.map(portMapping(vip = namedVIP, exposeHost = true, _))) else None,
          parameters = dockerParams,
          privileged = Some(false)
        )),
          None,
          if (requestedNetwork.equalsIgnoreCase("HOST")) Some(props.port_mappings.map(portDefinition(namedVIP, _))) else None
        )
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
              portMappings = if (stdNet.equalsIgnoreCase("BRIDGE")) Some(props.port_mappings.map(portMapping(vip = namedVIP, exposeHost = true, _))) else None,
              parameters = dockerParams,
              privileged = Some(false)
            )),
              None,
              if (stdNet.equalsIgnoreCase("HOST")) Some(props.port_mappings.map(portDefinition(namedVIP, _))) else None
            )
          case Some(userNetwork) =>
            val docker = Container.Docker(
              image = props.image,
              network = Some("USER"),
              forcePullImage = Some(props.force_pull),
              portMappings = Some(props.port_mappings.map(portMapping(vip = namedVIP, exposeHost = false, _))),
              parameters = dockerParams,
              privileged = Some(false)
            )
            val ippertask = AppUpdate.IPPerTaskInfo( discovery = None, networkName = Some(userNetwork) )
            (Some(docker), Some(ippertask), None)
        }
      }
    }

    val (docker,ipPerTask,portDefs) =
      if (isDocker) toDocker(props)
      else (None,None,None) // no support for non-docker ipPerTask right now

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
      id = None,
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
      portDefinitions = portDefs,
      labels = Some(props.labels),
      healthChecks = Some(props.health_checks map { hc => AppUpdate.HealthCheck(
        protocol = Some(hc.protocol),
        path = Some(hc.path),
        portIndex = hc.port_index,
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
        throw new IllegalArgumentException("Invalid provider JSON: " + Js.errorString(e))
      }
    }
    ContainerSpec(
      name = "",
      container_type = ctype,
      image = image,
      provider = prv
    )
  }
  
//  def requiredJsString(name: String, value: JsValue) = value match {
//    case u: JsUndefined => throw new IllegalArgumentException(s"'$name' is missing.")
//    case v => v.as[String]
//  }


}
