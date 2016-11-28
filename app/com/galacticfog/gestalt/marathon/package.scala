package com.galacticfog.gestalt
 
import java.util.UUID
import com.galacticfog.gestalt.data.models.GestaltResourceInstance

import scala.collection.generic
import scala.util.{Try,Success,Failure}
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import play.api.libs.json._
import play.api.{ Logger => log }
import play.api.libs.json.Reads._        // Custom validation helpers
import play.api.libs.functional.syntax._ // Combinator syntax

package object marathon {

  val DEFAULT_UPGRADE_STRATEGY_WITH_PERSISTENT_VOLUMES = UpgradeStrategy(
    minimumHealthCapacity = 0.5,
    maximumOverCapacity = 0.0
  )

  case class PersistentVolumeInfo(size: Int)

  case class Volume(
      container_path: String,
      host_path: Option[String],
      persistent: Option[PersistentVolumeInfo],
      mode: String) {
    def isPersistent: Boolean = persistent.isDefined
  }

  // TODO: Marathon health checks require a port index, to specify which port the check is run against
  // ours aren't well defined without something similar, like a label
  case class HealthCheck(
      protocol: String,
      path: String,
      grace_period_seconds: Int = 300,
      interval_seconds: Int = 60,
      timeout_seconds: Int = 10,
      max_consecutive_failures: Int = 3)
  
  case class InputProvider(
      id: UUID,
      name: Option[String] = None,
      locations: Option[Iterable[String]] = None)

  case class PortMapping(protocol: String,
                         container_port: Int,
                         host_port: Option[Int] = None,
                         service_port: Option[Int] = None,
                         label: Option[String] = None,
                         load_balanced: Option[Boolean] = None)

  case class InputContainerProperties(container_type: String,
                                      image: String,
                                      provider: InputProvider,
                                      port_mappings: Iterable[PortMapping] = Seq(),
                                      cpus: Double = 0.2,
                                      memory: Int = 128,
                                      num_instances: Int = 1,
                                      network: String = "BRIDGE",
                                      cmd: Option[String] = None,
                                      constraints: Option[Seq[String]] = None,
                                      accepted_resource_roles: Option[Seq[String]] = None,
                                      args: Option[Iterable[String]] = None,
                                      force_pull: Boolean = false,
                                      health_checks: Option[Iterable[HealthCheck]] = None,
                                      volumes: Option[Iterable[Volume]] = None,
                                      labels: Option[Map[String,String]] = None,
                                      env: Option[Map[String,String]] = None,
                                      user: Option[String] = None)

  case class InputContainer(
      id: UUID = UUID.randomUUID,
      name: String,
      resource_type: UUID = ResourceIds.Container,
      properties: InputContainerProperties)

  case class KeyValuePair(key: String, value: String)

  case class MarathonContainer( docker: Option[MarathonContainer.Docker],
                                containerType: String,
                                volumes: Option[Iterable[Volume]] = None )

  case object MarathonContainer {

    case class Docker(image: String,
                      network: String = "BRIDGE",
                      forcePullImage: Option[Boolean] = None,
                      portMappings: Option[Iterable[Docker.PortMapping]] = None,
                      parameters: Option[Iterable[KeyValuePair]] = None)

    case object Docker {

      case class PortMapping(protocol: Option[String] = Some("tcp"),
                             containerPort: Int,
                             hostPort: Option[Int] = Some(0),
                             servicePort: Option[Int] = Some(0),
                             name: Option[String] = None,
                             labels: Option[Map[String,String]] = None)

    }

  }

  case class MarathonHealthCheck(
                          protocol: Option[String] = Some("http"),
                          path: Option[String] = Some("/"),
                          portIndex: Option[Int] = Some(0),
                          gracePeriodSeconds: Option[Int] = None,
                          intervalSeconds: Option[Int] = None,
                          timeoutSeconds: Option[Int] = None,
                          maxConsecutiveFailures: Option[Int] = None)

  case class PortDefinition(port: Int,
                            protocol: Option[String],
                            name: Option[String],
                            labels: Option[JsObject])

  case class IPPerTaskInfo(discovery: Option[IPPerTaskInfo.DiscoveryInfo], networkName: Option[String] = None)

  case object IPPerTaskInfo {
    case class DiscoveryInfo(ports: Option[Seq[DiscoveryInfo.PortDiscovery]] = None)

    case object DiscoveryInfo {
      case class PortDiscovery(number: Int, name: String, protocol: String)
    }
  }

  case class UpgradeStrategy(minimumHealthCapacity: Double, maximumOverCapacity: Double)

  case class MarathonApp(
    id: String,
    container: MarathonContainer,
    cpus: Double = 0.2,
    mem: Double = 128.0,
    instances: Int = 1,
    tasksStaged: Option[Int] = None,
    tasksRunning: Option[Int] = None,
    tasksHealthy: Option[Int] = None,
    tasksUnhealthy: Option[Int] = None,
    cmd: Option[String] = None,
    constraints: Option[Seq[Seq[String]]] = None,
    acceptedResourceRoles: Option[Seq[String]] = None,
    args: Option[Iterable[String]] = None,
    ports: Option[Iterable[Int]] = None,
    portDefinitions: Option[Iterable[PortDefinition]] = None,
    labels: Option[Map[String, String]] = None,
    healthChecks: Option[Iterable[MarathonHealthCheck]] = None,
    env: Option[Map[String, String]] = None,
    deployments: Option[Seq[JsObject]] = None,
    ipAddress: Option[IPPerTaskInfo] = None,
    upgradeStrategy: Option[UpgradeStrategy] = None,
    user: Option[String] = None)

  implicit lazy val inputProviderFormat = Json.format[InputProvider]

  implicit lazy val marathonVolumePersistenceFmt = Json.format[PersistentVolumeInfo]

  implicit lazy val marathonUpgradeStrategyFmt = Json.format[UpgradeStrategy]

  lazy val marathonVolumeReads = new Reads[Volume] {
    lazy val simpleVolumeReads = (
      (__ \ "containerPath").read[String] and
        (__ \ "hostPath").readNullable[String] and
        (__ \ "persistent").readNullable[PersistentVolumeInfo] and
        (__ \ "mode").read[String]
      )(Volume.apply _)

    override def reads(json: JsValue): JsResult[Volume] = {
      json.validate[Volume](simpleVolumeReads) match {
        case s @ JsSuccess(Volume(cPath, Some(hPath), None,         mode), _) => s
        case s @ JsSuccess(Volume(cPath, None,        Some(pvInfo), mode), _) => s
        case s @ JsSuccess(Volume(_, None,    None,    _), _) => JsError("container volume must contain one of hostPath or persistent")
        case s @ JsSuccess(Volume(_, Some(_), Some(_), _), _) => JsError("container volume must contain one of hostPath or persistent")
        case e: JsError => e
      }
    }
  }

  lazy val marathonVolumeWrites = (
    (__ \ "containerPath").write[String] and
      (__ \ "hostPath").writeNullable[String] and
      (__ \ "persistent").writeNullable[PersistentVolumeInfo] and
      (__ \ "mode").write[String]
    )(unlift(Volume.unapply))


  implicit lazy val metaVolumeWrites = Json.writes[Volume]

  implicit lazy val metaVolumeReads = new Reads[Volume] {
    lazy val simpleVolumeReads = Json.reads[Volume]
    override def reads(json: JsValue): JsResult[Volume] = {
      json.validate[Volume](simpleVolumeReads) match {
        case s @ JsSuccess(Volume(cPath, Some(hPath), None,         mode), _) => s
        case s @ JsSuccess(Volume(cPath, None,        Some(pvInfo), mode), _) => s
        case s @ JsSuccess(Volume(_, None,    None,    _), _) => JsError("container volume must contain one of host_path or persistent")
        case s @ JsSuccess(Volume(_, Some(_), Some(_), _), _) => JsError("container volume must contain one of host_path or persistent")
        case e: JsError => e
      }
    }
  }

  implicit lazy val healthCheckFormat = Json.format[HealthCheck]

  implicit lazy val portMappingWrites = Json.writes[PortMapping]

  implicit lazy val portMappingFormat = Json.reads[PortMapping]

  implicit lazy val inputContainerPropertiesFormat = Json.format[InputContainerProperties]
  implicit lazy val inputContainerFormat = Json.format[InputContainer]

  implicit lazy val keyValuePairFormat = Json.format[KeyValuePair]
  implicit lazy val marathonHealthCheckFormat = Json.format[MarathonHealthCheck]

  implicit lazy val marathonPortMappingFormat = Json.format[MarathonContainer.Docker.PortMapping]

  implicit lazy val marathonPortDefintionFormat = Json.format[PortDefinition]
  implicit lazy val marathonDockerFormat = Json.format[MarathonContainer.Docker]

  implicit lazy val portDiscoveryFormat = Json.format[IPPerTaskInfo.DiscoveryInfo.PortDiscovery]
  implicit lazy val discoveryInfoFormat = Json.format[IPPerTaskInfo.DiscoveryInfo]
  implicit lazy val ipPerTaskInfoFormat = Json.format[IPPerTaskInfo]

  def getICBF()(implicit bf: generic.CanBuildFrom[Iterable[_], Volume, Iterable[Volume]]) = bf

  implicit lazy val marathonContainerReads: Reads[MarathonContainer] = (
    (__ \ "docker").readNullable[MarathonContainer.Docker] and
      (__ \ "type").read[String] and
      (__ \ "volumes").readNullable[Iterable[Volume]](Reads.traversableReads[Iterable,Volume](getICBF(), marathonVolumeReads))
    )(MarathonContainer.apply _)

  implicit lazy val marathonContainerWrites: Writes[MarathonContainer] = (
    (__ \ "docker").writeNullable[MarathonContainer.Docker] and
      (__ \ "type").write[String] and
      (__ \ "volumes").writeNullable[Iterable[Volume]](Writes.traversableWrites[Volume](marathonVolumeWrites))
    )(unlift(MarathonContainer.unapply))

  implicit lazy val marathonAppReads: Reads[MarathonApp] = (
    (__ \ "id").read[String] and
      (__ \ "container").read[MarathonContainer] and
      (__ \ "cpus").read[Double] and
      (__ \ "mem").read[Double] and
      (__ \ "instances").read[Int] and
      (__ \ "tasksStaged").readNullable[Int] and
      (__ \ "tasksRunning").readNullable[Int] and
      (__ \ "tasksHealthy").readNullable[Int] and
      (__ \ "tasksUnhealthy").readNullable[Int] and
      (__ \ "cmd").readNullable[String] and
      (__ \ "constraints").readNullable[Seq[Seq[String]]] and
      (__ \ "acceptedResourceRoles").readNullable[Seq[String]] and
      (__ \ "args").readNullable[Iterable[String]] and
      (__ \ "ports").readNullable[Iterable[Int]] and
      (__ \ "portDefinitions").readNullable[Iterable[PortDefinition]] and
      (__ \ "labels").readNullable[Map[String,String]] and
      (__ \ "healthChecks").readNullable[Iterable[MarathonHealthCheck]] and
      (__ \ "env").readNullable[Map[String,String]] and
      (__ \ "deployments").readNullable[Seq[JsObject]] and
      (__ \ "ipAddress").readNullable[IPPerTaskInfo] and
      (__ \ "upgradeStrategy").readNullable[UpgradeStrategy] and
      (__ \ "user").readNullable[String]
    )(MarathonApp.apply _)

  implicit lazy val marathonAppWrites = (
    (__ \ "id").write[String] and
      (__ \ "acceptedResourceRoles").write[JsValue] and
      (__ \ "args").write[JsValue] and
      (__ \ "container").write[MarathonContainer] and
      (__ \ "cmd").write[JsValue] and
      (__ \ "cpus").write[Double] and
      (__ \ "env").write[Map[String,String]] and
      (__ \ "healthChecks").write[Seq[MarathonHealthCheck]] and
      (__ \ "instances").write[Int] and
      (__ \ "ipAddress").write[JsValue] and
      (__ \ "labels").write[Map[String,String]] and
      (__ \ "mem").write[Double] and
      (__ \ "portDefinitions").write[Seq[PortDefinition]] and
      (__ \ "ports").writeNullable[Iterable[Int]] and
      (__ \ "upgradeStrategy").writeNullable[UpgradeStrategy] and
      (__ \ "user").write[JsValue] and
      (__ \ "constraints").write[Seq[Seq[String]]] and
      (__ \ "deployments").write[Seq[JsObject]] and
      (__ \ "tasksStaged").writeNullable[Int] and
      (__ \ "tasksRunning").writeNullable[Int] and
      (__ \ "tasksHealthy").writeNullable[Int] and
      (__ \ "tasksUnhealthy").writeNullable[Int]
    )(
    (a: MarathonApp) => (
      a.id, a.acceptedResourceRoles.fold[JsValue](JsNull)(Json.toJson(_)),
      a.args.map(as => JsArray(as.toSeq.map(JsString(_)))).getOrElse(if (a.cmd.isDefined) JsNull else JsArray()),
      a.container,
      a.cmd.fold[JsValue](JsNull)(JsString(_)), a.cpus,
      a.env.getOrElse(Map.empty), a.healthChecks.map(_.toSeq).getOrElse(Seq.empty), a.instances,
      a.ipAddress.fold[JsValue](JsNull)(Json.toJson(_)), a.labels.getOrElse(Map.empty),
      a.mem,
      a.portDefinitions.map(_.toSeq).getOrElse(Seq.empty),
      a.ports,
      a.upgradeStrategy,
      a.user.fold[JsValue](JsNull)(JsString(_)),
      a.constraints.getOrElse(Seq.empty),
      a.deployments.getOrElse(Seq.empty),
      a.tasksStaged, a.tasksRunning, a.tasksHealthy, a.tasksUnhealthy
    )
  )

  import com.galacticfog.gestalt.data.ResourceFactory

  /**
   * Convert Marathon App JSON to Meta Container JSON
   */
  def marathonApp2MetaContainer(inputJson: JsObject, providerId: UUID): JsObject = {
    log.debug("Entered marathonApp2MetaContainer...")
    log.debug("Received:\n" + Json.prettyPrint(inputJson))
    log.debug("Deserializing Marathon App JSON...")

    val app = inputJson.validate[MarathonApp].map {
      case app: MarathonApp => app
    }.recoverTotal { e =>
      throw new BadRequestException("Could not parse Marathon App JSON: " + JsError.toFlatJson(e).toString)
    }

    log.debug("Marathon App:\n" + app)
    log.debug("Looking up Provider : " + providerId)
    val provider = {
      val p = ResourceFactory.findById(ResourceIds.MarathonProvider, providerId) getOrElse {
        throw new ResourceNotFoundException(s"MarathonProvider with ID '$providerId' not found.")
      }
      InputProvider(id = p.id, name = Some(p.name))
    }
    log.debug("Provider:\n" + provider)

    val props = InputContainerProperties(
      container_type = "DOCKER",
      image = app.container.docker map {_.image} getOrElse "",
      provider = provider,
      port_mappings = app.container.docker flatMap {_.portMappings.map {_.zipWithIndex.map { case (pm,index) => PortMapping(
        label = Some(index.toString),
        protocol = pm.protocol getOrElse "tcp",
        container_port = pm.containerPort,
        host_port = pm.hostPort,
        service_port = pm.servicePort
      )}}} getOrElse Seq(),
      cpus = app.cpus,
      memory = app.mem.toInt,
      num_instances = app.instances,
      network = app.container.docker map {_.network} getOrElse "",
      cmd = app.cmd,
      constraints = app.constraints.map(_.map(_.foldLeft("")(_ + ":" + _))),
      accepted_resource_roles = app.acceptedResourceRoles,
      args = app.args,
      force_pull = app.container.docker flatMap {_.forcePullImage} getOrElse false,
      health_checks = app.healthChecks map { _.map{ check => HealthCheck(
        protocol = check.protocol getOrElse "http",
        path = check.path getOrElse "/",
        grace_period_seconds = check.gracePeriodSeconds getOrElse 15,
        interval_seconds = check.intervalSeconds getOrElse 10,
        timeout_seconds = check.timeoutSeconds getOrElse 20,
        max_consecutive_failures = check.maxConsecutiveFailures getOrElse 3
      )}},
      volumes = app.container.volumes,
      labels = app.labels,
      env = app.env
    )

    // Meta container name is last component of Marathon ID 'path'
    val containerName = {
        val cmps = app.id.stripPrefix("/").stripSuffix("/").split("/")
        cmps(cmps.size-1)
    }
    log.debug("Container-Name : " + containerName)

    val output = Json.toJson(InputContainer(name = containerName, properties = props))

    log.debug("Transform Complete:\n" + Json.prettyPrint(output))
    output.as[JsObject]
  }

  /**
    * Convert Meta container resource to MarathonApp object
 *
    * @param metaApp
    * @return
    */
  def meta2Marathon(metaApp: GestaltResourceInstance): Try[MarathonApp] = {

    def portmap(ps: Iterable[PortMapping]): Iterable[MarathonContainer.Docker.PortMapping] = {
      ps map {pm => MarathonContainer.Docker.PortMapping(
        protocol = Some(pm.protocol),
        containerPort = pm.container_port,
        hostPort = pm.host_port,
        servicePort = pm.service_port,
        name = pm.label
      )}
    }

    val mc = for {
      props <- Try{metaApp.properties.get}
      ctype <- Try{props("container_type")}
      provider <- Try{props("provider")} map {json => Json.parse(json).as[InputProvider]}
      cpus <- Try{props("cpus").toDouble}
      memory <- Try{props("memory").toInt}
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
      args = props.get("args") map {json => Json.parse(json).as[Iterable[String]]}
      health_checks = props.get("health_checks") map {json => Json.parse(json).as[Iterable[HealthCheck]]}
      volumes = props.get("volumes") map {json => Json.parse(json).as[Iterable[Volume]]}
      labels = props.get("labels") map {json => Json.parse(json).as[Map[String,String]]}
      env = props.get("env") map {json => Json.parse(json).as[Map[String,String]]}
      port_mappings = props.get("port_mappings") map {json => Json.parse(json).as[Iterable[PortMapping]]}
      tasks_running = props.get("tasks_running") map {_.toInt}
      tasks_healthy = props.get("tasks_healthy") map {_.toInt}
      tasks_unhealthy = props.get("tasks_unhealthy") map {_.toInt}
      tasks_staged = props.get("tasks_staged") map {_.toInt}
      user = props.get("user")
      docker = for {
        image <- props.get("image")
        network = props.get("network") getOrElse "HOST"
        force_pull = props.get("force_pull") map {_.toBoolean}
      } yield MarathonContainer.Docker(
        image = image,
        network = network,
        forcePullImage = force_pull,
        portMappings = port_mappings map portmap
      )
      container = MarathonContainer(
        docker = if (ctype.equalsIgnoreCase("DOCKER")) docker else None,
        containerType = ctype,
        volumes = volumes
      )
    } yield MarathonApp(
      id = "/" + metaApp.name,
      container = container,
      cpus = cpus,
      mem = memory,
      instances = num_instances,
      cmd = cmd,
      constraints = constraints,
      acceptedResourceRoles = acceptedResourceRoles flatMap {rs => if (rs.isEmpty) None else Some(rs)},
      args = args,
      ports = port_mappings map {_.flatMap { _.service_port }},
      portDefinitions = port_mappings map {_.map {
        pm => PortDefinition(port = pm.container_port, protocol = Some(pm.protocol), name = pm.label, labels = None)
      } },
      labels = labels,
      healthChecks = health_checks map {_.map { hc => MarathonHealthCheck(
        protocol = Some(hc.protocol),
        path = Some(hc.path),
        portIndex = None, // TODO: figure out how to define this
        gracePeriodSeconds = Some(hc.grace_period_seconds),
        intervalSeconds = Some(hc.interval_seconds),
        timeoutSeconds = Some(hc.timeout_seconds),
        maxConsecutiveFailures = Some(hc.max_consecutive_failures)
      )}},
      env = env,
      tasksStaged = tasks_staged,
      tasksRunning = tasks_running,
      tasksHealthy = tasks_healthy,
      tasksUnhealthy = tasks_unhealthy,
      deployments = Some(Seq.empty),
      user = user
    )
    mc recoverWith {
      case e: Throwable => throw new IllegalArgumentException("Could not parse container properties",e)
    }
  }


  /**
   * Convert Meta Container JSON to Marathon App object.
   * TODO: convert this to a Future[MarathonApp]
   */
  def toMarathonApp(fqon: String, workspaceName: String, environmentName: String, name: String, props: InputContainerProperties, provider: GestaltResourceInstance): MarathonApp = {

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


    def portmap(vip: String, exposeHost: Boolean, pm: PortMapping): MarathonContainer.Docker.PortMapping = {
      MarathonContainer.Docker.PortMapping(
        protocol = Some(pm.protocol),
        containerPort = pm.container_port,
        hostPort = pm.host_port,
        servicePort = pm.service_port.filter(_ => exposeHost),
        name = pm.label,
        labels = if (pm.load_balanced.contains(true)) Some(Map("VIP_0" -> s"${vip}:${pm.container_port}")) else None
      )
    }

    val namedVIP = "/" + (Array(name.stripPrefix("/").stripSuffix("/"),environmentName,workspaceName) ++ fqon.split('.').reverse).mkString(".")

    def toDocker(props: InputContainerProperties): (Option[MarathonContainer.Docker], Option[IPPerTaskInfo]) = {
      val requestedNetwork = props.network
      val dockerParams = props.user.filter(_.trim.nonEmpty).map(u => Seq(KeyValuePair("user",u)))
      if (providerNetworkNames.isEmpty) {
        (Some(MarathonContainer.Docker(
          image = props.image,
          network = requestedNetwork,
          forcePullImage = Some(props.force_pull),
          portMappings = if (requestedNetwork.equalsIgnoreCase("BRIDGE")) Some(props.port_mappings.map(portmap(vip = namedVIP, exposeHost = true, _))) else None,
          parameters = dockerParams
        )), None)
      } else {
        providerNetworkNames.find(_.equalsIgnoreCase(requestedNetwork)) match {
          case None => throw new BadRequestException(
            message = "invalid network name: container network was not among list of provider networks",
            payload = Some(Json.toJson(props))
          )
          case Some(stdNet) if stdNet.equalsIgnoreCase("HOST") || stdNet.equalsIgnoreCase("BRIDGE") =>
            (Some(MarathonContainer.Docker(
              image = props.image,
              network = stdNet,
              forcePullImage = Some(props.force_pull),
              portMappings = if (stdNet.equalsIgnoreCase("BRIDGE")) Some(props.port_mappings.map(portmap(vip = namedVIP, exposeHost = true, _))) else None,
              parameters = dockerParams
            )), None)
          case Some(calicoNet) =>
            val docker = MarathonContainer.Docker(
              image = props.image,
              network = "USER",
              forcePullImage = Some(props.force_pull),
              portMappings = Some(props.port_mappings.map(portmap(vip = namedVIP, exposeHost = false, _))),
              parameters = dockerParams
            )
            val ippertask = IPPerTaskInfo( discovery = None, networkName = Some(calicoNet) )
            (Some(docker), Some(ippertask))
        }
      }
    }

    val (docker,ipPerTask) =
      if (isDocker) toDocker(props)
      else (None,None) // no support for non-docker ipPerTask right now

    val container = MarathonContainer(
        docker = docker,
        containerType = props.container_type,
        volumes = props.volumes)

    val upgradeStrategy = if( container.volumes.exists(_.exists(_.isPersistent)) ) Some(DEFAULT_UPGRADE_STRATEGY_WITH_PERSISTENT_VOLUMES) else None

    MarathonApp(
      id = "/" + name.stripPrefix("/"),
      container = container,
      constraints = props.constraints.map(_.map(
        _.split(":") match {
          case Array(f1,f2) =>
            Seq(f1,f2.toUpperCase)
          case Array(f1,f2,f3) =>
            Seq(f1,f2.toUpperCase,f3)
          case e => e.toSeq
        }
      )),
      cpus = props.cpus,
      mem = props.memory,
      instances = props.num_instances,
      cmd = props.cmd,
      acceptedResourceRoles = props.accepted_resource_roles flatMap {rs => if (rs.isEmpty) None else Some(rs)},
      args = props.args,
      ports = None,
      portDefinitions = if (ipPerTask.isEmpty) Some(props.port_mappings map {
        pm => PortDefinition(
          port = pm.container_port,
          protocol = Some(pm.protocol),
          name = pm.label,
          labels = if (pm.load_balanced.contains(true)) Some(Json.obj("VIP_0" -> "")) else None
        )
      }) else None,
      labels = props.labels,
      healthChecks = props.health_checks map {_.map { hc => MarathonHealthCheck(
        protocol = Some(hc.protocol),
        path = Some(hc.path),
        portIndex = None, // TODO: don't know how to define this
        gracePeriodSeconds = Some(hc.grace_period_seconds),
        intervalSeconds = Some(hc.interval_seconds),
        timeoutSeconds = Some(hc.timeout_seconds),
        maxConsecutiveFailures = Some(hc.max_consecutive_failures)
      )}},
      env = props.env,
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
      case v => v.validate[InputProvider].map {
        case p: InputProvider => p
      }.recoverTotal { e =>
        throw new IllegalArgumentException("Invalid provider JSON: " + JsError.toFlatJson(e).toString)  
      }
    }
    InputContainerProperties(
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
