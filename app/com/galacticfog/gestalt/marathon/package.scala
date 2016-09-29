package com.galacticfog.gestalt
 
import java.util.UUID
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import play.api.data.validation.ValidationError

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

  case class PersistentVolumeInfo(size: Long)

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
      locations: Option[Seq[String]] = None)

  case class PortMapping(
      protocol: String,
      container_port: Int, 
      host_port: Int = 0, 
      service_port: Int = 0,
      label: Option[String] = None)

  case class InputContainerProperties(container_type: String,
                                      image: String,
                                      provider: InputProvider,
                                      port_mappings: Seq[PortMapping] = Seq(),
                                      cpus: Double = 0.2,
                                      memory: Int = 128,
                                      disk: Double = 0.0,
                                      num_instances: Int = 1,
                                      network: String = "BRIDGE",
                                      cmd: Option[String] = None,
                                      constraints: Seq[String] = Seq(),
                                      accepted_resource_roles: Option[Seq[String]] = None,
                                      args: Option[Seq[String]] = None,
                                      force_pull: Boolean = false,
                                      health_checks: Seq[HealthCheck] = Seq(),
                                      volumes: Seq[Volume] = Seq(),
                                      labels: Map[String,String] = Map(),
                                      env: Map[String,String] = Map(),
                                      user: Option[String] = None)

  case class InputContainer(
      id: UUID = UUID.randomUUID,
      name: String,
      resource_type: UUID = ResourceIds.Container,
      properties: InputContainerProperties)

  case class KeyValuePair(key: String, value: String)

  case class MarathonPortMapping(
                          protocol: Option[String] = Some("tcp"),
                          containerPort: Int,
                          hostPort: Option[Int] = Some(0),
                          servicePort: Option[Int] = Some(0))

  case class MarathonDocker(
      image: String,
      network: String = "BRIDGE",
      forcePullImage: Option[Boolean] = None,
      portMappings: Option[Seq[MarathonPortMapping]] = None,
      parameters: Option[Seq[KeyValuePair]] = None)

  case class MarathonContainer(docker: Option[MarathonDocker],
                               `type`: String,
                               volumes: Option[Seq[Volume]] = None)

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

  case class PortDiscovery(number: Int, name: String, protocol: String)
  case class DiscoveryInfo(ports: Option[Seq[PortDiscovery]] = None)
  case class IPPerTaskInfo(discovery: Option[DiscoveryInfo])

  case class UpgradeStrategy(minimumHealthCapacity: Double, maximumOverCapacity: Double)

  case class MarathonAppUpdate(
    id: String,
    container: MarathonContainer,
    cpus: Double = 1.0,
    mem: Double = 128.0,
    disk: Double = 0,
    instances: Int = 1,
    cmd: Option[String] = None,
    constraints: Seq[Seq[String]] = Seq(),
    acceptedResourceRoles: Option[Seq[String]] = None,
    args: Option[Seq[String]] = None,
    portDefinitions: Seq[PortDefinition] = Seq(),
    labels: Map[String, String] = Map(),
    healthChecks: Seq[MarathonHealthCheck] = Seq(),
    env: Map[String, String] = Map(),
    ipAddress: Option[IPPerTaskInfo] = None,
    upgradeStrategy: Option[UpgradeStrategy] = None,
    user: Option[String] = None)

  implicit lazy val inputProviderFormat = Json.format[InputProvider]

  implicit lazy val marathonVolumePersistenceFmt = Json.format[PersistentVolumeInfo]

  implicit lazy val marathonUpgradeStrategyFmt = Json.format[UpgradeStrategy]

  lazy val marathonVolumeReads =  (
    (__ \ "containerPath").read[String] and
      (__ \ "hostPath").readNullable[String] and
      (__ \ "persistent").readNullable[PersistentVolumeInfo] and
      (__ \ "mode").read[String]
    )(Volume.apply _)

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

  implicit lazy val portMappingFormat = new Reads[PortMapping] {
    override def reads(json: JsValue): JsResult[PortMapping] = {
      (for {
        protocol <- (json \ "protocol").asOpt[String]
        container_port <- (json \ "container_port").asOpt[Int]
        host_port = (json \ "host_port").asOpt[Int] getOrElse 0
        service_port = (json \ "service_port").asOpt[Int] getOrElse 0
        label = (json \ "label").asOpt[String]
      } yield PortMapping(
        protocol = protocol,
        container_port = container_port,
        host_port = host_port,
        service_port = service_port,
        label = label
      )) match {
        case Some(pm) => JsSuccess(pm)
        case None => JsError("Could not parse PortMapping")
      }
    }
  }

  lazy val inputContainerPropertiesWrites = Json.writes[InputContainerProperties]
  lazy val inputContainerPropertiesReads: Reads[InputContainerProperties] = (
    (__ \ "container_type").read[String] and
    (__ \ "image").read[String] and
    (__ \ "provider").read[InputProvider] and
    ((__ \ "port_mappings").read[Seq[PortMapping]] orElse Reads.pure(Seq())) and
    ((__ \ "cpus").read[Double] orElse Reads.pure(0.2)) and
    ((__ \ "memory").read[Int] orElse Reads.pure(128)) and
    ((__ \ "disk").read[Double] orElse Reads.pure(0.0)) and
    ((__ \ "num_instances").read[Int] orElse Reads.pure(1)) and
    ((__ \ "network").read[String] orElse Reads.pure("BRIDGE")) and
    (__ \ "cmd").readNullable[String] and
    ((__ \ "constraints").read[Seq[String]] orElse Reads.pure(Seq())) and
    (__ \ "accepted_resource_roles").readNullable[Seq[String]] and
    (__ \ "args").readNullable[Seq[String]] and
    ((__ \ "force_pull").read[Boolean] orElse Reads.pure(false)) and
    ((__ \ "health_checks").read[Seq[HealthCheck]] orElse Reads.pure(Seq())) and
    ((__ \ "volumes").read[Seq[Volume]] orElse Reads.pure(Seq())) and
    ((__ \ "labels").read[Map[String,String]] orElse Reads.pure(Map())) and
    ((__ \ "env").read[Map[String,String]] orElse Reads.pure(Map())) and
    (__ \ "user").readNullable[String]
    )(InputContainerProperties.apply _)

  implicit lazy val inputContainerPropertiesFormat = Format(inputContainerPropertiesReads, inputContainerPropertiesWrites)


  implicit lazy val inputContainerFormat = Json.format[InputContainer]

  implicit lazy val keyValuePairFormat = Json.format[KeyValuePair]
  implicit lazy val marathonHealthCheckFormat = Json.format[MarathonHealthCheck]
  implicit lazy val marathonPortMappingFormat = Json.format[MarathonPortMapping]
  implicit lazy val marathonPortDefintionFormat = Json.format[PortDefinition]
  implicit lazy val marathonDockerFormat = Json.format[MarathonDocker]

  implicit lazy val portDiscoveryFormat = Json.format[PortDiscovery]
  implicit lazy val discoveryInfoFormat = Json.format[DiscoveryInfo]
  implicit lazy val ipPerTaskInfoFormat = Json.format[IPPerTaskInfo]

  def getICBF()(implicit bf: generic.CanBuildFrom[Seq[_], Volume, Seq[Volume]]) = bf

  implicit lazy val marathonContainerReads: Reads[MarathonContainer] = (
    (__ \ "docker").readNullable[MarathonDocker] and
      ((__ \ "type").read[String] orElse Reads.pure("DOCKER")) and
      (__ \ "volumes").readNullable[Seq[Volume]](Reads.traversableReads[Seq,Volume](getICBF(), marathonVolumeReads))
    )(MarathonContainer(_, _, _))
    .filterNot(ValidationError("container volume must contain one of hostPath or persistent")) { container =>
      container.volumes.exists(_.exists { vol =>
        !(vol.host_path.isDefined ^ vol.persistent.isDefined)
      })
    }

  implicit lazy val marathonContainerWrites: Writes[MarathonContainer] = (
    (__ \ "docker").writeNullable[MarathonDocker] and
      (__ \ "type").write[String] and
      (__ \ "volumes").writeNullable[Seq[Volume]](Writes.traversableWrites[Volume](marathonVolumeWrites))
    )(unlift(MarathonContainer.unapply))

  implicit lazy val marathonAppReads: Reads[MarathonAppUpdate] = (
    (__ \ "id").read[String] and
      (__ \ "container").read[MarathonContainer] and
      ((__ \ "cpus").read[Double] orElse Reads.pure(1.0)) and
      ((__ \ "mem").read[Double] orElse Reads.pure(128.0)) and
      ((__ \ "disk").read[Double] orElse Reads.pure(0)) and
      ((__ \ "instances").read[Int] orElse Reads.pure(1)) and
      (__ \ "cmd").readNullable[String] and
      ((__ \ "constraints").read[Seq[Seq[String]]] orElse Reads.pure(Seq())) and
      (__ \ "acceptedResourceRoles").readNullable[Seq[String]] and
      (__ \ "args").readNullable[Seq[String]] and
      ((__ \ "portDefinitions").read[Seq[PortDefinition]] orElse Reads.pure(Seq())) and
      ((__ \ "labels").read[Map[String,String]] orElse Reads.pure(Map())) and
      ((__ \ "healthChecks").read[Seq[MarathonHealthCheck]] orElse Reads.pure(Seq())) and
      ((__ \ "env").read[Map[String,String]] orElse Reads.pure(Map())) and
      (__ \ "ipAddress").readNullable[IPPerTaskInfo] and
      (__ \ "upgradeStrategy").readNullable[UpgradeStrategy] and
      (__ \ "user").readNullable[String]
    )(MarathonAppUpdate.apply _)

  implicit lazy val marathonAppUpdateWrites = (
      (__ \ "acceptedResourceRoles").write[JsValue] and
      (__ \ "args").write[JsValue] and
      (__ \ "cmd").write[JsValue] and
      (__ \ "constraints").write[Seq[Seq[String]]] and
      (__ \ "container").write[MarathonContainer] and
      (__ \ "cpus").write[Double] and
      (__ \ "disk").write[Double] and
      (__ \ "env").write[Map[String,String]] and
      (__ \ "healthChecks").write[Seq[MarathonHealthCheck]] and
      (__ \ "id").write[String] and
      (__ \ "instances").write[Int] and
      (__ \ "ipAddress").write[JsValue] and
      (__ \ "labels").write[Map[String,String]] and
      (__ \ "mem").write[Double] and
      (__ \ "portDefinitions").write[Seq[PortDefinition]] and
      (__ \ "upgradeStrategy").writeNullable[UpgradeStrategy] and
      (__ \ "user").write[JsValue]
    )(
    (a: MarathonAppUpdate) => (
      a.acceptedResourceRoles.fold[JsValue](JsNull)(Json.toJson(_)),
      a.args.map(as => JsArray(as.toSeq.map(JsString(_)))).getOrElse(if (a.cmd.isDefined) JsNull else JsArray()),
      a.cmd.fold[JsValue](JsNull)(JsString(_)),
      a.constraints,
      a.container,
      a.cpus,
      a.disk,
      a.env,
      a.healthChecks,
      a.id,
      a.instances,
      a.ipAddress.fold[JsValue](JsNull)(Json.toJson(_)),
      a.labels,
      a.mem,
      a.portDefinitions,
      a.upgradeStrategy,
      a.user.fold[JsValue](JsNull)(JsString(_))
    )
  )

  import com.galacticfog.gestalt.data.ResourceFactory

  /**
   * Convert Marathon App JSON to Meta Container JSON
   */
  def marathonApp2MetaContainer(inputJson: JsValue, provider: GestaltResourceInstance): Try[(String,InputContainerProperties)] = Try {
    log.debug("Entered marathonApp2MetaContainer...")
    log.debug("Received:\n" + Json.prettyPrint(inputJson))
    log.debug("Deserializing Marathon App JSON...")

    val app = inputJson.validate[MarathonAppUpdate].recoverTotal { e =>
      throw new BadRequestException("Could not parse Marathon App JSON: " + JsError.toFlatJson(e).toString)
    }

    log.debug("Marathon App:\n" + app)

    val name = app.id
    val props = InputContainerProperties(
      container_type = "DOCKER",
      image = app.container.docker map {_.image} getOrElse "",
      provider = InputProvider(id = provider.id, name = Some(provider.name)),
      port_mappings = app.container.docker flatMap {_.portMappings.map {_.zipWithIndex.map { case (pm,index) => PortMapping(
        label = Some(index.toString),
        protocol = pm.protocol getOrElse "tcp",
        container_port = pm.containerPort,
        host_port = pm.hostPort getOrElse 0,
        service_port = pm.servicePort getOrElse 0
      )}}} getOrElse Seq(),
      cpus = app.cpus,
      memory = app.mem.toInt,
      num_instances = app.instances,
      network = app.container.docker map {_.network} getOrElse "",
      cmd = app.cmd,
      constraints = app.constraints.map(_.foldLeft("")(_ + ":" + _)),
      accepted_resource_roles = app.acceptedResourceRoles,
      args = app.args.map(_.toSeq),
      force_pull = app.container.docker flatMap {_.forcePullImage} getOrElse false,
      health_checks = app.healthChecks map { check => HealthCheck(
        protocol = check.protocol getOrElse "http",
        path = check.path getOrElse "/",
        grace_period_seconds = check.gracePeriodSeconds getOrElse 15,
        interval_seconds = check.intervalSeconds getOrElse 10,
        timeout_seconds = check.timeoutSeconds getOrElse 20,
        max_consecutive_failures = check.maxConsecutiveFailures getOrElse 3
      )},
      volumes = app.container.volumes getOrElse Seq(),
      labels = app.labels,
      env = app.env
    )
    (name,props)
  }

  /**
    * Convert Meta container resource to MarathonApp object
 *
    * @param metaApp
    * @return
    */
  def meta2Marathon(metaApp: GestaltResourceInstance): Try[MarathonAppUpdate] = {

    def portmap(ps: Seq[PortMapping]): Seq[MarathonPortMapping] = {
      ps map {pm => MarathonPortMapping(
        protocol = Some(pm.protocol),
        containerPort = pm.container_port,
        hostPort = Some(pm.host_port),
        servicePort = Some(pm.service_port)
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
      args = props.get("args") map {json => Json.parse(json).as[Seq[String]]}
      health_checks = props.get("health_checks") map {json => Json.parse(json).as[Seq[HealthCheck]]}
      volumes = props.get("volumes") map {json => Json.parse(json).as[Seq[Volume]]}
      labels = props.get("labels") map {json => Json.parse(json).as[Map[String,String]]}
      env = props.get("env") map {json => Json.parse(json).as[Map[String,String]]}
      port_mappings = props.get("port_mappings") map {json => Json.parse(json).as[Seq[PortMapping]]}
      tasks_running = props.get("tasks_running") map {_.toInt}
      tasks_healthy = props.get("tasks_healthy") map {_.toInt}
      tasks_unhealthy = props.get("tasks_unhealthy") map {_.toInt}
      tasks_staged = props.get("tasks_staged") map {_.toInt}
      user = props.get("user")
      docker = for {
        image <- props.get("image")
        network = props.get("network") getOrElse "HOST"
        force_pull = props.get("force_pull") map {_.toBoolean}
      } yield MarathonDocker(
        image = image,
        network = network,
        forcePullImage = force_pull,
        portMappings = port_mappings map portmap
      )
      container = MarathonContainer(
        docker = if (ctype.equalsIgnoreCase("DOCKER")) docker else None,
        `type` = ctype,
        volumes = volumes
      )
    } yield MarathonAppUpdate(
      id = "/" + metaApp.name,
      container = container,
      cpus = cpus,
      mem = memory,
      instances = num_instances,
      cmd = cmd,
      constraints = constraints getOrElse Seq(),
      acceptedResourceRoles = acceptedResourceRoles flatMap {rs => if (rs.isEmpty) None else Some(rs)},
      args = args,
      portDefinitions = port_mappings map {_.map {
        pm => PortDefinition(port = pm.container_port, protocol = Some(pm.protocol), name = pm.label, labels = None)
      } } getOrElse Seq(),
      labels = labels getOrElse Map(),
      healthChecks = health_checks map {_.map { hc => MarathonHealthCheck(
        protocol = Some(hc.protocol),
        path = Some(hc.path),
        portIndex = None, // TODO: figure out how to define this
        gracePeriodSeconds = Some(hc.grace_period_seconds),
        intervalSeconds = Some(hc.interval_seconds),
        timeoutSeconds = Some(hc.timeout_seconds),
        maxConsecutiveFailures = Some(hc.max_consecutive_failures)
      )}} getOrElse Seq(),
      env = env getOrElse Map(),
      // TODO
//      tasksStaged = tasks_staged,
//      tasksRunning = tasks_running,
//      tasksHealthy = tasks_healthy,
//      tasksUnhealthy = tasks_unhealthy,
//      deployments = Some(Seq.empty),
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
  def toMarathonApp(name: String, props: InputContainerProperties, provider: GestaltResourceInstance): MarathonAppUpdate = {

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


    def portmap(ps: Seq[PortMapping]): Seq[MarathonPortMapping] = {
      ps map {pm => MarathonPortMapping(
        protocol = Some(pm.protocol),
        containerPort = pm.container_port,
        hostPort = Some(pm.host_port),
        servicePort = Some(pm.service_port)
      )}
    }

    def toDocker(props: InputContainerProperties): (Option[MarathonDocker], Option[IPPerTaskInfo]) = {
      val requestedNetwork = props.network
      val dockerParams = props.user.filter(_.trim.nonEmpty).map(u => Seq(KeyValuePair("user",u)))
      if (providerNetworkNames.isEmpty) {
        (Some(MarathonDocker(
          image = props.image,
          network = requestedNetwork,
          forcePullImage = Some(props.force_pull),
          portMappings = if (requestedNetwork.equalsIgnoreCase("BRIDGE")) Some(portmap(props.port_mappings)) else None,
          parameters = dockerParams
        )), None)
      } else {
        providerNetworkNames.find(_.equalsIgnoreCase(requestedNetwork)) match {
          case None => throw new BadRequestException(
            message = "invalid network name: container network was not among list of provider networks",
            payload = Some(Json.toJson(props))
          )
          case Some(stdNet) if stdNet.equalsIgnoreCase("HOST") || stdNet.equalsIgnoreCase("BRIDGE") =>
            (Some(MarathonDocker(
              image = props.image,
              network = stdNet,
              forcePullImage = Some(props.force_pull),
              portMappings = if (stdNet.equalsIgnoreCase("BRIDGE")) Some(portmap(props.port_mappings)) else None,
              parameters = dockerParams
            )), None)
          case Some(calicoNet) =>
            val docker = MarathonDocker(
              image = props.image,
              network = "HOST",
              forcePullImage = Some(props.force_pull),
              portMappings = None,
              parameters = Some(Seq(
                KeyValuePair("net", calicoNet)
              ) ++ dockerParams.getOrElse(Seq()))
            )
            val ippertask = IPPerTaskInfo(
                discovery = Some(DiscoveryInfo(
                  ports = Some(props.port_mappings.map(pm => PortDiscovery(
                    number = pm.container_port,
                    name = pm.label getOrElse pm.container_port.toString,
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

    val container = MarathonContainer(
        docker = docker,
        `type` = props.container_type,
        volumes = Some(props.volumes))

    val upgradeStrategy = if( container.volumes.exists(_.exists(_.isPersistent) ) ) Some(DEFAULT_UPGRADE_STRATEGY_WITH_PERSISTENT_VOLUMES) else None

    MarathonAppUpdate(
      id = "/" + name.stripPrefix("/"),
      container = container,
      constraints = props.constraints.map(
        _.split(":") match {
          case Array(f1,f2) =>
            Seq(f1,f2.toUpperCase)
          case Array(f1,f2,f3) =>
            Seq(f1,f2.toUpperCase,f3)
          case e => e.toSeq
        }
      ),
      cpus = props.cpus,
      mem = props.memory,
      instances = props.num_instances,
      cmd = props.cmd,
      acceptedResourceRoles = props.accepted_resource_roles flatMap {rs => if (rs.isEmpty) None else Some(rs)},
      args = props.args,
      portDefinitions = if (ipPerTask.isEmpty) props.port_mappings map {
        pm => PortDefinition(port = pm.container_port, protocol = Some(pm.protocol), name = pm.label, labels = None)
      } else Seq(),
      labels = props.labels,
      healthChecks = props.health_checks map { hc => MarathonHealthCheck(
        protocol = Some(hc.protocol),
        path = Some(hc.path),
        portIndex = None, // TODO: don't know how to define this
        gracePeriodSeconds = Some(hc.grace_period_seconds),
        intervalSeconds = Some(hc.interval_seconds),
        timeoutSeconds = Some(hc.timeout_seconds),
        maxConsecutiveFailures = Some(hc.max_consecutive_failures)
      )},
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
