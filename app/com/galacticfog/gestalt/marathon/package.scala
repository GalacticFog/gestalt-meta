package com.galacticfog.gestalt
 
import java.util.UUID
import com.galacticfog.gestalt.data.models.GestaltResourceInstance

import scala.util.{Try,Success,Failure}
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import play.api.libs.json._
import play.api.{ Logger => log }
import play.api.libs.json.Reads._ // Custom validation helpers
import play.api.libs.functional.syntax._ // Combinator syntax

package object marathon {

  case class Volume(
      container_path: String,
      host_path: String,
      mode: String)

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

  case class PortMapping(
      protocol: String,
      container_port: Int, 
      host_port: Int = 0, 
      service_port: Int = 0,
      label: Option[String] = None)
      
  case class InputContainerProperties(
      container_type: String,
      image: String,
      provider: InputProvider,
      port_mappings: Iterable[PortMapping] = Seq(),
      cpus: Double = 0.2,
      memory: Int = 128,
      num_instances: Int = 1,
      network: String = "BRIDGE",
      cmd: Option[String] = None,
      args: Option[Iterable[String]] = None,
      force_pull: Boolean = false,
      health_checks: Option[Iterable[HealthCheck]] = None,
      volumes: Option[Iterable[Volume]] = None,
      labels: Option[Map[String,String]] = None,
      env: Option[Map[String,String]] = None)
  
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
      portMappings: Option[Iterable[MarathonPortMapping]] = None,
      parameters: Option[Iterable[KeyValuePair]] = None)

  case class MarathonContainer(
      docker: Option[MarathonDocker],
      containerType: String,
      volumes: Option[Iterable[Volume]] = None
  )

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
    args: Option[Iterable[String]] = None,
    ports: Option[Iterable[Int]] = None,
    portDefinitions: Option[Iterable[PortDefinition]] = None,
    labels: Option[Map[String, String]] = None,
    healthChecks: Option[Iterable[MarathonHealthCheck]] = None,
    env: Option[Map[String, String]] = None,
    deployments: Option[Seq[JsObject]] = None,
    ipAddress: Option[IPPerTaskInfo] = None)
    
  implicit lazy val inputProviderFormat = Json.format[InputProvider]
  implicit lazy val volumeFormat = Json.format[Volume]
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

  implicit lazy val inputContainerPropertiesFormat = Json.format[InputContainerProperties]
  implicit lazy val inputContainerFormat = Json.format[InputContainer]
  
  implicit lazy val keyValuePairFormat = Json.format[KeyValuePair]
  implicit lazy val marathonHealthCheckFormat = Json.format[MarathonHealthCheck]
  implicit lazy val marathonPortMappingFormat = Json.format[MarathonPortMapping]
  implicit lazy val marathonPortDefintionFormat = Json.format[PortDefinition]
  implicit lazy val marathonDockerFormat = Json.format[MarathonDocker]

  implicit lazy val portDiscoveryFormat = Json.format[PortDiscovery]
  implicit lazy val discoveryInfoFormat = Json.format[DiscoveryInfo]
  implicit lazy val ipPerTaskInfoFormat = Json.format[IPPerTaskInfo]

  implicit lazy val marathonContainerReads: Reads[MarathonContainer] = (
    (__ \ "docker").readNullable[MarathonDocker] and
      (__ \ "type").read[String] and
      (__ \ "volumes").readNullable[Iterable[Volume]]
    )(MarathonContainer.apply _)

  implicit lazy val marathonContainerWrites: Writes[MarathonContainer] = (
    (__ \ "docker").writeNullable[MarathonDocker] and
      (__ \ "type").write[String] and
      (__ \ "volumes").writeNullable[Iterable[Volume]]
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
      (__ \ "args").readNullable[Iterable[String]] and
      (__ \ "ports").readNullable[Iterable[Int]] and
      (__ \ "portDefinitions").readNullable[Iterable[PortDefinition]] and
      (__ \ "labels").readNullable[Map[String,String]] and
      (__ \ "healthChecks").readNullable[Iterable[MarathonHealthCheck]] and
      (__ \ "env").readNullable[Map[String,String]] and
      (__ \ "deployments").readNullable[Seq[JsObject]] and
      (__ \ "ipAddress").readNullable[IPPerTaskInfo]
    )(MarathonApp.apply _)

  implicit lazy val marathonAppWrites = new Writes[MarathonApp] {
    override def writes(o: MarathonApp): JsValue = {
      val base = Json.obj(
        "id" -> o.id,
        "container" -> Json.toJson(o.container),
        "cpus" -> o.cpus,
        "mem" -> o.mem,
        "instances" -> o.instances,
        "cmd" -> o.cmd.getOrElse(null),
        "args" -> o.args.getOrElse(Seq()).map(Json.toJson(_)),
        "labels" -> Json.toJson(o.labels.getOrElse(Map())),
        "healthChecks" -> Json.toJson(o.healthChecks.getOrElse(Seq())),
        "env" -> Json.toJson(o.env.getOrElse(Map())),
        "deployments" -> Json.toJson(o.deployments.getOrElse(Seq())),
        "tasksStaged" -> Json.toJson(o.tasksStaged.getOrElse(0)),
        "tasksRunning" -> Json.toJson(o.tasksRunning.getOrElse(0)),
        "tasksHealthy" -> Json.toJson(o.tasksHealthy.getOrElse(0)),
        "tasksUnhealthy" -> Json.toJson(o.tasksUnhealthy.getOrElse(0))
      )
      base ++ o.portDefinitions.map(pd => Json.obj("portDefinitions" -> Json.toJson(pd))).getOrElse(Json.obj()) ++
              o.ipAddress.map(ip => Json.obj("ipAddress" -> Json.toJson(ip))).getOrElse(Json.obj())
    }
  }

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
        host_port = pm.hostPort getOrElse 0,
        service_port = pm.servicePort getOrElse 0
      )}}} getOrElse Seq(),
      cpus = app.cpus,
      memory = app.mem.toInt,
      num_instances = app.instances,
      network = app.container.docker map {_.network} getOrElse "",
      cmd = app.cmd,
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

    def portmap(ps: Iterable[PortMapping]): Iterable[MarathonPortMapping] = {
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
      args = args,
      ports = None,
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
      tasksUnhealthy = tasks_unhealthy
    )
    mc recoverWith {
      case e: Throwable => throw new IllegalArgumentException("Could not parse container properties",e)
    }
  }


  /**
   * Convert Meta Container JSON to Marathon App object.
   */
  def toMarathonApp(name: String, inputJson: JsObject, provider: GestaltResourceInstance): MarathonApp = {
    
    val props = (inputJson \ "properties").validate[InputContainerProperties].map {
      case ps: InputContainerProperties => ps
    }.recoverTotal { e =>
      throw new IllegalArgumentException(
          "Could not parse container properties: " + JsError.toFlatJson(e).toString)
    }

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


    def portmap(ps: Iterable[PortMapping]): Iterable[MarathonPortMapping] = {
      ps map {pm => MarathonPortMapping(
        protocol = Some(pm.protocol),
        containerPort = pm.container_port,
        hostPort = Some(pm.host_port),
        servicePort = Some(pm.service_port)
      )}
    }

    val portLabelToIndex = props.port_mappings.zipWithIndex.map {
      case (m,i) => (m.label getOrElse s"${m.protocol}/${m.container_port}") -> i
    }.toMap

    val (docker,ipPerTask) = if (isDocker) {
      val requestedNetwork = props.network
      val (dockerNet,dockerParams,ipPerTask) = if (providerNetworkNames.isEmpty) {
        (requestedNetwork, None, None)
      } else {
        providerNetworkNames.find(_.equalsIgnoreCase(requestedNetwork)) match {
          case None => throw new BadRequestException(
            message = "invalid network name: container network was not among list of provider networks",
            payload = Some(inputJson)
          )
          case Some(stdNet) if stdNet.equalsIgnoreCase("HOST") || stdNet.equalsIgnoreCase("BRIDGE") =>
            (stdNet, None, None)
          case Some(calicoNet) =>
            (
              "HOST",
              Some(Seq(
                KeyValuePair("net", calicoNet)
              )),
              Some(IPPerTaskInfo(
                discovery = Some(DiscoveryInfo(
                  ports = Some(props.port_mappings.map(pm => PortDiscovery(
                    number = pm.container_port,
                    name = pm.label getOrElse pm.container_port.toString,
                    protocol = pm.protocol
                  )).toSeq)
                ))
              ))
            )
        }
      }
      (Some(MarathonDocker(
        image = props.image,
        network = dockerNet,
        forcePullImage = Some(props.force_pull),
        portMappings = if (dockerNet.equalsIgnoreCase("BRIDGE")) Some(portmap(props.port_mappings)) else None,
        parameters = dockerParams
      )), ipPerTask)
    } else (None,None) // no support for non-docker ipPerTask right now

    val container = MarathonContainer(
        docker = docker,
        containerType = props.container_type,
        volumes = props.volumes)

    MarathonApp(
      id = "/" + name,
      container = container,
      cpus = props.cpus,
      mem = props.memory,
      instances = props.num_instances,
      cmd = props.cmd,
      args = props.args,
      ports = None,
      portDefinitions = if (ipPerTask.isEmpty) Some(props.port_mappings map {
        pm => PortDefinition(port = pm.container_port, protocol = Some(pm.protocol), name = pm.label, labels = None)
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
      ipAddress = ipPerTask
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
