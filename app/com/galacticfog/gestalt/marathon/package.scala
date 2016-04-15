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
    ports: Iterable[Int] = Seq(0),
    labels: Map[String, String] = Map(),
    healthChecks: Iterable[MarathonHealthCheck] = None,
    env: Map[String, String] = Map(),
    deployments: Seq[JsObject] = Seq())
    
  implicit lazy val inputProviderFormat = Json.format[InputProvider]
  implicit lazy val volumeFormat = Json.format[Volume]
  implicit lazy val healthCheckFormat = Json.format[HealthCheck]
  implicit lazy val portMappingFormat = Json.format[PortMapping]
  implicit lazy val inputContainerPropertiesFormat = Json.format[InputContainerProperties]
  implicit lazy val inputContainerFormat = Json.format[InputContainer]
  
  implicit lazy val keyValuePairFormat = Json.format[KeyValuePair]
  implicit lazy val marathonHealthCheckFormat = Json.format[MarathonHealthCheck]
  implicit lazy val marathonPortMappingFormat = Json.format[MarathonPortMapping]
  implicit lazy val marathonDockerFormat = Json.format[MarathonDocker]

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
      (__ \ "cmd").read[Option[String]] and
      (__ \ "args").read[Option[Iterable[String]]] and
      (__ \ "ports").read[Iterable[Int]] and
      (__ \ "labels").read[Map[String,String]] and
      (__ \ "healthChecks").read[Iterable[MarathonHealthCheck]] and
      (__ \ "env").read[Map[String,String]] and
      (__ \ "deployments").read[Seq[JsObject]]
    )(MarathonApp.apply _)

  implicit lazy val marathonAppWrites: Writes[MarathonApp] = (
    (__ \ "id").write[String] and
      (__ \ "container").write[MarathonContainer] and
      (__ \ "cpus").write[Double] and
      (__ \ "mem").write[Double] and
      (__ \ "instances").write[Int] and
      (__ \ "tasksStaged").writeNullable[Int] and
      (__ \ "tasksRunning").writeNullable[Int] and
      (__ \ "tasksHealthy").writeNullable[Int] and
      (__ \ "tasksUnhealthy").writeNullable[Int] and
      (__ \ "cmd").write[Option[String]] and
      (__ \ "args").write[Option[Iterable[String]]] and
      (__ \ "ports").write[Iterable[Int]] and
      (__ \ "labels").write[Map[String,String]] and
      (__ \ "healthChecks").write[Iterable[MarathonHealthCheck]] and
      (__ \ "env").write[Map[String,String]] and
      (__ \ "deployments").write[Seq[JsObject]]
    )(unlift(MarathonApp.unapply))
  
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
      health_checks = Some(app.healthChecks map {check => HealthCheck(
        protocol = check.protocol getOrElse "http",
        path = check.path getOrElse "/",
        grace_period_seconds = check.gracePeriodSeconds getOrElse 15,
        interval_seconds = check.intervalSeconds getOrElse 10,
        timeout_seconds = check.timeoutSeconds getOrElse 20,
        max_consecutive_failures = check.maxConsecutiveFailures getOrElse 3
      )}),
      volumes = app.container.volumes,
      labels = Some(app.labels),
      env = Some(app.env))
    
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
      labels = props.get("labels") map {json => Json.parse(json).as[Map[String,String]]} getOrElse Map()
      env = props.get("env") map {json => Json.parse(json).as[Map[String,String]]} getOrElse Map()
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
        docker = if (ctype.toUpperCase == "DOCKER") docker else None,
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
      ports = port_mappings map { _.map {_.service_port} } getOrElse Seq(),
      labels = labels,
      healthChecks = health_checks map {_.map { hc => MarathonHealthCheck(
        protocol = Some(hc.protocol),
        path = Some(hc.path),
        portIndex = None, // TODO: figure out how to define this
        gracePeriodSeconds = Some(hc.grace_period_seconds),
        intervalSeconds = Some(hc.interval_seconds),
        timeoutSeconds = Some(hc.timeout_seconds),
        maxConsecutiveFailures = Some(hc.max_consecutive_failures)
      )}} getOrElse Seq(),
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
  def toMarathonApp(name: String, inputJson: JsObject): MarathonApp = {
    
    val props = (inputJson \ "properties").validate[InputContainerProperties].map {
      case ps: InputContainerProperties => ps
    }.recoverTotal { e =>
      throw new IllegalArgumentException(
          "Could not parse container properties: " + JsError.toFlatJson(e).toString)
    }
    
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

    val docker = if(props.container_type.toLowerCase == "DOCKER") Some(MarathonDocker(
        image = props.image,
        network = props.network,
        forcePullImage = Some(props.force_pull),
        portMappings = Some(portmap(props.port_mappings))
    )) else None

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
      ports = props.port_mappings map { _.service_port },
      labels = props.labels getOrElse Map(),
      healthChecks = props.health_checks map {_.map { hc => MarathonHealthCheck(
        protocol = Some(hc.protocol),
        path = Some(hc.path),
        portIndex = None, // TODO: don't know how to define this
        gracePeriodSeconds = Some(hc.grace_period_seconds),
        intervalSeconds = Some(hc.interval_seconds),
        timeoutSeconds = Some(hc.timeout_seconds),
        maxConsecutiveFailures = Some(hc.max_consecutive_failures)
      )}} getOrElse Seq(),
      env = props.env getOrElse Map()
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
