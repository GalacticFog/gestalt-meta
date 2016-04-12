package com.galacticfog.gestalt
 
import java.util.UUID
import scala.util.{Try,Success,Failure}
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import play.api.libs.json._
import play.api.{ Logger => log }

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
      docker: MarathonDocker, // TODO: this is actually optional, only necessary if the container type is DOCKER
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
    mem: Int = 128,
    instances: Int = 1,
    cmd: Option[String] = None,
    args: Option[Iterable[String]] = None,
    ports: Iterable[Int] = Seq(0),
    labels: Option[Map[String, String]] = None,
    healthChecks: Option[Iterable[MarathonHealthCheck]] = None,
    env: Option[Map[String, String]] = None)
    
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
  implicit lazy val marathonContainerFormat = Json.format[MarathonContainer]
  implicit lazy val marathonAppFormat = Json.format[MarathonApp]  
  
  import com.galacticfog.gestalt.data.ResourceFactory
  
  
  /**
   * Convert Marathon App JSON to Meta Container JSON
   */
  def marathonApp2MetaContainer(inputJson: JsObject, providerId: UUID) = {
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
    
    // TODO: Add 'containerType' to Marathon object, remove "DOCKER" constant.
    val props = InputContainerProperties(
      container_type = "DOCKER",
      image = app.container.docker.image,
      provider = provider,
      port_mappings = app.container.docker.portMappings.map {_.zipWithIndex.map { case (pm,index) => PortMapping(
        label = Some(index.toString),
        protocol = pm.protocol getOrElse "tcp",
        container_port = pm.containerPort,
        host_port = pm.hostPort getOrElse 0,
        service_port = pm.servicePort getOrElse 0
      )}} getOrElse Seq(),
      cpus = app.cpus,
      memory = app.mem,
      num_instances = app.instances,
      network = app.container.docker.network,
      cmd = app.cmd,
      args = app.args,
      force_pull = app.container.docker.forcePullImage getOrElse false,
      health_checks = app.healthChecks map { _.map {check => HealthCheck(
        protocol = check.protocol getOrElse "http",
        path = check.path getOrElse "/",
        grace_period_seconds = check.gracePeriodSeconds getOrElse 15,
        interval_seconds = check.intervalSeconds getOrElse 10,
        timeout_seconds = check.timeoutSeconds getOrElse 20,
        max_consecutive_failures = check.maxConsecutiveFailures getOrElse 3
      )}},
      volumes = app.container.volumes,
      labels = app.labels,
      env = app.env)
    
    // Meta container name is last component of Marathon ID 'path'
    val containerName = {
        val cmps = app.id.stripPrefix("/").stripSuffix("/").split("/")
        cmps(cmps.size-1)
    }
    log.debug("Container-Name : " + containerName)
    
    val output = Json.toJson(InputContainer(name = containerName, properties = props))
    
    log.debug("Transform Complete:\n" + Json.prettyPrint(output))
    output
  }
  
  
  /**
   * Convert Meta Container JSON to Marathon App object.
   */
  def toMarathonApp(name: String, inputJson: JsObject) = {
    
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

    val docker = MarathonDocker(
        image = props.image,
        network = props.network,
        forcePullImage = Some(props.force_pull),
        portMappings = Some(portmap(props.port_mappings))
    )
        
    val container = MarathonContainer(
        docker = docker,
        volumes = props.volumes)

    MarathonApp(
      id = name,
      container = container,
      cpus = props.cpus,
      mem = props.memory,
      instances = props.num_instances,
      cmd = props.cmd,
      args = props.args,
      ports = props.port_mappings map { _.service_port },
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
      env = props.env
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