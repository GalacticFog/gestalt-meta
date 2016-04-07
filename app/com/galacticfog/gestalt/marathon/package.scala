package com.galacticfog.gestalt
 
import java.util.UUID
import scala.util.{Try,Success,Failure}
import com.galacticfog.gestalt.meta.api.sdk._
import play.api.libs.json._
  
package object marathon {

  
  case class Volume(
      container_path: String,
      host_path: String,
      mode: String)
      
  case class HealthCheck(
      protocol: String,
      path: String,
      grace_period_seconds: Int = 300,
      interval_seconds: Int = 60,
      timeout_seconds: Int = 10,
      max_consecutive_failures: Int = 3)
  
  case class InputProvider(
      id: UUID,
      locations: Option[Iterable[String]] = None)

  case class PortMapping(
      protocol: String,
      container_port: Int, 
      host_port: Int = 0, 
      service_port: Int = 0,
      lable: Option[String] = None)
      
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
      
  case class KeyValuePair(key: String, value: String)
  
  case class MarathonDocker(
      image: String,
      network: String = "BRIDGED",
      forcePullImage: Boolean = false,
      portMappings: Iterable[PortMapping] = Seq(),
      parameters: Option[Iterable[KeyValuePair]] = None)

  case class MarathonContainer(
      docker: MarathonDocker,
      volumes: Option[Iterable[Volume]] = None
  )
  
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
    healthChecks: Option[Iterable[HealthCheck]] = None,
    env: Option[Map[String, String]] = None)
    
  implicit lazy val inputProviderFormat = Json.format[InputProvider]
  implicit lazy val volumeFormat = Json.format[Volume]
  implicit lazy val healthCheckFormat = Json.format[HealthCheck]
  implicit lazy val portMappingFormat = Json.format[PortMapping]
  implicit lazy val inputContainerPropertiesFormat = Json.format[InputContainerProperties]
  
  
  implicit lazy val keyValuePairFormat = Json.format[KeyValuePair]
  implicit lazy val marathonDockerFormat = Json.format[MarathonDocker]
  implicit lazy val marathonContainerFormat = Json.format[MarathonContainer]
  implicit lazy val marathonAppFormat = Json.format[MarathonApp]  
  
  
  def toMarathonApp(name: String, inputJson: JsObject /*props: InputContainerProperties*/) = {
    
    val props = (inputJson \ "properties").validate[InputContainerProperties].map {
      case ps: InputContainerProperties => ps
    }.recoverTotal { e =>
      throw new IllegalArgumentException(
          "Could not parse container properties: " + JsError.toFlatJson(e).toString)
    }
    
    def portmap(ps: Iterable[PortMapping]): Iterable[PortMapping] = {
      ps map { _.copy(lable = None) }
    }
    
    def appPorts(ps: Iterable[PortMapping]): Iterable[Int] = {
      val output = ps map { _.service_port }
      if (output exists { _ > 0 }) output else Seq(0)
    }
    
    val docker = MarathonDocker(
        image = props.image,
        network = props.network,
        forcePullImage = props.force_pull,
        portMappings = portmap(props.port_mappings))
        
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
       ports = appPorts(props.port_mappings),
       labels = props.labels,
       healthChecks = props.health_checks,
       env = props.env)
  }  
  
  def containerWithDefaults(json: JsValue) = {
    val ctype = (json \ "properties" \ "container_type") match {
      case u: JsUndefined => throw new IllegalArgumentException(s"'container_type' is missing.")
      case v => v.as[String]
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
    InputContainerProperties(ctype, image, prv)
  }
  
  def requiredJsString(name: String, value: JsValue) = value match {
    case u: JsUndefined => throw new IllegalArgumentException(s"'$name' is missing.")
    case v => v.as[String]
  }
  
  protected def safeGetInputJson(json: JsValue): Try[GestaltResourceInput] = Try {
    implicit def jsarray2str(arr: JsArray) = arr.toString
    json.validate[GestaltResourceInput].map {
      case resource: GestaltResourceInput => resource
    }.recoverTotal { e => 
      throw new RuntimeException((JsError.toFlatJson(e).toString))
    }
  }
  
  def containerIntake(json: JsValue): Try[GestaltResourceInput] = {
    val defaults = containerWithDefaults(json)
    val name = requiredJsString("name", (json \ "name"))
    val app = toMarathonApp(name, json.as[JsObject])

    val newprops = (json \ "properties").as[JsObject] ++ (Json.toJson(defaults).as[JsObject])
    safeGetInputJson {  
      json.as[JsObject] ++ Json.obj("properties" -> newprops)
    }
  }
  
  
}