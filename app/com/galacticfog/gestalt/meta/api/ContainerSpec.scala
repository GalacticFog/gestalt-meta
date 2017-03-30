package com.galacticfog.gestalt.meta.api

import java.util.UUID

import com.galacticfog.gestalt.data
import com.galacticfog.gestalt.data.{ResourceState, ResourceFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.security.api.errors.BadRequestException
import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.json.Reads._        // Custom validation helpers
import play.api.libs.functional.syntax._ // Combinator syntax

import scala.util.{Failure, Try}

case class ContainerSpec(name: String = "",
                         description: Option[String] = None,
                         container_type: String,
                         image: String,
                         provider: ContainerSpec.InputProvider,
                         port_mappings: Seq[ContainerSpec.PortMapping] = Seq(),
                         cpus: Double = 0.2,
                         memory: Double = 128.0,
                         disk: Double = 0.0,
                         num_instances: Int = 1,
                         network: Option[String] = None,
                         cmd: Option[String] = None,
                         constraints: Seq[String] = Seq(),
                         accepted_resource_roles: Option[Seq[String]] = None,
                         args: Option[Seq[String]] = None,
                         force_pull: Boolean = false,
                         health_checks: Seq[ContainerSpec.HealthCheck] = Seq(),
                         volumes: Seq[ContainerSpec.Volume] = Seq(),
                         labels: Map[String,String] = Map(),
                         env: Map[String,String] = Map(),
                         user: Option[String] = None,
                         external_id: Option[String] = None,
                         created: Option[DateTime] = None
                        ) extends Spec {

}

case object ContainerSpec extends Spec {

  case class ServiceAddress(host: String, port: Int, protocol: Option[String], virtual_hosts: Option[Seq[String]] = None)
  case object ServiceAddress {
    implicit val formatServiceAddress = Json.format[ServiceAddress]
  }  
  
  case class PortMapping(protocol: String,
                         container_port: Option[Int] = None,
                         host_port: Option[Int] = None,
                         service_port: Option[Int] = None,
                         name: Option[String] = None,
                         labels: Option[Map[String,String]] = None,
                         expose_endpoint: Option[Boolean] = None,
                         service_address: Option[ServiceAddress] = None,
                         virtual_hosts: Option[Seq[String]] = None)
                         
  case class Volume(container_path: String,
                    host_path: Option[String],
                    persistent: Option[Volume.PersistentVolumeInfo],
                    mode: String) {
    def isPersistent: Boolean = persistent.isDefined
  }

  case object Volume {

    case class PersistentVolumeInfo(size: Long)

  }

  case class InputProvider(id: UUID,
                           name: Option[String] = None,
                           locations: Option[Seq[String]] = None)

  // TODO: Marathon health checks require a port index, to specify which port the check is run against
  // ours aren't well defined without something similar, like a label
  case class HealthCheck(protocol: String,
                         path: String,
                         grace_period_seconds: Int = 300,
                         interval_seconds: Int = 60,
                         timeout_seconds: Int = 10,
                         max_consecutive_failures: Int = 3)
  
  def toResourcePrototype(spec: ContainerSpec, status: Option[String] = None): GestaltResourceInput = GestaltResourceInput(
    name = spec.name,
    resource_type = Some(ResourceIds.Container),
    description = spec.description,
    resource_state = None,
    properties = Some(
      Map[String,JsValue](
      "container_type" -> JsString(spec.container_type),
      "image" -> JsString(spec.image),
      "provider" -> Json.toJson(spec.provider),
      "cpus" -> JsNumber(spec.cpus),
      "memory" -> JsNumber(spec.memory),
      "disk" -> JsNumber(spec.disk),
      "num_instances" -> JsNumber(spec.num_instances),
      "port_mappings" -> Json.toJson(spec.port_mappings),
      "constraints" -> Json.toJson(spec.constraints),
      "force_pull" -> Json.toJson(spec.force_pull),
      "health_checks" -> Json.toJson(spec.health_checks),
      "volumes" -> Json.toJson(spec.volumes),
      "labels" -> Json.toJson(spec.labels),
      "env" -> Json.toJson(spec.env)
    ) ++ Seq[Option[(String,JsValue)]](
        spec.cmd map ("cmd" -> JsString(_)),
        spec.network map ("network" -> JsString(_)),
        spec.accepted_resource_roles map ("accepted_resource_roles" -> Json.toJson(_)),
        status map ("status" -> JsString(_)),
        spec.args map ("args" -> Json.toJson(_)),
        spec.user map ("user" -> Json.toJson(_))
    ).flatten.toMap)
  )

  def fromResourceInstance(metaContainerSpec: GestaltResourceInstance): Try[ContainerSpec] = {
    if (metaContainerSpec.typeId != ResourceIds.Container) return Failure(new RuntimeException("cannot convert non-Container resource into ContainerSpec"))
    println("***Loading Container from Resource...")
    val created: Option[DateTime] = for {
      c <- metaContainerSpec.created
      ts <- c.get("timestamp")
      parsedTimestamp <- Try{DateTime.parse(ts)}.toOption
    } yield parsedTimestamp

    
    val attempt = for {
      props <- Try{metaContainerSpec.properties.get}
      ctype <- Try{props("container_type")}
      provider <- Try{props("provider")} map {json => Json.parse(json).as[ContainerSpec.InputProvider]}
      cpus = props.get("cpus").flatMap( c => Try(c.toDouble).toOption )
      memory = props.get("memory").flatMap( c => Try(c.toDouble).toOption )
      disk = props.get("disk").flatMap( c => Try(c.toDouble).toOption )
      num_instances = props.get("num_instances").flatMap( c => Try(c.toInt).toOption )
      cmd = props.get("cmd")
      constraints = props.get("constraints") map {json => Json.parse(json).as[Seq[String]]}
      acceptedResourceRoles = props.get("accepted_resource_roles") map {json => Json.parse(json).as[Seq[String]]}
      args = props.get("args") map {json => Json.parse(json).as[Seq[String]]}
      health_checks = props.get("health_checks") map {json => Json.parse(json).as[Seq[ContainerSpec.HealthCheck]]}
      volumes = props.get("volumes") map {json => Json.parse(json).as[Seq[ContainerSpec.Volume]]}
      labels = props.get("labels") map {json => Json.parse(json).as[Map[String,String]]}
      env = props.get("env") map {json => Json.parse(json).as[Map[String,String]]}      
      
      port_mappings = props.get("port_mappings") map {json => Json.parse(json).as[Seq[ContainerSpec.PortMapping]]}
      user = props.get("user")
      image <- if (ctype.equalsIgnoreCase("DOCKER")) Try{props("image")} else Try("")
      network = props.get("network")
      force_pull = props.get("force_pull") map {_.toBoolean}
      external_id = props.get("external_id")
      
    } yield ContainerSpec(
      name = metaContainerSpec.name,
      description = metaContainerSpec.description,
      container_type = ctype,
      image = image,
      provider = provider,
      port_mappings = port_mappings getOrElse Seq(),
      cpus = cpus getOrElse 1.0,
      memory = memory getOrElse 128.0,
      disk = disk getOrElse 0.0,
      num_instances = num_instances getOrElse 1,
      network = network,
      cmd = cmd,
      constraints = constraints getOrElse Seq(),
      accepted_resource_roles = acceptedResourceRoles,
      args = args,
      force_pull = force_pull getOrElse false,
      health_checks = health_checks getOrElse Seq(),
      volumes = volumes getOrElse Seq(),
      labels = labels getOrElse Map(),
      env = env getOrElse Map(),
      user = user,
      external_id = external_id,
      created = created
    )
    println("***Finished conversion.")
    attempt.recoverWith {
      case e: Throwable => Failure(new RuntimeException(s"Could not convert GestaltResourceInstance into ContainerSpec: ${e.getMessage}"))
    }
  }

  implicit lazy val inputProviderFmt = Json.format[ContainerSpec.InputProvider]

  implicit lazy val metaHealthCheckFmt = Json.format[ContainerSpec.HealthCheck]

  implicit lazy val metaPersistentVolumeFmt = Json.format[ContainerSpec.Volume.PersistentVolumeInfo]

  implicit lazy val metaPortMappingSpecFmt = Json.format[ContainerSpec.PortMapping]

  implicit lazy val metaVolumeSpecFmt = Json.format[ContainerSpec.Volume]

  lazy val containerSpecWrites: Writes[ContainerSpec] = (
    (__ \ "container_type").write[String] and
      (__ \ "image").write[String] and
      (__ \ "provider").write[ContainerSpec.InputProvider] and
      (__ \ "port_mappings").write[Seq[ContainerSpec.PortMapping]] and
      (__ \ "cpus").write[Double] and
      (__ \ "memory").write[Double] and
      (__ \ "disk").write[Double] and
      (__ \ "num_instances").write[Int] and
      (__ \ "network").writeNullable[String] and
      (__ \ "cmd").writeNullable[String] and
      (__ \ "constraints").write[Seq[String]] and
      (__ \ "accepted_resource_roles").writeNullable[Seq[String]] and
      (__ \ "args").writeNullable[Seq[String]] and
      (__ \ "force_pull").write[Boolean] and
      (__ \ "health_checks").write[Seq[ContainerSpec.HealthCheck]] and
      (__ \ "volumes").write[Seq[ContainerSpec.Volume]] and
      (__ \ "labels").write[Map[String,String]] and
      (__ \ "env").write[Map[String,String]] and
      (__ \ "user").writeNullable[String]
    )( (c: ContainerSpec) => (
    c.container_type,
    c.image,
    c.provider,
    c.port_mappings,
    c.cpus,
    c.memory,
    c.disk,
    c.num_instances,
    c.network,
    c.cmd,
    c.constraints,
    c.accepted_resource_roles,
    c.args,
    c.force_pull,
    c.health_checks,
    c.volumes,
    c.labels,
    c.env,
    c.user
    )
  )

  lazy val containerSpecReads: Reads[ContainerSpec] = (
    ((__ \ "name").read[String] orElse Reads.pure[String]("")) and
      (__ \ "description").readNullable[String] and
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
    )(ContainerSpec.apply(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))

  implicit lazy val metaContainerSpec = Format(containerSpecReads, containerSpecWrites)

}
