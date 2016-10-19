package com.galacticfog.gestalt.meta.api

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.json.Reads._        // Custom validation helpers
import play.api.libs.functional.syntax._ // Combinator syntax

import scala.util.Try

case class ContainerSpec(name: String = "",
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
                         resource: Option[GestaltResourceInstance] = None,
                         external_id: Option[String] = None) extends Spec {

  def id: Option[UUID] = resource map (_.id)

  def created: Option[DateTime] = for {
    ri <- resource
    c <- ri.created
    ts <- c.get("timestamp")
    parsedTimestamp <- Try{DateTime.parse(ts)}.toOption
  } yield parsedTimestamp

}

case object ContainerSpec extends Spec {

  case class PortMapping(protocol: String,
                         container_port: Int,
                         host_port: Int = 0,
                         service_port: Int = 0,
                         name: Option[String] = None,
                         labels: Map[String,String] = Map())

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

  def fromResourceInstance(metaContainerSpec: GestaltResourceInstance): Try[ContainerSpec] = {
    for {
      props <- Try{metaContainerSpec.properties.get}
      ctype <- Try{props("container_type")}
      provider <- Try{props("provider")} map {json => Json.parse(json).as[ContainerSpec.InputProvider]}
      cpus <- Try{props("cpus").toDouble}
      memory <- Try{props("memory").toDouble}
      disk = props.get("disk").map(_.toDouble)
      num_instances <- Try{props("num_instances").toInt}
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
      container_type = ctype,
      image = image,
      provider = provider,
      port_mappings = port_mappings getOrElse Seq(),
      cpus = cpus,
      memory = memory,
      disk = disk getOrElse 0.0,
      num_instances = num_instances,
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
      resource = Some(metaContainerSpec),
      external_id = external_id
    )
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
    )(ContainerSpec.apply(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))

  implicit lazy val metaContainerSpec = Format(containerSpecReads, containerSpecWrites)

}
