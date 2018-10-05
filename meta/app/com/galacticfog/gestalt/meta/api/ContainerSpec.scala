package com.galacticfog.gestalt.meta.api

import java.util.UUID

import ai.x.play.json.Jsonx
import ai.x.play.json.implicits.optionWithNull
import com.galacticfog.gestalt.util.Helpers.JodaJsonFormats._
import com.galacticfog.gestalt.data.models.ResourceLike
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk._
import org.joda.time.DateTime
import play.api.Logger
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.language.{implicitConversions, postfixOps}
import scala.util.{Failure, Success, Try}

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
                         volumes: Seq[ContainerSpec.VolumeMountSpec] = Seq(),
                         labels: Map[String,String] = Map(),
                         env: Map[String,String] = Map(),
                         user: Option[String] = None,
                         external_id: Option[String] = None,
                         created: Option[DateTime] = None,
                         secrets: Seq[ContainerSpec.SecretMount] = Seq.empty
                        ) extends Spec {

}

case object ContainerSpec extends Spec {

  val log = Logger(this.getClass)

  case class ServiceAddress(host: String, port: Int, protocol: Option[String], virtual_hosts: Option[Seq[String]] = None)
  case object ServiceAddress {
    implicit val formatServiceAddress = Json.format[ServiceAddress]
  }

  sealed trait SecretMount {
    def secret_id: UUID
    def path: String
    def mount_type: String
  }

  sealed trait VolumeSecretMount {
    def path: String
  }
  case class SecretEnvMount(secret_id: UUID, path: String, secret_key: String) extends SecretMount {
    override def mount_type: String = "env"
  }
  case class SecretFileMount(secret_id: UUID, path: String, secret_key: String) extends SecretMount with VolumeSecretMount {
    override def mount_type: String = "file"
  }
  case class SecretDirMount(secret_id: UUID, path: String) extends SecretMount with VolumeSecretMount {
    override def mount_type: String = "directory"
  }

  case class PortMapping(protocol: String,
                         container_port: Option[Int] = None,
                         host_port: Option[Int] = None,
                         service_port: Option[Int] = None,
                         name: Option[String] = None,
                         labels: Option[Map[String,String]] = None,
                         expose_endpoint: Option[Boolean] = None,
                         service_address: Option[ServiceAddress] = None,
                         virtual_hosts: Option[Seq[String]] = None,
                         lb_port: Option[Int] = None,
                         `type`: Option[String] = None,
                         lb_address: Option[ServiceAddress] = None )

  sealed trait VolumeMountSpec {
    def mount_path: String
  }

  case class InlineVolumeMountSpec( mount_path: String,
                                    volume_resource: GestaltResourceInput ) extends VolumeMountSpec
  case class ExistingVolumeMountSpec( mount_path: String,
                                      volume_id: UUID ) extends VolumeMountSpec

  case object Volume {
  
    case class PersistentVolumeInfo(size: Long)
  
  }
  
  case class InputProvider(id: UUID,
                           name: Option[String] = None,
                           locations: Option[Seq[String]] = None)
                           
  // ours aren't well defined without something similar, like a label
  case class HealthCheck( protocol: String,
                          path: Option[String] = None,
                          command: Option[String] = None,
                          grace_period_seconds: Int = 300,
                          interval_seconds: Int = 60,
                          timeout_seconds: Int = 10,
                          max_consecutive_failures: Int = 3,
                          port_index: Option[Int] = None,
                          port: Option[Int] = None
                        )

  case object HealthCheck {
    sealed trait Protocol
    case object HTTP      extends Protocol
    case object HTTPS     extends Protocol
    case object TCP       extends Protocol
    case object COMMAND   extends Protocol

    case object Protocol {
      def all: Seq[Protocol] = Seq(HTTP, HTTPS, TCP, COMMAND)
      implicit def parse(proto: String): Try[Protocol] = all.find(_.toString.equalsIgnoreCase(proto)) match {
        case Some(p) =>
          Success(p)
        case err =>
          Failure(new BadRequestException(s"HealthCheck protocol '${err}' invalid, must be one of 'HTTP', 'HTTPS', 'TCP' or 'COMMAND'"))
      }

      implicit def fromString(proto: String): Protocol = parse(proto).get

      implicit def toString(proto: Protocol): String = proto.toString

    }
  }
  
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
      "secrets" -> Json.toJson(spec.secrets),
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


  def fromResourceInstance(metaContainer: ResourceLike): Try[ContainerSpec] = {
    if (metaContainer.typeId != ResourceIds.Container) return Failure(new RuntimeException("cannot convert non-Container resource into ContainerSpec"))
    log.debug(s"loading Container from Resource ${metaContainer.id}")
    val created: Option[DateTime] = for {
      c <- metaContainer.created
      ts <- c.get("timestamp")
      parsedTimestamp <- Try{DateTime.parse(ts)}.toOption
    } yield parsedTimestamp

    val attempt = for {
      props <- Try{metaContainer.properties.get}
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
      volumes = props.get("volumes") map {json => Json.parse(json).as[Seq[ContainerSpec.VolumeMountSpec]]}
      secrets = props.get("secrets") map {json => Json.parse(json).as[Seq[ContainerSpec.SecretMount]]}
      labels = props.get("labels") map {json => Json.parse(json).as[Map[String,String]]}
      env = props.get("env") map {json => Json.parse(json).as[Map[String,String]]}

      port_mappings = props.get("port_mappings") map {json => Json.parse(json).as[Seq[ContainerSpec.PortMapping]]}

      user = props.get("user")
      image <- if (ctype.equalsIgnoreCase("DOCKER")) Try{props("image")} else Try("")
      network = props.get("network")
      force_pull = props.get("force_pull") map {_.toBoolean}
      external_id = props.get("external_id")

    } yield ContainerSpec(
      name = metaContainer.name,
      description = metaContainer.description,
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
      secrets = secrets getOrElse Seq(),
      user = user,
      external_id = external_id,
      created = created
    )
    log.debug("finished conversion")
    attempt.recoverWith {
      case e: Throwable => Failure(new RuntimeException(s"Could not convert GestaltResourceInstance into ContainerSpec: ${e.getMessage}"))
    }
  }

  implicit val inputProviderFmt = Json.format[ContainerSpec.InputProvider]

  val secretDirMountReads = Json.reads[SecretDirMount]
  val secretFileMountReads = Json.reads[SecretFileMount]
  val secretEnvMountReads = Json.reads[SecretEnvMount]

  val secretDirMountWrites = Json.writes[SecretDirMount]
  val secretFileMountWrites = Json.writes[SecretFileMount]
  val secretEnvMountWrites = Json.writes[SecretEnvMount]

  implicit val secretMountReads = new Reads[SecretMount] {
    override def reads(json: JsValue): JsResult[SecretMount] = (json \ "mount_type").asOpt[String] match {
      case Some("directory") => secretDirMountReads.reads(json)
      case Some("file")      => secretFileMountReads.reads(json)
      case Some("env")       => secretEnvMountReads.reads(json)
      case _                 => JsError(__ \ "mount_type", "must be one of 'directory', 'file' or 'env'")
    }
  }

  implicit val secretMountWrites = new Writes[SecretMount] {
    override def writes(sm: SecretMount): JsValue = (sm match {
      case env: SecretEnvMount   => secretEnvMountWrites.writes(env)
      case file: SecretFileMount => secretFileMountWrites.writes(file)
      case dir: SecretDirMount   => secretDirMountWrites.writes(dir)
    }) ++ Json.obj("mount_type" -> sm.mount_type)
  }

  implicit val metaHealthCheckFmt = Json.format[ContainerSpec.HealthCheck]

  implicit val metaPersistentVolumeFmt = Json.format[ContainerSpec.Volume.PersistentVolumeInfo]

  implicit val metaPortMappingSpecReads: Reads[ContainerSpec.PortMapping] = (
    (__ \ "protocol").read[String] and
      (__ \ "container_port").readNullable[Int](max(65535) andKeep min(1)) and
      (__ \ "host_port").readNullable[Int](max(65535) andKeep min(0)) and
      (__ \ "service_port").readNullable[Int](max(65535) andKeep min(0)) and
      (__ \ "name").readNullable[String](pattern("^[a-z0-9]([a-z0-9-]*[a-z0-9])*$".r, "error.iana_svc_name") andKeep maxLength(15)) and
      (__ \ "labels").readNullable[Map[String,String]] and
      (__ \ "expose_endpoint").readNullable[Boolean] and
      (__ \ "service_address").readNullable[ServiceAddress] and
      (__ \ "virtual_hosts").readNullable[Seq[String]] and
      (__ \ "lb_port").readNullable[Int](max(65535) andKeep min(0)) and
      (__ \ "type").readNullable[String](verifying(Set("internal","external","loadBalancer").contains(_))) and
      (__ \ "lb_address").readNullable[ServiceAddress]
    )(ContainerSpec.PortMapping.apply _)

  implicit val metaPortMappingSpecWrites = Json.writes[ContainerSpec.PortMapping]
  implicit val existingVMSFmt = Json.format[ContainerSpec.ExistingVolumeMountSpec]
  implicit val inlineVMSRds = Json.format[ContainerSpec.InlineVolumeMountSpec]

  // check for ExistingVolumeMountSpec first!
  implicit val metaVMCSpecRds =
    Json.reads[ContainerSpec.ExistingVolumeMountSpec].map( vms => vms: ContainerSpec.VolumeMountSpec ) |
      Json.reads[ContainerSpec.InlineVolumeMountSpec].map( vms => vms: ContainerSpec.VolumeMountSpec)
  implicit val metaVMCSpecWrts = Writes[ContainerSpec.VolumeMountSpec] {
    case evmc : ContainerSpec.ExistingVolumeMountSpec => Json.toJson(evmc)
    case ivmc : ContainerSpec.InlineVolumeMountSpec   => Json.toJson(ivmc)
  }

  val containerSpecJsonFormat = Jsonx.formatCaseClassUseDefaults[ContainerSpec]

  implicit val containerSpecWrites = new Writes[ContainerSpec] {
    override def writes(containerSpec: ContainerSpec): JsValue = {
      containerSpecJsonFormat.writes(containerSpec) - "name"
    }
  }

  implicit val containerSpecReads = new Reads[ContainerSpec] {
    override def reads(json: JsValue): JsResult[ContainerSpec] = {
      containerSpecJsonFormat.reads(json)
    }
  }

}








