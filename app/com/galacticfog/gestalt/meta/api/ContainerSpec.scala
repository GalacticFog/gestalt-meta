package com.galacticfog.gestalt.meta.api

import java.util.UUID

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
                                    volume_resource: VolumeSpec ) extends VolumeMountSpec
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

  val volumeSpecJsonOutputTransformer = (
    (__ \ 'name).json.copyFrom( (__ \ 'name).json.pick ) and
    (__ \ 'description).json.copyFrom( (__ \ 'description).readNullable[JsValue].map(_.getOrElse(JsNull)) ) and
    (__ \ 'properties).json.copyFrom(
      (__ \ 'name).json.prune andThen (__ \ 'description).json.prune
    )
  ) reduce
  val volumeSpecJsonInputTransformer = (
    __.json.copyFrom( (__ \ 'properties).json.pick ) and
    (__ \ 'name).json.copyFrom( (__ \ 'name).json.pick ) and
    (__ \ 'description).json.copyFrom( (__ \ 'description).readNullable[JsValue].map(_.getOrElse(JsNull)) )
    reduce
  )

  implicit val metaPortMappingSpecWrites = Json.writes[ContainerSpec.PortMapping]
  implicit val existingVMSFmt = Json.format[ContainerSpec.ExistingVolumeMountSpec]
  val naturalVolSpec = Json.format[VolumeSpec]
  implicit val volSpecRds = Reads[VolumeSpec] (json => json.transform(volumeSpecJsonInputTransformer).flatMap(naturalVolSpec.reads))
  implicit val volSpecWrts = Writes[VolumeSpec] {
    (spec: VolumeSpec) => naturalVolSpec.writes(spec).transform(volumeSpecJsonOutputTransformer).get
  }
  implicit val inlineVMSFmt = Json.format[ContainerSpec.InlineVolumeMountSpec]

  implicit val metaVMCSpecRds =
    Json.reads[ContainerSpec.ExistingVolumeMountSpec].map( vms => vms: ContainerSpec.VolumeMountSpec ) |
      Json.reads[ContainerSpec.InlineVolumeMountSpec].map( vms => vms: ContainerSpec.VolumeMountSpec)
  implicit val metaVMCSpecWrts = Writes[ContainerSpec.VolumeMountSpec] {
    case evmc : ContainerSpec.ExistingVolumeMountSpec => Json.toJson(evmc)
    case ivmc : ContainerSpec.InlineVolumeMountSpec   => Json.toJson(ivmc)
  }

  val containerSpecWrites: Writes[ContainerSpec] = (
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
      (__ \ "volumes").write[Seq[ContainerSpec.VolumeMountSpec]] and
      (__ \ "secrets").write[Seq[ContainerSpec.SecretMount]] and
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
    c.secrets,
    c.labels,
    c.env,
    c.user
    )
  )

  val containerSpecReads: Reads[ContainerSpec] = (
    ((__ \ "name").read[String] orElse Reads.pure[String]("")) and
      (__ \ "description").readNullable[String] and
      (__ \ "container_type").read[String] and
      (__ \ "image").read[String] and
      (__ \ "provider").read[ContainerSpec.InputProvider] and
      ((__ \ "port_mappings").read[Seq[ContainerSpec.PortMapping]] orElse Reads.pure(Seq.empty)) and
      ((__ \ "cpus").read[Double] orElse Reads.pure(0.2)) and
      ((__ \ "memory").read[Double] orElse Reads.pure(128.0)) and
      ((__ \ "disk").read[Double] orElse Reads.pure(0.0)) and
      ((__ \ "num_instances").read[Int] orElse Reads.pure(1)) and
      (__ \ "network").readNullable[String] and
      (__ \ "cmd").readNullable[String] and
      ((__ \ "constraints").read[Seq[String]] orElse Reads.pure(Seq.empty)) and
      (__ \ "accepted_resource_roles").readNullable[Seq[String]] and
      (__ \ "args").readNullable[Seq[String]] and
      ((__ \ "force_pull").read[Boolean] orElse Reads.pure(false)) and
      ((__ \ "health_checks").read[Seq[ContainerSpec.HealthCheck]] orElse Reads.pure(Seq.empty)) and
      ((__ \ "volumes").read[Seq[ContainerSpec.VolumeMountSpec]] orElse Reads.pure(Seq.empty)) and
      ((__ \ "labels").read[Map[String,String]] orElse Reads.pure(Map.empty[String,String])) and
      ((__ \ "env").read[Map[String,String]] orElse Reads.pure(Map.empty[String,String])) and
      (__ \ "user").readNullable[String] and
      ((__ \ "secrets").read[Seq[SecretMount]] orElse Reads.pure(Seq.empty))
    )(ContainerSpec.apply(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,None,None,_))

  implicit val metaContainerSpec = Format(containerSpecReads, containerSpecWrites)

}








