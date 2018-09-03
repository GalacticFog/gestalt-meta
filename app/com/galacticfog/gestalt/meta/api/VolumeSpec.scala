package com.galacticfog.gestalt.meta.api

import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.GestaltResourceInput
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

case class VolumeSpec(name: String = "",
                      description: Option[String] = None,
                      provider: ContainerSpec.InputProvider,
                      `type`: VolumeSpec.Type,
                      config: JsValue,
                      size: Int,
                      access_mode: VolumeSpec.AccessMode,
                      reclamation_policy: Option[String] = None,
                      external_id: Option[String] = None,
                      mount_path: Option[String] = None
                     ) extends Spec {

  import VolumeSpec._

  def parseConfig: JsResult[VolumeConfig] = {
    `type` match {
      case Persistent => JsSuccess(PersistentVolume)
      case HostPath   => config.validate[HostPathVolume](hostPathVolumeFmt)
      case Dynamic    => config.validate[DynamicVolume](dynamicVolumeFmt)
      case External   => config.validate[ExternalVolume](externalVolumeRds)
    }
  }

}

case object VolumeSpec {

  sealed trait AccessMode
  case object ReadWriteOnce extends AccessMode
  case object ReadWriteMany extends AccessMode
  case object ReadOnlyMany  extends AccessMode
  object AccessMode {
    val values = Seq(ReadWriteOnce, ReadWriteMany, ReadOnlyMany)
    def fromString(s: String) =
      values.find(_.toString == s).map(Success(_)).getOrElse(
        Failure[AccessMode](new BadRequestException(s"Volume Access Mode type must be one of ${values.map("'" + _.toString + "'").mkString(", ")}"))
      )

    implicit val volumeAccessModeRds = Reads[AccessMode] {
      _.validate[String].map(AccessMode.fromString).flatMap{_ match {
        case Success(vam) => JsSuccess(vam)
        case Failure(err) => JsError(err.getMessage)
      }}
    }

    implicit val volumeAccessModeWrts = Writes[AccessMode] { vam => JsString(vam.toString) }
  }

  implicit def vam2skubervam(mode: AccessMode): skuber.PersistentVolume.AccessMode.AccessMode = skuber.PersistentVolume.AccessMode.withName(mode.toString)

  sealed trait Type {
    def label: String
  }
  case object Persistent extends Type {val label = "persistent"}
  case object HostPath extends Type {val label = "host_path"}
  case object External extends Type {val label = "external"}
  case object Dynamic extends Type {val label = "dynamic"}
  object Type {
    val values = Seq(Persistent,HostPath,External,Dynamic)
    def fromString(s: String) =
      values.find(_.label == s).map(Success(_)).getOrElse(
        Failure[Type](new BadRequestException(s"/properties/type was '$s', must be one of ${values.map("'" + _.label + "'").mkString(", ")}"))
      )
  }

  implicit val volumeTypeReads = Reads[VolumeSpec.Type] {
    _.validate[String].map(VolumeSpec.Type.fromString).flatMap(_ match {
      case Success(t) => JsSuccess(t)
      case Failure(f) => JsError(f.getMessage)
    })
  }

  implicit val volumeTypeWrites = Writes[VolumeSpec.Type] { t => Json.toJson(t.label) }

  implicit val volumeSpecFmt = Json.format[VolumeSpec]

  sealed trait VolumeConfig
  case object PersistentVolume extends VolumeConfig
  case class HostPathVolume(host_path: String) extends VolumeConfig
  case class DynamicVolume(storage_class: String) extends VolumeConfig
  case class ExternalVolume(config: JsValue) extends VolumeConfig

  implicit val hostPathVolumeFmt = Json.format[HostPathVolume]
  implicit val dynamicVolumeFmt = Json.format[DynamicVolume]
  implicit val externalVolumeRds = JsPath.read[JsValue].map(ExternalVolume.apply)
  implicit val externalVolumeWrts = Writes[ExternalVolume] { v: ExternalVolume => v.config }

  def toResourcePrototype(spec: VolumeSpec): GestaltResourceInput = GestaltResourceInput(
    name = spec.name,
    resource_type = Some(migrations.V13.VOLUME_TYPE_ID),
    description = spec.description,
    resource_state = None,
    properties = Some(Map[String,JsValue](
      "provider" -> Json.toJson(spec.provider),
      "type" -> JsString(spec.`type`.label),
      "config" -> spec.config,
      "size" -> JsNumber(spec.size),
      "access_mode" -> JsString(spec.access_mode.toString)
    ) ++ Seq[Option[(String,JsValue)]](
      spec.reclamation_policy map ("reclamation_policy" -> JsString(_)),
      spec.external_id map ("external_id" -> JsString(_))
    ).flatten.toMap)
  )

  def fromResourceInstance(metaVolumeSpec: ResourceLike): Try[VolumeSpec] = {
    if (metaVolumeSpec.typeId != migrations.V13.VOLUME_TYPE_ID) return Failure(new RuntimeException("cannot convert non-Volume resource into VolumeSpec"))
    for {
      props <- Try{metaVolumeSpec.properties.get}
      provider <- Try{props("provider")} map {json => Json.parse(json).as[ContainerSpec.InputProvider]}
      tpe <- Try{props("type")}.flatMap(Type.fromString)
      mode <- Try{props("access_mode")}.flatMap(AccessMode.fromString)
      size <- Try{props("size").toInt}
      config <- Try{Json.parse(props("config"))}
    } yield VolumeSpec(
      name = metaVolumeSpec.name,
      description = metaVolumeSpec.description,
      provider = provider,
      `type` = tpe,
      config = config,
      reclamation_policy = props.get("reclamation_policy"),
      external_id = props.get("external_id"),
      access_mode = mode,
      size = size
    )
  }

}