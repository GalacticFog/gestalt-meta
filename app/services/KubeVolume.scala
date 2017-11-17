package services


import scala.language.implicitConversions
import scala.language.postfixOps

import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.errors.UnprocessableEntityException

import skuber._


case class KubeVolume(v: ContainerSpec.Volume) {

  import KubeVolume._

  val name = v.name getOrElse {
    unprocessable("Malformed volume specification. Must supply 'name'")
  }

  def asVolume(): Volume = v match {
    case ContainerSpec.Volume(containerPath, Some(hostPath), None, maybeMode, Some(name)) =>
      Volume(
        name = name,
        source = Volume.HostPath(hostPath)
      )
    case ContainerSpec.Volume(containerPath, None, Some(persistentVolumeInfo), maybeMode, Some(name)) =>
      Volume(
        name = name,
        source = Volume.PersistentVolumeClaimRef(
          claimName = name,
          readOnly = resolveAccessMode(maybeMode) == PersistentVolume.AccessMode.ReadOnlyMany
        )
      )
    case _ =>
      unprocessable("Could not resolve ContainerSpec.Volume to an appropriate volume mount.")
  }

  def asVolumeMount(): Volume.Mount = v match {
    case ContainerSpec.Volume(containerPath, Some(_), None, maybeMode, Some(name)) =>
      Volume.Mount(
        name = name,
        mountPath = containerPath,
        readOnly = !maybeMode.contains("RW")
      )
    case ContainerSpec.Volume(containerPath, None, Some(persistentVolumeInfo), maybeMode, Some(name)) =>
      Volume.Mount(
        name = name,
        mountPath = containerPath,
        readOnly = resolveAccessMode(maybeMode) == PersistentVolume.AccessMode.ReadOnlyMany
      )
    case _ =>
      unprocessable("Could not resolve ContainerSpec.Volume to an appropriate volume mount.")
  }

  def asVolumeClaim(namespace: String): Option[PersistentVolumeClaim] = {
    val metaData = {
      ObjectMeta(
        name = name,
        namespace = namespace,
        annotations = Map(
          "volume.beta.kubernetes.io/storage-class" -> name
        )
      )
    }

    for {
      p <- v.persistent
      accessModes = resolveAccessMode(v.mode)
      capacity    = storageQuantity(p.size)
    } yield PersistentVolumeClaim(
      metadata = metaData,
      spec = Some(PersistentVolumeClaim.Spec(
        accessModes = List(accessModes),
        resources = Some(Resource.Requirements(requests = Map("storage" -> capacity))))
      )
    )
  }

  private def storageQuantity(n: Long): Resource.Quantity = {
    new Resource.Quantity("%sMi".format(n.toString))
  }

}

object KubeVolume {
  def resolveAccessMode(mode: Option[String]): PersistentVolume.AccessMode.AccessMode = {
    if (mode.isEmpty) unprocessable("You must supply a value for 'volume.mode'")
    else {
      val m = mode flatMap { s =>
        PersistentVolume.AccessMode.values find { _.toString.equalsIgnoreCase(s) }
      }
      m getOrElse {
        unprocessable(s"Invalid 'volume.mode'. found : '${mode.get}'")
      }
    }
  }

  def unprocessable(message: String) = throw UnprocessableEntityException(message)
}
