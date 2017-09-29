package services


import scala.language.implicitConversions
import scala.language.postfixOps

import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.errors.UnprocessableEntityException

import skuber._


case class KubeVolume(v: ContainerSpec.Volume) {

  val name = v.name getOrElse {
    unprocessable("Malformed volume specification. Must supply 'name'")        
  }
  val mount_path = v.container_path
  val access_mode = v.mode

  def asVolumeMount(): Volume.Mount = Volume.Mount(name, mount_path)
  
  def asVolumeClaim(namespace: Option[String] = None): PersistentVolumeClaim = {

    val accessModes = resolveAccessMode(access_mode)
    val capacity    = storageQuantity(v.persistent.get.size)
    val annot       = Map("volume.beta.kubernetes.io/storage-class" -> name)
    
    val metaData = {
      val nm = namespace getOrElse "default"
      ObjectMeta(name = name, namespace = nm, annotations = annot) 
    }
    
    PersistentVolumeClaim(
        metadata = metaData,
        spec = Some(PersistentVolumeClaim.Spec(
            accessModes = List(accessModes),
            resources = Some(Resource.Requirements(requests = Map("storage" -> capacity))))))
  }
  
  private def storageQuantity(n: Long): Resource.Quantity = {
    new Resource.Quantity("%sMi".format(n.toString))
  }
  
  private def resolveAccessMode(mode: Option[String]): PersistentVolume.AccessMode.AccessMode = {
    if (mode.isEmpty) unprocessable("You must supply a value for 'volume.mount'")
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
