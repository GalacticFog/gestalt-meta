package services.kubernetes

// import java.util.UUID
import play.api.libs.json._
import play.api.test.PlaySpecification
import org.specs2.specification.{BeforeAfterEach, BeforeAll}
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.meta.api.VolumeSpec
import com.galacticfog.gestalt.util.Error

class MkKubernetesSpecSpec extends PlaySpecification with ResourceScope with BeforeAll with BeforeAfterEach {
  override def beforeAll(): Unit = pristineDatabase()

  override def before: Unit = scalikejdbc.config.DBs.setupAll()

  override def after: Unit = scalikejdbc.config.DBs.closeAll()

  sequential

  lazy val testK8SProvider = newInstance(
    typeId = ResourceIds.KubeProvider,
    name = "test-provider",
    properties = Option(Map(
      "parent" -> "{}",
      "config" -> Json.obj(
        "host_volume_whitelist" -> Json.arr("/supported-path/sub-path"),
        "storage_classes" -> Json.arr("storage-class-1")
      ).toString
    ))
  )

  "MkKubernetesSpec#mkPVCandPV" should {
    "create host_path PV&PVC" in {
      val mks = new MkKubernetesSpec { }
      val volume = newInstance(
        migrations.V13.VOLUME_TYPE_ID,
        "test-volume",
        properties = Some(Map(
          "type" -> "host_path",
          "size" -> "1000",
          "access_mode" -> "ReadWriteOnce",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "config" -> Json.toJson(VolumeSpec.HostPathVolume("/supported-path/sub-path")).toString
        ))
      )
      val Right(res) = mks.mkPVCandPV(testK8SProvider, volume)
      val (pvc, pv) = res
      pv === Some(skuber.PersistentVolume(
        metadata = skuber.ObjectMeta(
          name = s"test-vol-${volume.id}",
          labels = Map("meta/volume" -> s"${volume.id}")
        ),
        spec = Some(skuber.PersistentVolume.Spec(
          capacity = Map(skuber.Resource.storage -> skuber.Resource.Quantity(s"1000Mi")),
          source = skuber.Volume.HostPath("/supported-path/sub-path"),
          accessModes = List(skuber.PersistentVolume.AccessMode.ReadWriteOnce),
          claimRef = Some(skuber.ObjectReference(
            namespace = "__SET_ME__",
            name = "test-volume"
          ))
        ))
      ))
      pvc === skuber.PersistentVolumeClaim(
        metadata = skuber.ObjectMeta(
          name = s"test-volume",
          namespace = "__SET_ME__",
          labels = Map("meta/volume" -> s"${volume.id}")
        ),
        spec = Some(skuber.PersistentVolumeClaim.Spec(
          resources = Some(skuber.Resource.Requirements(
            requests = Map(
              skuber.Resource.storage -> s"1000Mi"
            )
          )),
          accessModes = List(skuber.PersistentVolume.AccessMode.ReadWriteOnce),
          storageClassName = None
        ))
      )
    }

    "not create host_path PV&PVC if host path is not in whitelist" in {
      val mks = new MkKubernetesSpec { }
      val volume = newInstance(
        migrations.V13.VOLUME_TYPE_ID,
        "test-volume",
        properties = Some(Map(
          "type" -> "host_path",
          "size" -> "1000",
          "access_mode" -> "ReadWriteOnce",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "config" -> Json.toJson(VolumeSpec.HostPathVolume("/test")).toString
        ))
      )
      val res = mks.mkPVCandPV(testK8SProvider, volume)
      res === Left(Error.UnprocessableEntity("host_path '/test' is not in provider's white-list"))
    }

    "not create persistent PV&PVC" in {
      val mks = new MkKubernetesSpec { }
      val volume = newInstance(
        migrations.V13.VOLUME_TYPE_ID,
        "test-volume",
        properties = Some(Map(
          "type" -> "persistent",
          "size" -> "1000",
          "access_mode" -> "ReadWriteOnce",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "config" -> "{}"
        ))
      )
      val res = mks.mkPVCandPV(testK8SProvider, volume)
      res === Left(Error.BadRequest("Kubernetes providers only support volumes of type 'external', 'dynamic' and 'host_path'"))
    }

    "create external PV&PVC" in {
      val mks = new MkKubernetesSpec { }
      val volume = newInstance(
        migrations.V13.VOLUME_TYPE_ID,
        "test-volume",
        properties = Some(Map(
          "type" -> "external",
          "size" -> "1000",
          "access_mode" -> "ReadWriteOnce",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "config" -> Json.toJson(VolumeSpec.ExternalVolume(Json.obj())).toString
        ))
      )
      val Right(res) = mks.mkPVCandPV(testK8SProvider, volume)
      val (pvc, pv) = res
      pv === Some(skuber.PersistentVolume(
        metadata = skuber.ObjectMeta(
          name = s"test-vol-${volume.id}",
          labels = Map("meta/volume" -> s"${volume.id}")
        ),
        spec = Some(skuber.PersistentVolume.Spec(
          capacity = Map(skuber.Resource.storage -> skuber.Resource.Quantity(s"1000Mi")),
          source = skuber.Volume.GenericVolumeSource("{}"),
          accessModes = List(skuber.PersistentVolume.AccessMode.ReadWriteOnce),
          claimRef = Some(skuber.ObjectReference(
            namespace = "__SET_ME__",
            name = "test-volume"
          ))
        ))
      ))
      pvc === skuber.PersistentVolumeClaim(
        metadata = skuber.ObjectMeta(
          name = s"test-volume",
          namespace = "__SET_ME__",
          labels = Map("meta/volume" -> s"${volume.id}")
        ),
        spec = Some(skuber.PersistentVolumeClaim.Spec(
          resources = Some(skuber.Resource.Requirements(
            requests = Map(
              skuber.Resource.storage -> s"1000Mi"
            )
          )),
          accessModes = List(skuber.PersistentVolume.AccessMode.ReadWriteOnce),
          storageClassName = None
        ))
      )
    }

    "create dynamic PV&PVC" in {
      val mks = new MkKubernetesSpec { }
      val volume = newInstance(
        migrations.V13.VOLUME_TYPE_ID,
        "test-volume",
        properties = Some(Map(
          "type" -> "dynamic",
          "size" -> "1000",
          "access_mode" -> "ReadWriteOnce",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "config" -> Json.toJson(VolumeSpec.DynamicVolume("storage-class-1")).toString
        ))
      )
      val Right(res) = mks.mkPVCandPV(testK8SProvider, volume)
      val (pvc, pv) = res
      pv === None
      pvc === skuber.PersistentVolumeClaim(
        metadata = skuber.ObjectMeta(
          name = s"test-volume",
          namespace = "__SET_ME__",
          labels = Map("meta/volume" -> s"${volume.id}")
        ),
        spec = Some(skuber.PersistentVolumeClaim.Spec(
          resources = Some(skuber.Resource.Requirements(
            requests = Map(
              skuber.Resource.storage -> s"1000Mi"
            )
          )),
          accessModes = List(skuber.PersistentVolume.AccessMode.ReadWriteOnce),
          storageClassName = Some("storage-class-1")
        ))
      )
    }

    "not create dynamic PV&PVC if storage class is not in whitelist" in {
      val mks = new MkKubernetesSpec { }
      val volume = newInstance(
        migrations.V13.VOLUME_TYPE_ID,
        "test-volume",
        properties = Some(Map(
          "type" -> "dynamic",
          "size" -> "1000",
          "access_mode" -> "ReadWriteOnce",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "config" -> Json.toJson(VolumeSpec.DynamicVolume("storage-class-2")).toString
        ))
      )
      val res = mks.mkPVCandPV(testK8SProvider, volume)
      res === Left(Error.UnprocessableEntity("storage_class 'storage-class-2' is not in provider's white-list"))
    }
  }
}