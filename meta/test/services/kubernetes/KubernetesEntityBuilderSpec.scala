package services.kubernetes

// import java.util.UUID
import play.api.libs.json._
import play.api.test.PlaySpecification
// 
import org.specs2.specification.{BeforeAfterEach, BeforeAll}
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.meta.api.{ContainerSpec, VolumeSpec}
import com.galacticfog.gestalt.util.ResourceSerde
import com.galacticfog.gestalt.util.EitherWithErrors._
import com.galacticfog.gestalt.util.Error

class KubernetesEntityBuilderSpec extends PlaySpecification with ResourceScope with BeforeAll with BeforeAfterEach {
  override def beforeAll(): Unit = pristineDatabase()

  override def before: Unit = scalikejdbc.config.DBs.setupAll()

  override def after: Unit = scalikejdbc.config.DBs.closeAll()

  sequential
    
  lazy val keb = new KubernetesEntityBuilder { }

  def deserializeProperties[A: Reads](provider: GestaltResourceInstance, resource: GestaltResourceInstance): EitherError[(KubernetesProviderProperties.Properties,A)] = {
    import cats.syntax.either._

    import KubernetesProviderProperties.Implicits._

    for(
      specProperties <- ResourceSerde.deserialize[A,Error.UnprocessableEntity](resource);
      providerProperties <- ResourceSerde.deserialize[KubernetesProviderProperties.Properties,Error.UnprocessableEntity](provider)
    ) yield (providerProperties, specProperties)
  }

  "KubernetesEntityBuilder#mkPvSpec & KubernetesEntityBuilder#mkPvcSpec" should {
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

    def mkPvcAndPv(pp: KubernetesProviderProperties.Properties, sp: VolumeSpec): EitherError[(skuber.PersistentVolumeClaim.Spec,Option[skuber.PersistentVolume.Spec])] = {
      import cats.syntax.either._
      for(
        pvc <- keb.mkPvcSpec(pp, sp);
        pv <- keb.mkPvSpec(pp, sp)
      ) yield (pvc, pv)
    }

    "create host_path PV&PVC" in {
      val volume = newInstance(
        migrations.V13.VOLUME_TYPE_ID,
        "test-volume",
        properties = Some(Map(
          "name" -> "test-volume",
          "type" -> "host_path",
          "size" -> "1000",
          "access_mode" -> "ReadWriteOnce",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "config" -> Json.toJson(VolumeSpec.HostPathVolume("/supported-path/sub-path")).toString
        ))
      )
      val Right((pp, sp)) = deserializeProperties[VolumeSpec](testK8SProvider, volume)
      val Right(res) = mkPvcAndPv(pp, sp)
      val (pvc, pv) = res
      pv === Some(skuber.PersistentVolume.Spec(
        capacity = Map(skuber.Resource.storage -> skuber.Resource.Quantity(s"1000Mi")),
        source = skuber.Volume.HostPath("/supported-path/sub-path"),
        accessModes = List(skuber.PersistentVolume.AccessMode.ReadWriteOnce),
        claimRef = Some(skuber.ObjectReference(
          namespace = "__SET_ME__",
          name = "test-volume"
        ))
      ))
      pvc === skuber.PersistentVolumeClaim.Spec(
        resources = Some(skuber.Resource.Requirements(
          requests = Map(
            skuber.Resource.storage -> s"1000Mi"
          )
        )),
        accessModes = List(skuber.PersistentVolume.AccessMode.ReadWriteOnce),
        storageClassName = None
      )
    }

    "not create host_path PV&PVC if host path is not in whitelist" in {
      val volume = newInstance(
        migrations.V13.VOLUME_TYPE_ID,
        "test-volume",
        properties = Some(Map(
          "name" -> "test-volume",
          "type" -> "host_path",
          "size" -> "1000",
          "access_mode" -> "ReadWriteOnce",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "config" -> Json.toJson(VolumeSpec.HostPathVolume("/test")).toString
        ))
      )
      val Right((pp, sp)) = deserializeProperties[VolumeSpec](testK8SProvider, volume)
      val res = mkPvcAndPv(pp, sp)
      res === Left(Error.UnprocessableEntity("host_path '/test' is not in provider's white-list"))
    }

    "not create persistent PV&PVC" in {
      val volume = newInstance(
        migrations.V13.VOLUME_TYPE_ID,
        "test-volume",
        properties = Some(Map(
          "name" -> "test-volume",
          "type" -> "persistent",
          "size" -> "1000",
          "access_mode" -> "ReadWriteOnce",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "config" -> "{}"
        ))
      )
      val Right((pp, sp)) = deserializeProperties[VolumeSpec](testK8SProvider, volume)
      val res = mkPvcAndPv(pp, sp)
      res === Left(Error.BadRequest("Kubernetes providers only support volumes of type 'external', 'dynamic' and 'host_path'"))
    }

    "create external PV&PVC" in {
      val volume = newInstance(
        migrations.V13.VOLUME_TYPE_ID,
        "test-volume",
        properties = Some(Map(
          "name" -> "test-volume",
          "type" -> "external",
          "size" -> "1000",
          "access_mode" -> "ReadWriteOnce",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "config" -> Json.toJson(VolumeSpec.ExternalVolume(Json.obj())).toString
        ))
      )
      val Right((pp, sp)) = deserializeProperties[VolumeSpec](testK8SProvider, volume)
      val Right(res) = mkPvcAndPv(pp, sp)
      val (pvc, pv) = res
      pv === Some(skuber.PersistentVolume.Spec(
        capacity = Map(skuber.Resource.storage -> skuber.Resource.Quantity(s"1000Mi")),
        source = skuber.Volume.GenericVolumeSource("{}"),
        accessModes = List(skuber.PersistentVolume.AccessMode.ReadWriteOnce),
        claimRef = Some(skuber.ObjectReference(
          namespace = "__SET_ME__",
          name = "test-volume"
        ))
      ))
      pvc === skuber.PersistentVolumeClaim.Spec(
        resources = Some(skuber.Resource.Requirements(
          requests = Map(
            skuber.Resource.storage -> s"1000Mi"
          )
        )),
        accessModes = List(skuber.PersistentVolume.AccessMode.ReadWriteOnce),
        storageClassName = None
      )
    }

    "create dynamic PV&PVC" in {
      val volume = newInstance(
        migrations.V13.VOLUME_TYPE_ID,
        "test-volume",
        properties = Some(Map(
          "name" -> "test-volume",
          "type" -> "dynamic",
          "size" -> "1000",
          "access_mode" -> "ReadWriteOnce",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "config" -> Json.toJson(VolumeSpec.DynamicVolume("storage-class-1")).toString
        ))
      )
      val Right((pp, sp)) = deserializeProperties[VolumeSpec](testK8SProvider, volume)
      val Right(res) = mkPvcAndPv(pp, sp)
      val (pvc, pv) = res
      pv === None
      pvc === skuber.PersistentVolumeClaim.Spec(
        resources = Some(skuber.Resource.Requirements(
          requests = Map(
            skuber.Resource.storage -> s"1000Mi"
          )
        )),
        accessModes = List(skuber.PersistentVolume.AccessMode.ReadWriteOnce),
        storageClassName = Some("storage-class-1")
      )
    }

    "not create dynamic PV&PVC if storage class is not in whitelist" in {
      val volume = newInstance(
        migrations.V13.VOLUME_TYPE_ID,
        "test-volume",
        properties = Some(Map(
          "name" -> "test-volume",
          "type" -> "dynamic",
          "size" -> "1000",
          "access_mode" -> "ReadWriteOnce",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "config" -> Json.toJson(VolumeSpec.DynamicVolume("storage-class-2")).toString
        ))
      )
      val Right((pp, sp)) = deserializeProperties[VolumeSpec](testK8SProvider, volume)
      val res = mkPvcAndPv(pp, sp)
      res === Left(Error.UnprocessableEntity("storage_class 'storage-class-2' is not in provider's white-list"))
    }
  }


  "KubernetesEntityBuilder#mkPodTemplate" should {
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

    def mkPodTemplate(provider: GestaltResourceInstance, resource: GestaltResourceInstance): EitherError[skuber.Pod.Template.Spec] = {
      import cats.syntax.either._

      for(
        ppsp <- deserializeProperties[ContainerSpec](provider, resource);
        r <- keb.mkPodTemplate(ppsp._1, ppsp._2)
      ) yield r
    }


    "create pod template for a minimal container" in {
      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "cpus" -> "2.0",
          "memory" -> "768.0",
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> "[]",
          "env" -> Json.obj(
            "VAR1" -> "VAL1",
            "VAR2" -> "VAL2"
          ).toString,
          "network" -> ""
        ))
      )
      val res = mkPodTemplate(testK8SProvider, container)
      res === Right(
        skuber.Pod.Template.Spec(
          spec = Some(skuber.Pod.Spec(
            containers = List(
              skuber.Container(
                name = "test-container",
                image = "nginx:alpine",
                ports = List(),
                env = List(
                  skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP")), 
                  skuber.EnvVar("VAR1", skuber.EnvVar.StringValue("VAL1")),
                  skuber.EnvVar("VAR2", skuber.EnvVar.StringValue("VAL2"))
                ),
                resources = Some(skuber.Resource.Requirements(
                  limits = Map("memory" -> "768.000M"),
                  requests = Map("cpu" -> "2.000", "memory" -> "768.000M")
                )),
                livenessProbe = None,
                imagePullPolicy = skuber.Container.PullPolicy.Always
              )
            ),
            volumes = List(),
            affinity = None,
            dnsPolicy = skuber.DNSPolicy.ClusterFirst,
            imagePullSecrets = List(
              skuber.LocalObjectReference("imagepullsecret-1"),
              skuber.LocalObjectReference("imagepullsecret-2"),
              skuber.LocalObjectReference("imagepullsecret-3"),
              skuber.LocalObjectReference("imagepullsecret-4"),
              skuber.LocalObjectReference("imagepullsecret-5")
            )
          ))
        )
      )
    }

    "create pod template with port mappings" in {
      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> Json.arr(
            Json.obj(
              "protocol" -> "tcp",
              "container_port" -> 80,
              "expose_endpoint" -> true,
              "name" -> "http"
            ),
            Json.obj(
              "protocol" -> "tcp",
              "container_port" -> 443,
              "lb_port" -> 0,
              "expose_endpoint" -> true,
              "name" -> "https",
              "type" -> "external",
              "service_port" -> 32000
            ),
            Json.obj(
              "protocol" -> "tcp",
              "container_port" -> 444,
              "lb_port" -> 8444,
              "expose_endpoint" -> true,
              "name" -> "https2",
              "type" -> "internal",
              "service_port" -> 32001
            ),
            Json.obj(
              "protocol" -> "tcp",
              "container_port" -> 444,
              "lb_port" -> 8444,
              "expose_endpoint" -> true,
              "name" -> "https3",
              "type" -> "loadBalancer"
            ),
            Json.obj(
              "protocol" -> "udp",
              "container_port" -> 10000,
              "lb_port" -> 0,
              "expose_endpoint" -> false,
              "name" -> "debug",
              "type" -> "internal",
              "host_port" -> 10000
            )
          ).toString,
          "env" -> "{}",
          "network" -> ""
        ))
      )
      val res = mkPodTemplate(testK8SProvider, container)
      res === Right(
        skuber.Pod.Template.Spec(
          spec = Some(skuber.Pod.Spec(
            containers = List(
              skuber.Container(
                name = "test-container",
                image = "nginx:alpine",
                ports = List(
                  skuber.Container.Port(containerPort = 80, protocol = skuber.Protocol.TCP, name = "http"),
                  skuber.Container.Port(containerPort = 443, protocol = skuber.Protocol.TCP, name = "https"),
                  skuber.Container.Port(containerPort = 444, protocol = skuber.Protocol.TCP, name = "https2"),
                  skuber.Container.Port(containerPort = 444, protocol = skuber.Protocol.TCP, name = "https3"),
                  skuber.Container.Port(containerPort = 10000, protocol = skuber.Protocol.UDP, name = "debug", hostPort = Some(10000))
                ),
                env = List(
                  skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP"))
                ),
                resources = Some(skuber.Resource.Requirements(
                  limits = Map("memory" -> "128.000M"),
                  requests = Map("cpu" -> "1.000", "memory" -> "128.000M")
                )),
                livenessProbe = None,
                imagePullPolicy = skuber.Container.PullPolicy.Always
              )
            ),
            volumes = List(),
            affinity = None,
            dnsPolicy = skuber.DNSPolicy.ClusterFirst,
            imagePullSecrets = List(
              skuber.LocalObjectReference("imagepullsecret-1"),
              skuber.LocalObjectReference("imagepullsecret-2"),
              skuber.LocalObjectReference("imagepullsecret-3"),
              skuber.LocalObjectReference("imagepullsecret-4"),
              skuber.LocalObjectReference("imagepullsecret-5")
            )
          ))
        )
      )
    }

    "fail to create pod template if multiple health checks are set" in {
      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> "[]",
          "health_checks" -> Json.arr(
            Json.obj(
              "protocol" -> "TCP",
              "grace_period_seconds" -> 32,
              "interval_seconds" -> 17,
              "timeout_seconds" -> 7,
              "max_consecutive_failures" -> 3,
              "port" -> 8888
            ),
            Json.obj(
              "protocol" -> "HTTP",
              "path" -> "/youGood",
              "grace_period_seconds" -> 30,
              "interval_seconds" -> 15,
              "timeout_seconds" -> 5,
              "max_consecutive_failures" -> 1,
              "port" -> 8080
            )
          ).toString,
          "env" -> "{}",
          "network" -> ""
        ))
      )
      val res = mkPodTemplate(testK8SProvider, container)
      res === Left(Error.UnprocessableEntity("Kubernetes supports at most one health check/liveness probe."))
    }

    "create pod template with HTTP liveness probe" in {
      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> Json.arr(
            Json.obj(
              "protocol" -> "tcp",
              "container_port" -> 8080,
              "name" -> "service"
            )
          ).toString,
          "health_checks" -> Json.arr(
            Json.obj(
              "protocol" -> "HTTP",
              "path" -> "/youGood",
              "grace_period_seconds" -> 30,
              "interval_seconds" -> 15,
              "timeout_seconds" -> 5,
              "max_consecutive_failures" -> 1,
              "port" -> 8080
            )
          ).toString,
          "env" -> "{}",
          "network" -> ""
        ))
      )
      val res = mkPodTemplate(testK8SProvider, container)
      res === Right(
        skuber.Pod.Template.Spec(
          spec = Some(skuber.Pod.Spec(
            containers = List(
              skuber.Container(
                name = "test-container",
                image = "nginx:alpine",
                ports = List(
                  skuber.Container.Port(containerPort = 8080, protocol = skuber.Protocol.TCP, name = "service")
                ),
                env = List(
                  skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP"))
                ),
                resources = Some(skuber.Resource.Requirements(
                  limits = Map("memory" -> "128.000M"),
                  requests = Map("cpu" -> "1.000", "memory" -> "128.000M")
                )),
                livenessProbe = Some(skuber.Probe(
                  action = skuber.HTTPGetAction(port = Left(8080), path = "/youGood", schema = "HTTP"),
                  initialDelaySeconds = 30,
                  timeoutSeconds = 5
                )),
                imagePullPolicy = skuber.Container.PullPolicy.Always
              )
            ),
            volumes = List(),
            affinity = None,
            dnsPolicy = skuber.DNSPolicy.ClusterFirst,
            imagePullSecrets = List(
              skuber.LocalObjectReference("imagepullsecret-1"),
              skuber.LocalObjectReference("imagepullsecret-2"),
              skuber.LocalObjectReference("imagepullsecret-3"),
              skuber.LocalObjectReference("imagepullsecret-4"),
              skuber.LocalObjectReference("imagepullsecret-5")
            )
          ))
        )
      )
    }

    "create pod template with HTTPS liveness probe" in {
      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> Json.arr(
            Json.obj(
              "protocol" -> "tcp",
              "container_port" -> 8443,
              "name" -> "service"
            )
          ).toString,
          "health_checks" -> Json.arr(
            Json.obj(
              "protocol" -> "HTTPS",
              "path" -> "/youGoodAndSecure",
              "grace_period_seconds" -> 31,
              "interval_seconds" -> 16,
              "timeout_seconds" -> 6,
              "max_consecutive_failures" -> 2,
              "port" -> 8443
            )
          ).toString,
          "env" -> "{}",
          "network" -> ""
        ))
      )
      val res = mkPodTemplate(testK8SProvider, container)
      res === Right(
        skuber.Pod.Template.Spec(
          spec = Some(skuber.Pod.Spec(
            containers = List(
              skuber.Container(
                name = "test-container",
                image = "nginx:alpine",
                ports = List(
                  skuber.Container.Port(containerPort = 8443, protocol = skuber.Protocol.TCP, name = "service")
                ),
                env = List(
                  skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP"))
                ),
                resources = Some(skuber.Resource.Requirements(
                  limits = Map("memory" -> "128.000M"),
                  requests = Map("cpu" -> "1.000", "memory" -> "128.000M")
                )),
                livenessProbe = Some(skuber.Probe(
                  action = skuber.HTTPGetAction(port = Left(8443), path = "/youGoodAndSecure", schema = "HTTPS"),
                  initialDelaySeconds = 31,
                  timeoutSeconds = 6
                )),
                imagePullPolicy = skuber.Container.PullPolicy.Always
              )
            ),
            volumes = List(),
            affinity = None,
            dnsPolicy = skuber.DNSPolicy.ClusterFirst,
            imagePullSecrets = List(
              skuber.LocalObjectReference("imagepullsecret-1"),
              skuber.LocalObjectReference("imagepullsecret-2"),
              skuber.LocalObjectReference("imagepullsecret-3"),
              skuber.LocalObjectReference("imagepullsecret-4"),
              skuber.LocalObjectReference("imagepullsecret-5")
            )
          ))
        )
      )
    }

    "create pod template with TCP liveness probe" in {
      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> Json.arr(
            Json.obj(
              "protocol" -> "tcp",
              "container_port" -> 8888,
              "name" -> "service"
            )
          ).toString,
          "health_checks" -> Json.arr(
            Json.obj(
              "protocol" -> "TCP",
              "grace_period_seconds" -> 32,
              "interval_seconds" -> 17,
              "timeout_seconds" -> 7,
              "max_consecutive_failures" -> 3,
              "port" -> 8888
            )
          ).toString,
          "env" -> "{}",
          "network" -> ""
        ))
      )
      val res = mkPodTemplate(testK8SProvider, container)
      res === Right(
        skuber.Pod.Template.Spec(
          spec = Some(skuber.Pod.Spec(
            containers = List(
              skuber.Container(
                name = "test-container",
                image = "nginx:alpine",
                ports = List(
                  skuber.Container.Port(containerPort = 8888, protocol = skuber.Protocol.TCP, name = "service")
                ),
                env = List(
                  skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP"))
                ),
                resources = Some(skuber.Resource.Requirements(
                  limits = Map("memory" -> "128.000M"),
                  requests = Map("cpu" -> "1.000", "memory" -> "128.000M")
                )),
                livenessProbe = Some(skuber.Probe(
                  action = skuber.TCPSocketAction(port = Left(8888)),
                  initialDelaySeconds = 32,
                  timeoutSeconds = 7
                )),
                imagePullPolicy = skuber.Container.PullPolicy.Always
              )
            ),
            volumes = List(),
            affinity = None,
            dnsPolicy = skuber.DNSPolicy.ClusterFirst,
            imagePullSecrets = List(
              skuber.LocalObjectReference("imagepullsecret-1"),
              skuber.LocalObjectReference("imagepullsecret-2"),
              skuber.LocalObjectReference("imagepullsecret-3"),
              skuber.LocalObjectReference("imagepullsecret-4"),
              skuber.LocalObjectReference("imagepullsecret-5")
            )
          ))
        )
      )
    }

    "create pod template with COMMAND liveness probe" in {
      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> "[]",
          "health_checks" -> Json.arr(
            Json.obj(
              "protocol" -> "COMMAND",
              "command" -> "curl localhost:8888",
              "grace_period_seconds" -> 33,
              "interval_seconds" -> 18,
              "timeout_seconds" -> 8,
              "max_consecutive_failures" -> 4,
              "port" -> 8080
            )
          ).toString,
          "env" -> "{}",
          "network" -> ""
        ))
      )
      val res = mkPodTemplate(testK8SProvider, container)
      res === Right(
        skuber.Pod.Template.Spec(
          spec = Some(skuber.Pod.Spec(
            containers = List(
              skuber.Container(
                name = "test-container",
                image = "nginx:alpine",
                ports = List(),
                env = List(
                  skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP"))
                ),
                resources = Some(skuber.Resource.Requirements(
                  limits = Map("memory" -> "128.000M"),
                  requests = Map("cpu" -> "1.000", "memory" -> "128.000M")
                )),
                livenessProbe = Some(skuber.Probe(
                  action = skuber.ExecAction(List("/bin/sh", "curl", "localhost:8888")),
                  initialDelaySeconds = 33,
                  timeoutSeconds = 8
                )),
                imagePullPolicy = skuber.Container.PullPolicy.Always
              )
            ),
            volumes = List(),
            affinity = None,
            dnsPolicy = skuber.DNSPolicy.ClusterFirst,
            imagePullSecrets = List(
              skuber.LocalObjectReference("imagepullsecret-1"),
              skuber.LocalObjectReference("imagepullsecret-2"),
              skuber.LocalObjectReference("imagepullsecret-3"),
              skuber.LocalObjectReference("imagepullsecret-4"),
              skuber.LocalObjectReference("imagepullsecret-5")
            )
          ))
        )
      )
    }

    "create pod template with HTTP liveness probe and port_index" in {
      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> Json.arr(
            Json.obj(
              "protocol" -> "tcp",
              "container_port" -> 9999,
              "name" -> "debug"
            ),
            Json.obj(
              "protocol" -> "tcp",
              "container_port" -> 8080,
              "name" -> "web"
            )
          ).toString,
          "health_checks" -> Json.arr(
            Json.obj(
              "protocol" -> "HTTP",
              "path" -> "/youGood",
              "grace_period_seconds" -> 30,
              "interval_seconds" -> 15,
              "timeout_seconds" -> 5,
              "max_consecutive_failures" -> 1,
              "port_index" -> 1
            )
          ).toString,
          "env" -> "{}",
          "network" -> ""
        ))
      )
      val res = mkPodTemplate(testK8SProvider, container)
      res === Right(
        skuber.Pod.Template.Spec(
          spec = Some(skuber.Pod.Spec(
            containers = List(
              skuber.Container(
                name = "test-container",
                image = "nginx:alpine",
                ports = List(
                  skuber.Container.Port(containerPort = 9999, protocol = skuber.Protocol.TCP, name = "debug"),
                  skuber.Container.Port(containerPort = 8080, protocol = skuber.Protocol.TCP, name = "web")
                ),
                env = List(
                  skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP"))
                ),
                resources = Some(skuber.Resource.Requirements(
                  limits = Map("memory" -> "128.000M"),
                  requests = Map("cpu" -> "1.000", "memory" -> "128.000M")
                )),
                livenessProbe = Some(skuber.Probe(
                  action = skuber.HTTPGetAction(port = Left(8080), path = "/youGood", schema = "HTTP"),
                  initialDelaySeconds = 30,
                  timeoutSeconds = 5
                )),
                imagePullPolicy = skuber.Container.PullPolicy.Always
              )
            ),
            volumes = List(),
            affinity = None,
            dnsPolicy = skuber.DNSPolicy.ClusterFirst,
            imagePullSecrets = List(
              skuber.LocalObjectReference("imagepullsecret-1"),
              skuber.LocalObjectReference("imagepullsecret-2"),
              skuber.LocalObjectReference("imagepullsecret-3"),
              skuber.LocalObjectReference("imagepullsecret-4"),
              skuber.LocalObjectReference("imagepullsecret-5")
            )
          ))
        )
      )
    }

    "fail to create pod template with invalid port_index on the health check" in {
      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> Json.arr(
            Json.obj(
              "protocol" -> "tcp",
              "container_port" -> 9999,
              "name" -> "debug"
            )
          ).toString,
          "health_checks" -> Json.arr(
            Json.obj(
              "protocol" -> "HTTP",
              "path" -> "/youGood",
              "grace_period_seconds" -> 30,
              "interval_seconds" -> 15,
              "timeout_seconds" -> 5,
              "max_consecutive_failures" -> 1,
              "port_index" -> 1
            )
          ).toString,
          "env" -> "{}",
          "network" -> ""
        ))
      )
      val res = mkPodTemplate(testK8SProvider, container)
      res === Left(Error.UnprocessableEntity("HealthCheck port_index '1' was out of bounds for the provided port mappings array"))
    }

    "create pod template with force_pull=false, explicit command and args" in {
      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "num_instances" -> "1",
          "force_pull" -> "false",
          "port_mappings" -> "[]",
          "env" -> "{}",
          "network" -> "",
          "cmd" -> "python -m SimpleHTTPServer $PORT",
          "args" -> Json.arr("echo", "hello", "world").toString
        ))
      )
      val res = mkPodTemplate(testK8SProvider, container)
      res === Right(
        skuber.Pod.Template.Spec(
          spec = Some(skuber.Pod.Spec(
            containers = List(
              skuber.Container(
                name = "test-container",
                image = "nginx:alpine",
                command = List("python", "-m", "SimpleHTTPServer", "$PORT"),
                args = List("echo", "hello", "world"),
                ports = List(),
                env = List(
                  skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP"))
                ),
                resources = Some(skuber.Resource.Requirements(
                  limits = Map("memory" -> "128.000M"),
                  requests = Map("cpu" -> "1.000", "memory" -> "128.000M")
                )),
                livenessProbe = None,
                imagePullPolicy = skuber.Container.PullPolicy.IfNotPresent
              )
            ),
            volumes = List(),
            affinity = None,
            dnsPolicy = skuber.DNSPolicy.ClusterFirst,
            imagePullSecrets = List(
              skuber.LocalObjectReference("imagepullsecret-1"),
              skuber.LocalObjectReference("imagepullsecret-2"),
              skuber.LocalObjectReference("imagepullsecret-3"),
              skuber.LocalObjectReference("imagepullsecret-4"),
              skuber.LocalObjectReference("imagepullsecret-5")
            )
          ))
        )
      )
    }

    "create pod template with custom cpu/memory constraints" in {
      val customTestK8SProvider = newInstance(
        typeId = ResourceIds.KubeProvider,
        name = "test-provider",
        properties = Option(Map(
          "parent" -> "{}",
          "config" -> Json.obj(
            "cpu_requirement_type" -> Json.arr("request", "limit"),
            "memory_requirement_type" -> Json.arr("request", "limit")
          ).toString
        ))
      )

      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> customTestK8SProvider.id
          ).toString,
          "num_instances" -> "1",
          "port_mappings" -> "[]",
          "env" -> "{}",
          "network" -> ""
        ))
      )
      val res = mkPodTemplate(customTestK8SProvider, container)
      res === Right(
        skuber.Pod.Template.Spec(
          spec = Some(skuber.Pod.Spec(
            containers = List(
              skuber.Container(
                name = "test-container",
                image = "nginx:alpine",
                ports = List(),
                env = List(
                  skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP"))
                ),
                resources = Some(skuber.Resource.Requirements(
                  limits = Map("cpu" -> "1.000", "memory" -> "128.000M"),
                  requests = Map("cpu" -> "1.000", "memory" -> "128.000M")
                )),
                livenessProbe = None,
                imagePullPolicy = skuber.Container.PullPolicy.IfNotPresent
              )
            ),
            volumes = List(),
            affinity = None,
            dnsPolicy = skuber.DNSPolicy.ClusterFirst,
            imagePullSecrets = List(
              skuber.LocalObjectReference("imagepullsecret-1"),
              skuber.LocalObjectReference("imagepullsecret-2"),
              skuber.LocalObjectReference("imagepullsecret-3"),
              skuber.LocalObjectReference("imagepullsecret-4"),
              skuber.LocalObjectReference("imagepullsecret-5")
            )
          ))
        )
      )
    }

    "create pod template with disabled cpu/memory constraints" in {
      val customTestK8SProvider = newInstance(
        typeId = ResourceIds.KubeProvider,
        name = "test-provider",
        properties = Option(Map(
          "parent" -> "{}",
          "config" -> Json.obj(
            "cpu_requirement_type" -> Json.arr(),
            "memory_requirement_type" -> Json.arr()
          ).toString
        ))
      )

      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> customTestK8SProvider.id
          ).toString,
          "num_instances" -> "1",
          "port_mappings" -> "[]",
          "env" -> "{}",
          "network" -> ""
        ))
      )
      val res = mkPodTemplate(customTestK8SProvider, container)
      res === Right(
        skuber.Pod.Template.Spec(
          spec = Some(skuber.Pod.Spec(
            containers = List(
              skuber.Container(
                name = "test-container",
                image = "nginx:alpine",
                ports = List(),
                env = List(
                  skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP"))
                ),
                resources = Some(skuber.Resource.Requirements(
                  limits = Map(),
                  requests = Map()
                )),
                livenessProbe = None,
                imagePullPolicy = skuber.Container.PullPolicy.IfNotPresent
              )
            ),
            volumes = List(),
            affinity = None,
            dnsPolicy = skuber.DNSPolicy.ClusterFirst,
            imagePullSecrets = List(
              skuber.LocalObjectReference("imagepullsecret-1"),
              skuber.LocalObjectReference("imagepullsecret-2"),
              skuber.LocalObjectReference("imagepullsecret-3"),
              skuber.LocalObjectReference("imagepullsecret-4"),
              skuber.LocalObjectReference("imagepullsecret-5")
            )
          ))
        )
      )
    }

    "create pod template with gpu" in {
      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "cpus" -> "2.0",
          "memory" -> "768.0",
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> "[]",
          "env" -> Json.obj(
            "VAR1" -> "VAL1",
            "VAR2" -> "VAL2"
          ).toString,
          "network" -> "",
          "gpu_support" -> Json.obj(
            "enabled" -> true,
            "count" -> 2,
            "type" -> "nvidia"
          ).toString
        ))
      )
      val res = mkPodTemplate(testK8SProvider, container)
      res === Right(
        skuber.Pod.Template.Spec(
          spec = Some(skuber.Pod.Spec(
            containers = List(
              skuber.Container(
                name = "test-container",
                image = "nginx:alpine",
                ports = List(),
                env = List(
                  skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP")), 
                  skuber.EnvVar("VAR1", skuber.EnvVar.StringValue("VAL1")),
                  skuber.EnvVar("VAR2", skuber.EnvVar.StringValue("VAL2"))
                ),
                resources = Some(skuber.Resource.Requirements(
                  limits = Map("memory" -> "768.000M", "nvidia.com/gpu" -> "2"),
                  requests = Map("cpu" -> "2.000", "memory" -> "768.000M")
                )),
                livenessProbe = None,
                imagePullPolicy = skuber.Container.PullPolicy.Always
              )
            ),
            volumes = List(),
            affinity = None,
            dnsPolicy = skuber.DNSPolicy.ClusterFirst,
            imagePullSecrets = List(
              skuber.LocalObjectReference("imagepullsecret-1"),
              skuber.LocalObjectReference("imagepullsecret-2"),
              skuber.LocalObjectReference("imagepullsecret-3"),
              skuber.LocalObjectReference("imagepullsecret-4"),
              skuber.LocalObjectReference("imagepullsecret-5")
            )
          ))
        )
      )
    }

    "create pod template with secrets" in {
      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "num_instances" -> "1",
          "port_mappings" -> "[]",
          "env" -> "{}",
          "network" -> "",
          "secrets" -> Json.arr(
            Json.obj(
              "mount_type" -> "env",
              "secret_id" -> "00000000-0000-0000-0000-000000000000",
              "path" -> "SOME_ENV_VAR",
              "secret_key" -> "part-a"
            ),
            Json.obj(
              "mount_type" -> "file",
              "secret_id" -> "00000000-0000-0000-0000-000000000000",
              "path" -> "/mnt/secrets/files/file-a",
              "secret_key" -> "part-a"
            ),
            Json.obj(
              "mount_type" -> "file",
              "secret_id" -> "00000000-0000-0000-0000-000000000000",
              "path" -> "/mnt/secrets/files/file-b",
              "secret_key" -> "part-b"
            ),
            Json.obj(
              "mount_type" -> "file",
              "secret_id" -> "00000000-0000-0000-0000-000000000000",
              "path" -> "/mnt/secrets/files/sub/file-c",
              "secret_key" -> "part-b"
            ),
            Json.obj(
              "mount_type" -> "directory",
              "secret_id" -> "00000000-0000-0000-0000-000000000000",
              "path" -> "/mnt/secrets/dir"
            )
          ).toString
        ))
      )
      val res = mkPodTemplate(testK8SProvider, container)
      res === Right(
        skuber.Pod.Template.Spec(
          spec = Some(skuber.Pod.Spec(
            containers = List(
              skuber.Container(
                name = "test-container",
                image = "nginx:alpine",
                ports = List(),
                env = List(
                  skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP")),
                  skuber.EnvVar("SOME_ENV_VAR", skuber.EnvVar.SecretKeyRef("part-a", "!00000000-0000-0000-0000-000000000000"))
                ),
                resources = Some(skuber.Resource.Requirements(
                  limits = Map("memory" -> "128.000M"),
                  requests = Map("cpu" -> "1.000", "memory" -> "128.000M")
                )),
                livenessProbe = None,
                imagePullPolicy = skuber.Container.PullPolicy.IfNotPresent,
                volumeMounts = List(
                  skuber.Volume.Mount(name = "dir-4", mountPath = "/mnt/secrets/dir", readOnly = true),
                  skuber.Volume.Mount(name = "file-0", mountPath = "/mnt/secrets/files", readOnly = true)
                )
              )
            ),
            volumes = List(
              // these are not meant to be actual secret names, but secret ids in meta
              skuber.Volume("dir-4", skuber.Volume.Secret(secretName = "!00000000-0000-0000-0000-000000000000")),
              skuber.Volume("file-0", skuber.Volume.Secret(secretName = "!00000000-0000-0000-0000-000000000000", items = Some(List(
                skuber.Volume.KeyToPath("part-a","file-a"),
                skuber.Volume.KeyToPath("part-b","file-b"),
                skuber.Volume.KeyToPath("part-b","sub/file-c")
              ))))
            ),
            affinity = None,
            dnsPolicy = skuber.DNSPolicy.ClusterFirst,
            imagePullSecrets = List(
              skuber.LocalObjectReference("imagepullsecret-1"),
              skuber.LocalObjectReference("imagepullsecret-2"),
              skuber.LocalObjectReference("imagepullsecret-3"),
              skuber.LocalObjectReference("imagepullsecret-4"),
              skuber.LocalObjectReference("imagepullsecret-5")
            )
          ))
        )
      )
    }

    "fail to create pod template with duplicate secrets" in {
      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "num_instances" -> "1",
          "port_mappings" -> "[]",
          "env" -> "{}",
          "network" -> "",
          "secrets" -> Json.arr(
            Json.obj(
              "mount_type" -> "file",
              "secret_id" -> "00000000-0000-0000-0000-000000000000",
              "path" -> "/mnt/secrets/files/file",
              "secret_key" -> "part-a"
            ),
            Json.obj(
              "mount_type" -> "file",
              "secret_id" -> "00000000-0000-0000-0000-000000000000",
              "path" -> "/mnt/secrets/files/file",
              "secret_key" -> "part-b"
            )
          ).toString
        ))
      )
      val res = mkPodTemplate(testK8SProvider, container)
      res === Left(Error.BadRequest("secrets must have unique paths"))
    }

    "create pod template with volumes" in {
      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "num_instances" -> "1",
          "port_mappings" -> "[]",
          "env" -> "{}",
          "network" -> "",
          "secrets" -> "[]",
          "volumes" -> Json.arr(
            Json.obj("mount_path" -> "/tmp", "volume_id" -> "00000000-0000-0000-0000-000000000000")//,
            // Json.obj("mount_path" -> "/tmp1", "volume_resource" -> Json.obj(

            // ))
          ).toString
        ))
      )
      val res = mkPodTemplate(testK8SProvider, container)
      res === Right(
        skuber.Pod.Template.Spec(
          spec = Some(skuber.Pod.Spec(
            containers = List(
              skuber.Container(
                name = "test-container",
                image = "nginx:alpine",
                ports = List(),
                env = List(
                  skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP"))
                ),
                resources = Some(skuber.Resource.Requirements(
                  limits = Map("memory" -> "128.000M"),
                  requests = Map("cpu" -> "1.000", "memory" -> "128.000M")
                )),
                livenessProbe = None,
                imagePullPolicy = skuber.Container.PullPolicy.IfNotPresent,
                volumeMounts = List(
                  skuber.Volume.Mount(name = "volume-ex-00000000-0000-0000-0000-000000000000", mountPath = "/tmp")
                )
              )
            ),
            volumes = List(
              skuber.Volume(
                "volume-ex-00000000-0000-0000-0000-000000000000",
                skuber.Volume.PersistentVolumeClaimRef("!00000000-0000-0000-0000-000000000000"))
            ),
            affinity = None,
            dnsPolicy = skuber.DNSPolicy.ClusterFirst,
            imagePullSecrets = List(
              skuber.LocalObjectReference("imagepullsecret-1"),
              skuber.LocalObjectReference("imagepullsecret-2"),
              skuber.LocalObjectReference("imagepullsecret-3"),
              skuber.LocalObjectReference("imagepullsecret-4"),
              skuber.LocalObjectReference("imagepullsecret-5")
            )
          ))
        )
      )
    }
  }


  "KubernetesEntityBuilder#mk***ServiceSpec" should {
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

    "create service specs" in {
      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "num_instances" -> "1",
          "port_mappings" -> Json.arr(
            Json.obj(
              "protocol" -> "tcp",
              "container_port" -> 80,
              "expose_endpoint" -> true,
              "name" -> "http"
            ),
            Json.obj(
              "protocol" -> "tcp",
              "container_port" -> 443,
              "lb_port" -> 0,
              "expose_endpoint" -> true,
              "name" -> "https",
              "type" -> "external",
              "service_port" -> 32000
            ),
            Json.obj(
              "protocol" -> "tcp",
              "container_port" -> 444,
              "lb_port" -> 8444,
              "expose_endpoint" -> true,
              "name" -> "https2",
              "type" -> "internal",
              "service_port" -> 32001
            ),
            Json.obj(
              "protocol" -> "tcp",
              "container_port" -> 444,
              "lb_port" -> 8444,
              "expose_endpoint" -> true,
              "name" -> "https3",
              "type" -> "loadBalancer"
            ),
            Json.obj(
              "protocol" -> "udp",
              "container_port" -> 10000,
              "lb_port" -> 0,
              "expose_endpoint" -> false,
              "name" -> "debug",
              "type" -> "internal",
              "host_port" -> 10000
            )
          ).toString,
          "env" -> "{}",
          "network" -> "",
          "secrets" -> "[]"
        ))
      )
      val (pp, sp) = deserializeProperties[ContainerSpec](testK8SProvider, container).right.get

      keb.mkClusterIpServiceSpec(pp, sp) === Right(Some(skuber.Service.Spec(
        ports = List(
          skuber.Service.Port(
            name = "http",
            port = 80,
            targetPort = Some(Left(80))
          ),
          skuber.Service.Port(
            name = "https",
            port = 443,
            targetPort = Some(Left(443))
          ),
          skuber.Service.Port(
            name = "https2",
            port = 8444,
            targetPort = Some(Left(444))
          ),
          skuber.Service.Port(
            name = "https3",
            port = 8444,
            targetPort = Some(Left(444))
          )
        ),
        _type = skuber.Service.Type.ClusterIP
      )))
      keb.mkNodePortServiceSpec(pp, sp) === Right(Some(skuber.Service.Spec(
        ports = List(
          skuber.Service.Port(
            name = "https",
            port = 443,
            targetPort = Some(Left(443)),
            nodePort = 32000
          ),
          skuber.Service.Port(
            name = "https3",
            port = 8444,
            targetPort = Some(Left(444)),
            nodePort = 0
          )
        ),
        _type = skuber.Service.Type.NodePort
      )))
      keb.mkLoadBalancerServiceSpec(pp, sp) === Right(Some(skuber.Service.Spec(
        ports = List(
          skuber.Service.Port(
            name = "https3",
            port = 8444,
            targetPort = Some(Left(444))
          )
        ),
        _type = skuber.Service.Type.LoadBalancer
      )))
    }
    "do not create service specs with no port_mappings" in {
      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "num_instances" -> "1",
          "port_mappings" -> "[]",
          "env" -> "{}",
          "network" -> "",
          "secrets" -> "[]"
        ))
      )
      val (pp, sp) = deserializeProperties[ContainerSpec](testK8SProvider, container).right.get

      keb.mkClusterIpServiceSpec(pp, sp) === Right(None)
      keb.mkNodePortServiceSpec(pp, sp) === Right(None)
      keb.mkLoadBalancerServiceSpec(pp, sp) === Right(None)
    }

    "create service spec with loadBalancerIp and labels" in {
      val container = newInstance(
        ResourceIds.Container,
        "test-container",
        properties = Some(Map(
          "name" -> "test-container",
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testK8SProvider.id
          ).toString,
          "num_instances" -> "1",
          "port_mappings" -> Json.arr(
            Json.obj(
              "protocol" -> "tcp",
              "container_port" -> 443,
              "expose_endpoint" -> true,
              "name" -> "ep1",
              "type" -> "internal"
            ),
            Json.obj(
              "protocol" -> "tcp",
              "container_port" -> 444,
              "lb_port" -> 8444,
              "expose_endpoint" -> true,
              "name" -> "ep2",
              "type" -> "loadBalancer",
              "lb_ip" -> "100.200.10.20"
            )
          ).toString,
          "env" -> "{}",
          "network" -> "",
          "secrets" -> "[]"
        ))
      )
      val (pp, sp) = deserializeProperties[ContainerSpec](testK8SProvider, container).right.get

      keb.mkClusterIpServiceSpec(pp, sp) === Right(Some(skuber.Service.Spec(
        ports = List(
          skuber.Service.Port(
            name = "ep1",
            port = 443,
            targetPort = Some(Left(443))
          ),
          skuber.Service.Port(
            name = "ep2",
            port = 8444,
            targetPort = Some(Left(444))
          )
        ),
        _type = skuber.Service.Type.ClusterIP
      )))
      keb.mkNodePortServiceSpec(pp, sp) === Right(Some(skuber.Service.Spec(
        ports = List(
          skuber.Service.Port(
            name = "ep2",
            port = 8444,
            targetPort = Some(Left(444))
          )
        ),
        _type = skuber.Service.Type.NodePort
      )))
      keb.mkLoadBalancerServiceSpec(pp, sp) === Right(Some(skuber.Service.Spec(
        ports = List(
          skuber.Service.Port(
            name = "ep2",
            port = 8444,
            targetPort = Some(Left(444))
          )
        ),
        _type = skuber.Service.Type.LoadBalancer,
        loadBalancerIP = "100.200.10.20"
      )))
    }
  }
}