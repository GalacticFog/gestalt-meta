package services

import java.time.{ZoneOffset, ZonedDateTime}

import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.google.inject.AbstractModule
import controllers.util.GestaltSecurityMocking
import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.specs2.specification.{BeforeAll, Scope}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.PlaySpecification
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.{JsonMatchers, Matcher}
import skuber.api.client
import skuber.json.format._
import skuber.json.ext.format._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

@RunWith(classOf[JUnitRunner])
class KubeServiceSpec extends PlaySpecification with ResourceScope with BeforeAll
  with Mockito with GestaltSecurityMocking with JsonMatchers {

  override def beforeAll(): Unit = pristineDatabase()

  sequential

  case class FakeKubeModule(mockSkubeFactory: SkuberFactory) extends AbstractModule {
    override def configure(): Unit = {
      bind(classOf[SkuberFactory]).toInstance(mockSkubeFactory)
    }
  }

  abstract class FakeKube() extends Scope {
    var Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    Entitlements.setNewEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))
    var testProvider = createKubernetesProvider(testEnv.id, "test-provider").get

    val skDefaultNs = mock[skuber.Namespace]
    skDefaultNs.name returns "default"
    val skTestNs    = mock[skuber.Namespace]
    skTestNs.name returns testEnv.id.toString
    val mockSkuber = mock[client.RequestContext]
    val mockSkuberFactory = mock[SkuberFactory]
    mockSkuberFactory.initializeKube(testProvider.id, "default"          ) returns Future.successful(mockSkuber)
    mockSkuber.get[skuber.Namespace]("default") returns Future.successful(skDefaultNs)
    mockSkuberFactory.initializeKube(testProvider.id, testEnv.id.toString) returns Future.successful(mockSkuber)
    mockSkuber.get[skuber.Namespace](testEnv.id.toString) returns Future.successful(skTestNs)

    val injector =
      new GuiceApplicationBuilder()
        .disable[modules.ProdSecurityModule]
        .disable[modules.MetaDefaultSkuber]
        .disable[modules.MetaDefaultServices]
        .bindings(FakeKubeModule(mockSkuberFactory))
        .injector
    val ks = injector.instanceOf[KubernetesService]
  }

  abstract class FakeKubeCreate( force_pull: Boolean = true,
                                 args: Option[Seq[String]] = None,
                                 cmd: Option[String] = None ) extends FakeKube() {

    // three ports:
    // - two "exposed", one not
    // - one exposure has default container_port==service_port, the other overrides the service_port
    // - one port has a required host port
    val testProps = ContainerSpec(
      name = "",
      container_type = "DOCKER",
      image = "nginx",
      provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
      port_mappings = Seq(
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(80),
          host_port = None,
          service_port = None,
          name = Some("http"),
          labels = None,
          expose_endpoint = Some(true)
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(443),
          host_port = None,
          service_port = Some(8443),
          name = Some("https"),
          labels = None,
          expose_endpoint = Some(true)
        ),
        ContainerSpec.PortMapping(
          protocol = "udp",
          container_port = Some(9999),
          host_port = Some(9999),
          service_port = None,
          name = Some("debug"),
          labels = None,
          expose_endpoint = None
        )
      ),
      cpus = 1.0,
      memory = 128,
      disk = 0.0,
      num_instances = 1,
      network = Some("default"),
      cmd = cmd,
      constraints = Seq(),
      accepted_resource_roles = None,
      args = args,
      force_pull = force_pull,
      health_checks = Seq(),
      volumes = Seq(),
      labels = Map(),
      env = Map(),
      user = None
    )

    val Success(metaContainer) = createInstance(
      ResourceIds.Container,
      "test-container",
      parent = Some(testEnv.id),
      properties = Some(Map(
        "container_type" -> testProps.container_type,
        "image" -> testProps.image,
        "provider" -> Output.renderInstance(testProvider).toString,
        "cpus" -> testProps.cpus.toString,
        "memory" -> testProps.memory.toString,
        "num_instances" -> testProps.num_instances.toString,
        "force_pull" -> testProps.force_pull.toString,
        "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
        "network" -> testProps.network.get
      ) ++ Seq[Option[(String,String)]](
        args map ("args" -> Json.toJson(_).toString),
        cmd  map ("cmd" -> _)
      ).flatten.toMap)
    )

    val lbls = Map(KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString)

    val mockService = skuber.Service(
      name = "test-container",
      spec = skuber.Service.Spec(
        clusterIP = "10.0.161.84",
        ports = List(
          skuber.Service.Port("http",  skuber.Protocol.TCP,   80, Some(skuber.portNumToNameablePort(80)), 30784),
          skuber.Service.Port("https", skuber.Protocol.TCP, 8443, Some(skuber.portNumToNameablePort(443)), 31374)
        ),
        _type = skuber.Service.Type.NodePort
      )
    ).addLabels(lbls)

    val mockDepl = skuber.ext.Deployment(
      metadata = skuber.ObjectMeta(
        name = "test-container",
        labels = lbls,
        creationTimestamp = Some(ZonedDateTime.now(ZoneOffset.UTC))
      )
    ).withTemplate(
      skuber.Pod.Template.Spec().addContainer(
        skuber.Container(
          name = "test-container",
          image = testProps.image,
          ports = List(
            skuber.Container.Port(80, skuber.Protocol.TCP, "http"),
            skuber.Container.Port(443, skuber.Protocol.TCP, "https"),
            skuber.Container.Port(9999, skuber.Protocol.UDP, "debug")
          )
        )
      )
    )

    val startA = ZonedDateTime.now(ZoneOffset.UTC)
    val startB = ZonedDateTime.now(ZoneOffset.UTC)
    val mockPodA = skuber.Pod(
      metadata = skuber.ObjectMeta(
        name = "test-container-hash-a",
        labels = lbls
      ),
      status = Some(skuber.Pod.Status(
        hostIP = Some("host-a"),
        podIP = Some("10.10.10.1"),
        containerStatuses = List(skuber.Container.Status(
          name = "test-container-hash-a",
          ready = true,
          restartCount = 0,
          image = testProps.image,
          imageID = "who-knows",
          state = Some(skuber.Container.Running(startedAt = Some(startA)))
        )),
        startTime = Some(startA)
      ))
    )
    val mockPodB = skuber.Pod(
      metadata = skuber.ObjectMeta(
        name = "test-container-hash-b",
        labels = lbls
      ),
      status = Some(skuber.Pod.Status(
        hostIP = Some("host-b"),
        podIP = Some("10.10.10.2"),
        containerStatuses = List(skuber.Container.Status(
          name = "test-container-hash-b",
          ready = true,
          restartCount = 0,
          image = testProps.image,
          imageID = "who-knows",
          state = Some(skuber.Container.Running(startedAt = Some(startB)))
        )),
        startTime = Some(startB)
      ))
    )

    mockSkuber.list()(any,meq(skuber.ext.deplListKind)) returns Future.successful(skuber.ext.DeploymentList(items = List(mockDepl)))
    mockSkuber.list()(any,meq(client.serviceListKind)) returns Future.successful(skuber.ServiceList(items = List(mockService)))
    mockSkuber.list()(any,meq(client.podListKind)) returns Future.successful(skuber.PodList(items = List(mockPodA,mockPodB)))

    mockSkuber.create(any)(any,meq(skuber.ext.deploymentKind)) returns Future.successful(mock[skuber.ext.Deployment])
    mockSkuber.create(any)(any,meq(client.serviceKind)) returns Future.successful(mockService)
  }

  def inNamespace[R <: skuber.ObjectResource](name: String): Matcher[R] = { r: R =>
    (r.metadata.namespace == name,  r.name+" in namespace "+name, r.name+" not in namespace "+name)
  }

  def hasExactlyContainerPorts(ps: skuber.Container.Port*) = ((_: skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).map(_.containers.flatMap(_.ports)).getOrElse(List())) ^^ containTheSameElementsAs(Seq(ps:_*))

  def hasExactlyServicePorts(ps: skuber.Service.Port*) = ((_: skuber.Service).spec.map(_.ports).getOrElse(List())) ^^ containTheSameElementsAs(Seq(ps:_*))

  def hasPair(p: (String,String)) = ((_: skuber.ObjectResource).metadata.labels) ^^ havePair(p)

  def hasSelector(p: (String,String)) = ((_: skuber.Service).spec.map(_.selector).getOrElse(Map.empty)) ^^ havePair(p)

  def hasEnv(vs: (String,String)*) = ((_: skuber.ext.Deployment).spec) ^^ beSome(
    ((_: skuber.ext.Deployment.Spec).template) ^^ beSome(
      ((_ : skuber.Pod.Template.Spec).spec) ^^ beSome(
        ((_ : skuber.Pod.Spec).containers.headOption) ^^ beSome(
          ((_ : skuber.Container).env) ^^ containTheSameElementsAs(
            vs.map {
              case (k,v) => skuber.EnvVar(k, skuber.EnvVar.StringValue(v))
            }
          )
        )
      )
    )
  )

  "KubernetesService" should {

    "configure environment variables for containers" in new FakeKube {
      val Success(metaContainer) = createInstance(
        ResourceIds.Container,
        "test-container",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testProvider.id
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

      mockSkuber.create(any)(any,meq(skuber.ext.deploymentKind)) returns Future.successful(mock[skuber.ext.Deployment])

      val Some(updatedContainerProps) = await(ks.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties

      there was one(mockSkuber).create( argThat(
        inNamespace[skuber.ext.Deployment](skTestNs.name)
          and
          hasEnv(
            "VAR1" -> "VAL1",
            "VAR2" -> "VAL2"
          )
      ) )(any,meq(skuber.ext.deploymentKind))
    }

    "deploy service for exposed port mappings and set service addresses and host port" in new FakeKubeCreate {
      val Some(updatedContainerProps) = await(ks.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties

      there was two(mockSkuberFactory).initializeKube(meq(testProvider.id), any)(any)
      there was one(mockSkuber).create( argThat(
        inNamespace[skuber.ext.Deployment](skTestNs.name)
          and
          hasExactlyContainerPorts(
            skuber.Container.Port(80,   skuber.Protocol.TCP, "http"),
            skuber.Container.Port(443,  skuber.Protocol.TCP, "https"),
            skuber.Container.Port(9999, skuber.Protocol.UDP, "debug", hostPort = Some(9999))
          )
      ) )(any,meq(skuber.ext.deploymentKind))
      there was one(mockSkuber).create(argThat(
        inNamespace[skuber.Service](skTestNs.name)
          and
          hasExactlyServicePorts(
            skuber.Service.Port("http",  skuber.Protocol.TCP,   80, Some(skuber.portNumToNameablePort(80))),
            skuber.Service.Port("https", skuber.Protocol.TCP, 8443, Some(skuber.portNumToNameablePort(443)))
          )
          and
          hasPair(KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString)
          and
          hasSelector(KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString)
      ))(any,meq(client.serviceKind))

      import ContainerSpec.PortMapping
      import ContainerSpec.ServiceAddress

      val svcHost = s"${metaContainer.name}.${testEnv.id}.svc.cluster.local"
      updatedContainerProps.get("status") must beSome("LAUNCHED")
      val mappings = Json.parse(updatedContainerProps("port_mappings")).as[Seq[ContainerSpec.PortMapping]]
      mappings must contain(exactly(
        (pm: PortMapping) => pm.name.contains("http")  && pm.host_port.isEmpty && pm.service_address.contains(ServiceAddress(svcHost, 80, Some("tcp"))),
        (pm: PortMapping) => pm.name.contains("https") && pm.host_port.isEmpty && pm.service_address.contains(ServiceAddress(svcHost, 8443, Some("tcp"))),
        (pm: PortMapping) => pm.name.contains("debug") && pm.host_port.contains(9999) && pm.service_address.isEmpty
      ))
    }

    "set PullPolicy Always when force_pull == true" in new FakeKubeCreate(force_pull = true) {
      val Some(updatedContainerProps) = await(ks.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(mockSkuber).create( argThat(
          ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.imagePullPolicy)) ^^ beSome(
            skuber.Container.PullPolicy.Always
          )
      ) )(any,meq(skuber.ext.deploymentKind))
    }

    "set PullPolicy Always when force_pull == false" in new FakeKubeCreate(force_pull = false) {
      val Some(updatedContainerProps) = await(ks.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(mockSkuber).create( argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.imagePullPolicy)) ^^ beSome(
          skuber.Container.PullPolicy.IfNotPresent
        )
      ) )(any,meq(skuber.ext.deploymentKind))
    }

    "pass args when specified" in new FakeKubeCreate(args = Some(Seq("echo","hello","world"))) {
      val Some(updatedContainerProps) = await(ks.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(mockSkuber).create( argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.args)) ^^ beSome(
          containTheSameElementsAs(Seq("echo","hello","world"))
        )
      ) )(any,meq(skuber.ext.deploymentKind))
    }

    "pass no args when unspecified" in new FakeKubeCreate(args = None) {
      val Some(updatedContainerProps) = await(ks.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(mockSkuber).create( argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.args)) ^^ beSome(empty)
      ) )(any,meq(skuber.ext.deploymentKind))
    }

    "pass cmd when specified" in new FakeKubeCreate(cmd = Some("""echo "hello world" arg2""")) {
      val Some(updatedContainerProps) = await(ks.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(mockSkuber).create( argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.command)) ^^ beSome(
          containTheSameElementsAs(Seq("echo","hello world","arg2"))
        )
      ) )(any,meq(skuber.ext.deploymentKind))
    }

    "pass no cmd when unspecified" in new FakeKubeCreate(cmd = None) {
      val Some(updatedContainerProps) = await(ks.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(mockSkuber).create( argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.command)) ^^ beSome(empty)
      ) )(any,meq(skuber.ext.deploymentKind))
    }

    "set appropriate host port on container tasks from kubernetes Service node port" in new FakeKubeCreate {
      import com.galacticfog.gestalt.marathon.ContainerStats

      val Some(containerStats) = await(ks.find(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      ))

      val Seq(containerStats2) = await(ks.listInEnvironment(
        context = ProviderContext(play.api.test.FakeRequest("POST",s"/root/environments/${testEnv.id}/containers"), testProvider.id, None)
      ))

      containerStats.tasksRunning must_== 2
      containerStats.taskStats must beSome(containTheSameElementsAs(Seq(
        ContainerStats.TaskStat(
          id = "test-container-hash-a",
          host = "host-a",
          ipAddresses = Some(Seq(ContainerStats.TaskStat.IPAddress(
            ipAddress = "10.10.10.1",
            protocol = "IPv4"
          ))),
          ports = Seq(30784,31374,0),
          startedAt = Some(startA.toString)
        ),
        ContainerStats.TaskStat(
          id = "test-container-hash-b",
          host = "host-b",
          ipAddresses = Some(Seq(ContainerStats.TaskStat.IPAddress(
            ipAddress = "10.10.10.2",
            protocol = "IPv4"
          ))),
          ports = Seq(30784,31374,0),
          startedAt = Some(startB.toString)
        )
      )))

      containerStats must_== containerStats2
    }

    "delete service on container delete" in new FakeKube {
      val Success(metaContainer) = createInstance(
        ResourceIds.Container,
        "test-container",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> "DOCKER",
          "image" -> "nginx",
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> "1.0",
          "memory" -> "128",
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> "[]",
          "network" -> "default"
        ))
      )

      val label = KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString
      val mockDep = skuber.ext.Deployment(metadata = skuber.ObjectMeta(
        name = "test-container",
        labels = Map(label)
      ))
      val mockRS  = skuber.ext.ReplicaSet("test-container-hash").addLabel( label )
      val mockService = skuber.Service("test-container").withSelector( label ).addLabel( label )
      mockSkuber.list()(any, meq(skuber.ext.deplListKind)) returns Future.successful(skuber.ext.DeploymentList(items = List(mockDep)))
      mockSkuber.list()(any, meq(skuber.ext.replsetListKind)) returns Future.successful(skuber.ext.ReplicaSetList(items = List(mockRS)))
      mockSkuber.list()(any, meq(client.podListKind)) returns Future.successful(skuber.PodList())
      mockSkuber.list()(any, meq(client.serviceListKind)) returns Future.successful(skuber.ServiceList(items = List(mockService)))
      mockSkuber.delete(mockDep.name,0)(skuber.ext.deploymentKind) returns Future.successful(())
      mockSkuber.delete(mockRS.name, 0)(skuber.ext.replsetsKind) returns Future.successful(())
      mockSkuber.delete(mockService.name, 0)(client.serviceKind) returns Future.successful(())

      await(ks.destroyContainer(metaContainer))
      there was one(mockSkuber).delete(mockService.name,0)(client.serviceKind)
    }

    "not fail if no service to delete on container delete" in new FakeKube {
      val Success(metaContainer) = createInstance(
        ResourceIds.Container,
        "test-container",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> "DOCKER",
          "image" -> "nginx",
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> "1.0",
          "memory" -> "128",
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> "[]",
          "network" -> "default"
        ))
      )

      val label = KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString
      val mockDep = skuber.ext.Deployment(metadata = skuber.ObjectMeta(
        name = "test-container",
        labels = Map(label)
      ))
      val mockRS  = skuber.ext.ReplicaSet("test-container-hash").addLabel( label )
      mockSkuber.list()(any, meq(skuber.ext.deplListKind)) returns Future.successful(skuber.ext.DeploymentList(items = List(mockDep)))
      mockSkuber.list()(any, meq(skuber.ext.replsetListKind)) returns Future.successful(skuber.ext.ReplicaSetList(items = List(mockRS)))
      mockSkuber.list()(any, meq(client.podListKind)) returns Future.successful(skuber.PodList())
      mockSkuber.list()(any, meq(client.serviceListKind)) returns Future.successful(skuber.ServiceList())
      mockSkuber.delete(mockDep.name,0)(skuber.ext.deploymentKind) returns Future.successful(())
      mockSkuber.delete(mockRS.name, 0)(skuber.ext.replsetsKind) returns Future.successful(())

      await(ks.destroyContainer(metaContainer))
      there were no(mockSkuber).delete(any,any)(meq(client.serviceKind))
    }

  }

}
