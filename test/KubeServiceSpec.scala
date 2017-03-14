import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
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
import play.api.test.{FakeRequest, PlaySpecification}
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.{JsonMatchers, Matcher}
import services.{KubernetesService, ProviderContext, SkuberFactory}
import skuber.ObjectResource
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
  }

  def inNamespace[R <: skuber.ObjectResource](name: String): Matcher[R] = { r: R =>
    (r.metadata.namespace == name,  r.name+" in namespace "+name, r.name+" not in namespace "+name)
  }

  def hasExactlyContainerPorts(ps: skuber.Container.Port*) = ((_: skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).map(_.containers.flatMap(_.ports)).getOrElse(List())) ^^ containTheSameElementsAs(Seq(ps:_*))

  def hasExactlyServicePorts(ps: skuber.Service.Port*) = ((_: skuber.Service).spec.map(_.ports).getOrElse(List())) ^^ containTheSameElementsAs(Seq(ps:_*))

  def hasPair(p: (String,String)) = ((_: skuber.ObjectResource).metadata.labels) ^^ havePair(p)

  def hasSelector(p: (String,String)) = ((_: skuber.Service).spec.map(_.selector).getOrElse(Map.empty)) ^^ havePair(p)

  "KubernetesService" should {

    "deploy service for exposed port mappings and set service addresses" in new FakeKube {
      val ks = injector.instanceOf[KubernetesService]

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
        network = Some("BRIDGE"),
        cmd = None,
        constraints = Seq(),
        accepted_resource_roles = None,
        args = None,
        force_pull = false,
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
          "network" -> testProps.network.get)
        )
      )

      mockSkuber.create(any)(any,meq(skuber.ext.deploymentKind)) returns Future.successful(mock[skuber.ext.Deployment])
      mockSkuber.create(any)(any,meq(client.serviceKind)) returns Future.successful(mock[skuber.Service])

      val fupdatedMetaContainer = ks.create(
        context = ProviderContext(FakeRequest("POST",s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )

      val Some(updatedContainerProps) = await(fupdatedMetaContainer).properties

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

      there was one(mockSkuber).create( argThat(
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
      ) )(any,meq(client.serviceKind))

      import ContainerSpec.PortMapping
      import ContainerSpec.ServiceAddress

      val svcHost = s"${metaContainer.name}.${testEnv.id}.svc.cluster.local"
      updatedContainerProps.get("status") must beSome("LAUNCHED")
      val mappings = Json.parse(updatedContainerProps("port_mappings")).as[Seq[ContainerSpec.PortMapping]]
      mappings must contain(exactly(
        (pm: PortMapping) => pm.name == Some("http") && pm.service_address.contains(ServiceAddress(svcHost, 80, Some("tcp"))),
        (pm: PortMapping) => pm.name == Some("https") && pm.service_address.contains(ServiceAddress(svcHost, 8443, Some("tcp"))),
        (pm: PortMapping) => pm.name == Some("debug") && pm.service_address.isEmpty
      ))

    }

    "delete service on container delete" in new FakeKube {
      val ks = injector.instanceOf[KubernetesService]

      val testProps = ContainerSpec(
        name = "",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
        port_mappings = Seq(),
        cpus = 1.0,
        memory = 128,
        disk = 0.0,
        num_instances = 1,
        network = Some("BRIDGE"),
        cmd = None,
        constraints = Seq(),
        accepted_resource_roles = None,
        args = None,
        force_pull = false,
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
          "network" -> testProps.network.get)
        )
      )

      val mockDep = mock[skuber.ext.Deployment]
      mockDep.name returns "deployment-test-container"
      val mockRS  = mock[skuber.ext.ReplicaSet]
      mockRS.name returns "deployment-test-container-12431234"
      val mockService = skuber.Service("test-container").withSelector(
        KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString
      ).addLabel(
        KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString
      )

      mockSkuber.get(meq(mockDep.name))(any, meq(skuber.ext.deploymentKind)) returns Future.successful(mock[skuber.ext.Deployment])
      mockSkuber.list()(any, meq(skuber.ext.replsetListKind)) returns Future.successful(skuber.ext.ReplicaSetList(items = List(mockRS)))
      mockSkuber.list()(any, meq(client.podListKind)) returns Future.successful(skuber.PodList())
      mockSkuber.list()(any, meq(client.serviceListKind)) returns Future.successful(skuber.ServiceList(items = List(mockService)))
      mockSkuber.delete(mockDep.name,0)(skuber.ext.deploymentKind) returns Future.successful(())
      mockSkuber.delete(mockRS.name, 0)(skuber.ext.replsetsKind) returns Future.successful(())
      mockSkuber.delete(mockService.name, 0)(client.serviceKind) returns Future.successful(())

      val fDeleted = ks.destroyContainer( metaContainer )

      await(fDeleted)

      there was one(mockSkuber).delete(mockService.name,0)(client.serviceKind)
    }

  }

}
