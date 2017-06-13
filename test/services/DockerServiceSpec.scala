package services

import com.fasterxml.jackson.databind.ObjectMapper
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import com.google.inject.AbstractModule
import com.spotify.docker.client.messages._
import controllers.util.GestaltSecurityMocking
import org.junit.runner.RunWith
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.{JsonMatchers, Matcher}
import org.specs2.mock.Mockito
import org.specs2.runner.JUnitRunner
import org.specs2.specification.{BeforeAll, Scope}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.PlaySpecification
import collection.JavaConverters._

import scala.concurrent.Future
import play.api.inject.bind
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import services.DockerService.DockerClient

import scala.util.Success

@RunWith(classOf[JUnitRunner])
class DockerServiceSpec extends PlaySpecification with ResourceScope with BeforeAll
  with Mockito with GestaltSecurityMocking with JsonMatchers {

  override def beforeAll(): Unit = pristineDatabase()

  case class FakeDockerModule(mockDockerClientFactory: DockerClientFactory) extends AbstractModule {
    override def configure(): Unit = {
      bind(classOf[DockerClientFactory]).toInstance(mockDockerClientFactory)
    }
  }

  abstract class FakeDocker() extends Scope {
    val Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    Entitlements.setNewEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))
    val testProvider = createDockerProvider(testEnv.id, "test-provider").get

    val mockDocker = mock[DockerClient]
    val mockDockerFactory = mock[DockerClientFactory]
    mockDockerFactory.getDockerClient(testProvider.id) returns Success(mockDocker)

    val injector =
      new GuiceApplicationBuilder()
        .disable[modules.ProdSecurityModule]
        .disable[modules.MetaDefaultSkuber]
        .disable[modules.MetaDefaultServices]
        .disable[modules.HealthModule]
        .bindings(
          FakeDockerModule(mockDockerFactory),
          bind(classOf[GestaltSecurityConfig]).toInstance(mock[GestaltSecurityConfig])
        )
        .injector
    val ds = injector.instanceOf[DockerService]
  }

  abstract class FakeDockerCreate( force_pull: Boolean = true,
                                   env: Option[Map[String,String]] = None,
                                   args: Option[Seq[String]] = None,
                                   cmd: Option[String] = None,
                                   virtual_hosts: Map[Int,Seq[String]] = Map.empty,
                                   labels: Map[String,String] = Map.empty ) extends FakeDocker() {

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
          expose_endpoint = Some(true),
          virtual_hosts = virtual_hosts.get(0)
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(443),
          host_port = None,
          service_port = Some(8443),
          name = Some("https"),
          labels = None,
          expose_endpoint = Some(true),
          virtual_hosts = virtual_hosts.get(1)
        ),
        ContainerSpec.PortMapping(
          protocol = "udp",
          container_port = Some(9999),
          host_port = Some(9999),
          service_port = None,
          name = Some("debug"),
          labels = None,
          expose_endpoint = None,
          virtual_hosts = virtual_hosts.get(2)
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
      labels = labels,
      env = env.getOrElse(Map.empty),
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
        "env" -> Json.toJson(testProps.env).toString,
        "num_instances" -> testProps.num_instances.toString,
        "force_pull" -> testProps.force_pull.toString,
        "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
        "network" -> testProps.network.get,
        "labels" -> Json.toJson(labels).toString
      ) ++ Seq[Option[(String,String)]](
        args map ("args" -> Json.toJson(_).toString),
        cmd  map ("cmd" -> _)
      ).flatten.toMap)
    )

    val origExtId = s"${testEnv.id}-${testProps.name}"

    val lbls = Map(DockerService.META_CONTAINER_KEY -> metaContainer.id.toString)

    val objectMapper = new ObjectMapper()
    mockDocker.inspectContainer(origExtId) returns mock[ContainerInfo] // objectMapper.readValue[ContainerInfo]("", classOf[ContainerInfo])
    mockDocker.createContainer(any) returns mock[ContainerCreation]
  }

  "DockerService" should {

    "configure environment variables for containers" in new FakeDocker {
      ko("write me")
    }.pendingUntilFixed

    "provision with the expected external_id property and meta-specific labels" in new FakeDockerCreate() {
      val Some(updatedContainerProps) = await(ds.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      updatedContainerProps must havePair(
        "external_id" -> s"${testEnv.id}-test-container"
      )
      there was one(mockDocker).createService(
        ((_:swarm.ServiceSpec).labels().asScala) ^^ havePairs(
          DockerService.META_CONTAINER_KEY -> metaContainer.id.toString,
          DockerService.META_ENVIRONMENT_KEY -> testEnv.id.toString,
          DockerService.META_WORKSPACE_KEY -> testWork.id.toString,
          DockerService.META_FQON_KEY -> "root",
          DockerService.META_PROVIDER_KEY -> testProvider.id.toString
        )
      )
    }

    "provision with the requested labels" in new FakeDockerCreate(labels = Map(
      "labela" -> "value a",
      "labelb" -> "value b"
    )) {
      val Some(updatedContainerProps) = await(ds.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(mockDocker).createService(
        ((_:swarm.ServiceSpec).labels().asScala) ^^ havePairs(
          "labela" -> "value a",
          "labelb" -> "value b"
        )
      )
    }

    "set PullPolicy Always when force_pull == true" in new FakeDockerCreate(force_pull = true) {
      ko("write me")
//      val Some(updatedContainerProps) = await(ds.create(
//        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
//        container = metaContainer
//      )).properties
//      there was one(mockSkuber).create(argThat(
//        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.imagePullPolicy)) ^^ beSome(
//          skuber.Container.PullPolicy.Always
//        )
//      ))(any,meq(skuber.ext.deploymentKind))
    }.pendingUntilFixed

    "set PullPolicy Always when force_pull == false" in new FakeDockerCreate(force_pull = false) {
      ko("write me")
//      val Some(updatedContainerProps) = await(ds.create(
//        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
//        container = metaContainer
//      )).properties
//      there was one(mockSkuber).create(argThat(
//        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.imagePullPolicy)) ^^ beSome(
//          skuber.Container.PullPolicy.IfNotPresent
//        )
//      ))(any,meq(skuber.ext.deploymentKind))
    }.pendingUntilFixed

    "pass args when specified" in new FakeDockerCreate(args = Some(Seq("echo","hello","world"))) {
      ko("write me")
//      val Some(updatedContainerProps) = await(ds.create(
//        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
//        container = metaContainer
//      )).properties
//      there was one(mockSkuber).create(argThat(
//        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.args)) ^^ beSome(
//          containTheSameElementsAs(Seq("echo","hello","world"))
//        )
//      ))(any,meq(skuber.ext.deploymentKind))
    }.pendingUntilFixed

    "pass no args when unspecified" in new FakeDockerCreate(args = None) {
      ko("write me")
//      val Some(updatedContainerProps) = await(ds.create(
//        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
//        container = metaContainer
//      )).properties
//      there was one(mockSkuber).create(argThat(
//        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.args)) ^^ beSome(empty)
//      ))(any,meq(skuber.ext.deploymentKind))
    }.pendingUntilFixed

    "pass simple cmd when specified" in new FakeDockerCreate(cmd = Some("""/usr/bin/python""")) {
      ko("write me")
//      val Some(updatedContainerProps) = await(ds.create(
//        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
//        container = metaContainer
//      )).properties
//      there was one(mockSkuber).create(argThat(
//        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.command)) ^^ beSome(
//          containTheSameElementsAs(Seq("/usr/bin/python"))
//        )
//      ))(any,meq(skuber.ext.deploymentKind))
    }.pendingUntilFixed

    "pass complicated cmd when specified" in new FakeDockerCreate(cmd = Some("python -m SimpleHTTPServer $PORT")) {
      ko("write me")
//      val Some(updatedContainerProps) = await(ds.create(
//        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
//        container = metaContainer
//      )).properties
//      there was one(mockSkuber).create(argThat(
//        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.command)) ^^ beSome(
//          containTheSameElementsAs(Seq("python","-m","SimpleHTTPServer","$PORT"))
//        )
//      ))(any,meq(skuber.ext.deploymentKind))
    }.pendingUntilFixed

    "pass complicated cmd with difficult bash-compatible spacing" in new FakeDockerCreate(cmd = Some("echo hello|wc")) {
      ko("write me")
//      val Some(updatedContainerProps) = await(ds.create(
//        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
//        container = metaContainer
//      )).properties
//      there was one(mockSkuber).create(argThat(
//        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.command)) ^^ beSome(
//          containTheSameElementsAs(Seq("echo","hello","|","wc"))
//        )
//      ))(any,meq(skuber.ext.deploymentKind))
    }.pendingUntilFixed("this is going to be hard")

    "pass no cmd when unspecified" in new FakeDockerCreate(cmd = None) {
      ko("write me")
//      val Some(updatedContainerProps) = await(ds.create(
//        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
//        container = metaContainer
//      )).properties
//      there was one(mockSkuber).create(argThat(
//        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.command)) ^^ beSome(empty)
//      ))(any,meq(skuber.ext.deploymentKind))
    }.pendingUntilFixed

  }

}
