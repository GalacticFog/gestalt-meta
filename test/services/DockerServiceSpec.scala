package services

import com.fasterxml.jackson.databind.ObjectMapper
import com.galacticfog.gestalt.data.bootstrap.Bootstrap
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceOwnerLink}
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import com.google.inject.AbstractModule
import com.spotify.docker.client.messages._
import com.spotify.docker.client.messages.swarm.PortConfig.PortConfigPublishMode
import com.spotify.docker.client.messages.swarm.{EndpointSpec, NetworkAttachmentConfig, PortConfig}
import controllers.util.{DataStore, GestaltSecurityMocking}
import org.junit.runner.RunWith
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.JsonMatchers
import org.specs2.mock.Mockito
import org.specs2.runner.JUnitRunner
import org.specs2.specification.{BeforeAll, Scope}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.PlaySpecification

import collection.JavaConverters._
import play.api.inject.bind
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import services.DockerService.DockerClient

import scala.util.Success

@RunWith(classOf[JUnitRunner])
class DockerServiceSpec extends PlaySpecification with ResourceScope
  with Mockito with GestaltSecurityMocking with JsonMatchers {

  sequential

  val dataStore = {
    println("creating a datastore in the test")
    new GuiceApplicationBuilder()
      .disable[modules.ProdSecurityModule]
      .disable[modules.MetaDefaultSkuber]
      .disable[modules.MetaDefaultServices]
      .disable[modules.HealthModule]
      .disable[modules.MetaDefaultDocker]
      .build().injector.instanceOf[DataStore]
  }

  val owner = ResourceOwnerLink(ResourceIds.User, adminUserId.toString())
  val db = new Bootstrap(ResourceIds.Org,
    dummyRootOrgId, dummyRootOrgId, owner, dataStore.dataSource)

  for {
    a <- db.clean
    b <- db.migrate
    c <- db.loadReferenceData
    d <- db.loadSystemTypes
    e <- db.initialize("root")
  } yield e

  val Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
  Entitlements.setNewEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

  val testNetworkName = "test-network"

  val testProvider = createDockerProvider(testEnv.id, "test-provider").get

  case class FakeDockerModule(mockDockerClientFactory: DockerClientFactory) extends AbstractModule {
    override def configure(): Unit = {
      bind(classOf[DockerClientFactory]).toInstance(mockDockerClientFactory)
    }
  }

  abstract class FakeDocker() extends Scope {
    val mockDockerFactory = mock[DockerClientFactory]

    val injector =
      new GuiceApplicationBuilder()
        .disable[modules.ProdSecurityModule]
        .disable[modules.MetaDefaultSkuber]
        .disable[modules.MetaDefaultServices]
        .disable[modules.HealthModule]
        .disable[modules.MetaDefaultDocker]
        .bindings(
          FakeDockerModule(mockDockerFactory),
          bind[GestaltSecurityConfig].toInstance(mock[GestaltSecurityConfig]),
          bind[DataStore].toInstance(dataStore)
        )
        .injector
    val ds = injector.instanceOf[DockerService]

    val mockDocker = mock[DockerClient]
    mockDockerFactory.getDockerClient(testProvider.id) returns Success(mockDocker)
  }

  abstract class FakeDockerCreate( name: String = "test-container",
                                   image: String = "nginx",
                                   num_instances: Int = 1,
                                   force_pull: Boolean = true,
                                   env: Option[Map[String,String]] = None,
                                   cpus: Double = 1.0,
                                   network: Option[String] = None,
                                   memory: Double = 128.0,
                                   args: Option[Seq[String]] = None,
                                   cmd: Option[String] = None,
                                   port_mappings: Seq[ContainerSpec.PortMapping] = Seq.empty,
                                   labels: Map[String,String] = Map.empty ) extends FakeDocker() {

    val testProps = ContainerSpec(
      name = name,
      container_type = "DOCKER",
      image = image,
      provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
      port_mappings = port_mappings,
      cpus = cpus,
      memory = memory,
      disk = 0.0,
      num_instances = num_instances,
      network = network,
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
      typeId = ResourceIds.Container,
      name = testProps.name,
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
        "labels" -> Json.toJson(labels).toString
      ) ++ Seq[Option[(String,String)]](
        args map ("args" -> Json.toJson(_).toString),
        cmd  map ("cmd" -> _),
        network map ("network" -> _)
      ).flatten.toMap)
    )

    val origExtId = s"${testEnv.id}-${testProps.name}"

    val lbls = Map(DockerService.META_CONTAINER_KEY -> metaContainer.id.toString)

    val objectMapper = new ObjectMapper()
    mockDocker.inspectContainer(origExtId) returns mock[ContainerInfo] // objectMapper.readValue[ContainerInfo]("", classOf[ContainerInfo])
    mockDocker.createContainer(any) returns mock[ContainerCreation]

    val Some(updatedContainerProps) = await(ds.create(
      context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
      container = metaContainer
    )).properties
  }

  "DockerService" should {

    "configure environment variables for containers" in new FakeDockerCreate(
      env = Some(Map(
        "TEST_VAR_1" -> "TEST_VAL_1",
        "TEST_VAR_2" -> "TEST_VAL_2"
      ))
    ) {
      Json.parse(updatedContainerProps.get("env").get).as[Map[String,String]] must havePairs(
        "TEST_VAR_1" -> "TEST_VAL_1",
        "TEST_VAR_2" -> "TEST_VAL_2"
      )
      there was one(mockDocker).createService(
        (((_:swarm.ServiceSpec).taskTemplate().containerSpec().env().asScala) ^^ containTheSameElementsAs(Seq(
          "TEST_VAR_1=TEST_VAL_1",
          "TEST_VAR_2=TEST_VAL_2"
        )))
      )
    }

    "provision with the expected external_id property and meta-specific labels" in new FakeDockerCreate() {
      updatedContainerProps must havePair(
        "external_id" -> s"${testEnv.id.toString.replace("-","")}-test-container"
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

    "provision with the short external_id for long container names" in new FakeDockerCreate(name = "this-name-is-longer-than-the-allowable-sixty-three-characters") {
      updatedContainerProps must havePair(
        "external_id" -> s"${testEnv.id.toString.replace("-","")}-this-name-is-longer-thfc815c7f"
      )
      there was one(mockDocker).createService(
        ((_:swarm.ServiceSpec).labels().asScala) ^^ havePairs(
          DockerService.META_CONTAINER_KEY -> metaContainer.id.toString,
          DockerService.META_ENVIRONMENT_KEY -> testEnv.id.toString,
          DockerService.META_WORKSPACE_KEY -> testWork.id.toString,
          DockerService.META_FQON_KEY -> "root",
          DockerService.META_PROVIDER_KEY -> testProvider.id.toString
        ) and ((_:swarm.ServiceSpec).name().length) ^^ beLessThanOrEqualTo(63)
      )
    }

    "provision with the requested labels" in new FakeDockerCreate(labels = Map(
      "labela" -> "value a",
      "labelb" -> "value b"
    )) {
      there was one(mockDocker).createService(
        ((_:swarm.ServiceSpec).labels().asScala) ^^ havePairs(
          "labela" -> "value a",
          "labelb" -> "value b"
        )
      )
    }

    "provision with the appropriate replication" in new FakeDockerCreate(num_instances = 2) {
      there was one(mockDocker).createService(
        ((_:swarm.ServiceSpec).mode().replicated().replicas()) ^^ be_==(2)
      )
    }

    "provision with the appropriate cpu share" in new FakeDockerCreate(cpus = 2.0) {
      there was one(mockDocker).createService(
        ((_:swarm.ServiceSpec).taskTemplate().resources().limits().nanoCpus()) ^^ be_==(2000000000)
      )
    }

    "provision with the appropriate memory share" in new FakeDockerCreate(memory = 128.5) {
      there was one(mockDocker).createService(
        ((_:swarm.ServiceSpec).taskTemplate().resources().limits().memoryBytes()) ^^ be_==(1024.toLong * 1024 * 128.5)
      )
    }

    "provision the appropriate image" in new FakeDockerCreate(
      image = "nginx:test"
    ) {
      there was one(mockDocker).createService(
        ((_:swarm.ServiceSpec).taskTemplate().containerSpec().image()) ^^ be_==("nginx:test")
      )
    }

    "handle force_pull == false" in new FakeDockerCreate(force_pull = true) {
      ko("fail")
    }.pendingUntilFixed("force_pull == false is not supported by docker: https://github.com/moby/moby/issues/24066")

    "pass args when specified" in new FakeDockerCreate(args = Some(Seq("echo","hello","world"))) {
      there was one(mockDocker).createService(
        ((_:swarm.ServiceSpec).taskTemplate().containerSpec().args().asScala) ^^ be_==(Seq("echo","hello","world"))
      )
    }

    "pass no args when unspecified" in new FakeDockerCreate(args = None) {
      there was one(mockDocker).createService(
        ((_:swarm.ServiceSpec).taskTemplate().containerSpec().args()) ^^ beNull
      )
    }

    "pass simple cmd when specified" in new FakeDockerCreate(cmd = Some("/usr/bin/python")) {
      there was one(mockDocker).createService(
        ((_:swarm.ServiceSpec).taskTemplate().containerSpec().command().asScala) ^^ be_==(Seq("/usr/bin/python"))
      )
    }

    "pass complicated cmd when specified" in new FakeDockerCreate(cmd = Some("python -m SimpleHTTPServer $PORT")) {
      there was one(mockDocker).createService(
        ((_:swarm.ServiceSpec).taskTemplate().containerSpec().command().asScala) ^^ be_==(Seq("python","-m","SimpleHTTPServer","$PORT"))
      )
    }

    "pass no cmd when not specified" in new FakeDockerCreate(cmd = None) {
      there was one(mockDocker).createService(
        ((_:swarm.ServiceSpec).taskTemplate().containerSpec().command()) ^^ beNull
      )
    }

    "pass complicated cmd with difficult bash-compatible spacing" in new FakeDockerCreate(cmd = Some("echo hello|wc")) {
      there was one(mockDocker).createService(
        ((_:swarm.ServiceSpec).taskTemplate().containerSpec().command().asScala) ^^ be_==(Seq("echo","hello","|","wc"))
      )
    }.pendingUntilFixed("this is going to be hard")

    "configure port exposures for port mappings" in new FakeDockerCreate(port_mappings = Seq(
      ContainerSpec.PortMapping("tcp", container_port = Some(80), name = Some("web"), expose_endpoint = Some(true)),
      ContainerSpec.PortMapping("tcp", container_port = Some(443), name = Some("ssl"), service_port = Some(8443), expose_endpoint = Some(true)),
      ContainerSpec.PortMapping("udp", container_port = Some(9998), name = Some("debug"), expose_endpoint = Some(false)),
      ContainerSpec.PortMapping("udp", container_port = Some(9999), name = Some("debug2"), expose_endpoint = None)
    )) {
      import ContainerSpec.PortMapping
      import ContainerSpec.ServiceAddress

      there was one(mockDocker).createService(
        (((_:swarm.ServiceSpec).endpointSpec().mode()) ^^ be_==(EndpointSpec.Mode.RESOLUTION_MODE_VIP))
          and
          (((_:swarm.ServiceSpec).endpointSpec().ports().asScala) ^^ containTheSameElementsAs(Seq(
            PortConfig.builder().name("web").protocol(PortConfig.PROTOCOL_TCP).targetPort(80).publishMode(PortConfigPublishMode.INGRESS).build(),
            PortConfig.builder().name("ssl").protocol(PortConfig.PROTOCOL_TCP).targetPort(443).publishedPort(8443).publishMode(PortConfigPublishMode.INGRESS).build()
          )))
      )

      val mappings = Json.parse(updatedContainerProps("port_mappings")).as[Seq[ContainerSpec.PortMapping]]
      mappings must containTheSameElementsAs(Seq(
        PortMapping(protocol = "tcp", container_port = Some(80), name = Some("web"), expose_endpoint = Some(true),
          service_address = Some(ServiceAddress(testEnv.id.toString.replace("-","") + "-" + metaContainer.name, 80, Some("tcp")))
        ),
        PortMapping(protocol = "tcp", container_port = Some(443), name = Some("ssl"), service_port = Some(8443), expose_endpoint = Some(true),
          service_address = Some(ServiceAddress(testEnv.id.toString.replace("-","") + "-" + metaContainer.name, 443, Some("tcp")))
        ),
        PortMapping(protocol = "udp", container_port = Some(9998), name = Some("debug"), expose_endpoint = Some(false)),
        PortMapping(protocol = "udp", container_port = Some(9999), name = Some("debug2"), expose_endpoint = None)
      ))
    }

    "configure for the selected network" in new FakeDockerCreate(
      network = Some("test-network")
    ) {
      there was one(mockDocker).createService(
        (((_:swarm.ServiceSpec).networks().asScala) ^^ containTheSameElementsAs(Seq(
          NetworkAttachmentConfig.builder().target("test-network").build()
        )))
      )
    }

    "configure for no network if not selected" in new FakeDockerCreate(
      network = None
    ) {
      there was one(mockDocker).createService(
        (((_:swarm.ServiceSpec).networks().asScala) ^^ beEmpty)
      )
    }

    "delete service" in new FakeDocker() {
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
      await(ds.destroy(metaContainer))
      there were one(mockDocker).removeService(testEnv.id.toString.replace("-","") + "-test-container")
    }

  }

}
