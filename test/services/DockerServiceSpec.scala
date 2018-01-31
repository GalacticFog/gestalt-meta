package services

import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.google.common.collect.{ImmutableList, ImmutableMap}
import com.spotify.docker.client.messages._
import com.spotify.docker.client.messages.swarm.PortConfig.PortConfigPublishMode
import com.spotify.docker.client.messages.swarm._
import controllers.SecurityResources
import controllers.util.GestaltSecurityMocking
import org.junit.runner.RunWith
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.JsonMatchers
import org.specs2.specification._
import play.api.libs.json.Json
import play.api.test.PlaySpecification

import collection.JavaConverters._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import services.DockerService.DockerClient

import scala.concurrent.duration._
import scala.concurrent.Await.result
import scala.util.Success
import scala.language.postfixOps

class DockerServiceSpec extends PlaySpecification with ResourceScope with BeforeAll with BeforeAfterEach with JsonMatchers {

  override def beforeAll(): Unit = pristineDatabase()

  override def before: Unit = scalikejdbc.config.DBs.setupAll()

  override def after: Unit = scalikejdbc.config.DBs.closeAll()

  sequential

  case class TestSetup(dockerService: DockerService,
                       dockerClient: DockerClient,
                       createdContainerProperties: Option[Hstore] )

  abstract class FakeDockerWithCreate(name: String = "test-container",
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
                                      labels: Map[String,String] = Map.empty ) extends Scope {

    lazy val testAuthResponse = GestaltSecurityMocking.dummyAuthResponseWithCreds()
    lazy val testCreds = testAuthResponse.creds
    lazy val user = AuthAccountWithCreds(testAuthResponse.account, Seq.empty, Seq.empty, testCreds, dummyRootOrgId)

    lazy val (testWork, testEnv) = {
      val (w,e) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment").get
      Entitlements.setNewResourceEntitlements(dummyRootOrgId, e.id, user, Some(w.id))
      (w,e)
    }

    lazy val testProvider = createDockerProvider(testEnv.id, "test-provider").get

    lazy val testProps = ContainerSpec(
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

      args = args,
      force_pull = force_pull,
      health_checks = Seq(),
      volumes = Seq(),
      labels = labels,
      env = env.getOrElse(Map.empty),
      user = None
    )

    lazy val Success(metaContainer) = createInstance(
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

    lazy val testSetup = {
      val dockerClient = mock[DockerClient]
      val origExtId = s"${testEnv.id}-${testProps.name}"
      dockerClient.inspectContainer(origExtId) returns mock[ContainerInfo] // objectMapper.readValue[ContainerInfo]("", classOf[ContainerInfo])
      dockerClient.createContainer(any) returns mock[ContainerCreation]

      val dockerFactory = mock[DockerClientFactory]
      dockerFactory.getDockerClient(testProvider.id) returns Success(dockerClient)

      val dockerService = new DockerService(dockerFactory)

      val Some(updatedContainerProps) = result(dockerService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      ), 5.seconds).properties

      TestSetup(dockerService, dockerClient, Some(updatedContainerProps))
    }

  }

  abstract class FakeDocker() extends Scope {
    lazy val testAuthResponse = GestaltSecurityMocking.dummyAuthResponseWithCreds()
    lazy val testCreds = testAuthResponse.creds
    lazy val user = AuthAccountWithCreds(testAuthResponse.account, Seq.empty, Seq.empty, testCreds, dummyRootOrgId)

    lazy val (testWork, testEnv) = {
      val (w,e) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment").get
      Entitlements.setNewResourceEntitlements(dummyRootOrgId, e.id, user, Some(w.id))
      (w,e)
    }

    lazy val testProvider = createDockerProvider(testEnv.id, "test-provider").get

    lazy val testSetup = {
      val dockerClient = mock[DockerClient]
      val dockerFactory = {
        val df = mock[DockerClientFactory]
        df.getDockerClient(testProvider.id) returns Success(dockerClient)
        df
      }
      val dockerService = new DockerService(dockerFactory)
      TestSetup(dockerService, dockerClient, None)
    }
  }

  "DockerService" should {

    "configure environment variables for containers" in new FakeDockerWithCreate(
      env = Some(Map(
        "TEST_VAR_1" -> "TEST_VAL_1",
        "TEST_VAR_2" -> "TEST_VAL_2"
      ))
    ) {
      Json.parse(testSetup.createdContainerProperties.get("env")).as[Map[String,String]] must havePairs(
        "TEST_VAR_1" -> "TEST_VAL_1",
        "TEST_VAR_2" -> "TEST_VAL_2"
      )
      there was one(testSetup.dockerClient).createService(
        (((_:swarm.ServiceSpec).taskTemplate().containerSpec().env().asScala) ^^ containTheSameElementsAs(Seq(
          "TEST_VAR_1=TEST_VAL_1",
          "TEST_VAR_2=TEST_VAL_2"
        )))
      )
    }

    "provision with the expected external_id property and meta-specific labels" in new FakeDockerWithCreate() {
      testSetup.createdContainerProperties.get must havePair(
        "external_id" -> s"${testEnv.id.toString.replace("-","")}-test-container"
      )
      there was one(testSetup.dockerClient).createService(
        ((_:swarm.ServiceSpec).labels().asScala) ^^ havePairs(
          DockerService.META_CONTAINER_KEY -> metaContainer.id.toString,
          DockerService.META_ENVIRONMENT_KEY -> testEnv.id.toString,
          DockerService.META_WORKSPACE_KEY -> testWork.id.toString,
          DockerService.META_FQON_KEY -> "root",
          DockerService.META_PROVIDER_KEY -> testProvider.id.toString
        )
      )
    }

    "provision with the short external_id for long container names" in new FakeDockerWithCreate(name = "this-name-is-longer-than-the-allowable-sixty-three-characters") {
      testSetup.createdContainerProperties.get must havePair(
        "external_id" -> s"${testEnv.id.toString.replace("-","")}-this-name-is-longer-thfc815c7f"
      )
      there was one(testSetup.dockerClient).createService(
        ((_:swarm.ServiceSpec).labels().asScala) ^^ havePairs(
          DockerService.META_CONTAINER_KEY -> metaContainer.id.toString,
          DockerService.META_ENVIRONMENT_KEY -> testEnv.id.toString,
          DockerService.META_WORKSPACE_KEY -> testWork.id.toString,
          DockerService.META_FQON_KEY -> "root",
          DockerService.META_PROVIDER_KEY -> testProvider.id.toString
        ) and ((_:swarm.ServiceSpec).name().length) ^^ beLessThanOrEqualTo(63)
      )
    }

    "provision with the requested labels" in new FakeDockerWithCreate(labels = Map(
      "labela" -> "value a",
      "labelb" -> "value b"
    )) {
      there was one(testSetup.dockerClient).createService(
        ((_:swarm.ServiceSpec).labels().asScala) ^^ havePairs(
          "labela" -> "value a",
          "labelb" -> "value b"
        )
      )
    }

    "provision with the appropriate replication" in new FakeDockerWithCreate(num_instances = 2) {
      there was one(testSetup.dockerClient).createService(
        ((_:swarm.ServiceSpec).mode().replicated().replicas()) ^^ be_==(2)
      )
    }

    "provision with the appropriate cpu share" in new FakeDockerWithCreate(cpus = 2.0) {
      there was one(testSetup.dockerClient).createService(
        ((_:swarm.ServiceSpec).taskTemplate().resources().limits().nanoCpus()) ^^ be_==(2000000000)
      )
    }.pendingUntilFixed("figure out what to do for resource limits/reservations")

    "provision with the appropriate memory share" in new FakeDockerWithCreate(memory = 128.5) {
      there was one(testSetup.dockerClient).createService(
        ((_:swarm.ServiceSpec).taskTemplate().resources().limits().memoryBytes()) ^^ be_==(1024.toLong * 1024 * 128.5)
      )
    }.pendingUntilFixed("figure out what to do for resource limits/reservations")

    "provision the appropriate image" in new FakeDockerWithCreate(
      image = "nginx:test"
    ) {
      there was one(testSetup.dockerClient).createService(
        ((_:swarm.ServiceSpec).taskTemplate().containerSpec().image()) ^^ be_==("nginx:test")
      )
    }

    "handle force_pull == false" in new FakeDockerWithCreate(force_pull = true) {
      ko("fail")
    }.pendingUntilFixed("force_pull == false is not supported by docker: https://github.com/moby/moby/issues/24066")

    "pass args when specified" in new FakeDockerWithCreate(args = Some(Seq("echo","hello","world"))) {
      there was one(testSetup.dockerClient).createService(
        ((_:swarm.ServiceSpec).taskTemplate().containerSpec().args().asScala) ^^ be_==(Seq("echo","hello","world"))
      )
    }

    "pass no args when unspecified" in new FakeDockerWithCreate(args = None) {
      there was one(testSetup.dockerClient).createService(
        ((_:swarm.ServiceSpec).taskTemplate().containerSpec().args()) ^^ beNull
      )
    }

    "pass simple cmd when specified" in new FakeDockerWithCreate(cmd = Some("/usr/bin/python")) {
      there was one(testSetup.dockerClient).createService(
        ((_:swarm.ServiceSpec).taskTemplate().containerSpec().command().asScala) ^^ be_==(Seq("/bin/sh","-c","/usr/bin/python"))
      )
    }

    "pass complicated cmd when specified" in new FakeDockerWithCreate(cmd = Some("python -m SimpleHTTPServer $PORT")) {
      there was one(testSetup.dockerClient).createService(
        ((_:swarm.ServiceSpec).taskTemplate().containerSpec().command().asScala) ^^ be_==(Seq("/bin/sh", "-c", "python -m SimpleHTTPServer $PORT"))
      )
    }

    "pass no cmd when not specified" in new FakeDockerWithCreate(cmd = None) {
      there was one(testSetup.dockerClient).createService(
        ((_:swarm.ServiceSpec).taskTemplate().containerSpec().command()) ^^ beNull
      )
    }

    "configure port exposures for port mappings" in new FakeDockerWithCreate(port_mappings = Seq(
      ContainerSpec.PortMapping("tcp", container_port = Some(80), name = Some("web"), expose_endpoint = Some(true)),
      ContainerSpec.PortMapping("tcp", container_port = Some(443), name = Some("ssl"), service_port = Some(8443), expose_endpoint = Some(true)),
      ContainerSpec.PortMapping("udp", container_port = Some(9998), name = Some("debug"), expose_endpoint = Some(false)),
      ContainerSpec.PortMapping("udp", container_port = Some(9999), name = Some("debug2"), expose_endpoint = None)
    )) {
      import ContainerSpec.PortMapping
      import ContainerSpec.ServiceAddress

      there was one(testSetup.dockerClient).createService(
        (((_: swarm.ServiceSpec).endpointSpec().mode()) ^^ be_==(EndpointSpec.Mode.RESOLUTION_MODE_VIP))
          and
          (((_: swarm.ServiceSpec).endpointSpec().ports().asScala) ^^ containTheSameElementsAs(Seq(
            PortConfig.builder().name("web").protocol(PortConfig.PROTOCOL_TCP).targetPort(80).publishMode(PortConfigPublishMode.INGRESS).build(),
            PortConfig.builder().name("ssl").protocol(PortConfig.PROTOCOL_TCP).targetPort(443).publishedPort(8443).publishMode(PortConfigPublishMode.INGRESS).build()
          )))
      )

      val mappings = Json.parse(testSetup.createdContainerProperties.get("port_mappings")).as[Seq[ContainerSpec.PortMapping]]
      mappings must containTheSameElementsAs(Seq(
        PortMapping(protocol = "tcp", container_port = Some(80), name = Some("web"), expose_endpoint = Some(true),
          service_address = Some(ServiceAddress(testEnv.id.toString.replace("-", "") + "-" + metaContainer.name, 80, Some("tcp")))
        ),
        PortMapping(protocol = "tcp", container_port = Some(443), name = Some("ssl"), service_port = Some(8443), expose_endpoint = Some(true),
          service_address = Some(ServiceAddress(testEnv.id.toString.replace("-", "") + "-" + metaContainer.name, 443, Some("tcp")))
        ),
        PortMapping(protocol = "udp", container_port = Some(9998), name = Some("debug"), expose_endpoint = Some(false)),
        PortMapping(protocol = "udp", container_port = Some(9999), name = Some("debug2"), expose_endpoint = None)
      ))
    }

    "configure for the selected network" in new FakeDockerWithCreate(
      network = Some("test-network")
    ) {
      there was one(testSetup.dockerClient).createService(
        (((_:swarm.ServiceSpec).networks().asScala) ^^ containTheSameElementsAs(Seq(
          NetworkAttachmentConfig.builder().target("test-network").build()
        )))
      )
    }

    "configure for no network if not selected" in new FakeDockerWithCreate(
      network = None
    ) {
      there was one(testSetup.dockerClient).createService(
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
      await(testSetup.dockerService.destroy(metaContainer))
      there were one(testSetup.dockerClient).removeService(testEnv.id.toString.replace("-","") + "-test-container")
    }

    "find service successfully" in new FakeDocker() {
      val extId = s"${testEnv.id.toString.replace("-","")}-test-container"
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
          "network" -> "default",
          "external_id" -> extId
        ))
      )

      val mockSvcSpec = mock[swarm.ServiceSpec]
      mockSvcSpec.name() returns extId
      val mockSvc = mock[swarm.Service]
      mockSvc.createdAt() returns java.util.Date.from(java.time.Instant.now())
      mockSvc.spec() returns mockSvcSpec
      testSetup.dockerClient.inspectService(extId) returns mockSvc

      val stats = await(testSetup.dockerService.find(
        container = metaContainer,
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None)
      ))
      stats must beSome
//      there were one(testSetup.dockerClient).inspectService()
    }

  }

}
