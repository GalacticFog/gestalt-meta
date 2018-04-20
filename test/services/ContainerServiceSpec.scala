package services

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.ContainerSpec.{SecretDirMount, SecretEnvMount, SecretFileMount, SecretMount}
import com.galacticfog.gestalt.meta.api.{ContainerSpec, SecretSpec}
import com.galacticfog.gestalt.meta.api.errors.{BadRequestException, ConflictException}
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.policy
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import controllers.{ContainerController, DeleteController, SecurityResources}
import controllers.util.{ContainerService, ContainerServiceImpl, GestaltSecurityMocking, Security}
import org.joda.time.DateTime
import org.specs2.execute.{AsResult, Result}
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.matcher.{JsonMatchers, Matcher}
import org.specs2.mutable.Specification
import org.specs2.specification.{BeforeAll, ForEach}
import org.mockito.Matchers.{eq => meq}
import play.api.http.HttpVerbs
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.FakeRequest

import scala.concurrent.Future
import scala.util.Success

case class TestScope( testWrk: GestaltResourceInstance,
                      testEnv: GestaltResourceInstance,
                      testProvider: GestaltResourceInstance,
                      mockCaasService: CaasService,
                      containerService: ContainerService )

trait TestApplication extends Specification with ForEach[TestScope] with ResourceScope with GestaltSecurityMocking {
  def foreach[R : AsResult](f: TestScope => R): Result = {
    scalikejdbc.config.DBs.setupAll()

    var Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    Entitlements.setNewResourceEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))
    val mockProviderManager  = mock[ProviderManager]
    var testProvider    = createKubernetesProvider(testEnv.id, "test-provider").get
    val mockCaasService = mock[KubernetesService]
    mockProviderManager.getProviderImpl(testProvider.typeId) returns Success(mockCaasService)
    val mockDeleteController = mock[DeleteController]
    val containerService = new ContainerServiceImpl(mockProviderManager, mockDeleteController)
    val security = mock[Security]
    try AsResult(f(TestScope(testWork, testEnv, testProvider, mockCaasService, containerService)))
    finally {
      scalikejdbc.config.DBs.closeAll()
    }
  }
}

class ContainerServiceSpec extends TestApplication with BeforeAll with JsonMatchers with org.specs2.specification.Tables {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    pristineDatabase()
    val Success(_) = Ents.createNewMetaUser(user, dummyRootOrgId, user.account,
      Some(Map(
        "firstName" -> user.account.firstName,
        "lastName" -> user.account.lastName,
        "email" -> user.account.email.getOrElse(""),
        "phoneNumber" -> user.account.phoneNumber.getOrElse("")
      )),
      user.account.description
    )
  }

  sequential

  def matchesProviderContext(provider: GestaltResourceInstance, workspace: GestaltResourceInstance, environment: GestaltResourceInstance): Matcher[ProviderContext] =
    ((_: ProviderContext).workspace.id == workspace.id, (_: ProviderContext).workspace.id.toString + " does not contain the expected workspace resource " + workspace.id) and
    ((_: ProviderContext).environment.id == environment.id, (_: ProviderContext).environment.id.toString + " does not contain the expected environment resource " + environment.id) and
    ((_: ProviderContext).environmentId == environment.id, (_: ProviderContext).environmentId.toString + " does not contain the expected environmentId " + environment.id) and
    ((_: ProviderContext).provider.id == provider.id, (_: ProviderContext).provider.id.toString + " does not contain the expected provider resource " + provider.id) and
    ((_: ProviderContext).providerId == provider.id, (_: ProviderContext).providerId.toString + " does not contain the expected providerId " + provider.id)

  "findPromotionRule" should {

    "find any container.promote.* rules that are within the given scope" >> {
      val org = newOrg(id = dummyRootOrgId)
      org must beSuccessfulTry

      val data = newDummyEnvironment(dummyRootOrgId)

      ContainerController.findPromotionRule(data("environment")).isEmpty must beTrue

      val (_, rule) = createEventRule(data("environment"), data("lambda"), "container.promote.pre")
      ResourceFactory.findById(ResourceIds.RuleEvent, rule) must beSome

      ContainerController.findPromotionRule(data("environment")) must beSome
    }
  }

  "findMigrationRule" should {

    "find any container.migrate.* rules that are within the given scope" >> {
      val org = newOrg(id = dummyRootOrgId)
      org must beSuccessfulTry

      val data = newDummyEnvironment(dummyRootOrgId)

      ContainerController.findMigrationRule(data("environment")).isEmpty must beTrue

      val (_, rule) = createEventRule(data("environment"), data("lambda"), "container.migrate.pre")
      ResourceFactory.findById(ResourceIds.RuleEvent, rule) must beSome

      ContainerController.findMigrationRule(data("environment")) must beSome
    }
  }

  "ContainerSpec.PortMapping" should {

    "be optional in ContainerSpec" in {
      Json.obj(
        "name" -> "test-container",
        "container_type" -> "DOCKER",
        "image" -> "nginx",
        "provider" -> Json.obj("id" -> UUID.randomUUID().toString),
        "port_mappings" -> Json.arr(Json.obj(
          "protocol" -> "tcp"
        ))
      ).validate[ContainerSpec] must beAnInstanceOf[JsSuccess[ContainerSpec]]
    }

    "not validate if port mapping name doesn't satisfy IANA_SVC_NAME" in {
      "reason"                        | "name"             |>
      "longer than 15 characters"     ! "name-length-gt15" |
      "has a period"                  ! "port.name"        |
      "starts with a dash"            ! "-port"            |
      "ends with a dash"              ! "port-"            |
      "contains _"                    ! "port_80"          |
      "contains a space"              ! "port 80"          |
      "is empty"                      ! ""                 |
      "is whitespace"                 ! " "                |
      { (_, name) =>
        Json.obj(
          "protocol" -> "tcp",
          "name" -> name
        ).validate[ContainerSpec.PortMapping] must beAnInstanceOf[JsError]
      }
    }

  }

  "ContainerSpec.SecretMount" should {

    "be optional in ContainerSpec" in {
      val cs = Json.obj(
        "name" -> "test-container",
        "container_type" -> "DOCKER",
        "image" -> "nginx",
        "provider" -> Json.obj("id" -> UUID.randomUUID().toString)
      ).validate[ContainerSpec]
      cs must beAnInstanceOf[JsSuccess[ContainerSpec]]
      cs.get.secrets must beEmpty
    }

    "be parsed when present in ContainerSpec" in {
      val secretMounts = Seq(
        SecretDirMount(UUID.randomUUID(), "/mnt/dir"),
        SecretFileMount(UUID.randomUUID(), "/mnt/dir/file", "secret_key"),
        SecretEnvMount(UUID.randomUUID(), "ENV_VAR", "secret_key")
      )
      val cs = Json.obj(
        "name" -> "test-container",
        "container_type" -> "DOCKER",
        "image" -> "nginx",
        "provider" -> Json.obj("id" -> UUID.randomUUID().toString),
        "secrets" -> Json.toJson(secretMounts)
      ).validate[ContainerSpec]
      cs must beAnInstanceOf[JsSuccess[ContainerSpec]]
      cs.get.secrets must containTheSameElementsAs(secretMounts)
    }

    "read/write SecretEnvMount" in {
      val json = Json.parse(
        """{
          |  "mount_type": "env",
          |  "path": "VAR3",
          |  "secret_id": "c6726f35-1e25-4239-9b20-f7a608ae6886",
          |  "secret_key": "part-b"
          |}
        """.stripMargin
      )
      val sm = json.validate[SecretMount]
      sm must beAnInstanceOf[JsSuccess[SecretMount]]
      sm.get must beAnInstanceOf[SecretEnvMount]
      Json.toJson(sm.get) must_== json
    }

    "read/write SecretFileMount" in {
      val json = Json.parse(
        """{
          |  "secret_id": "c6726f35-1e25-4239-9b20-f7a608ae6886",
          |  "mount_type": "file",
          |  "secret_key": "part-a",
          |  "path": "/mnt/my_secret/my_sub_secret"
          |}
        """.stripMargin
      )
      val sm = json.validate[SecretMount]
      sm must beAnInstanceOf[JsSuccess[SecretMount]]
      sm.get must beAnInstanceOf[SecretFileMount]
      Json.toJson(sm.get) must_== json
    }

    "read/write SecretDirMount" in {
      val json = Json.parse(
        """{
          |  "mount_type": "directory",
          |  "path": "/mnt/my_secret",
          |  "secret_id": "c6726f35-1e25-4239-9b20-f7a608ae6886"
          |}
        """.stripMargin
      )
      val sm = json.validate[SecretMount]
      sm must beAnInstanceOf[JsSuccess[SecretMount]]
      sm.get must beAnInstanceOf[SecretDirMount]
      Json.toJson(sm.get) must_== json
    }

  }

  "ContainerSpec" should {

    "be convertible to GestaltResourceInput with all fields" >> { t : TestScope =>
      val TestScope(testWrk, testEnv, testProvider, mockCaasService, containerService) = t
      val testProps = ContainerSpec(
        name = "test-container",
        description = Some("container description"),
        container_type = "DOCKER",
        image = "nginx:alpine",
        provider = ContainerSpec.InputProvider(id = uuid),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(80), name = Some("web")),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(443), name = Some("secure-web"))
        ),
        cpus = 1.0,
        memory = 128,
        disk = 0.0,
        num_instances = 1,
        network = Some("some-network"),
        cmd = Some("echo I am Groot! && sleep 3600"),
        constraints = Seq("hostname:UNIQUE"),
        accepted_resource_roles = Some(Seq("*")),
        args = Some(Seq("args","are","not","normally","valid","with","cmd")),
        force_pull = true,
        health_checks = Seq(),
        volumes = Seq(),
        labels = Map("blah" -> "blergh"),
        env = Map("foo" -> "bar"),
        user = Some("root")
      )
      val resourceInput = ContainerSpec.toResourcePrototype(testProps)
      resourceInput.description must beSome("container description")
      val protoProps = resourceInput.properties.get
      protoProps.get("image") must beSome(Json.toJson(testProps.image))
      protoProps.get("container_type") must beSome(Json.toJson(testProps.container_type))
      protoProps.get("cpus") must beSome(Json.toJson(testProps.cpus))
      protoProps.get("memory") must beSome(Json.toJson(testProps.memory))
      protoProps.get("disk") must beSome(Json.toJson(testProps.disk))
      protoProps.get("num_instances") must beSome(Json.toJson(testProps.num_instances))
      protoProps.get("network") must beSome(Json.toJson(testProps.network.get))
      protoProps.get("cmd") must beSome(Json.toJson(testProps.cmd.get))
      protoProps.get("constraints") must beSome(Json.toJson(testProps.constraints))
      protoProps.get("port_mappings") must beSome(Json.toJson(testProps.port_mappings))
      protoProps.get("accepted_resource_roles") must beSome(Json.toJson(testProps.accepted_resource_roles.get))
      protoProps.get("args") must beSome(Json.toJson(testProps.args.get))
      protoProps.get("force_pull") must beSome(Json.toJson(testProps.force_pull))
      protoProps.get("health_checks") must beSome(Json.toJson(testProps.health_checks))
      protoProps.get("volumes") must beSome(Json.toJson(testProps.volumes))
      protoProps.get("labels") must beSome(Json.toJson(testProps.labels))
      protoProps.get("env") must beSome(Json.toJson(testProps.env))
      protoProps.get("user") must beSome(Json.toJson(testProps.user.get))
    }

    "be convertible to GestaltResourceInput with default fields" >> { t : TestScope =>
      val TestScope(testWrk, testEnv, testProvider, mockCaasService, containerService) = t
      val testProps = ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:alpine",
        provider = ContainerSpec.InputProvider(id = uuid)
      )
      val resourceInput = ContainerSpec.toResourcePrototype(testProps)
      val protoProps = resourceInput.properties.get
      protoProps.get("image") must beSome(Json.toJson(testProps.image))
      protoProps.get("container_type") must beSome(Json.toJson(testProps.container_type))
      protoProps.get("cpus") must beSome(Json.toJson(testProps.cpus))
      protoProps.get("memory") must beSome(Json.toJson(testProps.memory))
      protoProps.get("disk") must beSome(Json.toJson(testProps.disk))
      protoProps.get("num_instances") must beSome(Json.toJson(testProps.num_instances))
      protoProps.get("network") must_== testProps.network
      protoProps.get("description") must_== testProps.description
      protoProps.get("cmd") must_== testProps.cmd
      protoProps.get("constraints") must beSome(Json.toJson(testProps.constraints))
      protoProps.get("accepted_resource_roles") must_== testProps.accepted_resource_roles
      protoProps.get("args") must_== testProps.args
      protoProps.get("force_pull") must beSome(Json.toJson(testProps.force_pull))
      protoProps.get("health_checks") must beSome(Json.toJson(testProps.health_checks))
      protoProps.get("volumes") must beSome(Json.toJson(testProps.volumes))
      protoProps.get("labels") must beSome(Json.toJson(testProps.labels))
      protoProps.get("env") must beSome(Json.toJson(testProps.env))
      protoProps.get("user") must_== testProps.user
    }

    "be convertible from GestaltResourceInstance container representation" >> { t : TestScope =>
      val TestScope(testWrk, testEnv, testProvider, mockCaasService, containerService) = t
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        description = Some("container description"),
        container_type = "DOCKER",
        image = "nginx:alpine",
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
        port_mappings = Seq(),
        cpus = 1.0,
        memory = 128,
        disk = 0.0,
        num_instances = 1,
        network = None,
        cmd = None,
        constraints = Seq(),
        accepted_resource_roles = None,
        args = None,
        force_pull = false,
        health_checks = Seq(),
        volumes = Seq(),
        labels = Map("SOME" -> "LABEL"),
        env = Map(),
        user = None
      )
      val createdResource = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEnv.id),
        description = Some("container description"),
        properties = Some(Map(
          "container_type" -> "DOCKER",
          "provider" -> Output.renderInstance(testProvider).toString,
          "image" -> testProps.image,
          "labels" -> Json.obj("SOME" -> "LABEL").toString
        ))
      ).get
      val created = DateTime.parse(createdResource.created.get("timestamp"))
      ContainerSpec.fromResourceInstance(createdResource) must beSuccessfulTry(testProps.copy(
        created = Some(created)
      ))
    }

    "throw an exception if attempting to create from non-Container resource" >> { t : TestScope =>
      val TestScope(testWrk, testEnv, testProvider, mockCaasService, containerService) = t
      val testResourceName = "some-name"
      val createdResource = createInstance(ResourceIds.Org, testResourceName,
        parent = Some(testEnv.id),
        properties = None
      ).get
      ContainerSpec.fromResourceInstance(createdResource) must beFailedTry.withThrowable[RuntimeException]("cannot convert non-Container resource into ContainerSpec")
    }

  }


  "ContainerService" should {

    "create containers using CaaSService interface" >> { t : TestScope =>
      val TestScope(testWrk, testEnv, testProvider, mockCaasService, containerService) = t
      val testContainerName = "test-container"
      val testSpec = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp", Some(80), None, None, None, None)),
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

      mockCaasService.create(any,any)(any) answers {
        (a: Any) =>
          val arr = a.asInstanceOf[Array[Object]]
          Future.successful(arr(1).asInstanceOf[GestaltResourceInstance])
      }

      val createdContainer = await(containerService.createContainer(
        context = ProviderContext(FakeURI(s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        user = user,
        containerSpec = testSpec,
        userRequestedId = None
      ))

      ResourceFactory.findParent(createdContainer.id) must beSome(
        (r: GestaltResourceInstance) => r.typeId == ResourceIds.Environment && r.id == testEnv.id
      )

      there was one(mockCaasService).create(
        context = argThat(matchesProviderContext(
          provider = testProvider,
          workspace = testWrk,
          environment = testEnv
        )),
        container = any
      )(any)
    }

    "enforce environment container limits" >> { t : TestScope =>
      val TestScope(testWrk, testEnv, testProvider, mockCaasService, containerService) = t
      val testContainerName = "test-container"
      val testSpec = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp", Some(80), None, None, None, None)),
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
      val Success(_) = createInstance(
        ResourceIds.Policy,
        org = dummyRootOrgId,
        name = "test-limit",
        properties = Option(Map(
          "parent" -> Json.stringify(Json.obj("parent" -> Json.obj("id" -> testEnv.id.toString)))
        )),
        parent = Option(testEnv.id)
      ) map { policy =>
        createInstance(ResourceIds.RuleLimit,
          org = dummyRootOrgId,
          name = "test-rule",
          parent = Option(policy.id),
          properties = Option(Map(
            "match_actions"    -> Json.toJson(List("container.create")).toString,
            "eval_logic" -> Json.obj(
              "property" -> "containers.count",
              "operator" -> "<=",
              "value" -> 0
            ).toString,
            "parent" -> policy.id.toString,
            "defined_at" -> policy.properties.get("parent")
          ))
        )
      }

      // have 0 instances in environment, want to create 1, policy only allows zero => fail
      await(containerService.createContainer(
        context = ProviderContext(FakeURI(s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        user = user,
        containerSpec = testSpec,
        userRequestedId = None
      )) must throwAn[ConflictException]

      there were no(mockCaasService).create(
        context = any,
        container = any
      )(any)
    }

    "enforce accumulative environment container limits" >> { t : TestScope =>
      val TestScope(testWrk, testEnv, testProvider, mockCaasService, containerService) = t
      val testContainerName = "test-container"
      val testSpec = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp", Some(80), None, None, None, None)),
        cpus = 1.0,
        memory = 128,
        disk = 0.0,
        num_instances = 2,
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
      val Success(_) = createInstance(
        ResourceIds.Policy,
        org = dummyRootOrgId,
        name = "test-limit",
        properties = Option(Map(
          "parent" -> Json.stringify(Json.obj("parent" -> Json.obj("id" -> testEnv.id.toString)))
        )),
        parent = Option(testEnv.id)
      ) map { policy =>
        createInstance(ResourceIds.RuleLimit,
          org = dummyRootOrgId,
          name = "test-rule",
          parent = Option(policy.id),
          properties = Option(Map(
            "match_actions"    -> Json.toJson(List("container.create")).toString,
            "eval_logic" -> Json.obj(
              "property" -> "containers.count",
              "operator" -> "<=",
              "value" -> 3
            ).toString,
            "parent" -> policy.id.toString,
            "defined_at" -> policy.properties.get("parent")
          ))
        )
      }
      val Success(_) = createInstance(
        ResourceIds.Container,
        org = dummyRootOrgId,
        name = "pre-existing-container",
        properties = Option(Map(
          "num_instances" -> "2",
          "provider" -> "",
          "image" -> "",
          "container_type" -> ""
        )),
        parent = Option(testEnv.id)
      )
      // have 2 instances in environment, want to create 2 more, policy only allows three => fail
      val Success(_) = createInstance(
        ResourceIds.Policy,
        org = dummyRootOrgId,
        name = "test-limit",
        properties = Option(Map("parent" ->
          Json.stringify(Json.obj("parent" -> Json.obj("id" -> testEnv.id.toString))))),
        parent = Option(testEnv.id)
      ) map { policy =>
        createInstance(ResourceIds.RuleLimit,
          org = dummyRootOrgId,
          name = "test-rule",
          parent = Option(policy.id),
          properties = Option(Map(
            "match_actions"    -> Json.toJson(List("container.create")).toString,
            "eval_logic" -> Json.obj(
              "property" -> "containers.count",
              "operator" -> "<=",
              "value" -> 3
            ).toString,
            "parent"     -> policy.id.toString
          ))
        )
      }

      await(containerService.createContainer(
        context = ProviderContext(FakeURI(s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        user = user,
        containerSpec = testSpec,
        userRequestedId = None
      )) must throwAn[ConflictException]

      there were no(mockCaasService).create(
        context = any,
        container = any
      )(any)
    }

    "throw 400 on bad provider during container creation" >> { t : TestScope =>
      val TestScope(testWrk, testEnv, testProvider, mockCaasService, containerService) = t
      val badProvider = UUID.randomUUID()
      val testContainerName = "test-container"
      val testSpec = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = badProvider),
        network = Some("BRIDGE")
      )

      containerService.createContainer(
        context = ProviderContext(FakeURI(s"/root/environments/${testEnv.id}/containers"), badProvider, None),
        user = user,
        containerSpec = testSpec,
        userRequestedId = None
      ) must throwAn[BadRequestException]("is absent or not a recognized CaaS provider")
    }

    "throw 400 if patching a container with the same name" >> { t : TestScope =>
      val TestScope(testWrk, testEnv, testProvider, mockCaasService, containerService) = t
      val Success(testContainer) = createInstance(
        ResourceIds.Container,
        "test-container",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> "DOCKER",
          "image" -> "nginx",
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> "1.0",
          "memory" -> "1024",
          "disk" -> "0",
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> "[]",
          "network" -> "default",
          "external_id" -> s"/${testEnv.id}/test-container"
        ))
      )

      val patchDoc = PatchDocument(PatchOp.Replace("/name", "updated-name"))

      await(containerService.patchContainer(
        container = testContainer,
        patch = patchDoc,
        user = user,
        request = FakeRequest(HttpVerbs.PATCH, s"/root/environments/${testEnv.id}/containers/${testContainer.id}")
      )) must throwAn[BadRequestException]("renaming containers is not supported")
    }

    "not throw 400 if patching a container with the same name" >> { t : TestScope =>
      val TestScope(testWrk, testEnv, testProvider, mockCaasService, containerService) = t
      val Success(testContainer) = createInstance(
        ResourceIds.Container,
        "test-container",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> "DOCKER",
          "image" -> "nginx",
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> "1.0",
          "memory" -> "1024",
          "disk" -> "0",
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> "[]",
          "network" -> "default",
          "external_id" -> s"/${testEnv.id}/test-container"
        ))
      )

      mockCaasService.update(any,any)(any) answers {
        (a: Any) =>
          val arr = a.asInstanceOf[Array[Object]]
          Future.successful(arr(1).asInstanceOf[GestaltResourceInstance])
      }

      val patchDoc = PatchDocument(PatchOp.Replace("/name", testContainer.name))

      val updatedContainer = await(containerService.patchContainer(
        container = testContainer,
        patch = patchDoc,
        user = user,
        request = FakeRequest(HttpVerbs.PATCH, s"/root/environments/${testEnv.id}/containers/${testContainer.id}")
      ))

      there was one(mockCaasService).update(
        context = argThat(matchesProviderContext(
          provider = testProvider,
          workspace = testWrk,
          environment = testEnv
        )),
        container = any
      )(any)
    }

    "patch containers using CaaSService interface" >> { t : TestScope =>
      val TestScope(testWrk, testEnv, testProvider, mockCaasService, containerService) = t
      val Success(testContainer) = createInstance(
        ResourceIds.Container,
        "test-container",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> "DOCKER",
          "image" -> "nginx",
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> "1.0",
          "memory" -> "1024",
          "disk" -> "0",
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> "[]",
          "network" -> "default",
          "external_id" -> s"/${testEnv.id}/test-container"
        ))
      )

      mockCaasService.update(any,any)(any) answers {
        (a: Any) =>
          val arr = a.asInstanceOf[Array[Object]]
          Future.successful(arr(1).asInstanceOf[GestaltResourceInstance])
      }

      val patchDoc = PatchDocument(PatchOp.Replace("/properties/image", "nginx:upgrade"))

      val updatedContainer = await(containerService.patchContainer(
        container = testContainer,
        patch = patchDoc,
        user = user,
        request = FakeRequest(HttpVerbs.PATCH, s"/root/environments/${testEnv.id}/containers/${testContainer.id}")
      ))

      there was one(mockCaasService).update(
        context = argThat(matchesProviderContext(
          provider = testProvider,
          workspace = testWrk,
          environment = testEnv
        )),
        container = any
      )(any)
    }

    "create secrets using CaaSService interface" >> { t : TestScope =>
      val TestScope(testWrk, testEnv, testProvider, mockCaasService, containerService) = t
      val testSecretName = "test-container"
      val testItems = Seq(
        SecretSpec.Item("item-a", Some("c2hoaGho")),
        SecretSpec.Item("item-b", Some("dGhpcyBpcyBhIHNlY3JldA=="))
      )
      val testSpec = SecretSpec(
        name = testSecretName,
        provider = ContainerSpec.InputProvider(id = testProvider.id),
        items = testItems
      )

      mockCaasService.createSecret(any,any,any)(any) answers {
        (a: Any) =>
          val arr = a.asInstanceOf[Array[Object]]
          Future.successful(arr(1).asInstanceOf[GestaltResourceInstance])
      }

      val createdSecret = await(containerService.createSecret(
        context = ProviderContext(FakeURI(s"/root/environments/${testEnv.id}/secrets"), testProvider.id, None),
        user = user,
        secretSpec = testSpec,
        userRequestedId = None
      ))

      there was one(mockCaasService).createSecret(
        context = argThat(matchesProviderContext(
          provider = testProvider,
          workspace = testWrk,
          environment = testEnv
        )),
        metaResource = any,
        items = meq(testItems)
      )(any)

      ResourceFactory.findParent(createdSecret.id) must beSome(
        (r: GestaltResourceInstance) => r.typeId == ResourceIds.Environment && r.id == testEnv.id
      )

      val Some(metaSecret) = ResourceFactory.findById(ResourceIds.Secret, createdSecret.id)
      Json.parse(metaSecret.properties.get.get("items").get).as[Seq[SecretSpec.Item]] must containTheSameElementsAs(testItems.map(_.copy(value = None)))
    }

    "throw 400 on bad provider during secret creation" >> { t : TestScope =>
      val TestScope(testWrk, testEnv, testProvider, mockCaasService, containerService) = t
      val badProvider = UUID.randomUUID()
      val testSecretName = "test-container"
      val testSpec = SecretSpec(
        name = testSecretName,
        provider = ContainerSpec.InputProvider(id = badProvider),
        items = Seq.empty
      )

      containerService.createSecret(
        context = ProviderContext(FakeURI(s"/root/environments/${testEnv.id}/secrets"), badProvider, None),
        user = user,
        secretSpec = testSpec,
        userRequestedId = None
      ) must throwAn[BadRequestException]("is absent or not a recognized CaaS provider")
    }

  }

}
