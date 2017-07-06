package services

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import controllers.{ContainerController, DeleteController, SecurityResources}
import controllers.util.{ContainerService, ContainerServiceImpl, GestaltSecurityMocking}
import org.joda.time.DateTime
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.matcher.{JsonMatchers, Matcher}
import org.specs2.specification.{BeforeAll, Scope}
import play.api.http.HttpVerbs
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.{FakeRequest, PlaySpecification}
import play.api.inject.bind

import scala.concurrent.Future
import scala.util.Success

class ContainerServiceSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll with JsonMatchers {

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

  abstract class FakeCaaSScope extends Scope {
    var Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    Entitlements.setNewEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))
    val mockProviderManager  = mock[ProviderManager]
    var testProvider    = createKubernetesProvider(testEnv.id, "test-provider").get
    val mockCaasService = mock[KubernetesService]
    mockProviderManager.getProviderImpl(testProvider.typeId) returns Success(mockCaasService)
    val mockDeleteController = mock[DeleteController]

    val injector =
      new GuiceApplicationBuilder()
        .disable[modules.ProdSecurityModule]
        .disable[modules.MetaDefaultSkuber]
        .disable[modules.MetaDefaultServices]
        .disable[modules.HealthModule]
        .bindings(
          bind(classOf[ProviderManager]).toInstance(mockProviderManager),
          bind(classOf[ContainerService]).to(classOf[ContainerServiceImpl]),
          bind(classOf[DeleteController]).toInstance(mockDeleteController),
          bind(classOf[GestaltSecurityConfig]).toInstance(mock[GestaltSecurityConfig])
        )
        .injector
  }

  def matchesProviderContext(provider: GestaltResourceInstance, workspace: GestaltResourceInstance, environment: GestaltResourceInstance): Matcher[ProviderContext] =
    ((_: ProviderContext).workspace.id == workspace.id, (_: ProviderContext).workspace.id.toString + " does not contain the expected workspace resource " + workspace.id) and
    ((_: ProviderContext).environment.id == environment.id, (_: ProviderContext).environment.id.toString + " does not contain the expected environment resource " + environment.id) and
    ((_: ProviderContext).environmentId == environment.id, (_: ProviderContext).environmentId.toString + " does not contain the expected environmentId " + environment.id) and
    ((_: ProviderContext).provider.id == provider.id, (_: ProviderContext).provider.id.toString + " does not contain the expected provider resource " + provider.id) and
    ((_: ProviderContext).providerId == provider.id, (_: ProviderContext).providerId.toString + " does not contain the expected providerId " + provider.id)

  trait TestApplication extends FakeCaaSScope {
  }

  "findPromotionRule" should {

    "find any container.promote.* rules that are within the given scope" in new TestApplication {
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

    "find any container.migrate.* rules that are within the given scope" in new TestApplication {
      val org = newOrg(id = dummyRootOrgId)
      org must beSuccessfulTry

      val data = newDummyEnvironment(dummyRootOrgId)

      ContainerController.findMigrationRule(data("environment")).isEmpty must beTrue

      val (_, rule) = createEventRule(data("environment"), data("lambda"), "container.migrate.pre")
      ResourceFactory.findById(ResourceIds.RuleEvent, rule) must beSome

      ContainerController.findMigrationRule(data("environment")) must beSome
    }
  }

  "ContainerSpec" should {

    "be convertible to GestaltResourceInput with all fields" in {
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

    "be convertible to GestaltResourceInput with default fields" in {
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

    "be convertible from GestaltResourceInstance container representation" in new TestApplication {
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

    "throw an exception if attempting to create from non-Container resource" in new TestApplication {
      val testResourceName = "some-name"
      val createdResource = createInstance(ResourceIds.Org, testResourceName,
        parent = Some(testEnv.id),
        properties = None
      ).get
      ContainerSpec.fromResourceInstance(createdResource) must beFailedTry.withThrowable[RuntimeException]("cannot convert non-Container resource into ContainerSpec")
    }

  }


  "ContainerService" should {

    "create containers using CaaSService interface" in new TestApplication {
      val containerService = injector.instanceOf[ContainerService]
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
          workspace = testWork,
          environment = testEnv
        )),
        container = any
      )(any)
    }

    "throw 400 on bad provider during container creation" in new TestApplication {
      val badProvider = UUID.randomUUID()
      val containerService = injector.instanceOf[ContainerService]
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

    "throw 400 if patching a container with the same name" in new TestApplication {
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

      val containerService = injector.instanceOf[ContainerService]

      val patchDoc = PatchDocument(PatchOp.Replace("/name", "updated-name"))

      await(containerService.patchContainer(
        container = testContainer,
        patch = patchDoc,
        user = user,
        request = FakeRequest(HttpVerbs.PATCH, s"/root/environments/${testEnv.id}/containers/${testContainer.id}")
      )) must throwAn[BadRequestException]("renaming containers is not supported")
    }

    "not throw 400 if patching a container with the same name" in new TestApplication {
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

      val containerService = injector.instanceOf[ContainerService]

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
          workspace = testWork,
          environment = testEnv
        )),
        container = any
      )(any)
    }

    "patch containers using CaaSService interface" in new TestApplication {
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

      val containerService = injector.instanceOf[ContainerService]

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
          workspace = testWork,
          environment = testEnv
        )),
        container = any
      )(any)
    }

  }

}