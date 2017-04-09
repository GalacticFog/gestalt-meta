package services

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import controllers.{ContainerController, DeleteController, SecurityResources}
import controllers.util.{ContainerService, ContainerServiceImpl, GestaltSecurityMocking}
import org.joda.time.DateTime
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.matcher.{JsonMatchers, Matcher}
import org.specs2.specification.{BeforeAll, Scope}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.PlaySpecification
import play.api.inject.bind

import scala.concurrent.Future
import scala.util.Success

class ContainerServiceSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll with JsonMatchers {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    pristineDatabase()
    val Success(createdUser) = Ents.createNewMetaUser(user, dummyRootOrgId, user.account,
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

  "findMigrationRule" should {

    "find any container.migrate.* rules that are within the given scope" in new TestApplication {
      val org = newOrg(id = dummyRootOrgId)
      org must beSuccessfulTry

      val data = newDummyEnvironment(dummyRootOrgId)

      ContainerController.findMigrationRule(data("environment")).isEmpty must beTrue

      val (_, rule) = createEventRule(data("environment"), data("lambda"), "container.migrate.pre")
      ResourceFactory.findById(ResourceIds.RuleEvent, rule) must beSome

      val event = ContainerController.findMigrationRule(data("environment"))

      event.isEmpty must beFalse
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

//    "get a container via the ContainerService interface" in new TestApplication {
//
//      val testProps = ContainerSpec(
//        name = "",
//        container_type = "DOCKER",
//        image = "nginx",
//        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
//        port_mappings = Seq(ContainerSpec.PortMapping("tcp", Some(80), None, None, None, None)),
//        cpus = 1.0,
//        memory = 128,
//        disk = 0.0,
//        num_instances = 1,
//        network = Some("BRIDGE"),
//        cmd = None,
//        constraints = Seq(),
//        accepted_resource_roles = None,
//        args = None,
//        force_pull = false,
//        health_checks = Seq(),
//        volumes = Seq(),
//        labels = Map(),
//        env = Map(),
//        user = None)
//
//      val testContainerName = "test-container"
//      val testContainer = createInstance(
//        ResourceIds.Container,
//        testContainerName,
//        parent = Some(testEnv.id),
//        properties = Some(Map(
//          "container_type" -> testProps.container_type,
//          "image" -> testProps.image,
//          "provider" -> Output.renderInstance(testProvider).toString,
//          "cpus" -> testProps.cpus.toString,
//          "memory" -> testProps.memory.toString,
//          "num_instances" -> testProps.num_instances.toString,
//          "force_pull" -> testProps.force_pull.toString,
//          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
//          "network" -> testProps.network.get))).get
//
//      containerService.getEnvironmentContainer("root", testEnv.id, testContainer.id) returns Future(Some(testContainer -> Seq.empty))
//
//      // Set entitlements on new environment for container creation
//      val ces = Ents.setNewEntitlements(dummyRootOrgId, testContainer.id, user, Some(testEnv.id))
//      ces exists { _.isFailure } must beFalse
//
//      containerService.findEnvironmentContainerByName(
//        any[String],
//        any[UUID],
//        any[String]) returns Future(Some(testContainer -> Seq.empty))
//
//      val path = s"/root/environments/${testEnv.id}/containers/${testContainer.id}"
//      val request = fakeAuthRequest(GET, path, testCreds)
//
//      val Some(result) = route(request)
//
//      contentAsString(result) must /("id" -> testContainer.id.toString)
//      status(result) must equalTo(OK)
//    }

//    "list containers via the ContainerService interface" in new TestApplication {
//
//      val testContainerName = "test-container"
//      val testProps = ContainerSpec(
//        name = "",
//        container_type = "DOCKER",
//        image = "nginx",
//        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
//        port_mappings = Seq(ContainerSpec.PortMapping("tcp", Some(80), None, None, None, None)),
//        cpus = 1.0,
//        memory = 128,
//        disk = 0.0,
//        num_instances = 1,
//        network = Some("BRIDGE"),
//        cmd = None,
//        constraints = Seq(),
//        accepted_resource_roles = None,
//        args = None,
//        force_pull = false,
//        health_checks = Seq(),
//        volumes = Seq(),
//        labels = Map(),
//        env = Map(),
//        user = None)
//      val testContainer = createInstance(ResourceIds.Container, testContainerName,
//        parent = Some(testEnv.id),
//        properties = Some(Map(
//          "container_type" -> testProps.container_type,
//          "image" -> testProps.image,
//          "provider" -> Output.renderInstance(testProvider).toString,
//          "cpus" -> testProps.cpus.toString,
//          "memory" -> testProps.memory.toString,
//          "num_instances" -> testProps.num_instances.toString,
//          "force_pull" -> testProps.force_pull.toString,
//          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
//          "network" -> testProps.network.get))).get
//
//      val ces = Ents.setNewEntitlements(
//        dummyRootOrgId, testContainer.id, user, Some(testEnv.id))
//      ces exists { _.isFailure } must beFalse
//
//      containerService.listEnvironmentContainers(
//        "root",
//        testEnv.id) returns Future(Seq(testContainer -> Seq.empty))
//
//      val path = s"/root/environments/${testEnv.id}/containers?expand=true"
//      val request = fakeAuthRequest(GET, path, testCreds)
//
//      val Some(result) = route(request)
//
//      contentAsString(result) must /#(0) / ("id" -> testContainer.id.toString)
//      status(result) must equalTo(OK)
//
//    }

//    "create containers with non-existent provider should 400" in new TestApplication {
//      val testContainerName = "test-container"
//      val testProps = ContainerSpec(
//        name = testContainerName,
//        container_type = "DOCKER",
//        image = "nginx",
//        provider = ContainerSpec.InputProvider(id = UUID.randomUUID())
//      )
//
//      val newResource = newInstance(
//        typeId = ResourceIds.Container,
//        name = testContainerName,
//        properties = Some(Map(
//          "container_type" -> testProps.container_type,
//          "image" -> testProps.image,
//          "provider" -> Json.toJson(testProps.provider).toString
//        ))
//      )
//
//      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/containers", testCreds).withBody(
//        Output.renderInstance(newResource)
//      )
//
//      val Some(result) = route(request)
//
//      status(result) must equalTo(BAD_REQUEST)
//      contentAsString(result) must contain("provider does not exist")
//    }

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

//    "create containers using the ContainerService interface with specific ID" in new TestApplication {
//      val testContainerName = "test-container"
//      val testProps = ContainerSpec(
//        name = testContainerName,
//        container_type = "DOCKER",
//        image = "nginx",
//        provider = ContainerSpec.InputProvider(id = testKubeProvider.id),
//        port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),None,None,None,None)),
//        cpus = 1.0,
//        memory = 128,
//        disk = 0.0,
//        num_instances = 1,
//        network = Some("BRIDGE"),
//        cmd = None,
//        constraints = Seq(),
//        accepted_resource_roles = None,
//        args = None,
//        force_pull = false,
//        health_checks = Seq(),
//        volumes = Seq(),
//        labels = Map(),
//        env = Map(),
//        user = None
//      )
//
//      val userSpecificUUID = uuid()
//      val newResource = newInstance(
//        typeId = ResourceIds.Container,
//        id = userSpecificUUID,
//        name = testContainerName,
//        properties = Some(Map(
//          "container_type" -> testProps.container_type,
//          "image" -> testProps.image,
//          "provider" -> Json.toJson(testProps.provider).toString,
//          "cpus" -> testProps.cpus.toString,
//          "memory" -> testProps.memory.toString,
//          "disk" -> testProps.disk.toString,
//          "num_instances" -> testProps.num_instances.toString,
//          "force_pull" -> testProps.force_pull.toString,
//          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
//          "network" -> testProps.network.get
//        ))
//      )
//
//      mockKubeService.create(any,any)(any) returns Future.successful(newResource)
//
//      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/containers", testCreds).withBody(
//        Output.renderInstance(newResource)
//      )
//      val Some(result) = route(request)
//      status(result) must equalTo(CREATED)
//      val json = contentAsJson(result)
//      (json \ "id").asOpt[UUID] must beSome(userSpecificUUID)
//      (json \ "name").asOpt[String] must beSome(testContainerName)
//      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Container")
//      (json \ "properties").asOpt[ContainerSpec] must beSome(testProps.copy(name = ""))
//
//      there was one(mockKubeService).create(
//        context = argThat(matchesProviderContext(
//          provider = testKubeProvider,
//          workspace = testWork,
//          environment = testEnv
//        )),
//        container = argThat(
//          (c: GestaltResourceInstance) => c.id == userSpecificUUID
//        )
//      )(any)
//    }

//    "delete kube containers using the ContainerService interface" in new TestApplication {
//      val testContainerName = "test-container"
//      val testProps = ContainerSpec(
//        name = testContainerName,
//        container_type = "DOCKER",
//        image = "nginx",
//        provider = ContainerSpec.InputProvider(id = testKubeProvider.id),
//        port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),None,None,None,None)),
//        cpus = 1.0,
//        memory = 128,
//        disk = 0.0,
//        num_instances = 1,
//        network = Some("BRIDGE"),
//        cmd = None,
//        constraints = Seq(),
//        accepted_resource_roles = None,
//        args = None,
//        force_pull = false,
//        health_checks = Seq(),
//        volumes = Seq(),
//        labels = Map(),
//        env = Map(),
//        user = None
//      )
//      val extId = s"/${testEnv.id}/test-container"
//      val createdResource = createInstance(ResourceIds.Container, testContainerName,
//        parent = Some(testEnv.id),
//        properties = Some(Map(
//          "container_type" -> testProps.container_type,
//          "image" -> testProps.image,
//          "provider" -> Output.renderInstance(testKubeProvider).toString,
//          "cpus" -> testProps.cpus.toString,
//          "memory" -> testProps.memory.toString,
//          "disk" -> testProps.disk.toString,
//          "num_instances" -> testProps.num_instances.toString,
//          "force_pull" -> testProps.force_pull.toString,
//          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
//          "network" -> testProps.network.get,
//          "external_id" -> s"${extId}"
//        ))
//      ).get
//
//      mockKubeService.destroyContainer(
//        argThat((c: GestaltResourceInstance) => c.id == createdResource.id)
//      ) returns Future(())
//
//      val request = fakeAuthRequest(DELETE,
//        s"/root/environments/${testEnv.id}/containers/${createdResource.id}", testCreds)
//
//      val Some(result) = route(request)
//
//      status(result) must equalTo(NO_CONTENT)
//
//      there was one(mockKubeService).destroyContainer(
//        argThat(
//          (r: GestaltResourceInstance) => r.id == createdResource.id && r.properties.flatMap(_.get("external_id")).contains(extId)
//        )
//      )
//      there was atLeastOne(providerManager).getProviderImpl(ResourceIds.KubeProvider)
//    }

//    "delete dcos containers using the ContainerService interface" in new TestApplication {
//      val testContainerName = "test-container"
//      val testProps = ContainerSpec(
//        name = testContainerName,
//        container_type = "DOCKER",
//        image = "nginx",
//        provider = ContainerSpec.InputProvider(id = testProvider.id),
//        port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),None,None,None,None)),
//        cpus = 1.0,
//        memory = 128,
//        disk = 0.0,
//        num_instances = 1,
//        network = Some("BRIDGE"),
//        cmd = None,
//        constraints = Seq(),
//        accepted_resource_roles = None,
//        args = None,
//        force_pull = false,
//        health_checks = Seq(),
//        volumes = Seq(),
//        labels = Map(),
//        env = Map(),
//        user = None
//      )
//      val extId = s"/${testEnv.id}/test-container"
//      val createdResource = createInstance(ResourceIds.Container, testContainerName,
//        parent = Some(testEnv.id),
//        properties = Some(Map(
//          "container_type" -> testProps.container_type,
//          "image" -> testProps.image,
//          "provider" -> Output.renderInstance(testProvider).toString,
//          "cpus" -> testProps.cpus.toString,
//          "memory" -> testProps.memory.toString,
//          "disk" -> testProps.disk.toString,
//          "num_instances" -> testProps.num_instances.toString,
//          "force_pull" -> testProps.force_pull.toString,
//          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
//          "network" -> testProps.network.get,
//          "external_id" -> s"${extId}"
//        ))
//      ).get
//
//      mockKubeService.destroyContainer(
//        argThat((c: GestaltResourceInstance) => c.id == createdResource.id)
//      ) returns Future(())
//
//      val request = fakeAuthRequest(DELETE,
//        s"/root/environments/${testEnv.id}/containers/${createdResource.id}", testCreds)
//
//      val Some(result) = route(request)
//
//      status(result) must equalTo(NO_CONTENT)
//
//      there was one(mocKubeSService).destroyContainer(
//        argThat(
//          (r: GestaltResourceInstance) => r.id == createdResource.id && r.properties.flatMap(_.get("external_id")).contains(extId)
//        )
//      )
//      there was atLeastOne(providerManager).getProviderImpl(ResourceIds.DcosProvider)
//    }

//    "scale containers using the ContainerService and CaaSService interfaces" in new TestApplication {
//      val testContainerName = "test-container"
//      val testProps = ContainerSpec(
//        name = testContainerName,
//        container_type = "DOCKER",
//        image = "nginx",
//        provider = ContainerSpec.InputProvider(id = testKubeProvider.id),
//        port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),None,None,None,None)),
//        cpus = 1.0,
//        memory = 128,
//        disk = 0.0,
//        num_instances = 1,
//        network = Some("BRIDGE"),
//        cmd = None,
//        constraints = Seq(),
//        accepted_resource_roles = None,
//        args = None,
//        force_pull = false,
//        health_checks = Seq(),
//        volumes = Seq(),
//        labels = Map(),
//        env = Map(),
//        user = None
//      )
//      val extId = s"/${testEnv.id}/test-container"
//      val createdResource = createInstance(ResourceIds.Container, testContainerName,
//        parent = Some(testEnv.id),
//        properties = Some(Map(
//          "container_type" -> testProps.container_type,
//          "image" -> testProps.image,
//          "provider" -> Output.renderInstance(testKubeProvider).toString,
//          "cpus" -> testProps.cpus.toString,
//          "memory" -> testProps.memory.toString,
//          "disk" -> testProps.disk.toString,
//          "num_instances" -> testProps.num_instances.toString,
//          "force_pull" -> testProps.force_pull.toString,
//          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
//          "network" -> testProps.network.get,
//          "external_id" -> s"${extId}"
//        ))
//      ).get
//
//      mockKubeService.scale(
//        any, any, any
//      ) returns Future(createdResource.copy(
//        properties = Some(createdResource.properties.get ++ Map("num_instances" -> "4"))
//      ))
//
//      val request = fakeAuthRequest(POST,
//        s"/root/environments/${testEnv.id}/containers/${createdResource.id}/scale?numInstances=4", testCreds
//      )
//
//      val Some(result) = route(request)
//
//      status(result) must equalTo(ACCEPTED)
//
//      there was one(mockKubeService).scale(
//        context = argThat(matchesProviderContext(
//          provider = testKubeProvider,
//          workspace = testWork,
//          environment = testEnv
//        )),
//        container = argThat(
//          (r: GestaltResourceInstance) => r.id == createdResource.id
//        ),
//        numInstances = meq(4)
//      )
//      there was atLeastOne(providerManager).getProviderImpl(ResourceIds.KubeProvider)
//    }

  }

}