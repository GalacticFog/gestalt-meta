package controllers

import java.util.UUID

import scala.concurrent.Future
import scala.util.Success
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.{JsonMatchers, Matcher}
import org.specs2.matcher.ValueCheck.typedValueCheck
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import controllers.util.ContainerService
import org.specs2.execute.{AsResult, Result}
import play.api.inject.guice.GuiceableModule
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.Json
import play.api.test.{PlaySpecification, WithApplication}
import services.{CaasService, MarathonClientFactory, ProviderContext, SkuberFactory}
import play.api.inject.bind

class ContainerControllerSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {

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

  def appWithMocks() = application(additionalBindings = Seq(
    bind(classOf[ContainerService]).toInstance(mock[ContainerService]),
    bind(classOf[ProviderManager]).toInstance(mock[ProviderManager]),
    bind(classOf[MarathonClientFactory]).toInstance(mock[MarathonClientFactory]),
    bind(classOf[SkuberFactory]).toInstance(mock[SkuberFactory])
  ))

  abstract class TestContainerController extends WithApplication(appWithMocks()) {
    var testWork: GestaltResourceInstance = null
    var testEnv: GestaltResourceInstance = null
    var containerService: ContainerService = null
    var providerManager: ProviderManager = null

    var testProvider: GestaltResourceInstance = null
    var mockCaasService: CaasService = null

    override def around[T: AsResult](t: => T): Result = super.around {
      scalikejdbc.config.DBs.setupAll()

      val Success(wrkEnv) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
      testWork = wrkEnv._1
      testEnv = wrkEnv._2

      Ents.setNewEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

      val injector = app.injector
      containerService = injector.instanceOf[ContainerService]
      providerManager = injector.instanceOf[ProviderManager]

      testProvider = createKubernetesProvider(testEnv.id, "test-kube-provider").get
      mockCaasService = mock[CaasService]
      providerManager.getProviderImpl(testProvider.typeId) returns Success(mockCaasService)
      t
    }
  }

  def matchesProviderContext(provider: GestaltResourceInstance, workspace: GestaltResourceInstance, environment: GestaltResourceInstance): Matcher[ProviderContext] =
    ((_: ProviderContext).workspace.id == workspace.id, (_: ProviderContext).workspace.id.toString + " does not contain the expected workspace resource " + workspace.id) and
    ((_: ProviderContext).environment.id == environment.id, (_: ProviderContext).environment.id.toString + " does not contain the expected environment resource " + environment.id) and
    ((_: ProviderContext).environmentId == environment.id, (_: ProviderContext).environmentId.toString + " does not contain the expected environmentId " + environment.id) and
    ((_: ProviderContext).provider.id == provider.id, (_: ProviderContext).provider.id.toString + " does not contain the expected provider resource " + provider.id) and
    ((_: ProviderContext).providerId == provider.id, (_: ProviderContext).providerId.toString + " does not contain the expected providerId " + provider.id)

  def hasName(name: => String): Matcher[GestaltResourceInstance] =
    ((_: GestaltResourceInstance).name) ^^ be_==(name)

  def hasDescription(desc: => String): Matcher[GestaltResourceInstance] =
    ((_: GestaltResourceInstance).description) ^^ beSome(desc)

  def hasProperties(props: (String,String)*): Matcher[GestaltResourceInstance] =
    ((_: GestaltResourceInstance).properties.get) ^^ havePairs(props:_*)

  def hasId(id: => UUID): Matcher[GestaltResourceInstance] =
    ((_: GestaltResourceInstance).id) ^^ be_==(id)

  "ContainerController" should {

    "get a container via the ContainerService interface" in new TestContainerController {

      val testProps = ContainerSpec(
        name = "",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
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
        user = None)
      val testContainerName = "test-container"
      val testContainer = createInstance(
        ResourceIds.Container,
        testContainerName,
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
          "network" -> testProps.network.get))).get

      containerService.getEnvironmentContainer("root", testEnv.id, testContainer.id) returns Future(Some(testContainer -> Seq.empty))

      // Set entitlements on new environment for container creation
      val ces = Ents.setNewEntitlements(dummyRootOrgId, testContainer.id, user, Some(testEnv.id))
      ces exists { _.isFailure } must beFalse

      val path = s"/root/environments/${testEnv.id}/containers/${testContainer.id}"
      val request = fakeAuthRequest(GET, path, testCreds)

      val Some(result) = route(request)

      contentAsString(result) must /("id" -> testContainer.id.toString)
      status(result) must equalTo(OK)
    }

    "update a container via the ContainerService interface with minimal input" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        cpus = .2,
        memory = 512,
        num_instances = 1,
        provider = ContainerSpec.InputProvider(id = testProvider.id)
      )
      val extId = s"/${testEnv.id}/test-container"
      val Success(createdResource) = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "disk" -> testProps.disk.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "external_id" -> s"${extId}"
        ))
      )
      containerService.updateContainer(
        any,
        hasId(createdResource.id),
        any,
        any
      ) answers {
        (a: Any) =>
          val arr = a.asInstanceOf[Array[Object]]
          val r = arr(1).asInstanceOf[GestaltResourceInstance]
          Future.successful(r)
      }

      val request = fakeAuthRequest(PUT,
        s"/root/containers/${createdResource.id}", testCreds
      ).withBody(
        Json.obj(
          "name" -> "updated-name",
          "description" -> "updated description",
          "properties" -> Json.obj(
            "env" ->  Json.obj(),
            "labels" ->  Json.obj(),
            "volumes" -> Json.arr(),
            "port_mappings" ->  Json.arr(),
            "health_checks" ->  Json.arr(),
            "container_type" ->  "DOCKER",
            "cpus" ->  2.0,
            "memory" ->  2048.0,
            "num_instances" -> 4,
            "network" -> "NEW-NETWORK",
            "image" -> "nginx:updated"
          )
        )
      )
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      there was one(containerService).updateContainer(
        any,
        (
          hasId(createdResource.id) and
          hasName("updated-name") and
          hasDescription("updated description") and
          hasProperties(
            "image" -> "nginx:updated",
            "cpus" -> "2",
            "memory" -> "2048",
            "num_instances" -> "4"
          )
        ),
        any,
        any
      )
    }

    "ignore system fields on container patch" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id)
      )
      val extId = s"/${testEnv.id}/test-container"
      val Success(createdResource) = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "disk" -> testProps.disk.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "external_id" -> s"${extId}"
        ))
      )
      containerService.updateContainer(
        any,
        hasId(createdResource.id),
        any,
        any
      ) answers {
        (a: Any) =>
          val arr = a.asInstanceOf[Array[Object]]
          val r = arr(1).asInstanceOf[GestaltResourceInstance]
          Future.successful(r)
      }

      val request = fakeAuthRequest(PUT,
        s"/root/containers/${createdResource.id}", testCreds
      ).withBody(
        Json.obj(
          "id" -> UUID.randomUUID().toString,
          "name" ->  "new-name",
          "description" -> "new description",
          "properties" -> Json.obj(
            "image" -> "nginx:updated",
            "container_type" -> "DOCKER",
            "external_id" -> "cannot-update-external_id",
            "provider" -> Json.obj(
              "id" -> UUID.randomUUID().toString,
              "name" -> "cannot-update-provider"
            )
          )
        )
      )
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      there was one(containerService).updateContainer(
        any,
        (
          hasId(createdResource.id) and
          hasProperties(
            "external_id" -> extId,
            "provider" -> Output.renderInstance(testProvider).toString
          )
        ),
        any,
        any
      )
    }

    "list containers via the ContainerService interface" in new TestContainerController {

      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = "",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
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
        user = None)
      val testContainer = createInstance(ResourceIds.Container, testContainerName,
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
          "network" -> testProps.network.get))).get

      val ces = Ents.setNewEntitlements(
        dummyRootOrgId, testContainer.id, user, Some(testEnv.id))
      ces exists { _.isFailure } must beFalse

      containerService.listEnvironmentContainers(
        "root",
        testEnv.id) returns Future(Seq(testContainer -> Seq.empty))

      val path = s"/root/environments/${testEnv.id}/containers?expand=true"
      val request = fakeAuthRequest(GET, path, testCreds)

      val Some(result) = route(request)

      contentAsString(result) must /#(0) / ("id" -> testContainer.id.toString)
      status(result) must equalTo(OK)

    }

    "create containers with non-existent provider should 400" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = UUID.randomUUID())
      )

      val newResource = newInstance(
        typeId = ResourceIds.Container,
        name = testContainerName,
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Json.toJson(testProps.provider).toString
        ))
      )

      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/containers", testCreds).withBody(
        Output.renderInstance(newResource)
      )

      val Some(result) = route(request)

      status(result) must equalTo(BAD_REQUEST)
      contentAsString(result) must contain("provider does not exist")
    }

    "create containers using CaaSService interface" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
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

      val newResource = newInstance(
        typeId = ResourceIds.Container,
        name = testContainerName,
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Json.toJson(testProps.provider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "disk" -> testProps.disk.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "network" -> testProps.network.get)
        )
      )

      containerService.createContainer(any,any,any,any) returns Future.successful(newResource)

      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/containers", testCreds).withBody(
        Output.renderInstance(newResource)
      )

      val Some(result) = route(request)

      status(result) must equalTo(CREATED)

      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome(newResource.id)
      (json \ "name").asOpt[String] must beSome(testContainerName)
      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Container")
      (json \ "properties").asOpt[ContainerSpec] must beSome(testProps.copy(name = ""))

      there was one(containerService).createContainer(
        context = matchesProviderContext(
          provider = testProvider,
          workspace = testWork,
          environment = testEnv
        ),
        any,any,any
      )
    }

    "create containers using the ContainerService interface with specific ID" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),None,None,None,None)),
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

      val userSpecificUUID = uuid()
      val newResource = newInstance(
        typeId = ResourceIds.Container,
        id = userSpecificUUID,
        name = testContainerName,
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Json.toJson(testProps.provider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "disk" -> testProps.disk.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "network" -> testProps.network.get
        ))
      )

      containerService.createContainer(any,any,any,any) returns Future.successful(newResource)

      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/containers", testCreds).withBody(
        Output.renderInstance(newResource)
      )
      val Some(result) = route(request)
      status(result) must equalTo(CREATED)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome(userSpecificUUID)
      (json \ "name").asOpt[String] must beSome(testContainerName)
      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Container")
      (json \ "properties").asOpt[ContainerSpec] must beSome(testProps.copy(name = ""))

      there was one(containerService).createContainer(
        context = matchesProviderContext(
          provider = testProvider,
          workspace = testWork,
          environment = testEnv
        ),
        user = argThat( (id: AuthAccountWithCreds) => id.account.id == testAuthResponse.account.id),
        containerSpec = any,
        userRequestedId = beSome(userSpecificUUID)
      )
    }

    "delete kube containers using the ContainerService interface" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),None,None,None,None)),
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
      val extId = s"/${testEnv.id}/test-container"
      val createdResource = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "disk" -> testProps.disk.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "network" -> testProps.network.get,
          "external_id" -> s"${extId}"
        ))
      ).get

      mockCaasService.destroy(
        hasId(createdResource.id)
      ) returns Future(())

      val request = fakeAuthRequest(DELETE,
        s"/root/environments/${testEnv.id}/containers/${createdResource.id}", testCreds)

      val Some(result) = route(request)

      status(result) must equalTo(NO_CONTENT)

      there was one(mockCaasService).destroy(
        hasId(createdResource.id) and hasProperties("external_id" -> extId)
      )
      there was atLeastOne(providerManager).getProviderImpl(ResourceIds.KubeProvider)
    }

    "delete dcos containers using the ContainerService interface" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),None,None,None,None)),
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
      val extId = s"/${testEnv.id}/test-container"
      val Success(createdResource) = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "disk" -> testProps.disk.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "network" -> testProps.network.get,
          "external_id" -> s"${extId}"
        ))
      )

      mockCaasService.destroy(
        hasId(createdResource.id)
      ) returns Future(())

      val request = fakeAuthRequest(DELETE,
        s"/root/environments/${testEnv.id}/containers/${createdResource.id}", testCreds)

      val Some(result) = route(request)

      status(result) must equalTo(NO_CONTENT)

      there was one(mockCaasService).destroy(
        hasId(createdResource.id) and hasProperties("external_id" -> extId)
      )
      there was atLeastOne(providerManager).getProviderImpl(testProvider.typeId)
    }

    "scale containers using the ContainerService and CaaSService interfaces" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),None,None,None,None)),
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
      val extId = s"/${testEnv.id}/test-container"
      val createdResource = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "disk" -> testProps.disk.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "network" -> testProps.network.get,
          "external_id" -> s"${extId}"
        ))
      ).get

      mockCaasService.scale(
        any, any, any
      ) returns Future(createdResource.copy(
        properties = Some(createdResource.properties.get ++ Map("num_instances" -> "4"))
      ))

      val request = fakeAuthRequest(POST,
        s"/root/containers/${createdResource.id}/scale?numInstances=4", testCreds
      )

      val Some(result) = route(request)

      status(result) must equalTo(ACCEPTED)

      there was one(mockCaasService).scale(
        context = matchesProviderContext(
          provider = testProvider,
          workspace = testWork,
          environment = testEnv
        ),
        container = hasId(createdResource.id),
        numInstances = meq(4)
      )
      there was atLeastOne(providerManager).getProviderImpl(ResourceIds.KubeProvider)
    }

  }

}