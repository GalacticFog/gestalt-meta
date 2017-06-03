package controllers

import java.util.UUID

import scala.concurrent.Future
import scala.util.Success
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.{JsonMatchers, Matcher}
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification.BeforeAll
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import controllers.util.{ContainerService, GestaltProviderMocking, GestaltSecurityMocking}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.Json
import play.api.test.PlaySpecification
import play.api.test.WithApplication
import play.api.inject.bind
import services.{CaasService, MarathonClientFactory, ProviderContext, SkuberFactory}

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
  
  abstract class FakeSecurity extends WithDb(containerApp())  
  
  def matchesProviderContext(provider: GestaltResourceInstance, workspace: GestaltResourceInstance, environment: GestaltResourceInstance): Matcher[ProviderContext] =
    ((_: ProviderContext).workspace.id == workspace.id, (_: ProviderContext).workspace.id.toString + " does not contain the expected workspace resource " + workspace.id) and
    ((_: ProviderContext).environment.id == environment.id, (_: ProviderContext).environment.id.toString + " does not contain the expected environment resource " + environment.id) and
    ((_: ProviderContext).environmentId == environment.id, (_: ProviderContext).environmentId.toString + " does not contain the expected environmentId " + environment.id) and
    ((_: ProviderContext).provider.id == provider.id, (_: ProviderContext).provider.id.toString + " does not contain the expected provider resource " + provider.id) and
    ((_: ProviderContext).providerId == provider.id, (_: ProviderContext).providerId.toString + " does not contain the expected providerId " + provider.id)

  trait TestContainerController extends FakeSecurity {
    val Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")

    Ents.setNewEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

    val testProvider = createKubernetesProvider(testEnv.id, "test-kube-provider").get
    val mockCaasService = mock[CaasService]
    mockProviderManager.getProviderImpl(any) returns Success(mockCaasService)
  }
  
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

      mockContainerService.getEnvironmentContainer("root", testEnv.id, testContainer.id) returns Future(Some(testContainer -> Seq.empty))

      // Set entitlements on new environment for container creation
      val ces = Ents.setNewEntitlements(dummyRootOrgId, testContainer.id, user, Some(testEnv.id))
      ces exists { _.isFailure } must beFalse

      val path = s"/root/environments/${testEnv.id}/containers/${testContainer.id}"
      val request = fakeAuthRequest(GET, path, testCreds)

      val Some(result) = route(request)

      contentAsString(result) must /("id" -> testContainer.id.toString)
      status(result) must equalTo(OK)
    }

    "update a container via the ContainerService interface" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id)
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
          "external_id" -> s"${extId}"
        ))
      ).get

      mockContainerService.updateContainer(any, any, any, any) answers {
        (a: Any) =>
          val arr = a.asInstanceOf[Array[Object]]
          val r = arr(1).asInstanceOf[GestaltResourceInstance]
          Future.successful(r)
      }

      val request = fakeAuthRequest(PUT,
        s"/root/environments/${testEnv.id}/containers/${createdResource.id}", testCreds
      ).withBody(
        Output.renderInstance(createdResource)
      )

      val Some(result) = route(request)

      status(result) must equalTo(OK)

//      there was one(mockContainerService).updateContainer(
//        container = argThat(
//          (r: GestaltResourceInstance) => r.id == createdResource.id
//        ),
//        context = any,
//        user = any,
//        request = any
//      )
      there was one(mockContainerService).updateContainer(any,any,any,any)

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

      mockContainerService.listEnvironmentContainers(
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

      mockContainerService.createContainer(any,any,any,any) returns Future.successful(newResource)

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

      there was one(mockContainerService).createContainer(
        context = argThat(matchesProviderContext(
          provider = testProvider,
          workspace = testWork,
          environment = testEnv
        )),
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

      mockContainerService.createContainer(any,any,any,any) returns Future.successful(newResource)

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

      there was one(mockContainerService).createContainer(
        context = argThat(matchesProviderContext(
          provider = testProvider,
          workspace = testWork,
          environment = testEnv
        )),
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
        argThat((c: GestaltResourceInstance) => c.id == createdResource.id)
      ) returns Future(())

      val request = fakeAuthRequest(DELETE,
        s"/root/environments/${testEnv.id}/containers/${createdResource.id}", testCreds)

      val Some(result) = route(request)

      status(result) must equalTo(NO_CONTENT)

      there was one(mockCaasService).destroy(
        argThat(
          (r: GestaltResourceInstance) => r.id == createdResource.id && r.properties.flatMap(_.get("external_id")).contains(extId)
        )
      )
      there was atLeastOne(mockProviderManager).getProviderImpl(ResourceIds.KubeProvider)
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
        argThat((c: GestaltResourceInstance) => c.id == createdResource.id)
      ) returns Future(())

      val request = fakeAuthRequest(DELETE,
        s"/root/environments/${testEnv.id}/containers/${createdResource.id}", testCreds)

      val Some(result) = route(request)

      status(result) must equalTo(NO_CONTENT)

      there was one(mockCaasService).destroy(
        argThat(
          (r: GestaltResourceInstance) => r.id == createdResource.id && r.properties.flatMap(_.get("external_id")).contains(extId)
        )
      )
      there was atLeastOne(mockProviderManager).getProviderImpl(testProvider.typeId)
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
        context = argThat(matchesProviderContext(
          provider = testProvider,
          workspace = testWork,
          environment = testEnv
        )),
        container = argThat(
          (r: GestaltResourceInstance) => r.id == createdResource.id
        ),
        numInstances = meq(4)
      )
      there was atLeastOne(mockProviderManager).getProviderImpl(ResourceIds.KubeProvider)
    }

  }

}