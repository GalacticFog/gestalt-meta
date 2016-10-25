package controllers


import java.util.UUID
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.security.api.GestaltSecurityClient
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.test.FakeGestaltSecurityEnvironment
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util.{ContainerService, GestaltSecurityMocking}
import org.joda.time.DateTimeZone
import org.specs2.execute.{Result, AsResult}
import org.specs2.matcher.JsonMatchers
import org.specs2.specification._
import play.api.{Play, Application, GlobalSettings}
import play.api.libs.json._
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import play.api.test._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import org.mockito.Matchers.{eq => meq}

import scala.concurrent.Future
import scala.util.{Try, Success}

class ContainerControllerSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll with JsonMatchers {

  override def beforeAll(): Unit = pristineDatabase()

  sequential

  lazy val creds = dummyCreds()
  lazy val authResponse = dummyAuthAccount()
  lazy val mockSecurityClient = mock[GestaltSecurityClient]
  lazy val fakeSecurity = FakeGestaltSecurityEnvironment[DummyAuthenticator](Seq(
    creds -> authResponse
  ), mockSecurityClient)

  class TestGlobal extends GlobalSettings {
    lazy val mockContainerService = mock[ContainerService]
    lazy val spiedContainerController = spy(new TestContainerController(mockContainerService,fakeSecurity,mockSecurityClient))
    lazy val spiedDeleteController = spy(new TestDeleteController(mockContainerService,fakeSecurity,mockSecurityClient))
    lazy val spiedResourceController = spy(new TestResourceController(mockContainerService,fakeSecurity,mockSecurityClient))
    override def getControllerInstance[A](controllerClass: Class[A]): A = {
      if (classOf[ContainerService] == controllerClass) mockContainerService.asInstanceOf[A]
      else if (classOf[ContainerController] == controllerClass) spiedContainerController.asInstanceOf[A]
      else if (classOf[DeleteController] == controllerClass) spiedDeleteController.asInstanceOf[A]
      else if (classOf[ResourceController] == controllerClass) spiedResourceController.asInstanceOf[A]
      else super.getControllerInstance(controllerClass)
    }
  }

  abstract class TestApplication extends WithApplication(FakeApplication(withGlobal = Some(new TestGlobal))) {

    var testEnv: Instance = null
    var testWork: Instance = null
    var testProvider: Instance = null
    var mockMarathonClient: MarathonClient = null

    def testFQON: String = "root"
    def testWID: UUID = testWork.id
    def testEID: UUID = testEnv.id
    def testPID: UUID = testProvider.id

    lazy val containerService    = app.global.getControllerInstance(classOf[ContainerService])
    lazy val resourceController  = app.global.getControllerInstance(classOf[ResourceController])
    lazy val deleteController    = app.global.getControllerInstance(classOf[DeleteController])
    lazy val containerController = app.global.getControllerInstance(classOf[ContainerController])

    override def around[T: AsResult](t: => T): Result = super.around {
      var Success((tW,tE)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
      testWork = tW
      testEnv = tE
      testProvider = createMarathonProvider(testEID, "test-provider").get
      mockMarathonClient = mock[MarathonClient]
      containerService.findWorkspaceEnvironment(testEID) returns Try((testWork,testEnv))
      containerService.marathonProvider(testPID) returns testProvider
      containerService.marathonClient(testProvider) returns mockMarathonClient
      t
    }

  }

  def fakeAuthRequest(method: String, path: String) = FakeRequest(method, path).withHeaders(AUTHORIZATION -> creds.headerValue)


  "findMigrationRule" should {

    "find any container.migrate.* rules that are within the given scope" in new TestApplication {
      val org = newOrg(id = dummyRootOrgId)
      org must beSuccessfulTry

      val data = newDummyEnvironment(dummyRootOrgId)

      val controller = new ContainerController(new ContainerService {})

      controller.findMigrationRule(data("environment")).isEmpty must beTrue

      val (_, rule) = createEventRule(data("environment"), data("lambda"), "container.migrate.pre")
      ResourceFactory.findById(ResourceIds.RuleEvent, rule) must beSome

      val event = controller.findMigrationRule(data("environment"))

      event.isEmpty must beFalse
    }
  }

  "ContainerSpec" should {

    "be convertible from GestaltResourceInstance container representations with appropriate defaults and link to GestaltResourceInstance" in new TestApplication {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx:alpine",
        provider = ContainerSpec.InputProvider(id = testPID, name = Some(testProvider.name)),
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
        labels = Map(),
        env = Map(),
        user = None
      )
      val createdResource = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEID),
        properties = Some(Map(
          "container_type" -> "DOCKER",
          "provider" -> Output.renderInstance(testProvider).toString,
          "image" -> testProps.image
        ))
      ).get
      ContainerSpec.fromResourceInstance(createdResource) must beSuccessfulTry(testProps.copy(resource = Some(createdResource)))
    }

    "throw an exception if attempting to create from non-Container resource" in new TestApplication {
      val testResourceName = "some-name"
      val createdResource = createInstance(ResourceIds.Org, testResourceName,
        parent = Some(testEID),
        properties = None
      ).get
      ContainerSpec.fromResourceInstance(createdResource) must beFailedTry.withThrowable[RuntimeException]("cannot convert non-Container resource into ContainerSpec")
    }

  }

  "ContainerController" should {

    "get a container via the ContainerService interface" in new TestApplication {
      val testProps = ContainerSpec(
        name = "",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testPID, name = Some(testProvider.name)),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp",80,0,0,None,Map())),
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
      val testContainerName = "test-container"
      val testContainer = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEID),
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
        ))
      ) flatMap ContainerSpec.fromResourceInstance get
      val jsResponse = Json.obj(
      )
      containerService.findEnvironmentContainerByName(
        "root",
        testEnv.id,
        testContainerName
      ) returns Future(Some(testContainer))

      val request = fakeAuthRequest(GET, s"/root/environments/${testEID}/containers/${testContainer.id.get}")

      val Some(result) = route(request)

      contentAsJson(result) must equalTo(jsResponse)
      status(result) must equalTo(OK)

      there was one(resourceController).getResources("root", s"environments/${testEID}/containers/${testContainer.id.get}")
      there was one(containerService).findEnvironmentContainerByName("root", testEnv.id, testContainerName)
    }

    "list containers via the ContainerService interface" in new TestApplication {
      val testProps = ContainerSpec(
        name = "",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testPID, name = Some(testProvider.name)),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp",80,0,0,None,Map())),
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
      val testContainerName = "test-container"
      val testContainer = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEID),
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
        ))
      ) flatMap ContainerSpec.fromResourceInstance get
      val jsResponse = Json.obj(
      )
      containerService.listEnvironmentContainers(
        "root",
        testWork,
        testEnv
      ) returns Future(Seq(testContainer))

      val request = fakeAuthRequest(GET, s"/root/environments/${testEID}/containers")

      val Some(result) = route(request)

      contentAsJson(result) must equalTo(jsResponse)
      status(result) must equalTo(OK)

      there was one(resourceController).getResources("root", s"environments/${testEID}/containers")
      there was one(containerService).listEnvironmentContainers("root", testWork, testEnv)
    }

    "create containers using the ContainerService interface" in new TestApplication {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testPID, name = Some(testProvider.name)),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp",80,0,0,None,Map())),
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
      val createdResource = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEID),
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
          "network" -> testProps.network.get
        ))
      ).get
      val createdContainer = ContainerSpec.fromResourceInstance(createdResource).get
      containerService.launchContainer(
        meq("root"),
        meq(testWork),
        meq(testEnv),
        any[AuthAccountWithCreds],
        any[ContainerSpec] /* TODO: meq(testProps) */
      ) returns Future(createdContainer)

      val request = fakeAuthRequest(POST, s"/root/environments/${testEID}/containers").withBody(
        Output.renderInstance(createdResource)
      )
      val Some(result) = route(request)
      status(result) must equalTo(CREATED)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome(createdContainer.id.get)
      (json \ "name").asOpt[String] must beSome(testContainerName)
      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Container")
      (json \ "properties").asOpt[ContainerSpec] must beSome(testProps.copy(name = ""))

      there was one(containerController).createContainer(anyString, any[UUID])
      there was one(containerService).launchContainer(anyString, meq(testWork), meq(testEnv), any[AuthAccountWithCreds], any[ContainerSpec])

      val getRequest = fakeAuthRequest(GET, s"/root/environments/${testEID}/containers/${createdContainer.id.get}")
      val Some(getResult) = route(getRequest)
      status(getResult) must equalTo(OK)
    }

    "delete containers using the ContainerService interface" in new TestApplication {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testPID, name = Some(testProvider.name)),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp",80,0,0,None,Map())),
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
      val extId = s"/some/dummy/external/id"
      val createdResource = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEID),
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

      containerService.deleteContainer(
        createdResource
      ) returns Future(())

      val request = fakeAuthRequest(DELETE, s"/root/environments/${testEID}/containers/${createdResource.id}")

      val Some(result) = route(request)

      status(result) must equalTo(NO_CONTENT)

      there was one(deleteController).hardDeleteResource("root", s"environments/${testEID}/containers/${createdResource.id}")
      there was one(containerService).deleteContainer(
        argThat(
          (c: ResourceLike) => c.id == createdResource.id && c.properties.flatMap(_.get("external_id")).contains(extId)
        )
      )
    }

  }

}