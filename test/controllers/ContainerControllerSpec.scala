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

  lazy val cs = mock[ContainerService]
  lazy val spiedContainerController = spy(new TestContainerController(cs,fakeSecurity,mockSecurityClient))
  lazy val spiedDeleteController = spy(new TestDeleteController(cs,fakeSecurity,mockSecurityClient))
  lazy val spiedResourceController = spy(new TestResourceController(cs,fakeSecurity,mockSecurityClient))

  def testGlobal() = new GlobalSettings {
    override def getControllerInstance[A](controllerClass: Class[A]): A = {
      if (classOf[ContainerService] == controllerClass) cs.asInstanceOf[A]
      else if (classOf[ContainerController] == controllerClass) spiedContainerController.asInstanceOf[A]
      else if (classOf[DeleteController] == controllerClass) spiedDeleteController.asInstanceOf[A]
      else if (classOf[ResourceController] == controllerClass) spiedResourceController.asInstanceOf[A]
      else super.getControllerInstance(controllerClass)
    }
  }

  abstract class TestApplication extends WithApplication(FakeApplication(withGlobal = Some(testGlobal))) {

    var testEnv: Instance = null
    var testWork: Instance = null
    var testProvider: Instance = null
    var mockMarathonClient: MarathonClient = null

    def testFQON: String = "root"
    def testWID: UUID = testWork.id
    def testEID: UUID = testEnv.id
    def testPID: UUID = testProvider.id

    override def around[T: AsResult](t: => T): Result = super.around {
      var Success((tW,tE)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
      testWork = tW
      testEnv = tE
      testProvider = createMarathonProvider(testEID, "test-provider").get
      mockMarathonClient = mock[MarathonClient]
      val cs = containerService
      cs.appComponents(testEID) returns Try((testWork,testEnv))
      cs.marathonProvider(testPID) returns testProvider
      cs.marathonClient(testProvider) returns mockMarathonClient
      t
    }

  }

  def fakeAuthRequest(method: String, path: String) = FakeRequest(method, path).withHeaders(AUTHORIZATION -> creds.headerValue)

  def containerService(implicit app: Application) = Play.global(app).getControllerInstance(classOf[ContainerService])

  "findMigrationRule" should {

    "find any container.migrate.* rules that are within the given scope" in new TestApplication {
      val org = newOrg(id = dummyRootOrgId)
      org must beSuccessfulTry
      
      val data = newDummyEnvironment(dummyRootOrgId)

      val containerController = new ContainerController(new ContainerService {})

      containerController.findMigrationRule(data("environment")).isEmpty must beTrue

      val (_, rule) = createEventRule(data("environment"), data("lambda"), "container.migrate.pre")
      ResourceFactory.findById(ResourceIds.RuleEvent, rule) must beSome
      
      val event = containerController.findMigrationRule(data("environment"))
      
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
        meq("root"),
        meq(testWork),
        meq(testEnv)
      ) returns Future(Seq(testContainer))

      val request = fakeAuthRequest(GET, s"/root/environments/${testEID}/containers")

      val Some(result) = route(request)

      there was one(spiedResourceController).getResources("root", s"environments/${testEID}/containers")
      there was one(containerService).listEnvironmentContainers("root", testWork, testEnv)

      contentAsJson(result) must equalTo(jsResponse)
      status(result) must equalTo(OK)
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

      there was one(spiedDeleteController).hardDeleteResource("root", s"environments/${testEID}/containers/${createdResource.id}")
      there was one(containerService).deleteContainer(
        argThat((c: ResourceLike) => c.id == createdResource.id && c.properties.flatMap(_.get("external_id")).contains(extId))
      )

      status(result) must equalTo(NO_CONTENT)
    }

  }

}