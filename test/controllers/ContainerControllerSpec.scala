package controllers


import java.util.UUID
import com.galacticfog.gestalt.data.bootstrap.Bootstrap
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.security.api.GestaltSecurityClient
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.test.FakeGestaltSecurityEnvironment
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util.{ContainerService, GestaltSecurityMocking}
import controllers.util.db.ConnectionManager
import org.joda.time.DateTimeZone
import org.specs2.execute.{Result, AsResult}
import org.specs2.matcher.JsonMatchers
import org.specs2.mutable._
import org.specs2.specification._
import play.api.{Play, Application, GlobalSettings}
import play.api.libs.json._
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
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

  def testGlobal() = new GlobalSettings {
    lazy val cs = mock[ContainerService]
    override def getControllerInstance[A](controllerClass: Class[A]): A = {
      if (classOf[ContainerService] == controllerClass) cs.asInstanceOf[A]
      else if (classOf[ContainerController] == controllerClass) {
        new ContainerController(cs) {
          override val env = fakeSecurity
          override val securityClient = mockSecurityClient
        }
      }.asInstanceOf[A]
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

  "ContainerController" should {

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

  }

}