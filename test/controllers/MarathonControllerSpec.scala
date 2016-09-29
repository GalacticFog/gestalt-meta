package controllers

import java.util.UUID

import com.galacticfog.gestalt.data.Instance
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.marathon
import com.galacticfog.gestalt.marathon.{InputContainerProperties, InputContainer, MarathonClient}
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.api.GestaltSecurityClient
import com.galacticfog.gestalt.security.play.silhouette.test.FakeGestaltSecurityEnvironment
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util.{ContainerService, GestaltSecurityMocking}
import org.specs2.execute.{Result, AsResult}
import org.specs2.specification._
import play.api.libs.json.{JsArray, Json}
import play.api.test._
import play.api.{Application, GlobalSettings, Play}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import org.mockito.Matchers.{eq => meq}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Try, Success}


class MarathonControllerSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll {

  override def beforeAll(): Unit = pristineDatabase()

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
      else if (classOf[MarathonAPIController] == controllerClass) {
        new MarathonAPIController(cs) {
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

  "MarathonAPIController" should {

    "support Marathon GET /v2/info" in new TestApplication {
      val js = Json.obj()
      mockMarathonClient.getInfo()(any[ExecutionContext]) returns Future.successful(js)

      val request = fakeAuthRequest(GET, s"/root/environments/${testEID}/providers/${testPID}/v2/info")
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      contentAsJson(result) must equalTo(js)
    }

    "support Marathon GET /v2/deployments" in new TestApplication {
      val js = Json.arr()
      mockMarathonClient.listDeploymentsAffectingEnvironment_marathon_v2(meq(testFQON), meq(testWork.name), meq(testEnv.name))(any[ExecutionContext]) returns Future.successful(js)
      mockMarathonClient.getInfo()(any[ExecutionContext]) returns Future.successful(js)

      val request = fakeAuthRequest(GET, s"/${testFQON}/environments/${testEID}/providers/${testPID}/v2/deployments")
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      contentAsJson(result) must equalTo(js)
    }

    "support Marathon POST /v2/apps (minimal container)" in new TestApplication {
      val requestBody = Json.parse(
        """{
          |  "id": "test-container",
          |  "container": {
          |    "docker": {
          |      "image": "nginx",
          |      "network": "BRIDGE",
          |      "ports": [
          |        {
          |          "containerPort": 80
          |        }
          |      ]
          |    }
          |  }
          |}
        """.stripMargin
      )
      val jsResponse = Json.parse(
        """
          |{
          |    "acceptedResourceRoles": null,
          |    "args": null,
          |    "cmd": null,
          |    "constraints": [],
          |    "container": {
          |        "docker": {
          |            "forcePullImage": false,
          |            "image": "nginx",
          |            "network": "BRIDGE",
          |            "parameters": [],
          |            "privileged": false
          |        },
          |        "type": "DOCKER",
          |        "volumes": []
          |    },
          |    "cpus": 1,
          |    "dependencies": [],
          |    "deployments": [
          |        {
          |            "id": "1f890ca1-b568-4485-a5ab-288d98f52cf0"
          |        }
          |    ],
          |    "disk": 0,
          |    "env": {},
          |    "executor": "",
          |    "fetch": [],
          |    "healthChecks": [],
          |    "id": "/test-container",
          |    "instances": 1,
          |    "ipAddress": null,
          |    "labels": {},
          |    "maxLaunchDelaySeconds": 3600,
          |    "mem": 128,
          |    "portDefinitions": [
          |        {
          |            "labels": {},
          |            "port": 0,
          |            "protocol": "tcp"
          |        }
          |    ],
          |    "ports": [
          |        0
          |    ],
          |    "readinessChecks": [],
          |    "requirePorts": false,
          |    "residency": null,
          |    "storeUrls": [],
          |    "tasks": [],
          |    "tasksHealthy": 0,
          |    "tasksRunning": 0,
          |    "tasksStaged": 0,
          |    "tasksUnhealthy": 0,
          |    "upgradeStrategy": {
          |        "maximumOverCapacity": 1,
          |        "minimumHealthCapacity": 1
          |    },
          |    "uris": [],
          |    "user": null,
          |    "version": "2016-09-28T19:55:37.200Z"
          |}
        """.stripMargin
      )
      val testProps = InputContainerProperties(
        container_type = "DOCKER",
        image = "nginx",
        provider = marathon.InputProvider(id = testPID, name = Some(testProvider.name)),
        port_mappings = Seq(marathon.PortMapping("tcp",80,0,0,None)),
        cpus = 1.0,
        memory = 128,
        disk = 0.0,
        num_instances = 1,
        network = "BRIDGE",
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
      val createdContainer = createInstance(ResourceIds.Container, testContainerName,
          parent = Some(testEID),
          properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> "1.0",
          "memory" -> "256",
          "num_instances" -> "3"
        ))
      ).get
      containerService.launchContainer(
        meq("root"),
        meq(testWork),
        meq(testEnv),
        meq(testContainerName),
        meq(testProps)
      ) returns Future(createdContainer)

      val request = fakeAuthRequest(POST, s"/root/environments/${testEID}/providers/${testPID}/v2/apps").withBody(requestBody)
      val Some(result) = route(request)
      status(result) must equalTo(CREATED)
      contentAsJson(result) must equalTo(jsResponse)
    }

  }
}