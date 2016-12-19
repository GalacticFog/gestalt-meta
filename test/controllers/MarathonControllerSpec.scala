package controllers

import java.util.UUID

import com.galacticfog.gestalt.data.Instance
import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.marathon
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.api.GestaltSecurityClient
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.test.FakeGestaltSecurityEnvironment
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util.{ContainerService, GestaltSecurityMocking}
import org.joda.time.{DateTimeZone, DateTime}
import org.specs2.execute.{Result, AsResult}
import org.specs2.matcher.JsonMatchers
import org.specs2.specification._
import play.api.libs.json.{JsArray, Json}
import play.api.test._
import play.api.{Application, GlobalSettings, Play}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import org.mockito.Matchers.{eq => meq}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Try, Success}

import marathon._

class MarathonControllerSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll with JsonMatchers {

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
      cs.findWorkspaceEnvironment(testEID) returns Try((testWork,testEnv))
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
      mockMarathonClient.listDeploymentsAffectingEnvironment(meq(testFQON), meq(testWork.name), meq(testEnv.name))(any[ExecutionContext]) returns Future.successful(js)
      mockMarathonClient.getInfo()(any[ExecutionContext]) returns Future.successful(js)

      val request = fakeAuthRequest(GET, s"/${testFQON}/environments/${testEID}/providers/${testPID}/v2/deployments")
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      contentAsJson(result) must equalTo(js)
    }

    "support Marathon GET /v2/apps" in new TestApplication {
      val testProps = ContainerSpec(
        name = "",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testPID, name = Some(testProvider.name)),
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
      ).get
      val testContainerSpec = ContainerSpec.fromResourceInstance(testContainer).get
      val jsResponse = Json.parse(
        s"""
           |{
           |  "apps": [
           |{
           |    "acceptedResourceRoles": null,
           |    "args": null,
           |    "backoffFactor": 1.15,
           |    "backoffSeconds": 1,
           |    "cmd": null,
           |    "constraints": [],
           |    "container": {
           |        "docker": {
           |            "forcePullImage": false,
           |            "image": "nginx",
           |            "network": "BRIDGE",
           |            "parameters": [],
           |            "portMappings": [
           |                {
           |                    "containerPort": 80,
           |                    "hostPort": 0,
           |                    "labels": {},
           |                    "protocol": "tcp",
           |                    "servicePort": 0
           |                }
           |            ],
           |            "privileged": false
           |        },
           |        "type": "DOCKER",
           |        "volumes": []
           |    },
           |    "cpus": 1,
           |    "dependencies": [],
           |    "deployments": [
           |    ],
           |    "disk": 0,
           |    "env": {},
           |    "executor": "",
           |    "fetch": [],
           |    "gpus": 0,
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
           |    "secrets": {},
           |    "storeUrls": [],
           |    "taskKillGracePeriodSeconds": null,
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
           |    "version": "${testContainerSpec.created.get.toDateTime(DateTimeZone.UTC).toString}"
           |}
           |  ]
           |}
        """.stripMargin
      )
      containerService.listEnvironmentContainers(
        meq("root"),
        meq(testEnv.id)
      ) returns Future(Seq(testContainer -> Seq.empty))

      val request = fakeAuthRequest(GET, s"/root/environments/${testEID}/providers/${testPID}/v2/apps")
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      contentAsJson(result) must equalTo(jsResponse)
    }

    "appropriate 404 Marathon GET /v2/apps/nonExistantApp" in new TestApplication {
      containerService.findEnvironmentContainerByName(
        meq("root"),
        meq(testEnv.id),
        meq("nonexistent")
      ) returns Future(None)

      val request = fakeAuthRequest(GET, s"/root/environments/${testEID}/providers/${testPID}/v2/apps/nonexistent")
      val Some(result) = route(request)
      status(result) must equalTo(NOT_FOUND)
      contentAsJson(result) must equalTo(Json.obj(
        "message" -> "App '/nonexistent' does not exist"
      ))
    }

    "appropriate 404 Marathon DELETE /v2/apps/nonExistantApp" in new TestApplication {
      containerService.findEnvironmentContainerByName(
        meq("root"),
        meq(testEnv.id),
        meq("nonexistent")
      ) returns Future(None)

      val request = fakeAuthRequest(DELETE, s"/root/environments/${testEID}/providers/${testPID}/v2/apps/nonexistent")
      val Some(result) = route(request)
      status(result) must equalTo(NOT_FOUND)
      contentAsJson(result) must equalTo(Json.obj(
        "message" -> "App '/nonexistent' does not exist"
      ))
    }

    "support Marathon GET /v2/apps/:appId" in new TestApplication {
      val testProps = ContainerSpec(
        name = "",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testPID, name = Some(testProvider.name)),
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
      ).get
      val testContainerSpec = ContainerSpec.fromResourceInstance(testContainer).get
      val jsResponse = Json.parse(
        s"""
           |{
           |  "app":
           |{
           |    "acceptedResourceRoles": null,
           |    "args": null,
           |    "backoffFactor": 1.15,
           |    "backoffSeconds": 1,
           |    "cmd": null,
           |    "constraints": [],
           |    "container": {
           |        "docker": {
           |            "forcePullImage": false,
           |            "image": "nginx",
           |            "network": "BRIDGE",
           |            "parameters": [],
           |            "portMappings": [
           |                {
           |                    "containerPort": 80,
           |                    "hostPort": 0,
           |                    "labels": {},
           |                    "protocol": "tcp",
           |                    "servicePort": 0
           |                }
           |            ],
           |            "privileged": false
           |        },
           |        "type": "DOCKER",
           |        "volumes": []
           |    },
           |    "cpus": 1,
           |    "dependencies": [],
           |    "deployments": [
           |    ],
           |    "disk": 0,
           |    "env": {},
           |    "executor": "",
           |    "fetch": [],
           |    "gpus": 0,
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
           |    "secrets": {},
           |    "storeUrls": [],
           |    "taskKillGracePeriodSeconds": null,
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
           |    "version": "${testContainerSpec.created.get.toDateTime(DateTimeZone.UTC).toString}"
           |}
           |}
        """.stripMargin
      )
      containerService.findEnvironmentContainerByName(
        meq("root"),
        meq(testEnv.id),
        meq("test-container")
      ) returns Future(Some(testContainer -> Seq.empty))

      val request = fakeAuthRequest(GET, s"/root/environments/${testEID}/providers/${testPID}/v2/apps/test-container")
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      contentAsJson(result) must equalTo(jsResponse)
    }

    "support Marathon DELETE /v2/apps/:appId" in new TestApplication {
      val testProps = ContainerSpec(
        name = "",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testPID, name = Some(testProvider.name)),
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
      ).get
      containerService.findEnvironmentContainerByName(
        meq("root"),
        meq(testEnv.id),
        meq("test-container")
      ) returns Future(Some(testContainer -> Seq.empty))
      containerService.deleteContainer(
        any[GestaltResourceInstance]
      ) returns Future(())

      val request = fakeAuthRequest(DELETE, s"/root/environments/${testEID}/providers/${testPID}/v2/apps/test-container")
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      (contentAsJson(result) \ "deployment").asOpt[UUID] must beSome
      (contentAsJson(result) \ "version").asOpt[String] must beSome
    }

    "support Marathon POST /v2/apps (minimal container)" in new TestApplication {
      val requestBody = Json.parse(
        """{
          |  "id": "test-container",
          |  "container": {
          |    "docker": {
          |      "image": "nginx",
          |      "network": "BRIDGE",
          |      "portMappings": [
          |        {
          |          "containerPort": 80
          |        }
          |      ]
          |    }
          |  }
          |}
        """.stripMargin
      )
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testPID, name = Some(testProvider.name)),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),None,Some(0),None,Some(Map.empty))),
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
      val createdContainer = createInstance(ResourceIds.Container, testContainerName,
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
      ).get
      val createdContainerSpec = ContainerSpec.fromResourceInstance(createdContainer).get
      val jsResponse = Json.parse(
        s"""
          |{
          |    "acceptedResourceRoles": null,
          |    "args": null,
          |    "backoffFactor": 1.15,
          |    "backoffSeconds": 1,
          |    "cmd": null,
          |    "constraints": [],
          |    "container": {
          |        "docker": {
          |            "forcePullImage": false,
          |            "image": "nginx",
          |            "network": "BRIDGE",
          |            "parameters": [],
          |            "portMappings": [
          |                {
          |                    "containerPort": 80,
          |                    "hostPort": 0,
          |                    "labels": {},
          |                    "protocol": "tcp",
          |                    "servicePort": 0
          |                }
          |            ],
          |            "privileged": false
          |        },
          |        "type": "DOCKER",
          |        "volumes": []
          |    },
          |    "cpus": 1,
          |    "dependencies": [],
          |    "deployments": [
          |    ],
          |    "disk": 0,
          |    "env": {},
          |    "executor": "",
          |    "fetch": [],
          |    "gpus": 0,
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
          |    "secrets": {},
          |    "storeUrls": [],
          |    "taskKillGracePeriodSeconds": null,
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
          |    "version": "${createdContainerSpec.created.get.toDateTime(DateTimeZone.UTC).toString}"
          |}
        """.stripMargin
      )
      containerService.launchContainer(
        meq("root"),
        meq(testWork),
        meq(testEnv),
        any[AuthAccountWithCreds],
        meq(testProps),
        any[Option[UUID]]
      ) returns Future(createdContainer -> Seq.empty)

      val request = fakeAuthRequest(POST, s"/root/environments/${testEID}/providers/${testPID}/v2/apps").withBody(requestBody)
      val Some(result) = route(request)
      status(result) must equalTo(CREATED)
      contentAsJson(result) must equalTo(jsResponse)
    }

  }
}