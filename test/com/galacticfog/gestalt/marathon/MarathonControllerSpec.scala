package com.galacticfog.gestalt.marathon

import java.util.UUID

import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import controllers.SecurityResources
import org.joda.time.DateTimeZone
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.JsonMatchers
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.PlaySpecification
import services.{MarathonClientFactory, MarathonService, ProviderContext}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

class MarathonControllerSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  lazy val mockMCF = mock[MarathonClientFactory]

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

  abstract class FakeSecurity extends WithDb(containerApp()) {
  }

  trait TestMarathonController extends FakeSecurity {
    val Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")

    Ents.setNewEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

    val testProvider = createMarathonProvider(testEnv.id, "test-provider").get
    val mockProviderService = mock[MarathonService]

    mockProviderManager.getProviderImpl(ResourceIds.DcosProvider) returns Success(mockProviderService)

    val mockMarathonClient = mock[MarathonClient]

    def testFQON: String = "root"
    def testWID: UUID = testWork.id
    def testEID: UUID = testEnv.id
    def testPID: UUID = testProvider.id
  }

  "MarathonAPIController" should {

    "support Marathon GET /v2/info" in new TestMarathonController {
      val js = Json.obj()
      mockMCF.getClient(testProvider) returns Future.successful(mockMarathonClient)
      mockMarathonClient.getInfo()(any[ExecutionContext]) returns Future.successful(js)

      val request = fakeAuthRequest(GET, s"/root/environments/${testEID}/providers/${testPID}/v2/info", testCreds)
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      contentAsJson(result) must equalTo(js)
    }.pendingUntilFixed("Getting null pointer exception on .getClient()")
    

    "support Marathon GET /v2/deployments" in new TestMarathonController {
      val js = Json.arr()
      val request = fakeAuthRequest(GET,
          s"/${testFQON}/environments/${testEID}/providers/${testPID}/v2/deployments", testCreds)
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      contentAsJson(result) must equalTo(js)
    }

    "support Marathon GET /v2/apps" in new TestMarathonController {
      val testProps = ContainerSpec(
        name = "",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testPID, name = Some(testProvider.name)),
        port_mappings = Seq(ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(80),
          host_port = Some(65000),
          service_port = Some(10100),
          name = Some("web"),
          labels = Some(Map("k" -> "v"))
        )),
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
           |                    "hostPort": 65000,
           |                    "labels": {"k": "v"},
           |                    "name": "web",
           |                    "protocol": "tcp",
           |                    "servicePort": 10100
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
           |            "labels": {"k": "v"},
           |            "port": 10100,
           |            "protocol": "tcp",
           |            "name": "web"
           |        }
           |    ],
           |    "ports": [
           |        10100
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
      mockContainerService.listEnvironmentContainers(
        meq("root"),
        meq(testEnv.id)
      ) returns Future(Seq(testContainer -> Seq.empty))

      val request = fakeAuthRequest(GET, s"/root/environments/${testEID}/providers/${testPID}/v2/apps", testCreds)
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      contentAsJson(result) must equalTo(jsResponse)
    }

    "appropriate 404 Marathon GET /v2/apps/nonExistantApp" in new TestMarathonController {
      val request = fakeAuthRequest(GET,
          s"/root/environments/${testEID}/providers/${testPID}/v2/apps/nonexistent", testCreds)
      val Some(result) = route(request)
      status(result) must equalTo(NOT_FOUND)
      contentAsJson(result) must equalTo(Json.obj(
        "message" -> "App '/nonexistent' does not exist"
      ))
    }

    "appropriate 404 Marathon DELETE /v2/apps/nonExistantApp" in new TestMarathonController {
      val request = fakeAuthRequest(DELETE,
          s"/root/environments/${testEID}/providers/${testPID}/v2/apps/nonexistent", testCreds)
      val Some(result) = route(request)
      status(result) must equalTo(NOT_FOUND)
      contentAsJson(result) must equalTo(Json.obj(
        "message" -> "App '/nonexistent' does not exist"
      ))
    }

    "support Marathon GET /v2/apps/:appId" in new TestMarathonController {
      val testProps = ContainerSpec(
        name = "",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testPID, name = Some(testProvider.name)),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),Some(0),Some(0),Some("web"),Some(Map.empty))),
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
           |                    "name": "web",
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
           |            "protocol": "tcp",
           |            "name": "web"
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
      mockContainerService.getEnvironmentContainer(
        any, any, any
      ) returns Future.successful(Some(testContainer -> Seq.empty))

      val request = fakeAuthRequest(GET, 
          s"/root/environments/${testEID}/providers/${testPID}/v2/apps/test-container", testCreds)
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      contentAsJson(result) must equalTo(jsResponse)
    }

    "support Marathon DELETE /v2/apps/:appId" in new TestMarathonController {
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
      mockContainerService.deleteContainer(
        any, any, any
      ) returns Future(())

      val request = fakeAuthRequest(DELETE, 
        s"/root/environments/${testEID}/providers/${testPID}/v2/apps/test-container", testCreds)
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      (contentAsJson(result) \ "deployment").asOpt[UUID] must beSome
      (contentAsJson(result) \ "version").asOpt[String] must beSome
    }

    "support Marathon POST /v2/apps (minimal container)" in new TestMarathonController {
      val requestBody = Json.parse(
        """{
          |  "id": "test-container",
          |  "container": {
          |    "docker": {
          |      "image": "nginx",
          |      "network": "BRIDGE",
          |      "portMappings": [
          |        {
          |          "containerPort": 80,
          |          "hostPort": 0,
          |          "servicePort": 0,
          |          "name": "web",
          |          "labels": {}
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
        port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),Some(0),Some(0),Some("web"),Some(Map.empty))),
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
          |                    "name": "web",
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
          |            "port": 0,
          |            "protocol": "tcp",
          |            "name": "web",
          |            "labels": {}
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
      mockContainerService.createContainer(
        any, any, meq(testProps), any
      ) returns Future(createdContainer)

      val request = fakeAuthRequest(POST, 
        s"/root/environments/${testEID}/providers/${testPID}/v2/apps", testCreds).withBody(requestBody)
      val Some(result) = route(request)
      status(result) must equalTo(CREATED)
      contentAsJson(result) must equalTo(jsResponse)

      there was one(mockContainerService).createContainer(
        context = argThat( (cp: ProviderContext) => cp.environmentId == testEID && cp.workspace.id == testWork.id && cp.providerId == testProvider.id),
        user = argThat( (id: AuthAccountWithCreds) => id.account.id == testAuthResponse.account.id),
        containerSpec = any,
        userRequestedId = any
      )
    }

  }
}