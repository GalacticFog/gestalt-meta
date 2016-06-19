package controllers

import java.util.UUID

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.laser.MarathonClient
import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import org.specs2.matcher.Expectations

import org.specs2.mock._
import org.specs2.mock.mockito._
import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Scope
import play.api.libs.json._


class SpecMarathonProxy extends Specification with MocksCreation with MockitoStubs with CapturedArgument with MockitoMatchers with ArgThat with Expectations {

  val wrkName: String = "Meta Workspace"
  val envName: String = "Test Environment"
  val fqon: String = "galacticfog.engineering.test"

  "MarathonClient" should {

    "xform deployments by env and filter affectedApps" in {
      val input = Json.parse(
        """
          |{
          |  "id" : "a0935106-6466-4019-b5b6-1388dedb0ba1",
          |  "version" : "2016-03-30T03:16:39.208Z",
          |  "affectedApps" : [
          |     "/galacticfog.engineering.test/meta-workspace/test-environment/test-app",
          |     "/galacticfog.engineering.test/meta-workspace/test-environment-2/another-test-app"
          |  ],
          |  "steps" : [ [ {
          |    "action" : "StartApplication",
          |    "app" : "/galacticfog.engineering.test/meta-workspace/test-environment/test-app"
          |  } ], [ {
          |    "action" : "ScaleApplication",
          |    "app" : "/galacticfog.engineering.test/meta-workspace/test-environment/test-app"
          |  } ] ],
          |  "currentActions" : [ {
          |      "action" : "ScaleApplication",
          |      "app" : "/galacticfog.engineering.test/meta-workspace/test-environment/test-app"
          |    }, {
          |      "action" : "ScaleApplication",
          |      "app" : "/galacticfog.engineering.test/meta-workspace/test-environment-2/another-test-app"
          |  } ],
          |  "currentStep" : 2,
          |  "totalSteps" : 2
          |}
        """.stripMargin).as[JsObject]
      val expected = Json.parse(
        """
          |{
          |  "id" : "a0935106-6466-4019-b5b6-1388dedb0ba1",
          |  "version" : "2016-03-30T03:16:39.208Z",
          |  "affectedApps" : [ "/test-app" ],
          |  "steps" : [],
          |  "currentActions" : [ {
          |    "action" : "ScaleApplication",
          |    "app" : "/test-app"
          |  } ],
          |  "currentStep" : 0,
          |  "totalSteps" : 0
          |}
        """.stripMargin).as[JsObject]
      val groupId = MarathonClient.metaContextToMarathonGroup(fqon, wrkName, envName)
      val result = MarathonClient.filterXformDeploymentsByGroup(groupId)(input)
      result must beSome(expected)
    }

    "filter out a deployments with no affected apps in the env" in {
      val input = Json.parse(
        """
          |{
          |  "id" : "a0935106-6466-4019-b5b6-1388dedb0ba1",
          |  "version" : "2016-03-30T03:16:39.208Z",
          |  "affectedApps" : [ "/galacticfog.engineering.test/meta-workspace/test-environment-2/test-app" ],
          |  "steps" : [ ],
          |  "currentActions" : [ ],
          |  "currentStep" : 0,
          |  "totalSteps" : 0
          |}
        """.stripMargin).as[JsObject]
      val groupId = MarathonClient.metaContextToMarathonGroup(fqon, wrkName, envName)
      val result = MarathonClient.filterXformDeploymentsByGroup(groupId)(input)
      result must beNone
    }

    "parse marathon apps" in {
      val inputJson = Json.parse(
        """
          {
            "container" : {
              "type" : "DOCKER",
              "docker" : {
                "image" : "nginx",
                "portMappings" : [ {
                  "containerPort" : 80
                } ],
                "network" : "BRIDGE"
              }
            },
            "id" : "cli-example-server",
            "mem" : 128.0,
            "cpus" : 0.1,
            "ports" : [ 0 ],
            "instances" : 1,
            "labels": {
              "key": "value"
            },
            "healthChecks" : [ {
              "portIndex" : 0,
              "protocol" : "HTTP",
              "gracePeriodSeconds" : 30,
              "maxConsecutiveFailures" : 10,
              "intervalSeconds" : 3,
              "path" : "/"
            } ]
          }
        """)

      val app = inputJson.as[MarathonApp]
      app must_== MarathonApp(
        id = "cli-example-server",
        container = MarathonContainer(containerType = "DOCKER", docker = Some(MarathonDocker(image = "nginx", network = "BRIDGE", portMappings = Some(Seq(
          MarathonPortMapping(containerPort = 80, protocol = None, hostPort = None, servicePort = None)
        )), parameters = None)), volumes = None),
        cpus = 0.1,
        mem = 128,
        instances = 1,
        cmd = None,
        args = None,
        ports = Some(Seq(0)),
        portDefinitions = None,
        labels = Some(Map("key" -> "value")),
        healthChecks = Some(Seq(MarathonHealthCheck(
          protocol = Some("HTTP"),
          path = Some("/"),
          portIndex = Some(0),
          gracePeriodSeconds = Some(30),
          intervalSeconds = Some(3),
          maxConsecutiveFailures = Some(10)
        )))
      )
    }

    def marathonProviderWithNetworks = {
      val p = mock[GestaltResourceInstance]
      p.properties returns Some(Map(
        "networks" -> Json.parse(
          """[
            |  {
            |    "name": "apps",
            |    "id": "8332a2e4711a",
            |    "description": "full ingress/egress",
            |    "sub_net": "192.168.0.0/16"
            |  }
            |]
          """.stripMargin
        ).toString
      ))
      val pid = UUID.randomUUID()
      p.id returns pid
//      name = "mar-provider",
//      typeId = ResourceIds.MarathonProvider,
//      orgId = UUID.randomUUID(),
//      owner = ResourceOwnerLink(typeId = ResourceIds.User, id = ""),
      p
    }

    "throw exception for marathon payload with invalid provider network" in {
      toMarathonApp("test-container", Json.obj(
        "properties" -> Json.toJson(InputContainerProperties(
          container_type = "DOCKER",
          image = "nginx:latest",
          provider = InputProvider(id = marathonProviderWithNetworks.id),
          network = "missing"
        ))
      ), marathonProviderWithNetworks) must throwA[BadRequestException]("invalid network name")
    }

    "generate marathon payload using provider networks" in {
      val args = Seq("arg1","arg2")
      val appsNet = "apps"
      val marApp = toMarathonApp("test-container", Json.obj(
        "properties" -> Json.toJson(InputContainerProperties(
          container_type = "DOCKER",
          image = "nginx:latest",
          provider = InputProvider(id = marathonProviderWithNetworks.id),
          port_mappings = Seq(
            PortMapping(protocol = "tcp", container_port = 80 , label = Some("http")),
            PortMapping(protocol = "tcp", container_port = 443 , label = Some("https"))
          ),
          network = appsNet,
          args = Some(args),
          num_instances = 1
        ))
      ), marathonProviderWithNetworks)


      marApp.container.docker must beSome
      marApp.container.docker.get.network must beOneOf("HOST", "BRIDGE")
      marApp.container.docker.get.parameters must beSome
      marApp.container.docker.get.parameters.get must containTheSameElementsAs(Seq(KeyValuePair("net", appsNet)))
    }

  }

}


