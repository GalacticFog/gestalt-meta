package controllers

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.laser.MarathonClient
import com.galacticfog.gestalt.meta.api.sdk._
import org.joda.time.DateTime
import org.specs2.mutable._
import play.api.libs.json
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.test.{DefaultAwaitTimeout, FutureAwaits, WithApplication}
import com.galacticfog.gestalt.marathon._

class SpecMarathonProxy extends Specification with FutureAwaits with DefaultAwaitTimeout {

  def pretty(json: JsValue) = Json.prettyPrint(json)

  val wrkName: String = "Meta Workspace"
  val envName: String = "Test Environment"
  val fqon: String = "galacticfog.engineering.test"

  "MarathonClient" should {

//    "handle env-specific app lists" in new WithApplication {
//      val client = MarathonClient(WS.client, "http://v2.galacticfog.com:8080")
//      val listBefore = await(client.listApplicationsInEnvironment(fqon, wrkName, envName))
//      listBefore must beEmpty
//      val marPayload = Json.parse(
//        """
//          {
//            "id": "test-app",
//            "cpus": 0.1,
//            "mem": 128.0,
//            "instances": 1,
//            "ports": [0],
//            "container": {
//               "type": "DOCKER",
//               "docker": {
//                 "image": "nginx",
//                 "network": "BRIDGE",
//                 "portMappings": [
//                    {"containerPort": 80}
//                 ]
//               }
//            }
//          }
//        """
//      ).as[JsObject]
//      await(client.launchContainer(fqon, wrkName, envName, marPayload)) must endWith("test-app")
//      val deployments = await(client.listDeploymentsAffectingEnvironment_marathon_v2(fqon, wrkName, envName))
//      val depsArr = deployments.as[Seq[JsObject]]
//      depsArr must not be empty
//      (depsArr.head \ "affectedApps").asOpt[String] must beSome("\test-app")
//      Thread.sleep(1000)
//      val listAfter = await(client.listApplicationsInEnvironment(fqon, wrkName, envName))
//      listAfter must haveSize(1)
//      listAfter.head.service must_== "/test-app" // strip the environment and stuff
//      Thread.sleep(1000)
//      await(client.deleteApplication(fqon, wrkName, envName, "test-app"))
//    }

//    "handle app groups under an environment" in new WithApplication {
//      val client = MarathonClient(WS.client, "http://v2.galacticfog.com:8080")
//      val listBefore = await(client.listApplicationsInEnvironment(fqon, wrkName, envName))
//      listBefore must beEmpty
//      val marPayload = Json.parse(
//        """
//          {
//            "id": "/web-app/ui",
//            "cpus": 0.1,
//            "mem": 128.0,
//            "instances": 1,
//            "ports": [0],
//            "container": {
//               "type": "DOCKER",
//               "docker": {
//                 "image": "nginx",
//                 "network": "BRIDGE",
//                 "portMappings": [
//                    {"containerPort": 80}
//                 ]
//               }
//            }
//          }
//        """
//      ).as[JsObject]
//      await(client.launchContainer(fqon, wrkName, envName, marPayload)) must endWith("web-app/ui")
//      Thread.sleep(1000)
//      val listAfter = await(client.listApplicationsInEnvironment(fqon, wrkName, envName))
//      listAfter must haveSize(1)
//      listAfter.head.service must_== "/web-app/ui" // strip the environment and stuff
//      Thread.sleep(1000)
//      await(client.deleteApplication(fqon, wrkName, envName, "web-app/ui"))
//    }

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
        container = MarathonContainer(MarathonDocker(image = "nginx", network = "BRIDGE", forcePullImage = false, portMappings = Seq(
          MarathonPortMapping(containerPort = 80)
        ), parameters = None), volumes = None),
        cpus = 0.1,
        mem = 128,
        instances = 1,
        cmd = None,
        args = None,
        ports = Seq(0),
        labels = None,
        healthChecks = Some(Seq(MarathonHealthCheck(
          protocol = Some("HTTP"),
          path = Some("/"),
          portIndex = Some(0),
          gracePeriodSeconds = Some(30),
          intervalSeconds = Some(3),
          timeoutSeconds = Some(10),
          maxConsecutiveFailures = Some(10)
        )))
      )
    }

  }

}


