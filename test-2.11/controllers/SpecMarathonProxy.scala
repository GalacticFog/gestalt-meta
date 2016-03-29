package controllers

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.laser.MarathonClient
import com.galacticfog.gestalt.meta.api.sdk._
import org.joda.time.DateTime
import org.specs2.mutable._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.test.{DefaultAwaitTimeout, FutureAwaits, WithApplication}

class SpecMarathonProxy extends Specification with FutureAwaits with DefaultAwaitTimeout {

  def pretty(json: JsValue) = Json.prettyPrint(json)

  val wrkName: String = "Test Workspace"
  val envName: String = "Test Environment"
  val fqon: String = "galacticfog.engineering.core"

  "MarathonClient" should {

    "handle env-specific app lists" in new WithApplication {
      val client = MarathonClient(WS.client, "http://v2.galacticfog.com:8080")
      val listBefore = await(client.listApplicationsInEnvironment(fqon, wrkName, envName))
      listBefore must beEmpty
      val marPayload = Json.parse(
        """
          {
            "id": "test-app",
            "cpus": 0.1,
            "mem": 128.0,
            "instances": 1,
            "ports": [0],
            "container": {
               "type": "DOCKER",
               "docker": {
                 "image": "nginx",
                 "network": "BRIDGE",
                 "portMappings": [
                    {"containerPort": 80}
                 ]
               }
            }
          }
        """
      ).as[JsObject]
      await(client.launchContainer(fqon, wrkName, envName, marPayload)) must endWith("test-app")
      Thread.sleep(1000)
      val listAfter = await(client.listApplicationsInEnvironment(fqon, wrkName, envName))
      listAfter must haveSize(1)
      listAfter.head.service must_== "/test-app" // strip the environment and stuff
      Thread.sleep(1000)
      await(client.deleteApplication(fqon, wrkName, envName, "test-app"))
    }

    "handle app groups under an environment" in new WithApplication {
      val client = MarathonClient(WS.client, "http://v2.galacticfog.com:8080")
      val listBefore = await(client.listApplicationsInEnvironment(fqon, wrkName, envName))
      listBefore must beEmpty
      val marPayload = Json.parse(
        """
          {
            "id": "/web-app/ui",
            "cpus": 0.1,
            "mem": 128.0,
            "instances": 1,
            "ports": [0],
            "container": {
               "type": "DOCKER",
               "docker": {
                 "image": "nginx",
                 "network": "BRIDGE",
                 "portMappings": [
                    {"containerPort": 80}
                 ]
               }
            }
          }
        """
      ).as[JsObject]
      await(client.launchContainer(fqon, wrkName, envName, marPayload)) must endWith("web-app/ui")
      Thread.sleep(1000)
      val listAfter = await(client.listApplicationsInEnvironment(fqon, wrkName, envName))
      listAfter must haveSize(1)
      listAfter.head.service must_== "/web-app/ui" // strip the environment and stuff
      Thread.sleep(1000)
      await(client.deleteApplication(fqon, wrkName, envName, "web-app/ui"))
    }

  }

}


