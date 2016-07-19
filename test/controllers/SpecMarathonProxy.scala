package controllers

import java.util.UUID

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import org.bouncycastle.util.IPAddress
import org.specs2.matcher.{JsonMatchers, Expectations}

import org.specs2.mock._
import org.specs2.mock.mockito._
import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Scope
import play.api.Logger
import play.api.libs.json._


class SpecMarathonProxy extends Specification with MocksCreation with MockitoStubs with CapturedArgument with MockitoMatchers with ArgThat with Expectations with JsonMatchers {

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
          |     "/galacticfog/engineering/test/meta-workspace/test-environment/test-app",
          |     "/galacticfog/engineering/test/meta-workspace/test-environment-2/another-test-app"
          |  ],
          |  "steps" : [ [ {
          |    "action" : "StartApplication",
          |    "app" : "/galacticfog/engineering/test/meta-workspace/test-environment/test-app"
          |  } ], [ {
          |    "action" : "ScaleApplication",
          |    "app" : "/galacticfog/engineering/test/meta-workspace/test-environment/test-app"
          |  } ] ],
          |  "currentActions" : [ {
          |      "action" : "ScaleApplication",
          |      "app" : "/galacticfog/engineering/test/meta-workspace/test-environment/test-app"
          |    }, {
          |      "action" : "ScaleApplication",
          |      "app" : "/galacticfog/engineering/test/meta-workspace/test-environment-2/another-test-app"
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
          |  "affectedApps" : [ "/galacticfog/engineering/test/meta-workspace/test-environment-2/test-app" ],
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
        )))), volumes = None),
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
      val config = Json.obj(
        "networks" -> Json.arr(Json.obj(
          "name" -> "apps",
          "id" -> "8332a2e4711a",
          "description" -> "full ingress/egress",
          "sub_net" -> "192.168.0.0/16"
        ))
      ).toString
      p.properties returns Some(Map(
        "config" -> config
      ))
      val pid = UUID.randomUUID()
      p.id returns pid
      p
    }

    def marathonProviderWithoutNetworks = {
      val p = mock[GestaltResourceInstance]
      p.properties returns Some(Map())
      val pid = UUID.randomUUID()
      p.id returns pid
      p
    }

    def marathonProviderWithStdNetworks = {
      val p = mock[GestaltResourceInstance]
      val config = Json.obj(
        "networks" -> Json.parse(
          """[
            |  {"name": "bridge"},
            |  {"name": "host"},
            |  {"name": "web-net"},
            |  {"name": "db-net"}
            |]
          """.stripMargin
        )
      ).toString
      p.properties returns Some(Map(
        "config" -> config
      ))
      val pid = UUID.randomUUID()
      p.id returns pid
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
      toMarathonApp("test-container", Json.obj(
        "properties" -> Json.toJson(InputContainerProperties(
          container_type = "DOCKER",
          image = "nginx:latest",
          provider = InputProvider(id = marathonProviderWithNetworks.id),
          network = "HOST"
        ))
      ), marathonProviderWithNetworks) must throwA[BadRequestException]("invalid network name")
      toMarathonApp("test-container", Json.obj(
        "properties" -> Json.toJson(InputContainerProperties(
          container_type = "DOCKER",
          image = "nginx:latest",
          provider = InputProvider(id = marathonProviderWithNetworks.id),
          network = "BRIDGE"
        ))
      ), marathonProviderWithNetworks) must throwA[BadRequestException]("invalid network name")
    }

    "generate marathon payload with support for standard host networking" in {
      val marApp = Json.toJson(toMarathonApp("test-container", Json.obj(
        "properties" -> Json.toJson(InputContainerProperties(
          container_type = "DOCKER",
          image = "nginx:latest",
          provider = InputProvider(id = marathonProviderWithoutNetworks.id),
          port_mappings = Seq(
            PortMapping(protocol = "tcp", container_port = 80 , label = Some("http")),
            PortMapping(protocol = "tcp", container_port = 443 , label = Some("https"))
          ),
          network = "HOST",
          num_instances = 1
        ))
      ), marathonProviderWithoutNetworks)).as[MarathonApp]
      marApp.container.docker must beSome
      marApp.container.docker.get.network.toUpperCase must_== "HOST"
      marApp.container.docker.get.portMappings must beNone
      marApp.ports must beNone
      marApp.portDefinitions must beSome
      marApp.ipAddress must beNone
      marApp.portDefinitions.get must containTheSameElementsAs(Seq(
        PortDefinition(80, Some("tcp"), name = Some("http"), labels = None),
        PortDefinition(443, Some("tcp"), name = Some("https"), labels = None)
      ))
    }

    "generate marathon payload with support for standard bridge networking" in {
      val marApp = Json.toJson(toMarathonApp("test-container", Json.obj(
        "properties" -> Json.toJson(InputContainerProperties(
          container_type = "DOCKER",
          image = "nginx:latest",
          provider = InputProvider(id = marathonProviderWithoutNetworks.id),
          port_mappings = Seq(
            PortMapping(protocol = "tcp", container_port = 80 , label = Some("http")),
            PortMapping(protocol = "tcp", container_port = 443 , label = Some("https"))
          ),
          network = "BRIDGE",
          num_instances = 1
        ))
      ), marathonProviderWithoutNetworks)).as[MarathonApp]
      marApp.container.docker must beSome
      marApp.container.docker.get.network.toUpperCase must_== "BRIDGE"
      marApp.ports must beNone
      marApp.portDefinitions must beSome
      marApp.ipAddress must beNone
      marApp.portDefinitions.get must containTheSameElementsAs(Seq(
        PortDefinition(80, Some("tcp"), name = Some("http"), labels = None),
        PortDefinition(443, Some("tcp"), name = Some("https"), labels = None)
      ))
    }



    "generate marathon payload using provider networks" in {
      val marApp = Json.toJson(toMarathonApp("test-container", Json.obj(
        "properties" -> Json.toJson(InputContainerProperties(
          container_type = "DOCKER",
          image = "nginx:latest",
          provider = InputProvider(id = marathonProviderWithNetworks.id),
          port_mappings = Seq(
            PortMapping(protocol = "tcp", container_port = 80 , label = Some("http")),
            PortMapping(protocol = "tcp", container_port = 443 , label = Some("https"))
          ),
          network = "apps",
          num_instances = 1
        ))
      ), marathonProviderWithNetworks)).as[MarathonApp]
      marApp.container.docker must beSome
      marApp.container.docker.get.network.toUpperCase must_== "HOST"
      marApp.container.docker.get.parameters must beSome(Seq(KeyValuePair("net", "apps")))
      marApp.ports must beNone
      marApp.portDefinitions must beNone
      marApp.ipAddress must beSome(IPPerTaskInfo(
        discovery = Some(DiscoveryInfo(
          ports = Some(Seq(
            PortDiscovery(number = 80, name = "http", protocol = "tcp"),
            PortDiscovery(number = 443, name = "https", protocol = "tcp")
          ))
        ))
      ))
    }

    "generate marathon payload with support for standard host networking on calico provider" in {
      val marApp = Json.toJson(toMarathonApp("test-container", Json.obj(
        "properties" -> Json.toJson(InputContainerProperties(
          container_type = "DOCKER",
          image = "nginx:latest",
          provider = InputProvider(id = marathonProviderWithStdNetworks.id),
          port_mappings = Seq(
            PortMapping(protocol = "tcp", container_port = 80 , label = Some("http")),
            PortMapping(protocol = "tcp", container_port = 443 , label = Some("https"))
          ),
          network = "host",
          num_instances = 1
        ))
      ), marathonProviderWithStdNetworks)).as[MarathonApp]
      marApp.container.docker must beSome
      marApp.container.docker.get.network.toUpperCase must_== "HOST"
      marApp.container.docker.get.parameters must beNone
      marApp.container.docker.get.portMappings must beNone
      marApp.ports must beNone
      marApp.portDefinitions must beSome
      marApp.ipAddress must beNone
      marApp.portDefinitions.get must containTheSameElementsAs(Seq(
        PortDefinition(80, Some("tcp"), name = Some("http"), labels = None),
        PortDefinition(443, Some("tcp"), name = Some("https"), labels = None)
      ))
    }

    "generate marathon payload with support for standard bridge networking on calico provider" in {
      val marApp = Json.toJson(toMarathonApp("test-container", Json.obj(
        "properties" -> Json.toJson(InputContainerProperties(
          container_type = "DOCKER",
          image = "nginx:latest",
          provider = InputProvider(id = marathonProviderWithStdNetworks.id),
          port_mappings = Seq(
            PortMapping(protocol = "tcp", container_port = 80 , label = Some("http")),
            PortMapping(protocol = "tcp", container_port = 443 , label = Some("https"))
          ),
          network = "bridge",
          num_instances = 1
        ))
      ), marathonProviderWithStdNetworks)).as[MarathonApp]
      marApp.container.docker must beSome
      marApp.container.docker.get.network.toUpperCase must_== "BRIDGE"
      marApp.container.docker.get.parameters must beNone
      marApp.ports must beNone
      marApp.ipAddress must beNone
      marApp.portDefinitions must beSome
      marApp.portDefinitions.get must containTheSameElementsAs(Seq(
        PortDefinition(80, Some("tcp"), name = Some("http"), labels = None),
        PortDefinition(443, Some("tcp"), name = Some("https"), labels = None)
      ))
    }


    "serialize marathon volumes appropriately" in {
      val marValidJson =  Json.obj(
        "type" -> "ctype",
        "volumes" -> Json.arr(Json.obj(
          "containerPath" -> "cpath",
          "hostPath" -> "hpath",
          "mode" -> "RW"
        ))
      )

      val marContainer = MarathonContainer(
        docker = None, containerType = "ctype", volumes = Some(Seq(Volume(
          container_path = "cpath", host_path = "hpath", mode = "RW"
        )))
      )

      Json.toJson(marContainer) must_== marValidJson
      marValidJson.as[MarathonContainer] must_== marContainer
    }

    "serialize marathon app appropriately" in {
      val marContainer = MarathonContainer(docker = None, containerType = "ctype")

      val marApp = MarathonApp(
        id = "someId",
        container = marContainer,
        user = Some("someUser"),
        ipAddress = Some(IPPerTaskInfo(discovery = None)),
        portDefinitions = Some(Seq()),
        cpus = 0.2,
        mem = 128.0,
        instances = 1
      )

      val marValidJson = Json.obj(
        "id" -> "someId",
        "container" -> Json.obj(
          "type" -> "ctype"
        ),
        "cpus" -> 0.2,
        "mem" -> 128.0,
        "instances" -> 1,
        "user" -> "someUser",
        "ipAddress" -> Json.obj(),
        "portDefinitions" -> Json.arr()
      )

      Json.toJson(marApp) must_== marValidJson
      marValidJson.as[MarathonApp] must_== marApp
    }

    "transform to Marathon API appropriately (host-style)" in {
      val providerId = UUID.randomUUID()
      val name = "/some/app/id"
      val resourceJson = Json.obj(
        "properties" -> Json.obj(
          "container_type" -> "DOCKER",
          "image" -> "some/image:tag",
          "provider" -> Json.obj(
            "id" -> providerId
          ),
          "port_mappings" -> Json.arr(),
          "cmd" -> "/usr/bin/someCmd",
          "cpus" -> 2.0,
          "memory" -> 256.0,
          "num_instances" -> 3,
          "network" -> "HOST",
          "force_pull" -> true,
          "user" -> "someUser",
          "env" -> Json.obj(
            "env_var_1" -> "env_val_1"
          )
        )
      )
      val provider = mock[GestaltResourceInstance]
      val config = Json.obj(
        "networks" -> Json.arr(Json.obj(
          "name" -> "HOST",
          "id" -> "8332a2e4711a",
          "description" -> "",
          "sub_net" -> "192.168.0.0/16"
        ))
      ).toString
      provider.properties returns Some(Map(
        "config" -> config
      ))
      provider.id returns providerId

      val marApp = MarathonApp(
        id = name,
        container = MarathonContainer(
          docker = Some(MarathonDocker(
            image = "some/image:tag",
            network = "HOST",
            forcePullImage = Some(true),
            parameters = Some(Seq(
              KeyValuePair("user","someUser")
            ))
          )),
          containerType = "DOCKER"
        ),
        cpus = 2.0,
        mem = 256.0,
        instances = 3,
        cmd = Some("/usr/bin/someCmd"),
        args = None,
        ipAddress = None,
        labels = None,
        portDefinitions = Some(Seq()),
        healthChecks = None,
        env = Some(Map(
          "env_var_1" -> "env_val_1"
        )),
        user = None
      )

      marApp must_== toMarathonApp(name, resourceJson, provider)
    }

    "transform to Marathon API appropriately (calico-style)" in {
      val provider = marathonProviderWithStdNetworks
      val name = "/some/app/id"
      val resourceJson = Json.obj(
        "properties" -> Json.obj(
          "container_type" -> "DOCKER",
          "image" -> "some/image:tag",
          "provider" -> Json.obj(
            "id" -> provider.id
          ),
          "port_mappings" -> Json.arr(),
          "cmd" -> "/usr/bin/someCmd",
          "cpus" -> 2.0,
          "memory" -> 256.0,
          "num_instances" -> 3,
          "network" -> "web-net",
          "force_pull" -> true,
          "user" -> "someUser",
          "env" -> Json.obj(
            "env_var_1" -> "env_val_1"
          )
        )
      )

      val marApp = MarathonApp(
        id = name,
        container = MarathonContainer(
          docker = Some(MarathonDocker(
            image = "some/image:tag",
            network = "HOST",
            forcePullImage = Some(true),
            parameters = Some(Seq(
              KeyValuePair("net", "web-net"),
              KeyValuePair("user","someUser")
            ))
          )),
          containerType = "DOCKER"
        ),
        cpus = 2.0,
        mem = 256.0,
        instances = 3,
        cmd = Some("/usr/bin/someCmd"),
        args = None,
        ipAddress = Some(IPPerTaskInfo(Some(DiscoveryInfo(Some(Seq(
        )))))),
        labels = None,
        portDefinitions = None,
        healthChecks = None,
        env = Some(Map(
          "env_var_1" -> "env_val_1"
        )),
        user = None
      )

      marApp must_== toMarathonApp(name, resourceJson, provider)
    }

  }


}


