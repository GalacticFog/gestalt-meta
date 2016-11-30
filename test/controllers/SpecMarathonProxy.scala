package controllers

import java.util.UUID

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.marathon.IPPerTaskInfo.DiscoveryInfo
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import org.bouncycastle.util.IPAddress
import org.specs2.matcher.{Matcher, JsonType, JsonMatchers, Expectations}

import org.specs2.mock._
import org.specs2.mock.mockito._
import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Scope
import play.api.Logger
import play.api.libs.json._

import scala.util.parsing.json.JSONArray


class SpecMarathonProxy extends Specification with Mockito with JsonMatchers {

  val wrkName: String = "Meta Workspace"
  val envName: String = "Test Environment"
  val fqon: String = "galacticfog.engineering.test"

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
          |  {"name": "BRIDGE"},
          |  {"name": "HOST"},
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
        container = MarathonContainer(containerType = "DOCKER", docker = Some(MarathonContainer.Docker(
          image = "nginx",
          network = "BRIDGE",
          portMappings = Some(Seq(
            MarathonContainer.Docker.PortMapping(containerPort = Some(80), protocol = None, hostPort = None, servicePort = None)
          )))),
          volumes = None
        ),
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


    "serialize marathon volumes appropriately" in {

      "with standard volume mapping" in {
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
            container_path = "cpath", host_path = Some("hpath"), mode = "RW", persistent = None
          )))
        )
        Json.toJson(marContainer) must_== marValidJson
        marValidJson.as[MarathonContainer] must_== marContainer
      }

      "with persistent volume" in {
        val marValidJson =  Json.obj(
          "type" -> "ctype",
          "volumes" -> Json.arr(Json.obj(
            "containerPath" -> "cpath",
            "mode" -> "RW",
            "persistent" -> Json.obj(
              "size" -> 42
            )
          ))
        )
        val marContainer = MarathonContainer(
          docker = None, containerType = "ctype", volumes = Some(Seq(Volume(
            container_path = "cpath", host_path = None, mode = "RW", persistent = Some(PersistentVolumeInfo(size = 42))
          )))
        )
        Json.toJson(marContainer) must_== marValidJson
        marValidJson.as[MarathonContainer] must_== marContainer
      }

      "with failure for missing persistent size" in {
        val marValidJson =  Json.obj(
          "type" -> "ctype",
          "volumes" -> Json.arr(Json.obj(
            "containerPath" -> "cpath",
            "mode" -> "RW",
            "persistent" -> Json.obj()
          ))
        )
        marValidJson.as[MarathonContainer] must throwA[JsResultException]
      }

      "with failure for host mapping and persistent" in {
        val marValidJson =  Json.obj(
          "type" -> "ctype",
          "volumes" -> Json.arr(Json.obj(
            "containerPath" -> "cpath",
            "hostPath" -> "hpath",
            "mode" -> "RW",
            "persistent" -> Json.obj(
              "size" -> 10
            )
          ))
        )
        marValidJson.as[MarathonContainer] must throwA[JsResultException]
      }

      "with failure for neither host mapping nor persistent" in {
        val marValidJson =  Json.obj(
          "type" -> "ctype",
          "volumes" -> Json.arr(Json.obj(
            "containerPath" -> "cpath",
            "mode" -> "RW"
          ))
        )
        marValidJson.as[MarathonContainer] must throwA[JsResultException]
      }

      "with failure for no mode" in {
        val marValidJson =  Json.obj(
          "type" -> "ctype",
          "volumes" -> Json.arr(Json.obj(
            "containerPath" -> "cpath",
            "hostPath" -> "hpath"
          ))
        )
        marValidJson.as[MarathonContainer] must throwA[JsResultException]
      }
    }

    "add default upgradeStrategy for persistent volumes" in {
      val provider = marathonProviderWithStdNetworks
      val name = "/some/app/id"
      val marApp = toMarathonApp("org","wkr","env","test-container", InputContainerProperties(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = InputProvider(id = marathonProviderWithoutNetworks.id),
        port_mappings = Seq(
          PortMapping(protocol = "tcp", container_port = Some(80), label = Some("http")),
          PortMapping(protocol = "tcp", container_port = Some(443), label = Some("https"))
        ),
        network = "HOST",
        num_instances = 1,
        volumes = Some(Seq(
          Volume("cpath1", Some("/hpath1"), None, "RW"),
          Volume("cpath1", None, Some(PersistentVolumeInfo(10)), "RW"),
          Volume("cpath3", Some("/hpath3"), None, "RO")
        ))
      ), marathonProviderWithoutNetworks)
      marApp.upgradeStrategy must beSome(UpgradeStrategy(
        0.5,
        0.0
      ))
    }

    "neglect default upgradeStrategy absent persistent volumes" in {
      val provider = marathonProviderWithStdNetworks
      val name = "/some/app/id"
      val marApp = toMarathonApp("org","wkr","env","test-container", InputContainerProperties(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = InputProvider(id = marathonProviderWithoutNetworks.id),
        port_mappings = Seq(
          PortMapping(protocol = "tcp", container_port = Some(80), label = Some("http")),
          PortMapping(protocol = "tcp", container_port = Some(443), label = Some("https"))
        ),
        network = "HOST",
        num_instances = 1,
        volumes = Some(Seq(
          Volume("cpath1", Some("/hpath1"), None, "RW"),
          Volume("cpath3", Some("/hpath3"), None, "RO")
        ))
      ), marathonProviderWithoutNetworks)
      marApp.upgradeStrategy must beNone
    }

  }

  "Marathon app payload generation" should {

    def aPortWith(port: Matcher[JsonType], name: Matcher[JsonType],  protocol: Matcher[JsonType]): Matcher[String] =
      /("port").andHave(port) and /("name").andHave(name) and /("protocol").andHave(protocol)

    def aDiscoveryPortWith(port: Matcher[JsonType], name: Matcher[JsonType],  protocol: Matcher[JsonType]): Matcher[String] =
      /("number").andHave(port) and /("name").andHave(name) and /("protocol").andHave(protocol)

    def havePortDefinitions(ports: Matcher[String]*): Matcher[String] =
      /("portDefinitions").andHave(allOf(ports:_*))

    def haveIPPerTaskPortDiscovery(ports: Matcher[String]*): Matcher[String] =
      /("ipAddress")./("discovery")./("ports").andHave(allOf(ports:_*))

    def haveArgs(args: Matcher[String]*): Matcher[String] =
      /("args").andHave(allOf(args:_*))

    def haveNoArgs(): Matcher[String] =
      /("args").and(be_==(JsNull))

    "transform to Marathon API appropriately (calico-style)" in {
      val provider = marathonProviderWithStdNetworks
      val name = "/some/app/id"
      val resourceJson = Json.obj(
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

      val marApp = MarathonApp(
        id = name,
        container = MarathonContainer(
          docker = Some(MarathonContainer.Docker(
            image = "some/image:tag",
            network = "USER",
            forcePullImage = Some(true),
            portMappings = Some(Seq.empty),
            parameters = Some(Seq(KeyValuePair("user", "someUser")))
          )),
          containerType = "DOCKER"
        ),
        cpus = 2.0,
        mem = 256.0,
        instances = 3,
        cmd = Some("/usr/bin/someCmd"),
        args = None,
        ipAddress = Some(IPPerTaskInfo(
          discovery = None,
          networkName = Some("web-net")
        )),
        labels = None,
        portDefinitions = None,
        healthChecks = None,
        env = Some(Map(
          "env_var_1" -> "env_val_1"
        )),
        user = None
      )

      toMarathonApp("org","wkr","env",name, resourceJson.as[InputContainerProperties], provider) must_== marApp
    }

    "generate valid payload with cmd and no args" in {
      val providerId = UUID.randomUUID()
      val name = "/some/app/id"
      val resourceJson = Json.obj(
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
        "force_pull" -> true
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

      val marPayload = Json.toJson(toMarathonApp("org","wkr","env",name, resourceJson.as[InputContainerProperties], provider))
      marPayload.toString must /("cmd" -> "/usr/bin/someCmd")
      (marPayload \ "args") must_== JsNull
    }

    "generate valid payload with neither cmd nor args" in {
      val providerId = UUID.randomUUID()
      val name = "/some/app/id"
      val resourceJson = Json.obj(
        "container_type" -> "DOCKER",
        "image" -> "some/image:tag",
        "provider" -> Json.obj(
          "id" -> providerId
        ),
        "port_mappings" -> Json.arr(),
        "cpus" -> 2.0,
        "memory" -> 256.0,
        "num_instances" -> 3,
        "network" -> "HOST",
        "force_pull" -> true
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


      val marPayload = Json.toJson(toMarathonApp("org","wkr","env",name, resourceJson.as[InputContainerProperties], provider)).toString
      marPayload must not /("cmd")
      marPayload must haveArgs()
    }


    "transform to Marathon API appropriately (host-style)" in {
      val providerId = UUID.randomUUID()
      val name = "/some/app/id"
      val resourceJson = Json.obj(
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
          docker = Some(MarathonContainer.Docker(
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
        ports = None,
        healthChecks = None,
        env = Some(Map(
          "env_var_1" -> "env_val_1"
        )),
        user = None
      )

      marApp must_== toMarathonApp("org","wkr","env",name, resourceJson.as[InputContainerProperties], provider)
    }

    "throw exception for marathon payload with invalid provider network" in {
      toMarathonApp("org","wkr","env","test-container", InputContainerProperties(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = InputProvider(id = marathonProviderWithNetworks.id),
        network = "missing"
      ), marathonProviderWithNetworks) must throwA[BadRequestException]("invalid network name")
      toMarathonApp("org","wkr","env","test-container", InputContainerProperties(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = InputProvider(id = marathonProviderWithNetworks.id),
        network = "HOST"
      ), marathonProviderWithNetworks) must throwA[BadRequestException]("invalid network name")
      toMarathonApp("org","wkr","env","test-container", InputContainerProperties(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = InputProvider(id = marathonProviderWithNetworks.id),
        network = "BRIDGE"
      ), marathonProviderWithNetworks) must throwA[BadRequestException]("invalid network name")
    }

    "generate marathon payload with support for standard host networking" in {
      val marJson = Json.toJson(toMarathonApp("org","wkr","env","test-container", InputContainerProperties(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = InputProvider(id = marathonProviderWithoutNetworks.id),
        port_mappings = Seq(
          PortMapping(protocol = "tcp", container_port = Some(80), label = Some("http")),
          PortMapping(protocol = "tcp", container_port = Some(443), label = Some("https"))
        ),
        network = "HOST",
        num_instances = 1
      ), marathonProviderWithoutNetworks)).toString
      marJson must /("container") /("docker") /("network" -> beEqualTo("HOST"))
      marJson must not /("container") /("docker") /("portMappings")
      marJson must not /("ports")
      marJson must not /("ipAddress")
      marJson must havePortDefinitions(
        aPortWith(port = 80, name = "http", protocol = "tcp"),
        aPortWith(port = 443, name = "https", protocol = "tcp")
      )
    }

    "generate marathon payload with support for standard bridge networking" in {
      val marJson = Json.toJson(toMarathonApp("org","wkr","env","test-container", InputContainerProperties(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = InputProvider(id = marathonProviderWithoutNetworks.id),
        port_mappings = Seq(
          PortMapping(protocol = "tcp", container_port = Some(80), label = Some("http")),
          PortMapping(protocol = "tcp", container_port = Some(443), label = Some("https"))
        ),
        network = "BRIDGE",
        num_instances = 1
      ), marathonProviderWithoutNetworks)).toString
      marJson must /("container") /("docker") /("network" -> beEqualTo("BRIDGE"))
      marJson must not /("ports")
      marJson must not /("ipAddress")
      marJson must havePortDefinitions(
        aPortWith(port = 80, name = "http", protocol = "tcp"),
        aPortWith(port = 443, name = "https", protocol = "tcp")
      )
    }

    "generate marathon payload with empty acceptedResourceRoles" in {
      val marJson = Json.toJson(toMarathonApp("org","wkr","env","test-container", InputContainerProperties(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = InputProvider(id = marathonProviderWithoutNetworks.id),
        accepted_resource_roles = Option(Seq.empty),
        network = "BRIDGE",
        num_instances = 1
      ), marathonProviderWithoutNetworks)).toString
      marJson must /("container") /("docker") /("network" -> beEqualTo("BRIDGE"))
      marJson must not /("acceptedResourceRoles")
    }

    "generate valid marathon payload from lower case constraints" in {
      val marJson = Json.toJson(toMarathonApp("org","wkr","env","test-container", InputContainerProperties(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = InputProvider(id = marathonProviderWithoutNetworks.id),
        constraints = Some(Seq("rack_id:like:1", "hostname:unique")),
        network = "BRIDGE",
        num_instances = 1
      ), marathonProviderWithoutNetworks)).toString
      marJson must /("container") /("docker") /("network" -> beEqualTo("BRIDGE"))
      marJson must /("constraints") */("UNIQUE")
      marJson must /("constraints") */("LIKE")
    }

    "generate marathon payload using provider networks" in {
      val marJson = Json.toJson(toMarathonApp("org","wkr","env","test-container", InputContainerProperties(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = InputProvider(id = marathonProviderWithNetworks.id),
        port_mappings = Seq(
          PortMapping(protocol = "tcp", container_port = Some(80), label = Some("http"), load_balanced = Some(true)),
          PortMapping(protocol = "tcp", container_port = Some(443), label = Some("https"))
        ),
        network = "apps",
        num_instances = 1
      ), marathonProviderWithNetworks)).toString
      marJson must /("container") /("docker") /("network" -> beEqualTo("USER"))
      marJson must /("ipAddress") /("networkName" -> beEqualTo("apps"))
      marJson must /("container") /("docker") /("portMappings") /#(0) /("name" -> "http")
      marJson must /("container") /("docker") /("portMappings") /#(0) /("labels") /("VIP_0" -> "")
      marJson must /("container") /("docker") /("portMappings") /#(1) /("name" -> "https")
      marJson must not /("container") /("docker") /("portMappings") /#(1) /("labels")
      marJson must not / "ports"
      marJson must havePortDefinitions()
      marJson must not /("ipAddress") /("discovery")
    }

    "generate marathon payload with support for standard host networking on calico provider" in {
      val marJson = Json.toJson(toMarathonApp("org","wkr","env","test-container", InputContainerProperties(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = InputProvider(id = marathonProviderWithStdNetworks.id),
        port_mappings = Seq(
          PortMapping(protocol = "tcp", container_port = Some(80), label = Some("http")),
          PortMapping(protocol = "tcp", container_port = Some(443), label = Some("https"))
        ),
        network = "HOST",
        num_instances = 1
      ), marathonProviderWithStdNetworks)).toString
      marJson must /("container") /("docker") /("network" -> beEqualTo("HOST"))
      marJson must not /("container") /("docker") / "portMappings"
      marJson must not /("container") /("docker") / "parameters"
      marJson must not / "ports"
      marJson must not / "ipAddress"
      marJson must havePortDefinitions(
        aPortWith(port = 80, name = "http", protocol = "tcp"),
        aPortWith(port = 443, name = "https", protocol = "tcp")
      )
    }

    "generate marathon payload with support for standard bridge networking on calico provider" in {
      val marJson = Json.toJson(toMarathonApp("org","wkr","env","test-container", InputContainerProperties(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = InputProvider(id = marathonProviderWithStdNetworks.id),
        port_mappings = Seq(
          PortMapping(protocol = "tcp", container_port = Some(80), label = Some("http")),
          PortMapping(protocol = "tcp", container_port = Some(443), label = Some("https"))
        ),
        network = "BRIDGE",
        num_instances = 1
      ), marathonProviderWithStdNetworks)).toString
      marJson must /("container") /("docker") /("network" -> beEqualTo("BRIDGE"))
      marJson must not /("container") /("docker") / "parameters"
      marJson must not / "ports"
      marJson must not / "ipAddress"
      marJson must havePortDefinitions(
        aPortWith(port = 80, name = "http", protocol = "tcp"),
        aPortWith(port = 443, name = "https", protocol = "tcp")
      )
    }

  }

}


