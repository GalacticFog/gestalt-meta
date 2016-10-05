package controllers

import java.util.UUID

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.api.ContainerSpec
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

      val app = inputJson.as[AppUpdate]
      app must_== AppUpdate(
        id = Some("cli-example-server"),
        container = Some(Container(`type` = "DOCKER", docker = Some(Container.Docker(
          image = "nginx",
          network = Some("BRIDGE"),
          portMappings = Some(Seq(
            Container.Docker.PortMapping(containerPort = 80, protocol = None, hostPort = None, servicePort = None)
          )),
          privileged = None,
          forcePullImage = None,
          parameters = None
        )), volumes = Seq())),
        cpus = Some(0.1),
        mem = Some(128.0),
        disk = None,
        instances = Some(1),
        cmd = None,
        args = None,
        portDefinitions = None,
        labels = Some(Map("key" -> "value")),
        healthChecks = Some(Seq(AppUpdate.HealthCheck(
          protocol = Some("HTTP"),
          path = Some("/"),
          portIndex = Some(0),
          gracePeriodSeconds = Some(30),
          intervalSeconds = Some(3),
          maxConsecutiveFailures = Some(10),
          timeoutSeconds = None
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
        val marContainer = Container(
          docker = None, `type` = "ctype", volumes = Seq(Container.Volume(
            containerPath = "cpath", hostPath = Some("hpath"), mode = "RW", persistent = None
          )
        ))
        Json.toJson(marContainer) must_== marValidJson
        marValidJson.as[Container] must_== marContainer
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
        val marContainer = Container(
          docker = None, `type` = "ctype", volumes = Seq(Container.Volume(
            containerPath = "cpath", hostPath = None, mode = "RW", persistent = Some(Container.PersistentVolumeInfo(size = 42))
          )
        ))
        Json.toJson(marContainer) must_== marValidJson
        marValidJson.as[Container] must_== marContainer
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
        marValidJson.as[Container] must throwA[JsResultException]
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
        marValidJson.as[Container] must throwA[JsResultException]
      }

      "with failure for neither host mapping nor persistent" in {
        val marValidJson =  Json.obj(
          "type" -> "ctype",
          "volumes" -> Json.arr(Json.obj(
            "containerPath" -> "cpath",
            "mode" -> "RW"
          ))
        )
        marValidJson.as[Container] must throwA[JsResultException]
      }

      "with failure for no mode" in {
        val marValidJson =  Json.obj(
          "type" -> "ctype",
          "volumes" -> Json.arr(Json.obj(
            "containerPath" -> "cpath",
            "hostPath" -> "hpath"
          ))
        )
        marValidJson.as[Container] must throwA[JsResultException]
      }
    }

    "add default upgradeStrategy for persistent volumes" in {
      val provider = marathonProviderWithStdNetworks
      val name = "/some/app/id"
      val marApp = toMarathonApp("test-container", ContainerSpec(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithoutNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = 80 , name = Some("http")),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = 443 , name = Some("https"))
        ),
        network = Some("HOST"),
        num_instances = 1,
        volumes = Seq(
          ContainerSpec.VolumeSpec("cpath1", Some("/hpath1"), None, "RW"),
          ContainerSpec.VolumeSpec("cpath1", None, Some(ContainerSpec.VolumeSpec.PersistentVolumeInfo(10)), "RW"),
          ContainerSpec.VolumeSpec("cpath3", Some("/hpath3"), None, "RO")
        )
      ), marathonProviderWithoutNetworks)
      marApp.upgradeStrategy must beSome(UpgradeStrategy(
        0.5,
        0.0
      ))
    }

    "neglect default upgradeStrategy absent persistent volumes" in {
      val provider = marathonProviderWithStdNetworks
      val name = "/some/app/id"
      val marApp = toMarathonApp("test-container", ContainerSpec(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithoutNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = 80 , name = Some("http")),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = 443 , name = Some("https"))
        ),
        network = Some("HOST"),
        num_instances = 1,
        volumes = Seq(
          ContainerSpec.VolumeSpec("cpath1", Some("/hpath1"), None, "RW"),
          ContainerSpec.VolumeSpec("cpath3", Some("/hpath3"), None, "RO")
        )
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

      val marApp = AppUpdate(
        id = Some(name),
        container = Some(Container(
          docker = Some(Container.Docker(
            image = "some/image:tag",
            network = Some("HOST"),
            forcePullImage = Some(true),
            parameters = Some(Seq(
              Container.Docker.Parameter("net", "web-net"),
              Container.Docker.Parameter("user","someUser")
            )),
            privileged = None,
            portMappings = None
          )),
          `type` = "DOCKER",
          volumes = Seq()
        )),
        cpus = Some(2.0),
        mem = Some(256.0),
        instances = Some(3),
        cmd = Some("/usr/bin/someCmd"),
        args = None,
        ipAddress = Some(AppUpdate.IPPerTaskInfo(Some(AppUpdate.DiscoveryInfo(Some(Seq(
        )))))),
        labels = Some(Map()),
        portDefinitions = Some(Seq()),
        healthChecks = Some(Seq()),
        env = Some(Map(
          "env_var_1" -> "env_val_1"
        )),
        user = None
      )

      marApp must_== toMarathonApp(name, resourceJson.as[ContainerSpec], provider)
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

      val marPayload = Json.toJson(toMarathonApp(name, resourceJson.as[ContainerSpec], provider))
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


      val marPayload = Json.toJson(toMarathonApp(name, resourceJson.as[ContainerSpec], provider)).toString
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

      val marApp = AppUpdate(
        id = Some(name),
        container = Some(Container(
          docker = Some(Container.Docker(
            image = "some/image:tag",
            network = Some("HOST"),
            forcePullImage = Some(true),
            parameters = Some(Seq(
              Container.Docker.Parameter("user","someUser")
            )),
            portMappings = None,
            privileged = None
          )),
          `type` = "DOCKER",
          volumes = Seq()
        )),
        cpus = Some(2.0),
        mem = Some(256.0),
        instances = Some(3),
        cmd = Some("/usr/bin/someCmd"),
        args = None,
        ipAddress = None,
        labels = None,
        portDefinitions = None,
        healthChecks = None,
        env = Some(Map(
          "env_var_1" -> "env_val_1"
        )),
        user = None
      )

      marApp must_== toMarathonApp(name, resourceJson.as[ContainerSpec], provider)
    }

    "throw exception for marathon payload with invalid provider network" in {
      toMarathonApp("test-container", ContainerSpec(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithNetworks.id),
        network = Some("missing")
      ), marathonProviderWithNetworks) must throwA[BadRequestException]("invalid network name")
      toMarathonApp("test-container", ContainerSpec(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithNetworks.id),
        network = Some("HOST")
      ), marathonProviderWithNetworks) must throwA[BadRequestException]("invalid network name")
      toMarathonApp("test-container", ContainerSpec(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithNetworks.id),
        network = Some("BRIDGE")
      ), marathonProviderWithNetworks) must throwA[BadRequestException]("invalid network name")
    }

    "generate marathon payload with support for standard host networking" in {
      val marJson = Json.toJson(toMarathonApp("test-container", ContainerSpec(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithoutNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = 80 , name = Some("http")),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = 443 , name = Some("https"))
        ),
        network = Some("HOST"),
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
      val marJson = Json.toJson(toMarathonApp("test-container", ContainerSpec(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithoutNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = 80 , name = Some("http")),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = 443 , name = Some("https"))
        ),
        network = Some("BRIDGE"),
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
      val marJson = Json.toJson(toMarathonApp("test-container", ContainerSpec(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithoutNetworks.id),
        accepted_resource_roles = Option(Seq.empty),
        network = Some("BRIDGE"),
        num_instances = 1
      ), marathonProviderWithoutNetworks)).toString
      marJson must /("container") /("docker") /("network" -> beEqualTo("BRIDGE"))
      marJson must not /("acceptedResourceRoles")
    }

    "generate valid marathon payload from lower case constraints" in {
      val marJson = Json.toJson(toMarathonApp("test-container", ContainerSpec(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithoutNetworks.id),
        constraints = Seq("rack_id:like:1", "hostname:unique"),
        network = Some("BRIDGE"),
        num_instances = 1
      ), marathonProviderWithoutNetworks)).toString
      marJson must /("container") /("docker") /("network" -> beEqualTo("BRIDGE"))
      marJson must /("constraints") */("UNIQUE")
      marJson must /("constraints") */("LIKE")
    }

    "generate marathon payload using provider networks" in {
      val marJson = Json.toJson(toMarathonApp("test-container", ContainerSpec(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = 80 , name = Some("http")),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = 443 , name = Some("https"))
        ),
        network = Some("apps"),
        num_instances = 1
      ), marathonProviderWithNetworks)).toString
      marJson must /("container") /("docker") /("network" -> beEqualTo("USER"))
      marJson must /("container") /("docker") /("parameters") /#(0) /#(0) / "net"
      marJson must /("container") /("docker") /("parameters") /#(0) /#(1) / "apps"
      marJson must not / "ports"
      marJson must havePortDefinitions().not
      marJson must haveIPPerTaskPortDiscovery(
        aDiscoveryPortWith(port = 80, name = "http", protocol = "tcp"),
        aDiscoveryPortWith(port = 443, name = "https", protocol = "tcp")
      )
    }

    "generate marathon payload with support for standard host networking on calico provider" in {
      val marJson = Json.toJson(toMarathonApp("test-container", ContainerSpec(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithStdNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = 80 , name = Some("http")),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = 443 , name = Some("https"))
        ),
        network = Some("HOST"),
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
      val marJson = Json.toJson(toMarathonApp("test-container", ContainerSpec(
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithStdNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = 80 , name = Some("http")),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = 443 , name = Some("https"))
        ),
        network = Some("BRIDGE"),
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


