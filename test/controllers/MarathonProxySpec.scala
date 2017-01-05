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


class MarathonProxySpec extends Specification with Mockito with JsonMatchers {

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
            Container.Docker.PortMapping(containerPort = Some(80))
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
      val marApp = toMarathonLaunchPayload("org","wrk","env","name",ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithoutNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(80) , name = Some("http")),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(443), name = Some("https"))
        ),
        network = Some("HOST"),
        num_instances = 1,
        volumes = Seq(
          ContainerSpec.Volume("cpath1", Some("/hpath1"), None, "RW"),
          ContainerSpec.Volume("cpath1", None, Some(ContainerSpec.Volume.PersistentVolumeInfo(10)), "RW"),
          ContainerSpec.Volume("cpath3", Some("/hpath3"), None, "RO")
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
      val marApp = toMarathonLaunchPayload("org","wrk","env","name",ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithoutNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(80), name = Some("http")),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(443), name = Some("https"))
        ),
        network = Some("HOST"),
        num_instances = 1,
        volumes = Seq(
          ContainerSpec.Volume("cpath1", Some("/hpath1"), None, "RW"),
          ContainerSpec.Volume("cpath3", Some("/hpath3"), None, "RO")
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
        id = None,
        container = Some(Container(
          docker = Some(Container.Docker(
            image = "some/image:tag",
            network = Some("USER"),
            forcePullImage = Some(true),
            parameters = Some(Seq(
              Container.Docker.Parameter("user","someUser")
            )),
            privileged = Some(false),
            portMappings = Some(Seq.empty)
          )),
          `type` = "DOCKER",
          volumes = Seq()
        )),
        constraints = None,
        cpus = Some(2.0),
        mem = Some(256.0),
        instances = Some(3),
        cmd = Some("/usr/bin/someCmd"),
        args = None,
        ipAddress = Some(AppUpdate.IPPerTaskInfo(discovery = None, networkName = Some("web-net"))),
        labels = Some(Map()),
        portDefinitions = None,
        healthChecks = Some(Seq()),
        env = Some(Map(
          "env_var_1" -> "env_val_1"
        )),
        user = None
      )

      toMarathonLaunchPayload("org","wrk","env","name",resourceJson.as[ContainerSpec], provider) must_== marApp
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

      val marPayload = Json.toJson(toMarathonLaunchPayload("org","wrk","env","name",resourceJson.as[ContainerSpec], provider)).as[JsObject]
      marPayload.toString must /("cmd" -> "/usr/bin/someCmd")
      marPayload.keys.contains("args") must beFalse
    }

    "transform to Marathon API appropriately (host-style)" in {
      val providerId = UUID.randomUUID()
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
        id = None,
        container = Some(Container(
          docker = Some(Container.Docker(
            image = "some/image:tag",
            network = Some("HOST"),
            forcePullImage = Some(true),
            parameters = Some(Seq(
              Container.Docker.Parameter("user","someUser")
            )),
            portMappings = None,
            privileged = Some(false)
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
        labels = Some(Map()),
        portDefinitions = Some(Seq()),
        healthChecks = Some(Seq()),
        env = Some(Map(
          "env_var_1" -> "env_val_1"
        )),
        user = None
      )

      toMarathonLaunchPayload("org","wrk","env","name",resourceJson.as[ContainerSpec], provider) must_== marApp
    }

    "throw exception for marathon payload with invalid provider network" in {
      toMarathonLaunchPayload("org","wrk","env","name",ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithNetworks.id),
        network = Some("missing")
      ), marathonProviderWithNetworks) must throwA[BadRequestException]("invalid network name")
      toMarathonLaunchPayload("org","wrk","env","name",ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithNetworks.id),
        network = Some("HOST")
      ), marathonProviderWithNetworks) must throwA[BadRequestException]("invalid network name")
      toMarathonLaunchPayload("org","wrk","env","name",ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithNetworks.id),
        network = Some("BRIDGE")
      ), marathonProviderWithNetworks) must throwA[BadRequestException]("invalid network name")
    }

    "generate marathon payload with support for standard host networking" in {
      val marJson = Json.toJson(toMarathonLaunchPayload("org","wrk","env","name",ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithoutNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", name = Some("http")),
          ContainerSpec.PortMapping(protocol = "tcp", name = Some("https"))
        ),
        network = Some("HOST"),
        num_instances = 1
      ), marathonProviderWithoutNetworks)).toString
      marJson must /("container") /("docker") /("network" -> beEqualTo("HOST"))
      marJson must not /("container") /("docker") /("portMappings")
      marJson must not /("ports")
      marJson must not /("ipAddress")
      marJson must havePortDefinitions(
        aPortWith(port = 0, name = "http", protocol = "tcp"),
        aPortWith(port = 0, name = "https", protocol = "tcp")
      )
    }

    "generate marathon payload with support for standard bridge networking" in {
      val marJson = Json.toJson(toMarathonLaunchPayload("org","wrk","env","name",ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithoutNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(80), name = Some("http")),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(443), name = Some("https"))
        ),
        network = Some("BRIDGE"),
        num_instances = 1
      ), marathonProviderWithoutNetworks)).toString
      marJson must /("container") /("docker") /("network" -> beEqualTo("BRIDGE"))
      marJson must not /("ports")
      marJson must not /("ipAddress")
      marJson must not /("portDefinitions")
    }

    "generate marathon payload with empty acceptedResourceRoles" in {
      val marJson = Json.toJson(toMarathonLaunchPayload("org","wrk","env","name",ContainerSpec(
        name = "test-container",
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
      val marJson = Json.toJson(toMarathonLaunchPayload("org","wrk","env","name",ContainerSpec(
        name = "test-container",
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
      val marJson = Json.toJson(toMarathonLaunchPayload("org","wrk","env","name",ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(80), name = Some("http")),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(443), name = Some("https"))
        ),
        network = Some("apps"),
        num_instances = 1
      ), marathonProviderWithNetworks)).toString
      marJson must /("container") /("docker") /("network" -> beEqualTo("USER"))
      marJson must /("ipAddress") /("networkName" -> "apps")
      marJson must not / "ports"
      marJson must havePortDefinitions().not
      marJson must not /("ipAddress") /("discoveryInfo")
    }

    "generate marathon payload with support for standard host networking on calico provider" in {
      val marJson = Json.toJson(toMarathonLaunchPayload("org","wrk","env","name",ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithStdNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", name = Some("http")),
          ContainerSpec.PortMapping(protocol = "tcp", name = Some("https"))
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
        aPortWith(port = 0, name = "http", protocol = "tcp"),
        aPortWith(port = 0, name = "https", protocol = "tcp")
      )
    }

    "generate marathon payload with support for standard bridge networking on calico provider" in {
      val marJson = Json.toJson(toMarathonLaunchPayload("org","wrk","env","name",ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithStdNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(80), name = Some("http")),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(443), name = Some("https"))
        ),
        network = Some("BRIDGE"),
        num_instances = 1
      ), marathonProviderWithStdNetworks)).toString
      marJson must /("container") /("docker") /("network" -> beEqualTo("BRIDGE"))
      marJson must not /("container") /("docker") / "parameters"
      marJson must /("container") /("docker") /("portMappings") /#(0) /("name" -> "http")
      marJson must /("container") /("docker") /("portMappings") /#(0) /("containerPort" -> 80)
      marJson must /("container") /("docker") /("portMappings") /#(1) /("name" -> "https")
      marJson must /("container") /("docker") /("portMappings") /#(1) /("containerPort" -> 443)
      marJson must not / "ports"
      marJson must not / "ipAddress"
      marJson must not /("portDefintions")
    }

  }

}


