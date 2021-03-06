package com.galacticfog.gestalt.marathon

import java.util.UUID

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.{ContainerSpec, VolumeSpec}
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.test.ResourceScope
import org.specs2.matcher.{JsonMatchers, JsonType, Matcher}
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.specification.{BeforeAfterEach, BeforeAll}
import play.api.libs.json._
import services.ProviderContext


class MarathonProxySpec extends Specification with Mockito with JsonMatchers with ResourceScope with BeforeAll with BeforeAfterEach {

  override def beforeAll(): Unit = pristineDatabase()

  override def before: Unit = scalikejdbc.config.DBs.setupAll()

  override def after: Unit = scalikejdbc.config.DBs.closeAll()

  val wrkName: String = "Meta Workspace"
  val envName: String = "Test Environment"
  val fqon: String = "galacticfog.engineering.test"

  def marPayload(spec: ContainerSpec, provider: GestaltResourceInstance): AppUpdate = {
    val fqon = "org"
    val mockW = mock[GestaltResourceInstance]
    mockW.name returns "wrk"
    val mockE = mock[GestaltResourceInstance]
    mockE.name returns "env"
    mockE.id returns UUID.randomUUID()
    val ctx = mock[ProviderContext]
    ctx.workspace returns mockW
    ctx.environment returns mockE
    ctx.fqon returns fqon
    ctx.provider returns provider
    ctx.providerId returns provider.id
    toMarathonLaunchPayload(fqon, defaultMarathonNamespace(ctx).get, mockE, spec, provider)
  }

  def marathonProviderWithNetworks = {
    val p = mock[GestaltResourceInstance]
    val config = Json.obj(
      "networks" -> Json.arr(Json.obj(
        "name" -> "apps"
      ))
    ).toString
    p.properties returns Some(Map(
      "config" -> config
    ))
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

  def createMarathonVolume(provider: GestaltResourceInstance, tpe: VolumeSpec.Type, config: JsValue, size: Int = 10, mode: VolumeSpec.AccessMode = VolumeSpec.ReadWriteOnce): GestaltResourceInstance = {
    val id = uuid()
    val spec = VolumeSpec(id.toString, None, Some(ContainerSpec.InputProvider(provider.id)), tpe, config, size = size, access_mode = mode)
    val props = controllers.util.stringmap(VolumeSpec.toResourcePrototype(spec).properties)
    createInstance(migrations.V13.VOLUME_TYPE_ID, id.toString, id, properties = props).get
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
          port = None,
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
            containerPath = "cpath", hostPath = Some("hpath"), mode = Some("RW"), persistent = None
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
            containerPath = "cpath", hostPath = None, mode = Some("RW"), persistent = Some(Container.PersistentVolumeInfo(size = 42))
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
      }.pendingUntilFixed

    }

    def providerWithPrefix(prefix: Option[String]) = {
      val p = mock[GestaltResourceInstance]
      val config = prefix.fold(Json.obj())(pre => Json.obj(
        APP_GROUP_PREFIX_PROP -> pre
      ))
      p.properties returns Some(Map(
        "config" -> config.toString
      ))
      val pid = UUID.randomUUID()
      p.id returns pid
      p
    }

    def prefixedPayload(prefix: Option[String], org: String, wrk: String, env: String, network: Option[String] = None, name: Option[String] = None) = {
      val mockW = mock[GestaltResourceInstance]
      mockW.name returns wrk
      val mockE = mock[GestaltResourceInstance]
      mockE.name returns env
      mockE.id returns UUID.randomUUID()
      val provider = providerWithPrefix(prefix)
      val ctx = mock[ProviderContext]
      ctx.workspace returns mockW
      ctx.environment returns mockE
      ctx.fqon returns org
      ctx.provider returns provider
      ctx.providerId returns provider.id
      toMarathonLaunchPayload(org,defaultMarathonNamespace(ctx).get,mockE,ContainerSpec(
        name = name.getOrElse("name"),
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = provider.id),
        port_mappings = Seq(ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(80),
          service_port = Some(80),
          name = Some("web"),
          expose_endpoint = Some(true)
        )),
        network = network,
        num_instances = 1
      ), provider)
    }

    "use provider appId prefix if one exists (singleton, with or without slashes)" in {
      def payload: Option[String] => AppUpdate = (prefix: Option[String]) => prefixedPayload(prefix, "org", "wrk", "env")

      payload(Some("theprefix")).id must   beSome("/theprefix/org/wrk/env/name")
      payload(Some("/theprefix")).id must  beSome("/theprefix/org/wrk/env/name")
      payload(Some("theprefix/")).id must  beSome("/theprefix/org/wrk/env/name")
      payload(Some("/theprefix/")).id must beSome("/theprefix/org/wrk/env/name")
    }

    "use provider appId prefix if one exists (tuple, with or without slashes)" in {
      def payload: Option[String] => AppUpdate = (prefix: Option[String]) => prefixedPayload(prefix, "org", "wrk", "env")

      payload(Some("prefix/in-two-parts")).id must   beSome("/prefix/in-two-parts/org/wrk/env/name")
      payload(Some("/prefix/in-two-parts")).id must  beSome("/prefix/in-two-parts/org/wrk/env/name")
      payload(Some("prefix/in-two-parts/")).id must  beSome("/prefix/in-two-parts/org/wrk/env/name")
      payload(Some("/prefix/in-two-parts/")).id must beSome("/prefix/in-two-parts/org/wrk/env/name")
    }

    "use provider appId prefix if one exists (starting with number)" in {
      def payload: Option[String] => AppUpdate = (prefix: Option[String]) => prefixedPayload(prefix, "org", "wrk", "env")

      payload(Some("123")).id must   beSome("/123/org/wrk/env/name")
      payload(Some("/123")).id must  beSome("/123/org/wrk/env/name")
      payload(Some("/123/")).id must beSome("/123/org/wrk/env/name")
      payload(Some("123/")).id must  beSome("/123/org/wrk/env/name")
    }

    "use provider appId prefix if one exists (includes dots)" in {
      def payload: Option[String] => AppUpdate = (prefix: Option[String]) => prefixedPayload(prefix, "org", "wrk", "env")

      payload(Some("blah.com")).id must   beSome("/blah.com/org/wrk/env/name")
      payload(Some("/blah.com")).id must  beSome("/blah.com/org/wrk/env/name")
      payload(Some("/blah.com/")).id must beSome("/blah.com/org/wrk/env/name")
      payload(Some("blah.com/")).id must  beSome("/blah.com/org/wrk/env/name")

      payload(Some("test.blah.com")).id must   beSome("/test.blah.com/org/wrk/env/name")
      payload(Some("/test.blah.com")).id must  beSome("/test.blah.com/org/wrk/env/name")
      payload(Some("/test.blah.com/")).id must beSome("/test.blah.com/org/wrk/env/name")
      payload(Some("test.blah.com/")).id must  beSome("/test.blah.com/org/wrk/env/name")
    }

    "use provider appId prefix if one exists (includes dashes and double-dashes)" in {
      def payload: Option[String] => AppUpdate = (prefix: Option[String]) => prefixedPayload(prefix, "org", "wrk", "env")

      payload(Some("blah-com")).id must   beSome("/blah-com/org/wrk/env/name")
      payload(Some("/blah-com")).id must  beSome("/blah-com/org/wrk/env/name")
      payload(Some("/blah-com/")).id must beSome("/blah-com/org/wrk/env/name")
      payload(Some("blah-com/")).id must  beSome("/blah-com/org/wrk/env/name")

      payload(Some("test-blah-com")).id must   beSome("/test-blah-com/org/wrk/env/name")
      payload(Some("/test-blah-com")).id must  beSome("/test-blah-com/org/wrk/env/name")
      payload(Some("/test-blah-com/")).id must beSome("/test-blah-com/org/wrk/env/name")
      payload(Some("test-blah-com/")).id must  beSome("/test-blah-com/org/wrk/env/name")

      payload(Some("blah--com")).id must   beSome("/blah--com/org/wrk/env/name")
      payload(Some("/blah--com")).id must  beSome("/blah--com/org/wrk/env/name")
      payload(Some("/blah--com/")).id must beSome("/blah--com/org/wrk/env/name")
      payload(Some("blah--com/")).id must  beSome("/blah--com/org/wrk/env/name")
    }

    "neglect provider appId prefix in missing from provider or empty" in {
      def payload: Option[String] => AppUpdate = (prefix: Option[String]) => prefixedPayload(prefix, "org", "wrk", "env")

      payload(None).id must beSome("/org/wrk/env/name")
      payload(Some("")).id must beSome("/org/wrk/env/name")
      payload(Some(" ")).id must beSome("/org/wrk/env/name")
      payload(Some("/")).id must beSome("/org/wrk/env/name")
      payload(Some("//")).id must beSome("/org/wrk/env/name")
      payload(Some("/ /")).id must beSome("/org/wrk/env/name")
    }

    "toMarathonLaunchPayload should validate appId components" in {
      prefixedPayload(Some("Caps"), "org", "wrk", "env") must throwA[BadRequestException]
      prefixedPayload(Some("under_score"), "org", "wrk", "env") must throwA[BadRequestException]
      prefixedPayload(Some("special^char"), "org", "wrk", "env") must throwA[BadRequestException]
      prefixedPayload(Some("special!char"), "org", "wrk", "env") must throwA[BadRequestException]
      prefixedPayload(Some("special@char"), "org", "wrk", "env") must throwA[BadRequestException]
      prefixedPayload(Some("special#char"), "org", "wrk", "env") must throwA[BadRequestException]
      prefixedPayload(Some("special$char"), "org", "wrk", "env") must throwA[BadRequestException]
      prefixedPayload(Some("special%char"), "org", "wrk", "env") must throwA[BadRequestException]
      prefixedPayload(Some("special^char"), "org", "wrk", "env") must throwA[BadRequestException]
      prefixedPayload(Some("special&char"), "org", "wrk", "env") must throwA[BadRequestException]
      prefixedPayload(Some("special(char"), "org", "wrk", "env") must throwA[BadRequestException]
      prefixedPayload(Some("special)char"), "org", "wrk", "env") must throwA[BadRequestException]
      prefixedPayload(Some("special+char"), "org", "wrk", "env") must throwA[BadRequestException]
      prefixedPayload(Some("special=char"), "org", "wrk", "env") must throwA[BadRequestException]
      prefixedPayload(None, "Caps", "wrk", "env")       must throwA[BadRequestException]
      prefixedPayload(None, "under_score", "wrk", "env")       must throwA[BadRequestException]
      prefixedPayload(None, "org", "Caps", "env")      must throwA[BadRequestException]
      prefixedPayload(None, "org", "under_score", "env")      must throwA[BadRequestException]
      prefixedPayload(None, "org", "work", "Caps")    must throwA[BadRequestException]
      prefixedPayload(None, "org", "work", "under_score")    must throwA[BadRequestException]
      prefixedPayload(None, "org", "work", "env", None, Some("Caps"))        must throwA[BadRequestException]
      prefixedPayload(None, "org", "work", "env", None, Some("under_score")) must throwA[BadRequestException]
      //
      prefixedPayload(Some(".startingdot.com"), "org", "work", "env") must throwA[BadRequestException]
      prefixedPayload(Some("endingdot.com."), "org", "work", "env") must throwA[BadRequestException]
      prefixedPayload(Some("two..dots"), "org", "work", "env") must throwA[BadRequestException]
      prefixedPayload(Some(".."), "org", "work", "env") must throwA[BadRequestException]
      prefixedPayload(Some("."), "org", "work", "env") must throwA[BadRequestException]
      prefixedPayload(Some("-starting-dash"), "org", "work", "env") must throwA[BadRequestException]
      prefixedPayload(Some("ending-dash-"), "org", "work", "env") must throwA[BadRequestException]
      //
      prefixedPayload(Some("/good/.startingdot.com"), "org", "work", "env") must throwA[BadRequestException]
      prefixedPayload(Some("/good/endingdot.com."), "org", "work", "env") must throwA[BadRequestException]
      prefixedPayload(Some("/good/two..dots"), "org", "work", "env") must throwA[BadRequestException]
      prefixedPayload(Some("/good/.."), "org", "work", "env") must throwA[BadRequestException]
      prefixedPayload(Some("/good/."), "org", "work", "env") must throwA[BadRequestException]
      prefixedPayload(Some("/good/-starting-dash"), "org", "work", "env") must throwA[BadRequestException]
      prefixedPayload(Some("/good/ending-dash-"), "org", "work", "env") must throwA[BadRequestException]
    }

    "handle portIndex for health checks" in {
      val marApp = marPayload(ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithStdNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(80) , name = Some("http")),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(443), name = Some("https"))
        ),
        network = Some("HOST"),
        num_instances = 1,
        health_checks = Seq(
          ContainerSpec.HealthCheck(
            protocol = "tcp",
            port_index = Some(1),
            grace_period_seconds = 1,
            interval_seconds = 2,
            timeout_seconds = 3,
            max_consecutive_failures = 4
          )
        )
      ), marathonProviderWithStdNetworks)
      marApp.healthChecks.get(0) must_== AppUpdate.HealthCheck(
        protocol = Some("TCP"),
        path = None,
        port = None,
        portIndex = Some(1),
        gracePeriodSeconds = Some(1),
        intervalSeconds = Some(2),
        timeoutSeconds = Some(3),
        maxConsecutiveFailures = Some(4)
      )
    }

    "handle port for health checks" in {
      val marApp = marPayload(ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithStdNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(9999) , name = Some("debug")),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(443), name = Some("https"))
        ),
        network = Some("HOST"),
        num_instances = 1,
        health_checks = Seq(
          ContainerSpec.HealthCheck(
            protocol = "tcp",
            port = Some(9999),
            grace_period_seconds = 1,
            interval_seconds = 2,
            timeout_seconds = 3,
            max_consecutive_failures = 4
          ),
          ContainerSpec.HealthCheck(
            protocol = "https",
            path = Some("/health"),
            port = Some(443),
            grace_period_seconds = 5,
            interval_seconds = 6,
            timeout_seconds = 7,
            max_consecutive_failures = 8
          )
        )
      ), marathonProviderWithStdNetworks)
      marApp.healthChecks.get(0) must_== AppUpdate.HealthCheck(
        protocol = Some("TCP"),
        path = None,
        portIndex = None,
        port = Some(9999),
        gracePeriodSeconds = Some(1),
        intervalSeconds = Some(2),
        timeoutSeconds = Some(3),
        maxConsecutiveFailures = Some(4)
      )
      marApp.healthChecks.get(1) must_== AppUpdate.HealthCheck(
        protocol = Some("HTTPS"),
        path = Some("/health"),
        port = Some(443),
        portIndex = None,
        gracePeriodSeconds = Some(5),
        intervalSeconds = Some(6),
        timeoutSeconds = Some(7),
        maxConsecutiveFailures = Some(8)
      )
    }

    "handle command-style health checks" in {
      val marApp = marPayload(ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithStdNetworks.id),
        port_mappings = Seq(),
        network = Some("HOST"),
        num_instances = 1,
        health_checks = Seq(
          ContainerSpec.HealthCheck(
            protocol = "command",
            command = Some("curl -f -X GET http://$HOST:$PORT0/health"),
            grace_period_seconds = 1,
            interval_seconds = 2,
            timeout_seconds = 3,
            max_consecutive_failures = 4
          )
        )
      ), marathonProviderWithStdNetworks)
      marApp.healthChecks.get(0) must_== AppUpdate.HealthCheck(
        protocol = Some("COMMAND"),
        path = None,
        command = Some(Json.obj(
          "value" -> "curl -f -X GET http://$HOST:$PORT0/health"
        )),
        portIndex = None,
        port = None,
        gracePeriodSeconds = Some(1),
        intervalSeconds = Some(2),
        timeoutSeconds = Some(3),
        maxConsecutiveFailures = Some(4)
      )
    }

    "neglect haproxy arguments if there are no virtual hosts" in {
      val userLabels = Map(
        "VARA" -> "VALA",
        "VARB" -> "VALB"
      )
      val marApp = marPayload(ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithStdNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(80) , name = Some("http"), expose_endpoint = Some(true), virtual_hosts = None),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(443), name = Some("https"), expose_endpoint = Some(true), virtual_hosts = Some(Seq.empty))
        ),
        labels = userLabels,
        network = Some("HOST"),
        num_instances = 1,
        health_checks = Seq(
          ContainerSpec.HealthCheck(
            protocol = "TCP",
            path = Some("/"),
            port_index = Some(1)
          )
        )
      ), marathonProviderWithStdNetworks)
      marApp.labels must beSome(userLabels)
    }

    "add haproxy arguments only for specified virtual hosts" in {
      val userLabels = Map(
        "VARA" -> "VALA",
        "VARB" -> "VALB"
      )
      val marApp = marPayload(ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithStdNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(80) , name = Some("http"),  expose_endpoint = Some(true), virtual_hosts = None),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(443), name = Some("https"), expose_endpoint = Some(true), virtual_hosts = Some(Seq("web.test.com")))
        ),
        labels = userLabels,
        network = Some("HOST"),
        num_instances = 1,
        health_checks = Seq(
          ContainerSpec.HealthCheck(
            protocol = "tcp",
            path = Some("/"),
            port_index = Some(1)
          )
        )
      ), marathonProviderWithStdNetworks)
      marApp.labels must beSome(userLabels ++ Map(
        "HAPROXY_1_VHOST" -> "web.test.com",
        "HAPROXY_1_GROUP" -> "external"
      ))
    }

    "support multiple virtual hosts for a port mapping and multiple port mappings" in {
      val userLabels = Map(
        "VARA" -> "VALA",
        "VARB" -> "VALB"
      )
      val marApp = marPayload(ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithStdNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(80) , name = Some("http"),  expose_endpoint = Some(true), virtual_hosts = Some(Seq("danger.test.com"))),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(443), name = Some("https"), expose_endpoint = Some(true), virtual_hosts = Some(Seq("web.test.com", "web2.test.com")))
        ),
        labels = userLabels,
        network = Some("HOST"),
        num_instances = 1,
        health_checks = Seq(
          ContainerSpec.HealthCheck(
            protocol = "tcp",
            path = Some("/"),
            port_index = Some(1)
          )
        )
      ), marathonProviderWithStdNetworks)
      marApp.labels must beSome(userLabels ++ Map(
        "HAPROXY_0_VHOST" -> "danger.test.com",
        "HAPROXY_0_GROUP" -> "external",
        "HAPROXY_1_VHOST" -> "web.test.com,web2.test.com",
        "HAPROXY_1_GROUP" -> "external"
      ))
    }

    "add default upgradeStrategy for persistent volumes" in {
      val p = marathonProviderWithStdNetworks
      val vol1 = createMarathonVolume(p,VolumeSpec.HostPath, Json.obj("host_path" -> "/hpath1", "access_mode" -> "ReadWriteOnce", "size" -> 1000))
      val vol2 = createMarathonVolume(p,VolumeSpec.Persistent, Json.obj("access_mode" -> "ReadWriteOnce", "size" -> 1000))
      val vol3 = createMarathonVolume(p,VolumeSpec.HostPath, Json.obj("host_path" -> "/hpath3", "access_mode" -> "ReadWriteOnce", "size" -> 1000))
      val marApp = marPayload(ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = p.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(80) , name = Some("http")),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(443), name = Some("https"))
        ),
        network = Some("HOST"),
        num_instances = 1,
        volumes = Seq(
          ContainerSpec.ExistingVolumeMountSpec(mount_path = "cpath1", vol1.id),
          ContainerSpec.ExistingVolumeMountSpec(mount_path = "cpath2", vol2.id),
          ContainerSpec.ExistingVolumeMountSpec(mount_path = "cpath3", vol3.id)
        )
      ), p)
      marApp.upgradeStrategy must beSome(UpgradeStrategy(
        0.5,
        0.0
      ))
    }

    "neglect default upgradeStrategy absent persistent volumes" in {
      val p = marathonProviderWithStdNetworks
      val vol1 = createMarathonVolume(p,VolumeSpec.HostPath, Json.obj("host_path" -> "/hpath1"))
      val vol2 = createMarathonVolume(p,VolumeSpec.HostPath, Json.obj("host_path" -> "/hpath2"))
      val marApp = marPayload(ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = p.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(80), name = Some("http")),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(443), name = Some("https"))
        ),
        network = Some("HOST"),
        num_instances = 1,
        volumes = Seq(
          ContainerSpec.ExistingVolumeMountSpec(mount_path = "cpath1", vol1.id),
          ContainerSpec.ExistingVolumeMountSpec(mount_path = "cpath2", vol2.id)
        )
      ), p)
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
        "name" -> "name",
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
        id = Some("/org/wrk/env/name"),
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
          "env_var_1" -> AppInfo.EnvVarString("env_val_1")
        )),
        user = None
      )

      marPayload(resourceJson.as[ContainerSpec], provider) must_== marApp
    }

    "generate valid payload with cmd and no args" in {
      val providerId = UUID.randomUUID()
      val resourceJson = Json.obj(
        "name" -> "name",
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
          "name" -> "HOST"
        ))
      ).toString
      provider.properties returns Some(Map(
        "config" -> config
      ))
      provider.id returns providerId

      val payload = Json.toJson(marPayload(resourceJson.as[ContainerSpec], provider)).as[JsObject]
      payload.toString must /("cmd" -> "/usr/bin/someCmd")
      payload.keys.contains("args") must beFalse
    }

    "transform to Marathon API appropriately (host-style)" in {
      val providerId = UUID.randomUUID()
      val resourceJson = Json.obj(
        "name" -> "name",
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
          "name" -> "HOST"
        ))
      ).toString
      provider.properties returns Some(Map(
        "config" -> config
      ))
      provider.id returns providerId

      val marApp = AppUpdate(
        id = Some("/org/wrk/env/name"),
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
          "env_var_1" -> AppInfo.EnvVarString("env_val_1")
        )),
        user = None
      )
      marPayload(resourceJson.as[ContainerSpec], provider) must_== marApp
    }

    "do not pass meta service_port through to marathon servicePort in HOST networking mode" in {
      val providerId = UUID.randomUUID()
      val resourceJson = Json.obj(
        "name" -> "name",
        "container_type" -> "DOCKER",
        "image" -> "some/image:tag",
        "provider" -> Json.obj(
          "id" -> providerId
        ),
        "port_mappings" -> Json.toJson(Seq(
          ContainerSpec.PortMapping(
            protocol = "tcp",
            service_port = Some(80), // user specified service port but...
            name = Some("web")
          )
        )),
        "cmd" -> "/usr/bin/someCmd",
        "cpus" -> 2.0,
        "memory" -> 256.0,
        "num_instances" -> 3,
        "network" -> "HOST",
        "force_pull" -> true,
        "env" -> Json.obj()
      )
      val provider = mock[GestaltResourceInstance]
      val config = Json.obj(
        "networks" -> Json.arr(Json.obj(
          "name" -> "HOST"
        ))
      ).toString
      provider.properties returns Some(Map(
        "config" -> config
      ))
      provider.id returns providerId
      val Some(Seq(pdef)) = marPayload(resourceJson.as[ContainerSpec], provider).portDefinitions
      pdef.port must_== 0          // we did not pass service port through to Marathon
    }

    "throw exception for marathon payload with invalid provider network" in {
      marPayload(ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithNetworks.id),
        network = Some("missing")
      ), marathonProviderWithNetworks) must throwA[BadRequestException]("invalid network name")
      marPayload(ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithNetworks.id),
        network = Some("HOST")
      ), marathonProviderWithNetworks) must throwA[BadRequestException]("invalid network name")
      marPayload(ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithNetworks.id),
        network = Some("BRIDGE")
      ), marathonProviderWithNetworks) must throwA[BadRequestException]("invalid network name")
    }

    "generate marathon payload with support for standard host networking" in {
      val marJson = Json.toJson(marPayload(ContainerSpec(
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
      marJson must not /("container") /("docker") /("portMappings")
      marJson must not /("ports")
      marJson must not /("ipAddress")
      marJson must havePortDefinitions(
        aPortWith(port = 0, name = "http", protocol = "tcp"),
        aPortWith(port = 0, name = "https", protocol = "tcp")
      )
    }

    "generate marathon payload with support for standard bridge networking" in {
      val marJson = Json.toJson(marPayload(ContainerSpec(
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
      marJson must not /("ports")
      marJson must not /("ipAddress")
      marJson must not /("portDefinitions")
    }

    "generate VIP payload with appropriate load balancer port (BRIDGE)" in {
      val marJson = Json.toJson(marPayload(ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithStdNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(9000),  host_port = Some(65000), name = Some("http"),  service_port = Some(12345), lb_port = Some(80),  expose_endpoint = Some(true)),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(443),                            name = Some("https"),                             lb_port = Some(443), expose_endpoint = Some(true)),
          ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(9999),                           name = Some("debug"),                                                  expose_endpoint = Some(false))
        ),
        network = Some("BRIDGE"),
        num_instances = 1
      ), marathonProviderWithStdNetworks))
      println(marJson)
      (marJson \"container" \"docker" \ "network").asOpt[String] must beSome("BRIDGE")
      ((marJson \ "container" \ "docker" \ "portMappings")(0) \ "containerPort").as[Int] must be_==(9000)
      ((marJson \ "container" \ "docker" \ "portMappings")(0) \ "servicePort").as[Int] must be_==(12345)
      ((marJson \ "container" \ "docker" \ "portMappings")(0) \ "hostPort").as[Int] must be_==(65000)
      ((marJson \ "container" \ "docker" \ "portMappings")(0) \ "labels" \ "VIP_0").as[String] must(endWith(":80"))
      ((marJson \ "container" \ "docker" \ "portMappings")(1) \ "containerPort").as[Int] must be_==(443)
      ((marJson \ "container" \ "docker" \ "portMappings")(1) \ "servicePort").asOpt[Int] must beNone
      ((marJson \ "container" \ "docker" \ "portMappings")(1) \ "hostPort").asOpt[Int] must beNone
      ((marJson \ "container" \ "docker" \ "portMappings")(1) \ "labels" \ "VIP_0").as[String] must(endWith(":443"))
      ((marJson \ "container" \ "docker" \ "portMappings")(2) \ "containerPort").as[Int] must be_==(9999)
      ((marJson \ "container" \ "docker" \ "portMappings")(2) \ "servicePort").asOpt[Int] must beNone
      ((marJson \ "container" \ "docker" \ "portMappings")(2) \ "hostPort").asOpt[Int] must beNone
      ((marJson \ "container" \ "docker" \ "portMappings")(2) \ "labels" \ "VIP_0").asOpt[JsValue] must beNone
    }

    "generate VIP payload with appropriate load balancer port (HOST)" in {
      val marJson = Json.toJson(marPayload(ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithStdNetworks.id),
        port_mappings = Seq(
          ContainerSpec.PortMapping(protocol = "tcp", host_port = Some(65000), name = Some("http"),  lb_port = Some(80),  expose_endpoint = Some(true)),
          ContainerSpec.PortMapping(protocol = "tcp",                          name = Some("https"), lb_port = Some(443), expose_endpoint = Some(true)),
          ContainerSpec.PortMapping(protocol = "tcp",                          name = Some("debug"),                      expose_endpoint = Some(false))
        ),
        network = Some("HOST"),
        num_instances = 1
      ), marathonProviderWithStdNetworks))
      (marJson \"container" \"docker" \ "network").asOpt[String] must beSome("HOST")
      ((marJson \ "portDefinitions")(0) \ "port").as[Int] must be_==(65000)
      ((marJson \ "portDefinitions")(0) \ "labels" \ "VIP_0").as[String] must(endWith(":80"))
      ((marJson \ "portDefinitions")(1) \ "port").as[Int] must be_==(0)
      ((marJson \ "portDefinitions")(1) \ "labels" \ "VIP_0").as[String] must(endWith(":443"))
      ((marJson \ "portDefinitions")(2) \ "port").as[Int] must be_==(0)
      ((marJson \ "portDefinitions")(2) \ "labels" \ "VIP_0").asOpt[JsValue] must beNone
    }

    "generate marathon payload with empty acceptedResourceRoles" in {
      val marJson = Json.toJson(marPayload(ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithStdNetworks.id),
        accepted_resource_roles = Option(Seq.empty),
        network = Some("BRIDGE"),
        num_instances = 1
      ), marathonProviderWithStdNetworks)).toString
      marJson must /("container") /("docker") /("network" -> beEqualTo("BRIDGE"))
      marJson must not /("acceptedResourceRoles")
    }

    "generate valid marathon payload from lower case constraints" in {
      val marJson = Json.toJson(marPayload(ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx:latest",
        provider = ContainerSpec.InputProvider(id = marathonProviderWithStdNetworks.id),
        constraints = Seq("rack_id:like:1", "hostname:unique"),
        network = Some("BRIDGE"),
        num_instances = 1
      ), marathonProviderWithStdNetworks)).toString
      marJson must /("container") /("docker") /("network" -> beEqualTo("BRIDGE"))
      marJson must /("constraints") */("UNIQUE")
      marJson must /("constraints") */("LIKE")
    }

    "generate marathon payload using provider networks" in {
      val marJson = Json.toJson(marPayload(ContainerSpec(
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
      val marJson = Json.toJson(marPayload(ContainerSpec(
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
      val marJson = Json.toJson(marPayload(ContainerSpec(
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


