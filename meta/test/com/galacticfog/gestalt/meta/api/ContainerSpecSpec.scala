package com.galacticfog.gestalt.meta.api

import java.util.UUID
import scala.util.Success
import org.joda.time.DateTime
import play.api.libs.json._
import org.specs2.specification.BeforeAll
import org.specs2.mutable._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.test._

class ContainerSpecSpec extends Specification with MetaRepositoryOps with BeforeAll with controllers.util.JsonInput {
  override def beforeAll() = pristineDatabase()

  lazy val owner = ResourceOwnerLink(ResourceIds.User, adminUserId.toString)

  val secretEnvMount = ContainerSpec.SecretEnvMount(UUID.fromString("5b7ac1b7-25bb-458e-8ca3-b9ae09269f9d"), "/path", "key")
  val secretFileMount = ContainerSpec.SecretFileMount(UUID.fromString("761167cf-8910-43f0-9dad-17ae137ef035"), "/path", "key")
  val secretDirMount = ContainerSpec.SecretDirMount(UUID.fromString("73e5ddf2-9bd7-41fd-8679-ad4545495f30"), "/path")

  val portMapping = ContainerSpec.PortMapping(
    protocol = "HTTP",
    container_port = Some(8000),
    host_port = Some(8001),
    service_port = Some(8002),
    name = Some("name"),
    labels = Some(Map(
      "label1" -> "1",
      "label2" -> "2"
    )),
    expose_endpoint = Some(true),
    service_address = Some(ContainerSpec.ServiceAddress("host", 80, Some("HTTP"), Some(Seq("a", "b")))),
    virtual_hosts = Some(Seq("a", "b")),
    lb_port = Some(8003),
    `type` = Some("internal"),
    lb_address = Some(ContainerSpec.ServiceAddress("lb host", 80, Some("HTTPS"), Some(Seq("a", "b")))) 
  )

  val inlineVolume = ContainerSpec.InlineVolumeMountSpec(
    "/path",
    GestaltResourceInput(
      "volume",
      Some(UUID.fromString("24361db7-0fc0-4be5-9973-d88d5602956f")),
      None,
      None,
      None,
      None,
      Some(Map(
        "type" -> JsString("persistent"),
        "config" -> Json.obj(),
        "size" -> JsNumber(100),
        "access_mode" -> JsString("ReadWriteOnce"),
        "reclamation_policy" -> JsString("delete"),
        "external_id" -> JsString("external id"),
        "mount_path" -> JsString("/path")
      )),
      None,
      None,
      None
    )
  )
  val existingVolume = ContainerSpec.ExistingVolumeMountSpec("/path", UUID.fromString("8bcde5df-ce9c-44a6-8cff-b41a4ad1e9cb"))

  val httpHealthCheck = ContainerSpec.HealthCheck(
    protocol = "HTTP",
    path = Some("host"),
    command = None,
    grace_period_seconds = 150,
    interval_seconds = 30,
    timeout_seconds = 15,
    max_consecutive_failures = 2,
    port_index = Some(0),
    port = Some(8000)
  )
  val cmdHealthCheck = ContainerSpec.HealthCheck(
    protocol = "COMMAND",
    path = None,
    command = Some("cmd"),
    grace_period_seconds = 300,
    interval_seconds = 60,
    timeout_seconds = 10,
    max_consecutive_failures = 3,
    port_index = None,
    port = None
  )

  val minimalContainerSpec = ContainerSpec(
    name = "test",
    container_type = "DOCKER",
    image = "nginx:latest",
    provider = ContainerSpec.InputProvider(id = UUID.fromString("c698901f-1ddc-426c-9b50-ae8740856c10"))
  )

  val fullContainerSpec = ContainerSpec(
    name = "test",
    description = Some("description"),
    container_type = "DOCKER",
    image = "nginx:latest",
    provider = ContainerSpec.InputProvider(id = UUID.fromString("c698901f-1ddc-426c-9b50-ae8740856c10")),
    port_mappings = Seq(portMapping),
    cpus = 2.1,
    memory = 256.0,
    disk = 50.75,
    num_instances = 2,
    network = Some("network"),
    cmd = Some("cmd"),
    constraints = Seq("a", "b"),
    accepted_resource_roles = Some(Seq("a", "b", "c")),
    args = Some(Seq("arg1", "arg2")),
    force_pull = true,
    health_checks = Seq(httpHealthCheck, cmdHealthCheck),
    volumes = Seq(existingVolume, inlineVolume),
    labels = Map("label1" -> "1", "label2" -> "2"),
    env = Map("ENV1" -> "1", "ENV2" -> "2"),
    user = Some("user"),
    external_id = Some("external id"),
    created = Some(new DateTime(0)),
    secrets = Seq(secretEnvMount, secretFileMount, secretDirMount)
  )

  "ContainerSpec#toResourcePrototype" should {
    "convert minimal ContainerSpec" in {
      val cs = minimalContainerSpec
      ContainerSpec.toResourcePrototype(cs).copy(id=None) must equalTo(GestaltResourceInput(
        "test",
        Some(UUID.fromString("28cbf0e0-2c48-4589-85d5-df97a3a1f318")),
        None,
        None,
        None,
        None,
        Some(Map(
          "container_type" -> JsString("DOCKER"),
          "image" -> JsString("nginx:latest"),
          "provider" -> Json.obj("id" -> "c698901f-1ddc-426c-9b50-ae8740856c10"),
          "port_mappings" -> Json.arr(),
          "cpus" -> JsNumber(0.2),
          "memory" -> JsNumber(128),
          "disk" -> JsNumber(0),
          "num_instances" -> JsNumber(1),
          "constraints" -> Json.arr(),
          "force_pull" -> JsBoolean(false),
          "health_checks" -> Json.arr(),
          "volumes" -> Json.arr(),
          "labels" -> Json.obj(),
          "env" -> Json.obj(),
          "secrets" -> Json.arr()
        )),
        None,
        None,
        None
      ))
    }

    "convert full ContainerSpec" in {
      val cs = fullContainerSpec
      ContainerSpec.toResourcePrototype(cs).copy(id=None) must equalTo(GestaltResourceInput(
        "test",
        Some(UUID.fromString("28cbf0e0-2c48-4589-85d5-df97a3a1f318")),
        None,
        None,
        None,
        Some("description"),
        Some(Map(
          "container_type" -> JsString("DOCKER"),
          "image" -> JsString("nginx:latest"),
          "provider" -> Json.obj("id" -> "c698901f-1ddc-426c-9b50-ae8740856c10"),
          "port_mappings" -> Json.arr(
            Json.obj(
              "protocol" -> "HTTP",
              "container_port" -> 8000,
              "host_port" -> 8001,
              "service_port" -> 8002,
              "name" -> "name",
              "labels" -> Json.obj("label1" -> "1","label2" -> "2"),
              "expose_endpoint" -> true,
              "service_address" -> Json.obj("host" -> "host","port" -> 80,"protocol" -> "HTTP","virtual_hosts" -> Json.arr("a","b")),
              "virtual_hosts" -> Json.arr("a","b"),
              "lb_port" -> 8003,
              "type" -> "internal",
              "lb_address" -> Json.obj("host" -> "lb host","port" -> 80,"protocol" -> "HTTPS","virtual_hosts" -> Json.arr("a","b"))
            )
          ),
          "cpus" -> JsNumber(2.1),
          "memory" -> JsNumber(256),
          "disk" -> JsNumber(50.75),
          "num_instances" -> JsNumber(2),
          "network" -> JsString("network"),
          "cmd" -> JsString("cmd"),
          "constraints" -> Json.arr("a","b"),
          "accepted_resource_roles" -> Json.arr("a","b","c"),
          "args" -> Json.arr("arg1","arg2"),
          "force_pull" -> JsBoolean(true),
          "health_checks" -> Json.arr(
            Json.obj("protocol" -> "HTTP","path" -> "host","grace_period_seconds" -> 150,"interval_seconds" -> 30,"timeout_seconds" -> 15,"max_consecutive_failures" -> 2,"port_index" -> 0,"port" -> 8000),
            Json.obj("protocol" -> "COMMAND","command" -> "cmd","grace_period_seconds" -> 300,"interval_seconds" -> 60,"timeout_seconds" -> 10,"max_consecutive_failures" -> 3)
          ),
          "volumes" -> Json.arr(
            Json.obj("mount_path" -> "/path","volume_id" -> "8bcde5df-ce9c-44a6-8cff-b41a4ad1e9cb"),
            Json.obj("mount_path" -> "/path","volume_resource" -> Json.obj(
              "name" -> "volume",
              "resource_type" -> "24361db7-0fc0-4be5-9973-d88d5602956f",
              "properties" -> Json.obj(
                "size" -> 100,
                "mount_path" -> "/path",
                "config" -> Json.obj(),
                "reclamation_policy" -> "delete",
                "access_mode" -> "ReadWriteOnce",
                "external_id" -> "external id",
                "type" -> "persistent"
              )
            ))
          ),
          "labels" -> Json.obj("label1" -> "1", "label2" -> "2"),
          "env" -> Json.obj("ENV1" -> "1", "ENV2" -> "2"),
          "user" -> JsString("user"),
          "secrets" -> Json.arr(
            Json.obj("secret_id" -> "5b7ac1b7-25bb-458e-8ca3-b9ae09269f9d", "path" -> "/path", "secret_key" -> "key", "mount_type" -> "env"),
            Json.obj("secret_id" -> "761167cf-8910-43f0-9dad-17ae137ef035", "path" -> "/path", "secret_key" -> "key", "mount_type" -> "file"),
            Json.obj("secret_id" -> "73e5ddf2-9bd7-41fd-8679-ad4545495f30", "path" -> "/path", "mount_type" -> "directory")
          )
        )),
        None,
        None,
        None
      ))
    }
  }

  "ContainerSpec#fromResourcePrototype" should {
    "convert minimal ContainerSpec" in {
      val cs = minimalContainerSpec
      val input = ContainerSpec.toResourcePrototype(cs).copy(owner=Some(owner))
      val instance = inputToInstance(UUID.randomUUID(), input)
      ContainerSpec.fromResourceInstance(instance) must equalTo(Success(cs))
    }

    "covert full ContainerSpec" in {
      val cs = fullContainerSpec
      val input = ContainerSpec.toResourcePrototype(cs).copy(owner=Some(owner))
      val instance = inputToInstance(UUID.randomUUID(), input)
      ContainerSpec.fromResourceInstance(instance) must equalTo(Success(cs.copy(external_id=None, created=None)))
    }

    "fail with invalid pm" in {
      val cs = fullContainerSpec
      val input = ContainerSpec.toResourcePrototype(cs).copy(owner=Some(owner))
      val updatedInput = input.copy(properties=Some(input.properties.get ++ Map(
        "port_mappings" -> Json.arr(
          Json.obj(
            "protocol" -> "HTTP",
            "container_port" -> 8000,
            "host_port" -> 8001,
            "service_port" -> 8002,
            "name" -> "name",
            "labels" -> Json.obj("label1" -> "1","label2" -> "2"),
            "expose_endpoint" -> true,
            "service_address" -> Json.obj("host" -> "host","port" -> 80,"protocol" -> "HTTP","virtual_hosts" -> Json.arr("a","b")),
            "virtual_hosts" -> Json.arr("a","b"),
            "lb_port" -> 8003,
            "type" -> "abcd",
            "lb_address" -> Json.obj("host" -> "lb host","port" -> 80,"protocol" -> "HTTPS","virtual_hosts" -> Json.arr("a","b"))
          )
        )
      )))
      val instance = inputToInstance(UUID.randomUUID(), updatedInput)
      ContainerSpec.fromResourceInstance(instance) must beFailedTry.withThrowable[RuntimeException]("Could not convert GestaltResourceInstance into ContainerSpec: JsResultException\\(errors:List\\(\\(\\(0\\)/type,List\\(JsonValidationError\\(List\\(error.invalid\\),WrappedArray\\(\\)\\)\\)\\)\\)\\)")
    }
  }
}