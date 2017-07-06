package services

import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.google.inject.AbstractModule
import controllers.util.GestaltSecurityMocking
import org.junit.runner.RunWith
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.{JsonMatchers, Matcher}
import org.specs2.mock.Mockito
import org.specs2.runner.JUnitRunner
import org.specs2.specification.{BeforeAll, Scope}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.{JsObject, Json}
import play.api.test.{FakeRequest, PlaySpecification}
import com.galacticfog.gestalt.marathon
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import MarathonService.Properties
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future
import scala.util.Success
import play.api.inject.bind

@RunWith(classOf[JUnitRunner])
class MarathonServiceSpec extends PlaySpecification with ResourceScope with BeforeAll
  with Mockito with GestaltSecurityMocking with JsonMatchers {

  override def beforeAll(): Unit = pristineDatabase()

  case class FakeDCOSModule(mockMCF: MarathonClientFactory) extends AbstractModule {
    override def configure(): Unit = {
      bind(classOf[MarathonClientFactory]).toInstance(mockMCF)
    }
  }

  abstract class FakeDCOS extends Scope {
    var Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    Entitlements.setNewEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))
    var testProvider = createMarathonProvider(testEnv.id, "test-provider").get

    val mockMarClient = mock[MarathonClient]
    val mockMCF = mock[MarathonClientFactory]
    mockMCF.getClient(testProvider) returns Future.successful(mockMarClient)

    val injector =
      new GuiceApplicationBuilder()
        .disable[modules.ProdSecurityModule]
        .disable[modules.MetaDefaultSkuber]
        .disable[modules.MetaDefaultServices]
        .disable[modules.MetaDefaultDCOS]
        .disable[modules.HealthModule]
        .bindings(
          FakeDCOSModule(mockMCF),
          bind(classOf[GestaltSecurityConfig]).toInstance(mock[GestaltSecurityConfig])
        )
        .injector

    val ms = injector.instanceOf[MarathonService]
  }

  def inNamespace[R <: skuber.ObjectResource](name: String): Matcher[R] = { r: R =>
    (r.metadata.namespace == name,  r.name+" in namespace "+name, r.name+" not in namespace "+name)
  }

  def hasExactlyContainerPorts(ps: marathon.Container.Docker.PortMapping*) = {
    (((_: JsObject).\("container").\("docker").\("portMappings").toOption) ^^ beSome) and
      (((_: JsObject).\("container").\("docker").\("portMappings").as[Seq[marathon.Container.Docker.PortMapping]]) ^^ containTheSameElementsAs(Seq(ps:_*)))
  }

  def hasExactlyPortDefs(ps: marathon.AppUpdate.PortDefinition*) = {
    (((_: JsObject).\("portDefinitions").toOption) ^^ beSome) and
      (((_: JsObject).\("portDefinitions").as[Seq[marathon.AppUpdate.PortDefinition]]) ^^ containTheSameElementsAs(Seq(ps:_*)))
  }

  def hasPair(p: (String,String)) = ((_: skuber.ObjectResource).metadata.labels) ^^ havePair(p)

  def hasSelector(p: (String,String)) = ((_: skuber.Service).spec.map(_.selector).getOrElse(Map.empty)) ^^ havePair(p)

  "MarathonService" should {

    "set labels for exposed port mappings and set service addresses (bridged networking)" in new FakeDCOS {
      // - one exposure has default container_port==service_port, the other overrides the service_port
      // - one port has a required host port
      val testProps = ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
        port_mappings = Seq(
          ContainerSpec.PortMapping(
            protocol = "tcp",
            container_port = Some(80),
            name = Some("http"),
            expose_endpoint = Some(true),
            virtual_hosts = Some(Seq("web.test.com"))
          ),
          ContainerSpec.PortMapping(
            protocol = "tcp",
            container_port = Some(443),
            service_port = Some(8443),
            name = Some("https"),
            expose_endpoint = Some(true)
          ),
          ContainerSpec.PortMapping(
            protocol = "udp",
            container_port = Some(9999),
            host_port = Some(9999),
            name = Some("debug"),
            expose_endpoint = None
          )
        ),
        cpus = 1.0,
        memory = 128,
        disk = 0.0,
        num_instances = 1,
        network = Some("BRIDGE")
      )

      val Success(metaContainer) = createInstance(
        ResourceIds.Container,
        "test-container",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "network" -> testProps.network.get,
          "labels" -> Json.obj(
            "USERVAR" -> "USERVAL"
          ).toString
        ))
      )

      mockMarClient.launchApp(any)(any) returns Future.successful(Json.parse(
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
          |                    "labels": {
          |                        "VIP_0": "/test-container.${testEnv.id}:80"
          |                    },
          |                    "name": "http",
          |                    "protocol": "tcp",
          |                    "servicePort": 0
          |                },
          |                {
          |                    "containerPort": 443,
          |                    "hostPort": 0,
          |                    "labels": {
          |                        "VIP_0": "/test-container.${testEnv.id}:8443"
          |                    },
          |                    "name": "https",
          |                    "protocol": "tcp",
          |                    "servicePort": 0
          |                },
          |                {
          |                    "containerPort": 9999,
          |                    "hostPort": 9999,
          |                    "labels": {},
          |                    "name": "debug",
          |                    "protocol": "udp",
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
          |        {
          |            "id": "abbc0eee-b7bb-44b3-9c8d-e7fb10d0a434"
          |        }
          |    ],
          |    "disk": 0,
          |    "env": {},
          |    "executor": "",
          |    "fetch": [],
          |    "gpus": 0,
          |    "healthChecks": [],
          |    "id": "/root/${testWork.name}/${testEnv.name}/test-container",
          |    "instances": 1,
          |    "ipAddress": null,
          |    "labels": {
          |       "HAPROXY_0_GROUP": "external",
          |       "HAPROXY_0_VHOST": "web.test.com",
          |       "USERVAR": "USERVAL"
          |    },
          |    "maxLaunchDelaySeconds": 3600,
          |    "mem": 128,
          |    "portDefinitions": [
          |        {
          |            "labels": {},
          |            "name": "http",
          |            "port": 0,
          |            "protocol": "tcp"
          |        },
          |        {
          |            "labels": {
          |                "VIP_0": "/test-container.${testEnv.id}:8443"
          |            },
          |            "name": "https",
          |            "port": 8443,
          |            "protocol": "tcp"
          |        },
          |        {
          |            "labels": {},
          |            "name": "debug",
          |            "port": 0,
          |            "protocol": "udp"
          |        }
          |    ],
          |    "ports": [
          |        0,
          |        8443,
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
          |    "version": "2017-03-27T17:07:03.684Z"
          |}
        """.stripMargin
      ))


      val fupdatedMetaContainer = ms.create(
        context = ProviderContext(FakeRequest("POST",s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )

      val Some(updatedContainerProps) = await(fupdatedMetaContainer).properties

      there was atLeastOne(mockMCF).getClient(testProvider)

      there was one(mockMarClient).launchApp(
        hasExactlyContainerPorts(
          marathon.Container.Docker.PortMapping(Some(80),         None, None, Some("tcp"), Some("http"),  Some(Map("VIP_0" -> s"/test-container.${testEnv.id}:80"))),
          marathon.Container.Docker.PortMapping(Some(443),        None, None, Some("tcp"), Some("https"), Some(Map("VIP_0" -> s"/test-container.${testEnv.id}:8443"))),
          marathon.Container.Docker.PortMapping(Some(9999), Some(9999), None, Some("udp"), Some("debug"), None)
        ) and
          ( ((_:JsObject).\("portDefinitions").toOption) ^^ beNone ) and
          ( ((_:JsObject).\("labels").as[Map[String,String]]) ^^ be_==(Map(
            "HAPROXY_0_GROUP" -> "external",
            "HAPROXY_0_VHOST" -> "web.test.com",
            "USERVAR" -> "USERVAL"
          )))
      )(any)

      import ContainerSpec.{PortMapping, ServiceAddress}

      val svcHost = s"${metaContainer.name}.${testEnv.id}.marathon.l4lb.thisdcos.directory"
      updatedContainerProps.get("status") must beSome("LAUNCHED")
      val mappings = Json.parse(updatedContainerProps("port_mappings")).as[Seq[ContainerSpec.PortMapping]]
      mappings must contain(exactly(
        (pm: PortMapping) => pm.name == Some("http")  && pm.service_address.contains(ServiceAddress(svcHost, 80, Some("tcp"))),
        (pm: PortMapping) => pm.name == Some("https") && pm.service_address.contains(ServiceAddress(svcHost, 8443, Some("tcp"))),
        (pm: PortMapping) => pm.name == Some("debug") && pm.service_address.isEmpty
      ))
      val persistedLabels = Json.parse(updatedContainerProps("labels")).asOpt[Map[String,String]]
      persistedLabels must beSome(Map("USERVAR" -> "USERVAL"))
    }

    "set labels for exposed port mappings and set service addresses (host networking)" in new FakeDCOS {
      // - one exposure has service_port, as required
      // - another exposure is missing service_port, so it won't work
      // - a third isn't exposed
      // also, verify that host_port isn't used
      val testProps = ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
        port_mappings = Seq(
          ContainerSpec.PortMapping(
            protocol = "tcp",
            host_port = Some(81),    // not used
            service_port = Some(80), // used *only* for VIP
            name = Some("http"),
            expose_endpoint = Some(true)
          ),
          ContainerSpec.PortMapping(
            protocol = "tcp",
            service_port = None, // this is necessary for a service address, will result in no service_address
            name = Some("https"),
            expose_endpoint = Some(true)
          ),
          ContainerSpec.PortMapping(
            protocol = "udp",
            name = Some("debug"),
            expose_endpoint = None
          )
        ),
        cpus = 1.0,
        memory = 128,
        disk = 0.0,
        num_instances = 1,
        network = Some("HOST")
      )

      val Success(metaContainer) = createInstance(
        ResourceIds.Container,
        "test-container",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "network" -> testProps.network.get)
        )
      )

      mockMarClient.launchApp(any)(any) returns Future.successful(Json.parse(
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
          |            "network": "HOST",
          |            "parameters": [],
          |            "portMappings": null,
          |            "privileged": false
          |        },
          |        "type": "DOCKER",
          |        "volumes": []
          |    },
          |    "cpus": 1,
          |    "dependencies": [],
          |    "deployments": [
          |        {
          |            "id": "a9d9b165-6d43-4df7-877a-5b53963534fe"
          |        }
          |    ],
          |    "disk": 0,
          |    "env": {},
          |    "executor": "",
          |    "fetch": [],
          |    "gpus": 0,
          |    "healthChecks": [],
          |    "id": "/root/${testWork.name}/${testEnv.name}/test-container",
          |    "instances": 1,
          |    "ipAddress": null,
          |    "labels": {},
          |    "maxLaunchDelaySeconds": 3600,
          |    "mem": 128,
          |    "portDefinitions": [
          |        {
          |            "labels": {
          |                "VIP_0": "/test-container.${testEnv.id}:80"
          |            },
          |            "name": "http",
          |            "port": 1234,
          |            "protocol": "tcp"
          |        },
          |        {
          |            "labels": {},
          |            "name": "https",
          |            "port": 1235,
          |            "protocol": "tcp"
          |        },
          |        {
          |            "labels": {},
          |            "name": "debug",
          |            "port": 1236,
          |            "protocol": "udp"
          |        }
          |    ],
          |    "ports": [
          |        1234,
          |        1235,
          |        1236
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
          |    "version": "2017-03-27T17:10:12.005Z"
          |}
        """.stripMargin
      ))

      val fupdatedMetaContainer = ms.create(
        context = ProviderContext(FakeRequest("POST",s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )

      val Some(updatedContainerProps) = await(fupdatedMetaContainer).properties

      there was atLeastOne(mockMCF).getClient(testProvider)

      there was one(mockMarClient).launchApp(
        hasExactlyPortDefs(
          marathon.AppUpdate.PortDefinition( 0, "tcp", Some("http"),  Map("VIP_0" -> s"/test-container.${testEnv.id}:80")),
          marathon.AppUpdate.PortDefinition( 0, "tcp", Some("https"), Map.empty),
          marathon.AppUpdate.PortDefinition( 0, "udp", Some("debug"), Map.empty)
        ) and ( ((_:JsObject).\("container").\("docker").\("portMappings").toOption) ^^ beNone )
      )(any)

      import ContainerSpec.{PortMapping, ServiceAddress}

      val svcHost = s"${metaContainer.name}.${testEnv.id}.marathon.l4lb.thisdcos.directory"
      updatedContainerProps.get("status") must beSome("LAUNCHED")
      val mappings = Json.parse(updatedContainerProps("port_mappings")).as[Seq[ContainerSpec.PortMapping]]
      mappings must contain(exactly(
        (pm: PortMapping) => pm.name == Some("http")  && pm.service_address.contains(ServiceAddress(svcHost, 80, Some("tcp"))),
        (pm: PortMapping) => pm.name == Some("https") && pm.service_address.isEmpty,
        (pm: PortMapping) => pm.name == Some("debug") && pm.service_address.isEmpty
      ))
    }

    "respect marathon framework and cluster name in service addresses" in new FakeDCOS {
      val marathonFrameworkName = "user-marathon"
      val dcosClusterName = "dev-dcos"

      val Success(testProviderLcl) = createInstance(
        ResourceIds.DcosProvider,
        "test-provider-with-namespace-override",
        parent = Some(testEnv.id),
        properties = Option(Map(
          "parent" -> "{}",
          "config" -> Json.obj(
            Properties.MARATHON_FRAMEWORK_NAME -> marathonFrameworkName,
            Properties.DCOS_CLUSTER_NAME -> dcosClusterName
          ).toString
        ))
      )

      mockMCF.getClient(testProviderLcl) returns Future.successful(mockMarClient)
      val testProps = ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProviderLcl.id, name = Some(testProviderLcl.name)),
        port_mappings = Seq(
          ContainerSpec.PortMapping(
            protocol = "tcp",
            host_port = Some(0),
            service_port = Some(80),
            name = Some("web"),
            labels = None,
            expose_endpoint = Some(true)
          )
        ),
        cpus = 1.0,
        memory = 128,
        disk = 0.0,
        num_instances = 1,
        network = Some("HOST")
      )

      val Success(metaContainer) = createInstance(
        ResourceIds.Container,
        "test-container",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProviderLcl).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "network" -> testProps.network.get)
        )
      )

      mockMarClient.launchApp(any)(any) returns Future.successful(Json.parse(
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
           |            "network": "HOST",
           |            "parameters": [],
           |            "portMappings": null,
           |            "privileged": false
           |        },
           |        "type": "DOCKER",
           |        "volumes": []
           |    },
           |    "cpus": 1,
           |    "dependencies": [],
           |    "deployments": [
           |        {
           |            "id": "a9d9b165-6d43-4df7-877a-5b53963534fe"
           |        }
           |    ],
           |    "disk": 0,
           |    "env": {},
           |    "executor": "",
           |    "fetch": [],
           |    "gpus": 0,
           |    "healthChecks": [],
           |    "id": "/root/${testWork.name}/${testEnv.name}/test-container",
           |    "instances": 1,
           |    "ipAddress": null,
           |    "labels": {},
           |    "maxLaunchDelaySeconds": 3600,
           |    "mem": 128,
           |    "portDefinitions": [
           |        {
           |            "labels": {
           |                "VIP_0": "/test-container.${testEnv.id}:80"
           |            },
           |            "name": "web",
           |            "port": 10010,
           |            "protocol": "tcp"
           |        }
           |    ],
           |    "ports": [
           |        10010
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
           |    "version": "2017-03-27T17:10:12.005Z"
           |}
        """.stripMargin
      ))

      val fupdatedMetaContainer = ms.create(
        context = ProviderContext(FakeRequest("POST",s"/root/environments/${testEnv.id}/containers"), testProviderLcl.id, None),
        container = metaContainer
      )

      val Some(updatedContainerProps) = await(fupdatedMetaContainer).properties

      there was atLeastOne(mockMCF).getClient(testProviderLcl)

      there was one(mockMarClient).launchApp(
        hasExactlyPortDefs(
          marathon.AppUpdate.PortDefinition( 0, "tcp", Some("web"),  Map("VIP_0" -> s"/test-container.${testEnv.id}:80"))
        )
      )(any)

      import ContainerSpec.{PortMapping, ServiceAddress}

      val svcHost = s"${metaContainer.name}.${testEnv.id}.${marathonFrameworkName}.l4lb.${dcosClusterName}.directory"
      updatedContainerProps.get("status") must beSome("LAUNCHED")
      val mappings = Json.parse(updatedContainerProps("port_mappings")).as[Seq[ContainerSpec.PortMapping]]
      mappings must containTheSameElementsAs(Seq(
        PortMapping(
          protocol = "tcp", container_port = None, host_port = Some(0), service_port = Some(80),
          name = Some("web"), labels = None, expose_endpoint = Some(true),
          service_address = Some(ServiceAddress(svcHost, 80, Some("tcp")))
        )
      ))
    }

    "delete service on container delete using external_id" in new FakeDCOS {
      val testProps = ContainerSpec(
        name = "",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
        port_mappings = Seq(),
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

      val Success(metaContainer) = createInstance(
        ResourceIds.Container,
        "test-container",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "network" -> testProps.network.get,
          "external_id" -> "/some/marathon/app"
        ))
      )

      mockMarClient.deleteApplication(any)(any) returns Future.successful(Json.parse(
        """
          |{
          |    "deploymentId": "23a8ddc0-94fa-4190-9c2d-85e2378a7e49",
          |    "version": "2017-03-27T17:12:46.910Z"
          |}
        """.stripMargin
      ))

      val fDeleted = ms.destroy( metaContainer )

      await(fDeleted)

      there was one(mockMarClient).deleteApplication(meq("/some/marathon/app"))(any)
    }

    "delete service on container delete reconstructing external_id" in new FakeDCOS {
      val testProps = ContainerSpec(
        name = "",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
        port_mappings = Seq(),
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

      val Success(metaContainer) = createInstance(
        ResourceIds.Container,
        "test-container",
        parent = Some(testEnv.id),
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
          // Missing in this test: "external_id" -> "/some/marathon/app"
        ))
      )

      val fDeleted = ms.destroy( metaContainer )

      await(fDeleted)

      there were no(mockMarClient).deleteApplication(any)(any)
    }

    "scale appropriately using Marathon PUT" in new FakeDCOS {
      val Success(metaContainer) = createInstance(
        ResourceIds.Container,
        "test-container",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> "DOCKER",
          "image" -> "nginx",
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> "1.0",
          "memory" -> "128",
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> "[]",
          "network" -> "default",
          "external_id" -> "/some/marathon/app"
        ))
      )

      mockMarClient.scaleApplication(any, any)(any) returns Future.successful(Json.parse(
        """{
  "deploymentId": "5ed4c0c5-9ff8-4a6f-a0cd-f57f59a34b43",
  "version": "2015-09-29T15:59:51.164Z"
}"""
      ))

      val Some(updatedContainerProps) = await(ms.scale(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers/${metaContainer.id}"), testProvider.id, None),
        container = metaContainer,
        numInstances = 4
      )).properties

      there was one(mockMarClient).scaleApplication(
        meq("/some/marathon/app"),
        meq(4)
      )(any)

      updatedContainerProps must havePair(
        "num_instances" -> "4"
      )
    }

    "update support using Marathon PUT" in new FakeDCOS {
      val initProps = ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
        port_mappings = Seq(
          ContainerSpec.PortMapping(
            protocol = "tcp",
            container_port = Some(80),
            name = Some("http"),
            expose_endpoint = Some(true)
          ),
          ContainerSpec.PortMapping(
            protocol = "tcp",
            container_port = Some(443),
            service_port = Some(8443),
            name = Some("https"),
            expose_endpoint = Some(true)
          ),
          ContainerSpec.PortMapping(
            protocol = "tcp",
            container_port = Some(9999),
            name = Some("debug"),
            expose_endpoint = Some(false)
          )
        ),
        cpus = 1.0,
        memory = 128,
        disk = 0.0,
        num_instances = 1,
        network = Some("BRIDGE")
      )

      val origExtId = s"/root/${testWork.name}/${testEnv.name}/test-container"

      val Success(metaContainer) = createInstance(
        ResourceIds.Container,
        name = initProps.name,
        parent = Some(testEnv.id),
        properties = Some(Map[String,String](
          "container_type" -> initProps.container_type,
          "image" -> initProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> initProps.cpus.toString,
          "memory" -> initProps.memory.toString,
          "num_instances" -> initProps.num_instances.toString,
          "force_pull" -> initProps.force_pull.toString,
          "port_mappings" -> Json.toJson(initProps.port_mappings).toString,
          "network" -> initProps.network.getOrElse(""),
          "external_id" -> origExtId
        ))
      )

      mockMarClient.updateApplication(any,any)(any) returns Future.successful(Json.parse(
      s"""
         |{
         |  "deploymentId": "5ed4c0c5-9ff8-4a6f-a0cd-f57f59a34b43",
         |  "version": "2015-09-29T15:59:51.164Z"
         |}
        """.stripMargin
      ))

      val updatedContainer = await(ms.update(
        context = ProviderContext(play.api.test.FakeRequest("PATCH", s"/root/environments/${testEnv.id}/containers/${metaContainer.id}"), testProvider.id, None),
        container = metaContainer.copy(
          properties = metaContainer.properties.map(
            _ ++ Map(
              "image" -> "nginx:updated"
            )
          )
        )
      ))
      val Some(updatedContainerProps) = updatedContainer.properties

      there was one(mockMarClient).updateApplication(
        meq(origExtId),
        argThat(
          (js: JsObject) =>
            (js \ "container" \ "docker" \ "image").as[String] == "nginx:updated"
              && (js \ "id").as[String] == origExtId
        )
      )(any)

      updatedContainerProps must havePair(
        "image" -> "nginx:updated"
      )
      Json.parse(updatedContainerProps("port_mappings")).as[Seq[ContainerSpec.PortMapping]] must containTheSameElementsAs(Seq(
        ContainerSpec.PortMapping("tcp", Some(80),   None,       None, Some("http"),  None, Some(true),  Some(ContainerSpec.ServiceAddress(s"test-container.${testEnv.id}.marathon.l4lb.thisdcos.directory",80,Some("tcp")))),
        ContainerSpec.PortMapping("tcp", Some(443),  None, Some(8443), Some("https"), None, Some(true),  Some(ContainerSpec.ServiceAddress(s"test-container.${testEnv.id}.marathon.l4lb.thisdcos.directory",8443,Some("tcp")))),
        ContainerSpec.PortMapping("tcp", Some(9999), None,       None, Some("debug"), None, Some(false), None)
      ))
    }

    "throw a 400 on container rename" in new FakeDCOS {
      val initProps = ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name))
      )

      val origExtId = "/some/marathon/app"

      val Success(metaContainer) = createInstance(
        ResourceIds.Container,
        name = initProps.name,
        parent = Some(testEnv.id),
        properties = Some(Map[String,String](
          "container_type" -> initProps.container_type,
          "image" -> initProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> initProps.cpus.toString,
          "memory" -> initProps.memory.toString,
          "num_instances" -> initProps.num_instances.toString,
          "force_pull" -> initProps.force_pull.toString,
          "port_mappings" -> Json.toJson(initProps.port_mappings).toString,
          "network" -> initProps.network.getOrElse(""),
          "external_id" -> origExtId
        ))
      )

      await(ms.update(
        context = ProviderContext(play.api.test.FakeRequest("PATCH", s"/root/environments/${testEnv.id}/containers/${metaContainer.id}"), testProvider.id, None),
        container = metaContainer.copy(
          name = "updated-name"
        )
      )) must throwAn[BadRequestException]("renaming containers is not supported")
    }

  }

}
