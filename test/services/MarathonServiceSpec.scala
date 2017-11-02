package services

import com.galacticfog.gestalt.marathon.{AppInfo, AppUpdate, MarathonClient}
import com.galacticfog.gestalt.meta.api.{ContainerSpec, SecretSpec}
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.google.inject.AbstractModule
import controllers.util.{ContainerService, GestaltSecurityMocking}
import org.junit.runner.RunWith
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.{JsonMatchers, Matcher}
import org.specs2.mock.Mockito
import org.specs2.runner.JUnitRunner
import org.specs2.specification.{BeforeAfterEach, BeforeAll, Scope}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.{JsObject, Json}
import play.api.test.{FakeRequest, PlaySpecification}
import com.galacticfog.gestalt.marathon
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import MarathonService.Properties
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.marathon.AppInfo.EnvVarString
import com.galacticfog.gestalt.meta.api.ContainerSpec.{SecretDirMount, SecretEnvMount, SecretFileMount}
import com.galacticfog.gestalt.meta.api.errors.{BadRequestException, UnprocessableEntityException}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import org.mockito.ArgumentCaptor
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future
import scala.util.Success
import play.api.inject.bind
import play.api.libs.json.Json.JsValueWrapper

@RunWith(classOf[JUnitRunner])
class MarathonServiceSpec extends PlaySpecification with ResourceScope with BeforeAll with BeforeAfterEach with JsonMatchers {

  case class TestSetup( mcf: MarathonClientFactory,
                        client: MarathonClient,
                        svc : MarathonService )

  override def beforeAll(): Unit = pristineDatabase()

  override def before: Unit = scalikejdbc.config.DBs.setupAll()

  override def after: Unit = scalikejdbc.config.DBs.closeAll()

  sequential

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

  // this could probably be done with a Fragments.foreach, but it's more in line with the other suites and i think more flexible
  abstract class FakeDCOS extends Scope {
    lazy val testAuthResponse = GestaltSecurityMocking.dummyAuthResponseWithCreds()
    lazy val testCreds = testAuthResponse.creds
    lazy val user = AuthAccountWithCreds(testAuthResponse.account, Seq.empty, Seq.empty, testCreds, dummyRootOrgId)

    lazy val (testWork, testEnv) = {
      val (tw, te) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment").get
      Entitlements.setNewEntitlements(dummyRootOrgId, te.id, user, Some(tw.id))
      (tw,te)
    }

    lazy val testProvider = createMarathonProvider(testEnv.id, "test-provider").get

    lazy val testSetup = {
      val mockMarClient = mock[MarathonClient]
      val mockMCF = mock[MarathonClientFactory]
      mockMCF.getClient(testProvider) returns Future.successful(mockMarClient)
      val svc = new MarathonService(mockMCF)
      TestSetup(mockMCF, mockMarClient, svc)
    }
  }

  abstract class FakeDCOSCreateContainer( force_pull: Boolean = true,
                                          cpu: Double = 1.0,
                                          memory: Double = 128,
                                          args: Option[Seq[String]] = None,
                                          env: Map[String,String] = Map.empty,
                                          cmd: Option[String] = None,
                                          health_checks: Seq[ContainerSpec.HealthCheck] = Seq.empty,
                                          port_mappings: Seq[ContainerSpec.PortMapping] = Seq.empty,
                                          labels: Map[String,String] = Map.empty,
                                          providerConfig: Seq[(String,JsValueWrapper)] = Seq.empty,
                                          secrets: Seq[ContainerSpec.SecretMount] = Seq.empty,
                                          volumes: Seq[ContainerSpec.Volume] = Seq.empty
                                        ) extends Scope {

    lazy val testAuthResponse = GestaltSecurityMocking.dummyAuthResponseWithCreds()
    lazy val testCreds = testAuthResponse.creds
    lazy val user = AuthAccountWithCreds(testAuthResponse.account, Seq.empty, Seq.empty, testCreds, dummyRootOrgId)

    lazy val (testWork, testEnv) = {
      val (tw, te) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment").get
      Entitlements.setNewEntitlements(dummyRootOrgId, te.id, user, Some(tw.id))
      (tw,te)
    }

    lazy val testProvider = createMarathonProvider(testEnv.id, "test-provider",
      Seq[(String,JsValueWrapper)](MarathonService.Properties.SECRET_SUPPORT -> true) ++ providerConfig
    ).get

    lazy val metaSecretItems = Seq(
      SecretSpec.Item("part-a", Some("dmFsdWUtYQ=="))
    )

    lazy val metaSecret = createInstance(
      ResourceIds.Secret,
      "test-secret",
      parent = Some(testEnv.id),
      properties = Some(Map(
        "provider" -> Output.renderInstance(testProvider).toString,
        "items" -> Json.toJson(metaSecretItems.map(_.copy(value = None))).toString,
        "external_id" -> "root/test-workspace/test-environment/test-secret"
      ))
    ).get

    val secretsWithId = secrets.collect {
      case env: SecretEnvMount  => env.copy(secret_id = metaSecret.id)
      case fil: SecretFileMount => fil.copy(secret_id = metaSecret.id)
      case dir: SecretDirMount  => dir.copy(secret_id = metaSecret.id)
    }

    lazy val initProps = ContainerSpec(
      name = "",
      container_type = "DOCKER",
      image = "nginx",
      provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
      port_mappings = port_mappings,
      cpus = cpu,
      memory = memory,
      disk = 0.0,
      num_instances = 1,
      network = Some("default"),
      cmd = cmd,
      constraints = Seq(),
      accepted_resource_roles = None,
      args = args,
      force_pull = force_pull,
      health_checks = health_checks,
      volumes = volumes,
      labels = labels,
      env = env,
      user = None,
      secrets = secretsWithId
    )

    lazy val metaContainer = createInstance(
      ResourceIds.Container,
      "test-container",
      parent = Some(testEnv.id),
      properties = Some(Map(
        "container_type" -> initProps.container_type,
        "image" -> initProps.image,
        "provider" -> Output.renderInstance(testProvider).toString,
        "cpus" -> initProps.cpus.toString,
        "memory" -> initProps.memory.toString,
        "num_instances" -> initProps.num_instances.toString,
        "env" -> Json.toJson(initProps.env).toString,
        "force_pull" -> initProps.force_pull.toString,
        "health_checks" -> Json.toJson(initProps.health_checks).toString,
        "port_mappings" -> Json.toJson(initProps.port_mappings).toString,
        "network" -> initProps.network.get,
        "labels" -> Json.toJson(labels).toString,
        "volumes" -> Json.toJson(initProps.volumes).toString,
        "secrets" -> Json.toJson(initProps.secrets).toString
      ) ++ Seq[Option[(String,String)]](
        args map ("args" -> Json.toJson(_).toString),
        cmd  map ("cmd" -> _)
      ).flatten.toMap)
    ).get

    lazy val containerLbls   = Map(
//      .META_ENVIRONMENT_KEY -> testEnv.id.toString,
//      .META_WORKSPACE_KEY -> testWork.id.toString,
//      .META_FQON_KEY -> "root",
//      .META_PROVIDER_KEY -> testProvider.id.toString
    )

   lazy val marAppCreateResponse = Json.parse(
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
    )

    lazy val testSetup = {
      val mockMarClient = mock[MarathonClient]
      val mockMCF = mock[MarathonClientFactory]
      mockMCF.getClient(testProvider) returns Future.successful(mockMarClient)

      mockMarClient.launchApp(any)(any) returns Future.successful(marAppCreateResponse)

      val svc = new MarathonService(mockMCF)
      TestSetup(mockMCF, mockMarClient, svc)
    }
  }

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

      testSetup.client.launchApp(any)(any) returns Future.successful(Json.parse(
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


      val fupdatedMetaContainer = testSetup.svc.create(
        context = ProviderContext(FakeRequest("POST",s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )

      val Some(updatedContainerProps) = await(fupdatedMetaContainer).properties

      there was atLeastOne(testSetup.mcf).getClient(testProvider)

      there was one(testSetup.client).launchApp(
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

    "set non-default labels for HAPROXY exposure" in new FakeDCOS {
      val customHaproxyGroup = "custom-haproxy-exposure-group"
      var testProviderWithCustomExposure = createMarathonProvider(testEnv.id, "test-provider", Seq(marathon.HAPROXY_EXP_GROUP_PROP -> customHaproxyGroup)).get
      // "register" this provider to the client factory
      testSetup.mcf.getClient(testProviderWithCustomExposure) returns Future.successful(testSetup.client)
      val testProps = ContainerSpec(
        name = "test-container",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProviderWithCustomExposure.name)),
        port_mappings = Seq(
          ContainerSpec.PortMapping(
            protocol = "tcp",
            container_port = Some(80),
            host_port = None,
            service_port = None,
            name = Some("http"),
            labels = None,
            expose_endpoint = Some(true),
            virtual_hosts = Some(Seq("web.test.com"))
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
          "provider" -> Output.renderInstance(testProviderWithCustomExposure).toString,
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
      testSetup.client.launchApp(any)(any) returns Future.successful(Json.parse(
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
           |                        "VIP_0": "/test-container.test-environment.test-workspace.root:80"
           |                    },
           |                    "name": "http",
           |                    "protocol": "tcp",
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
           |       "HAPROXY_0_GROUP": "${customHaproxyGroup}",
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
           |        }
           |    ],
           |    "ports": [
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
      val fupdatedMetaContainer = testSetup.svc.create(
        context = ProviderContext(FakeRequest("POST",s"/root/environments/${testEnv.id}/containers"), testProviderWithCustomExposure.id, None),
        container = metaContainer
      )
      val Some(updatedContainerProps) = await(fupdatedMetaContainer).properties
      there was atLeastOne(testSetup.mcf).getClient(testProviderWithCustomExposure)
      there was one(testSetup.client).launchApp(
        hasExactlyContainerPorts(
          marathon.Container.Docker.PortMapping(Some(80),         None, None, Some("tcp"), Some("http"),  Some(Map("VIP_0" -> s"/test-container.${testEnv.id}:80")))
        ) and
          ( ((_:JsObject).\("portDefinitions").toOption) ^^ beNone ) and
          ( ((_:JsObject).\("labels").as[Map[String,String]]) ^^ be_==(Map(
            "HAPROXY_0_GROUP" -> customHaproxyGroup,
            "HAPROXY_0_VHOST" -> "web.test.com",
            "USERVAR" -> "USERVAL"
          )))
      )(any)
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

      testSetup.client.launchApp(any)(any) returns Future.successful(Json.parse(
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

      val fupdatedMetaContainer = testSetup.svc.create(
        context = ProviderContext(FakeRequest("POST",s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )

      val Some(updatedContainerProps) = await(fupdatedMetaContainer).properties

      there was atLeastOne(testSetup.mcf).getClient(testProvider)

      there was one(testSetup.client).launchApp(
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

      testSetup.mcf.getClient(testProviderLcl) returns Future.successful(testSetup.client)
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

      testSetup.client.launchApp(any)(any) returns Future.successful(Json.parse(
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

      val fupdatedMetaContainer = testSetup.svc.create(
        context = ProviderContext(FakeRequest("POST",s"/root/environments/${testEnv.id}/containers"), testProviderLcl.id, None),
        container = metaContainer
      )

      val Some(updatedContainerProps) = await(fupdatedMetaContainer).properties

      there was atLeastOne(testSetup.mcf).getClient(testProviderLcl)

      there was one(testSetup.client).launchApp(
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

      testSetup.client.deleteApplication(any)(any) returns Future.successful(Json.parse(
        """
          |{
          |    "deploymentId": "23a8ddc0-94fa-4190-9c2d-85e2378a7e49",
          |    "version": "2017-03-27T17:12:46.910Z"
          |}
        """.stripMargin
      ))

      val fDeleted = testSetup.svc.destroy( metaContainer )

      await(fDeleted)

      there was one(testSetup.client).deleteApplication(meq("/some/marathon/app"))(any)
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

      val fDeleted = testSetup.svc.destroy( metaContainer )

      await(fDeleted)

      there were no(testSetup.client).deleteApplication(any)(any)
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

      testSetup.client.scaleApplication(any, any)(any) returns Future.successful(Json.parse(
        """{
          |  "deploymentId": "5ed4c0c5-9ff8-4a6f-a0cd-f57f59a34b43",
          |  "version": "2015-09-29T15:59:51.164Z"
          |}
        """.stripMargin
      ))

      val Some(updatedContainerProps) = await(testSetup.svc.scale(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers/${metaContainer.id}"), testProvider.id, None),
        container = metaContainer,
        numInstances = 4
      )).properties

      there was one(testSetup.client).scaleApplication(
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

      testSetup.client.updateApplication(any,any)(any) returns Future.successful(Json.parse(
        s"""
           |{
           |  "deploymentId": "5ed4c0c5-9ff8-4a6f-a0cd-f57f59a34b43",
           |  "version": "2015-09-29T15:59:51.164Z"
           |}
        """.stripMargin
      ))

      val updatedContainer = await(testSetup.svc.update(
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

      there was one(testSetup.client).updateApplication(
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

      await(testSetup.svc.update(
        context = ProviderContext(play.api.test.FakeRequest("PATCH", s"/root/environments/${testEnv.id}/containers/${metaContainer.id}"), testProvider.id, None),
        container = metaContainer.copy(
          name = "updated-name"
        )
      )) must throwAn[BadRequestException]("renaming containers is not supported")
    }

    "throw 400 on multi-part secret" in new FakeDCOS {
      lazy val metaSecretItems = Seq(
        SecretSpec.Item("part-a", Some("dmFsdWUtYQ==")),   // "value-a"
        SecretSpec.Item("part-b", Some("dmFsdWUtYg=="))    // "value-b"
      )

      lazy val Success(metaSecret) = createInstance(
        ResourceIds.Secret,
        "test-secret",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> Output.renderInstance(testProvider).toString,
          "items" -> Json.toJson(metaSecretItems.map(_.copy(value = None))).toString
        ))
      )

      await(testSetup.svc.createSecret(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/secrets"), testProvider.id, None),
        metaResource = metaSecret,
        items = metaSecretItems
      )) must throwA[BadRequestException]("does not support multi-part secrets")
    }

    "create secret with external_id property without persisting secret values" in new FakeDCOS {
      lazy val metaSecretItems = Seq(
        SecretSpec.Item("part-a", Some("dmFsdWUtYQ=="))    // "value-a"
      )

      lazy val Success(metaSecret) = createInstance(
        ResourceIds.Secret,
        "test-secret",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> Output.renderInstance(testProvider).toString,
          "items" -> Json.toJson(metaSecretItems.map(_.copy(value = None))).toString
        ))
      )

      testSetup.client.createSecret(
        secretId = meq(s"root/${testWork.name}/${testEnv.name}/${metaSecret.name}"),
        marPayload = meq(Json.obj("value" -> "value-a"))
      )(any) returns Future.successful(Json.obj(
        "value" -> "dmFsdWUtYQ=="
      ))

      val Some(updatedSecretProps) = await(testSetup.svc.createSecret(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/secrets"), testProvider.id, None),
        metaResource = metaSecret,
        items = metaSecretItems
      )).properties
      updatedSecretProps must havePair(
        "external_id" -> s"root/${testWork.name}/${testEnv.name}/${metaSecret.name}"
      )

      Json.parse(updatedSecretProps("items")).as[Seq[SecretSpec.Item]] must containTheSameElementsAs(
        metaSecretItems.map(_.copy(value = None))
      )

      val persistedProps = ResourceFactory.findById(ResourceIds.Secret, metaSecret.id).get.properties.get
      Json.parse(persistedProps("items")).as[Seq[SecretSpec.Item]] must containTheSameElementsAs(
        metaSecretItems.map(_.copy(value = None))
      )
    }

    "delete secret" in new FakeDCOS {
      lazy val metaSecretItems = Seq(
        SecretSpec.Item("part-a", Some("dmFsdWUtYQ=="))    // "value-a"
      )

      lazy val Success(metaSecret) = createInstance(
        ResourceIds.Secret,
        "test-secret",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> Output.renderInstance(testProvider).toString,
          "items" -> Json.toJson(metaSecretItems.map(_.copy(value = None))).toString,
          "external_id" -> s"root/${testWork.name}/${testEnv.name}/test-secret"
        ))
      )

      testSetup.client.deleteSecret(meq(s"root/${testWork.name}/${testEnv.name}/test-secret"))(any) returns Future.successful(Json.obj())

      await(testSetup.svc.destroySecret(metaSecret))
      there was one(testSetup.client).deleteSecret(meq(s"root/${testWork.name}/${testEnv.name}/${metaSecret.name}"))(any)
    }

    "throw 400 on file mount secrets" in new FakeDCOSCreateContainer(
      secrets = Seq(
        SecretFileMount(null, "/mnt/secrets/files/file-a", "part-a")
      )
    ) {
      await(testSetup.svc.create(
        context = ProviderContext(FakeRequest("POST",s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )) must throwA[BadRequestException]("does not support file-mounted secrets")
    }

    "throw 400 on directory mount secrets" in new FakeDCOSCreateContainer(
      secrets = Seq(
        SecretDirMount(null, "/mnt/secrets/dir")
      )
    ) {
      await(testSetup.svc.create(
        context = ProviderContext(FakeRequest("POST",s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )) must throwA[BadRequestException]("does not support directory-mounted secrets")
    }

    "mount specified env secrets on container create" in new FakeDCOSCreateContainer(
      env = Map(
        "NORMAL_ENV_VAR" -> "SOME_VALUE"
      ),
      secrets = Seq(
        SecretEnvMount(null, "SOME_ENV_VAR", "part-a")
      )
    ) {
      val Some(updatedContainerProps) = await(testSetup.svc.create(
        context = ProviderContext(FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties

      there was atLeastOne(testSetup.mcf).getClient(testProvider)

      val payloadCaptor = ArgumentCaptor.forClass(classOf[JsObject])
      there was one(testSetup.client).launchApp(payloadCaptor.capture())(any)

      val app = payloadCaptor.getValue.as[AppUpdate]
      val secretRefs = app.secrets.getOrElse(Map.empty)
      secretRefs must haveSize(1)
      val (secretRefName, secretSource) = secretRefs.head
      secretSource must_== AppUpdate.SecretSource(ContainerService.resourceExternalId(metaSecret).getOrElse("failure"))
      app.env.getOrElse(Map.empty) must havePairs(
        "SOME_ENV_VAR" -> AppInfo.EnvVarSecretRef(secretRefName),
        "NORMAL_ENV_VAR" -> EnvVarString("SOME_VALUE")
      )
    }

    "mount the same secret twice" in new FakeDCOSCreateContainer(
      env = Map(
        "NORMAL_ENV_VAR" -> "SOME_VALUE"
      ),
      secrets = Seq(
        SecretEnvMount(null, "SOME_ENV_VAR", "part-a"),
        SecretEnvMount(null, "ANOTHER_ENV_VAR", "part-a")
      )
    ) {
      val Some(updatedContainerProps) = await(testSetup.svc.create(
        context = ProviderContext(FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties

      there was atLeastOne(testSetup.mcf).getClient(testProvider)

      val payloadCaptor = ArgumentCaptor.forClass(classOf[JsObject])
      there was one(testSetup.client).launchApp(payloadCaptor.capture())(any)

      val app = payloadCaptor.getValue.as[AppUpdate]
      val secretRefs = app.secrets.getOrElse(Map.empty)
      secretRefs must haveSize(1)
      val (secretRefName, secretSource) = secretRefs.head
      secretSource must_== AppUpdate.SecretSource(ContainerService.resourceExternalId(metaSecret).getOrElse("failure"))
      app.env.getOrElse(Map.empty) must havePairs(
        "SOME_ENV_VAR" -> AppInfo.EnvVarSecretRef(secretRefName),
        "ANOTHER_ENV_VAR" -> AppInfo.EnvVarSecretRef(secretRefName),
        "NORMAL_ENV_VAR" -> EnvVarString("SOME_VALUE")
      )
    }

    "fail for secret against a different provider" in new FakeDCOSCreateContainer(
      secrets = Seq(
        SecretEnvMount(null, "SOME_ENV_VAR", "part-a")
      )
    ) {
      val differentProvider = createMarathonProvider(testEnv.id, "test-provider",
        Seq(MarathonService.Properties.SECRET_SUPPORT -> true)
      ).get

      val cntr2 = createInstance(
        ResourceIds.Container,
        "test-container",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> initProps.container_type,
          "image" -> initProps.image,
          "provider" -> Output.renderInstance(differentProvider).toString,
          "cpus" -> initProps.cpus.toString,
          "memory" -> initProps.memory.toString,
          "num_instances" -> initProps.num_instances.toString,
          "env" -> Json.toJson(initProps.env).toString,
          "force_pull" -> initProps.force_pull.toString,
          "port_mappings" -> Json.toJson(initProps.port_mappings).toString,
          "network" -> initProps.network.get,
          "volumes" -> Json.toJson(initProps.volumes).toString,
          "secrets" -> Json.toJson(initProps.secrets).toString
        ))
      ).get

      await(testSetup.svc.create(
        context = ProviderContext(FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), differentProvider.id, None),
        container = cntr2
      )) must throwA[UnprocessableEntityException]("secret.*belongs to a different provider")
    }

    "fail for provider without secret support" in new FakeDCOSCreateContainer(
      secrets = Seq(
        SecretEnvMount(null, "SOME_ENV_VAR", "part-a")
      ),
      providerConfig = Seq(
        MarathonService.Properties.SECRET_SUPPORT -> false
      )
    ) {
      await(testSetup.svc.create(
        context = ProviderContext(FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )) must throwA[UnprocessableEntityException]("provider.*is not configured with support for secrets")
    }

  }

}
