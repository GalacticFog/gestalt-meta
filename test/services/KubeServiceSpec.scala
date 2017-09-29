package services

import java.time.{ZoneOffset, ZonedDateTime}

import com.galacticfog.gestalt.caas.kube.Ascii
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.ContainerSpec.{SecretDirMount, SecretEnvMount, SecretFileMount}
import com.galacticfog.gestalt.meta.api.{ContainerSpec, SecretSpec}
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import controllers.util.GestaltSecurityMocking
import org.junit.runner.RunWith
import org.mockito.ArgumentCaptor
import org.mockito.Matchers.{eq => meq}
import org.specs2.runner.JUnitRunner
import org.specs2.specification.{BeforeAfterEach, BeforeAll, Scope}
import org.specs2.matcher.{JsonMatchers, Matcher}
import play.api.libs.json.Json
import play.api.test.PlaySpecification
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import skuber.api.client
import skuber.json.format._

import scala.concurrent.Future
import scala.util.Success

@RunWith(classOf[JUnitRunner])
class KubeServiceSpec extends PlaySpecification with ResourceScope with BeforeAll with BeforeAfterEach with JsonMatchers {

  case class TestSetup(kubeService: KubernetesService,
                       kubeClient: client.RequestContext,
                       skuberFactory: SkuberFactory,
                       testNS: skuber.Namespace,
                       createdContainer: Option[GestaltResourceInstance] )

  def haveName(name: => String): Matcher[skuber.ObjectResource] =
    ((_: skuber.ObjectResource).name) ^^ be_==(name)

  def inNamespace(name: String): Matcher[skuber.ObjectResource] = { r: skuber.ObjectResource =>
    (r.metadata.namespace == name,  r.name+" in namespace "+name, r.name+" not in namespace "+name)
  }

  override def beforeAll(): Unit = pristineDatabase()

  override def before: Unit = scalikejdbc.config.DBs.setupAll()

  override def after: Unit = scalikejdbc.config.DBs.closeAll()

  sequential

  abstract class FakeKube() extends Scope {
    lazy val testAuthResponse = GestaltSecurityMocking.dummyAuthResponseWithCreds()
    lazy val testCreds = testAuthResponse.creds
    lazy val user = AuthAccountWithCreds(testAuthResponse.account, Seq.empty, Seq.empty, testCreds, dummyRootOrgId)

    lazy val (testWork, testEnv) = {
      val (tw, te) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment").get
      Entitlements.setNewEntitlements(dummyRootOrgId, te.id, user, Some(tw.id))
      (tw,te)
    }

    lazy val testProvider = createKubernetesProvider(testEnv.id, "test-provider").get

    lazy val testSetup = {
      val skDefaultNs = mock[skuber.Namespace]
      skDefaultNs.name returns "default"
      val skTestNs    = mock[skuber.Namespace]
      skTestNs.name returns testEnv.id.toString
      val mockSkuber = mock[client.RequestContext]
      val mockSkuberFactory = mock[SkuberFactory]
      mockSkuberFactory.initializeKube(meq(testProvider.id), meq("default")         )(any) returns Future.successful(mockSkuber)
      mockSkuber.getOption[skuber.Namespace]("default") returns Future.successful(Some(skDefaultNs))
      mockSkuberFactory.initializeKube(meq(testProvider.id), meq(testEnv.id.toString))(any) returns Future.successful(mockSkuber)
      mockSkuber.getOption[skuber.Namespace](testEnv.id.toString) returns Future.successful(Some(skTestNs))

      val ks = new KubernetesService(mockSkuberFactory)
      TestSetup(ks, mockSkuber, mockSkuberFactory, skTestNs, None)
    }
  }

  abstract class FakeKubeWithPrexistingContainer(pms: Seq[ContainerSpec.PortMapping]) extends Scope {
    lazy val testAuthResponse = GestaltSecurityMocking.dummyAuthResponseWithCreds()
    lazy val testCreds = testAuthResponse.creds
    lazy val user = AuthAccountWithCreds(testAuthResponse.account, Seq.empty, Seq.empty, testCreds, dummyRootOrgId)

    lazy val (testWork, testEnv) = {
      val (tw, te) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment").get
      Entitlements.setNewEntitlements(dummyRootOrgId, te.id, user, Some(tw.id))
      (tw,te)
    }

    lazy val testProvider = createKubernetesProvider(testEnv.id, "test-provider").get

    lazy val initProps = ContainerSpec(
      name = "test-container",
      container_type = "DOCKER",
      image = "nginx",
      provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
      port_mappings = pms,
      cpus = 1.0,
      memory = 128,
      disk = 0.0,
      num_instances = 1,
      network = Some("BRIDGE")
    )

    lazy val origExtId = s"/namespaces/${testEnv.id}/deployments/${initProps.name}"

    lazy val metaContainer = createInstance(
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
    ).get

    lazy val testSetup = {
      val skDefaultNs = mock[skuber.Namespace]
      skDefaultNs.name returns "default"
      val skTestNs    = mock[skuber.Namespace]
      skTestNs.name returns testEnv.id.toString
      val mockSkuber = mock[client.RequestContext]
      val mockSkuberFactory = mock[SkuberFactory]
      mockSkuber.getOption[skuber.Namespace]("default") returns Future.successful(Some(skDefaultNs))
      mockSkuberFactory.initializeKube(meq(testProvider.id), meq("default")          )(any) returns Future.successful(mockSkuber)
      mockSkuberFactory.initializeKube(meq(testProvider.id), meq(testEnv.id.toString))(any) returns Future.successful(mockSkuber)
      mockSkuber.getOption[skuber.Namespace](testEnv.id.toString) returns Future.successful(Some(skTestNs))

      mockSkuber.getOption(meq(metaContainer.name))(any,meq(skuber.ext.deploymentKind)) returns Future.successful(Some(mock[skuber.ext.Deployment]))
      mockSkuber.update(any)(any,meq(skuber.ext.deploymentKind)) returns Future.successful(mock[skuber.ext.Deployment])
      mockSkuber.list()(any,meq(client.persistentVolumeClaimListKind)) returns Future.successful(skuber.PersistentVolumeClaimList(items = Nil))

      val ks = new KubernetesService(mockSkuberFactory)
      TestSetup(ks, mockSkuber, mockSkuberFactory, skTestNs, Some(metaContainer))
    }
  }

  abstract class FakeKubeCreate( force_pull: Boolean = true,
                                 cpu: Double = 1.0,
                                 memory: Double = 128,
                                 args: Option[Seq[String]] = None,
                                 cmd: Option[String] = None,
                                 port_mappings: Seq[ContainerSpec.PortMapping] = Seq.empty,
                                 labels: Map[String,String] = Map.empty,
                                 providerConfig: Seq[(String,String)] = Seq.empty,
                                 secrets: Seq[ContainerSpec.SecretMount] = Seq.empty
                               ) extends Scope {

    lazy val testAuthResponse = GestaltSecurityMocking.dummyAuthResponseWithCreds()
    lazy val testCreds = testAuthResponse.creds
    lazy val user = AuthAccountWithCreds(testAuthResponse.account, Seq.empty, Seq.empty, testCreds, dummyRootOrgId)

    lazy val (testWork, testEnv) = {
      val (tw, te) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment").get
      Entitlements.setNewEntitlements(dummyRootOrgId, te.id, user, Some(tw.id))
      (tw,te)
    }

    lazy val testProvider = createKubernetesProvider(testEnv.id, "test-provider", providerConfig).get

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
      health_checks = Seq(),
      volumes = Seq(),
      labels = labels,
      env = Map(),
      user = None
    )

    lazy val metaSecretItems = Seq(
      SecretSpec.Item("part-a", Some("value-a")),
      SecretSpec.Item("part-b", Some("value-b"))
    )

    lazy val metaSecret = createInstance(
      ResourceIds.Secret,
      "test-secret",
      parent = Some(testEnv.id),
      properties = Some(Map(
        "provider" -> Output.renderInstance(testProvider).toString,
        "items" -> Json.toJson(metaSecretItems.map(_.copy(value = None))).toString
      ))
    ).get

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
        "force_pull" -> initProps.force_pull.toString,
        "port_mappings" -> Json.toJson(initProps.port_mappings).toString,
        "network" -> initProps.network.get,
        "labels" -> Json.toJson(labels).toString
      ) ++ Seq[Option[(String,String)]](
        args map ("args" -> Json.toJson(_).toString),
        cmd  map ("cmd" -> _)
      ).flatten.toMap)
    ).get

    lazy val genericLbls   = Map(
      KubernetesService.META_ENVIRONMENT_KEY -> testEnv.id.toString,
      KubernetesService.META_WORKSPACE_KEY -> testWork.id.toString,
      KubernetesService.META_FQON_KEY -> "root",
      KubernetesService.META_PROVIDER_KEY -> testProvider.id.toString
    )
    lazy val secretLbls    = Map(KubernetesService.META_SECRET_KEY    -> metaSecret.id.toString) ++ genericLbls
    lazy val containerLbls = Map(KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString) ++ genericLbls

    lazy val mockSecret = skuber.Secret(
      metadata = skuber.ObjectMeta(
        name = metaSecret.name,
        namespace = testEnv.id.toString,
        labels = secretLbls
      )
    )

    lazy val mockService = skuber.Service(
      name = metaContainer.name,
      spec = skuber.Service.Spec(
        clusterIP = "10.0.161.84",
        ports = List(
          skuber.Service.Port("http",  skuber.Protocol.TCP,   80, Some(skuber.portNumToNameablePort(80)), 30784),
          skuber.Service.Port("https", skuber.Protocol.TCP, 8443, Some(skuber.portNumToNameablePort(443)), 31374)
        ),
        _type = skuber.Service.Type.NodePort
      )
    ).addLabels(containerLbls)

    lazy val mockDepl = skuber.ext.Deployment(
      metadata = skuber.ObjectMeta(
        name = metaContainer.name,
        namespace = testEnv.id.toString,
        labels = containerLbls,
        creationTimestamp = Some(ZonedDateTime.now(ZoneOffset.UTC))
      )
    ).withTemplate(
      skuber.Pod.Template.Spec().addContainer(
        skuber.Container(
          name = metaContainer.name,
          image = initProps.image,
          ports = List(
            skuber.Container.Port(80, skuber.Protocol.TCP, "http"),
            skuber.Container.Port(443, skuber.Protocol.TCP, "https"),
            skuber.Container.Port(9999, skuber.Protocol.UDP, "debug")
          )
        )
      )
    )

    lazy val startA = ZonedDateTime.now(ZoneOffset.UTC)
    lazy val startB = ZonedDateTime.now(ZoneOffset.UTC)
    lazy val mockPodA = skuber.Pod(
      metadata = skuber.ObjectMeta(
        name = s"${metaContainer.name}-hash-a",
        namespace = testEnv.id.toString,
        labels = containerLbls
      ),
      status = Some(skuber.Pod.Status(
        hostIP = Some("host-a"),
        podIP = Some("10.10.10.1"),
        containerStatuses = List(skuber.Container.Status(
          name = s"${metaContainer.name}-hash-a",
          ready = true,
          restartCount = 0,
          image = initProps.image,
          imageID = "who-knows",
          state = Some(skuber.Container.Running(startedAt = Some(startA)))
        )),
        startTime = Some(startA)
      ))
    )
    lazy val mockPodB = skuber.Pod(
      metadata = skuber.ObjectMeta(
        name = s"${metaContainer.name}-hash-b",
        namespace = testEnv.id.toString,
        labels = containerLbls
      ),
      status = Some(skuber.Pod.Status(
        hostIP = Some("host-b"),
        podIP = Some("10.10.10.2"),
        containerStatuses = List(skuber.Container.Status(
          name = s"${metaContainer.name}-hash-b",
          ready = true,
          restartCount = 0,
          image = initProps.image,
          imageID = "who-knows",
          state = Some(skuber.Container.Running(startedAt = Some(startB)))
        )),
        startTime = Some(startB)
      ))
    )

    lazy val testSetup = {
      val skDefaultNs = mock[skuber.Namespace]
      skDefaultNs.name returns "default"
      val skTestNs    = mock[skuber.Namespace]
      skTestNs.name returns testEnv.id.toString
      val mockSkuber = mock[client.RequestContext]
      val mockSkuberFactory = mock[SkuberFactory]
      mockSkuberFactory.initializeKube(meq(testProvider.id), meq("default")         )(any) returns Future.successful(mockSkuber)
      mockSkuber.getOption[skuber.Namespace]("default") returns Future.successful(Some(skDefaultNs))
      mockSkuberFactory.initializeKube(meq(testProvider.id), meq(testEnv.id.toString))(any) returns Future.successful(mockSkuber)
      mockSkuber.getOption[skuber.Namespace](testEnv.id.toString) returns Future.successful(Some(skTestNs))

      mockSkuber.list()(any,meq(client.persistentVolumeClaimListKind)) returns Future.successful(skuber.PersistentVolumeClaimList(items = Nil))
      mockSkuber.list()(any,meq(skuber.ext.deplListKind)) returns Future.successful(skuber.ext.DeploymentList(items = List(mockDepl)))
      mockSkuber.list()(any,meq(client.serviceListKind)) returns Future.successful(skuber.ServiceList(items = List(mockService)))
      mockSkuber.list()(any,meq(client.podListKind)) returns Future.successful(skuber.PodList(items = List(mockPodA,mockPodB)))
      mockSkuber.list()(any,meq(client.secretListKind)) returns Future.successful(skuber.SecretList(items = List(mockSecret)))

      mockSkuber.create(any)(any,meq(skuber.ext.deploymentKind)) returns Future.successful(mock[skuber.ext.Deployment])
      mockSkuber.create(any)(any,meq(client.serviceKind)) returns Future.successful(mockService)
      mockSkuber.create(any)(any,meq(client.secretKind)) returns Future.successful(mockSecret)

      mockSkuber.getOption(meq(metaContainer.name))(any,meq(client.serviceKind)) returns Future.successful(Some(mockService))
      mockSkuber.getOption(meq(metaSecret.name))(any,meq(client.secretKind)) returns Future.successful(Some(mockSecret))

      val ks = new KubernetesService(mockSkuberFactory)
      TestSetup(ks, mockSkuber, mockSkuberFactory, skTestNs, Some(metaContainer))
    }
  }

  def hasExactlyContainerPorts(ps: skuber.Container.Port*) = ((_: skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).map(_.containers.flatMap(_.ports)).getOrElse(List())) ^^ containTheSameElementsAs(Seq(ps:_*))

  def hasExactlyServicePorts(ps: skuber.Service.Port*) = ((_: skuber.Service).spec.map(_.ports).getOrElse(List())) ^^ containTheSameElementsAs(Seq(ps:_*))

  def hasPair(p: (String,String)) = ((_: skuber.ObjectResource).metadata.labels) ^^ havePair(p)

  def hasSelector(p: (String,String)) = ((_: skuber.Service).spec.map(_.selector).getOrElse(Map.empty)) ^^ havePair(p)

  def hasEnv(vs: (String,String)*) = ((_: skuber.ext.Deployment).spec) ^^ beSome(
    ((_: skuber.ext.Deployment.Spec).template) ^^ beSome(
      ((_ : skuber.Pod.Template.Spec).spec) ^^ beSome(
        ((_ : skuber.Pod.Spec).containers.headOption) ^^ beSome(
          ((_ : skuber.Container).env) ^^ containTheSameElementsAs(
            vs.map {
              case (k,v) => skuber.EnvVar(k, skuber.EnvVar.StringValue(v))
            }
          )
        )
      )
    )
  )

  "KubernetesService" should {

    "configure environment variables for containers" in new FakeKube {
      val Success(metaContainer) = createInstance(
        ResourceIds.Container,
        "test-container",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testProvider.id
          ).toString,
          "cpus" -> "2.0",
          "memory" -> "768.0",
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> "[]",
          "env" -> Json.obj(
            "VAR1" -> "VAL1",
            "VAR2" -> "VAL2"
          ).toString,
          "network" -> ""
        ))
      )
      testSetup.kubeClient.create(any)(any,meq(skuber.ext.deploymentKind)) returns Future.successful(mock[skuber.ext.Deployment])
      testSetup.kubeClient.list()(any,meq(client.persistentVolumeClaimListKind)) returns Future.successful(skuber.PersistentVolumeClaimList(items = Nil))

      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
          (((_: skuber.ext.Deployment).spec.get.template.get.spec.get.containers.head.env) ^^ containAllOf(Seq(
            skuber.EnvVar("VAR1", skuber.EnvVar.StringValue("VAL1")),
            skuber.EnvVar("VAR2", skuber.EnvVar.StringValue("VAL2"))
          )))
      ))(any,meq(skuber.ext.deploymentKind))
      there were two(testSetup.kubeClient).close
    }

    "request magical POD_IP env var for all containers" in new FakeKube {
      val Success(metaContainer) = createInstance(
        ResourceIds.Container,
        "test-container",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testProvider.id
          ).toString,
          "cpus" -> "2.0",
          "memory" -> "768.0",
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> "[]",
          "env" -> Json.obj(
            "VAR1" -> "VAL1",
            "VAR2" -> "VAL2"
          ).toString,
          "network" -> ""
        ))
      )
      testSetup.kubeClient.create(any)(any,meq(skuber.ext.deploymentKind)) returns Future.successful(mock[skuber.ext.Deployment])
      testSetup.kubeClient.list()(any,meq(client.persistentVolumeClaimListKind)) returns Future.successful(skuber.PersistentVolumeClaimList(items = Nil))

      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name) and
          (((_: skuber.ext.Deployment).spec.get.template.get.spec.get.containers.head.env) ^^ containTheSameElementsAs(Seq(
            skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP")),
            skuber.EnvVar("VAR1", skuber.EnvVar.StringValue("VAL1")),
            skuber.EnvVar("VAR2", skuber.EnvVar.StringValue("VAL2"))
          )))
      ))(any,meq(skuber.ext.deploymentKind))
      there were two(testSetup.kubeClient).close
    }

    "deploy service for exposed port mappings and set service addresses and host port" in new FakeKubeCreate(
      port_mappings = Seq(
        ContainerSpec.PortMapping(
          name = Some("http"), protocol = "tcp",
          container_port = Some(80),
          expose_endpoint = Some(true)
        ),
        ContainerSpec.PortMapping(
          name = Some("https"), protocol = "tcp",
          container_port = Some(443), service_port = Some(8443),
          expose_endpoint = Some(true)
        ),
        ContainerSpec.PortMapping(
          name = Some("debug"), protocol = "udp",
          container_port = Some(9999), host_port = Some(9999)
        )
      )
    ) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties

      there were two(testSetup.skuberFactory).initializeKube(meq(testProvider.id), any)(any)
      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
        hasExactlyContainerPorts(
          skuber.Container.Port(80,   skuber.Protocol.TCP, "http"),
          skuber.Container.Port(443,  skuber.Protocol.TCP, "https"),
          skuber.Container.Port(9999, skuber.Protocol.UDP, "debug", hostPort = Some(9999))
        )
      ))(any,meq(skuber.ext.deploymentKind))

      val serviceCaptor = ArgumentCaptor.forClass(classOf[skuber.Service])
      there was one(testSetup.kubeClient).create(serviceCaptor.capture())(any, meq(client.serviceKind))
      val createdService = serviceCaptor.getValue
      createdService must inNamespace(testSetup.testNS.name) and
        hasExactlyServicePorts(
          skuber.Service.Port("http",  skuber.Protocol.TCP,   80, Some(skuber.portNumToNameablePort(80))),
          skuber.Service.Port("https", skuber.Protocol.TCP, 8443, Some(skuber.portNumToNameablePort(443)))
        )  and hasSelector(KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString) and
        (((_: skuber.Service).name) ^^ be_==(metaContainer.name))
      createdService.metadata.labels must havePairs(
        KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString,
        KubernetesService.META_ENVIRONMENT_KEY -> testEnv.id.toString,
        KubernetesService.META_WORKSPACE_KEY -> testWork.id.toString,
        KubernetesService.META_FQON_KEY -> "root",
        KubernetesService.META_PROVIDER_KEY -> testProvider.id.toString
      )

      import ContainerSpec.PortMapping
      import ContainerSpec.ServiceAddress

      val svcHost = s"${createdService.name}.${testEnv.id}.svc.cluster.local"
      updatedContainerProps.get("status") must beSome("LAUNCHED")
      val mappings = Json.parse(updatedContainerProps("port_mappings")).as[Seq[ContainerSpec.PortMapping]]
      mappings must containTheSameElementsAs(Seq(
        PortMapping("tcp", Some(80), None, None, Some("http"), None, Some(true), Some(ServiceAddress(svcHost, 80, Some("tcp"), None)), None),
        PortMapping("tcp", Some(443), None, Some(8443), Some("https"), None, Some(true), Some(ServiceAddress(svcHost, 8443, Some("tcp"), None)), None),
        PortMapping("udp", Some(9999), Some(9999), None, Some("debug"), None, None, None, None)
      ))
      there were two(testSetup.kubeClient).close
    }

    "provision containers and secrets with the expected external_id property" in new FakeKubeCreate() {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      updatedContainerProps must havePair(
        "external_id" -> s"/namespaces/${testEnv.id}/deployments/test-container"
      )

      val Some(updatedSecretProps) = await(testSetup.kubeService.createSecret(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/secrets"), testProvider.id, None),
        secret = metaSecret,
        items = Seq.empty
      )).properties
      updatedSecretProps must havePair(
        "external_id" -> s"/namespaces/${testEnv.id}/secrets/test-secret"
      )
    }

    "listInEnvironment must use external_id in the ContainerStats" in new FakeKubeCreate() {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties

      val Seq(stat) = await(testSetup.kubeService.listInEnvironment(
        context = ProviderContext(play.api.test.FakeRequest("GET", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None)
      ))

      val Some(externalId) = updatedContainerProps.get("external_id")

      stat.external_id must_== externalId
      there were three(testSetup.kubeClient).close
    }

    "provision secrets and containers with the requested labels and meta-specific labels" in new FakeKubeCreate(labels = Map(
      "labela" -> "value a",
      "labelb" -> "value b"
    )) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.ext.Deployment).metadata.labels) ^^ havePairs(
          "labela" -> "value a",
          "labelb" -> "value b",
          KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString,
          KubernetesService.META_ENVIRONMENT_KEY -> testEnv.id.toString,
          KubernetesService.META_WORKSPACE_KEY -> testWork.id.toString,
          KubernetesService.META_FQON_KEY -> "root",
          KubernetesService.META_PROVIDER_KEY -> testProvider.id.toString
        )
      ))(any,meq(skuber.ext.deploymentKind))

      val Some(updatedSecretProps) = await(testSetup.kubeService.createSecret(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/secrets"), testProvider.id, None),
        secret = metaSecret,
        items = Seq.empty
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.Secret).metadata.labels) ^^ havePairs(
          // "labela" -> "value a",
          // "labelb" -> "value b",
          KubernetesService.META_SECRET_KEY -> metaSecret.id.toString,
          KubernetesService.META_ENVIRONMENT_KEY -> testEnv.id.toString,
          KubernetesService.META_WORKSPACE_KEY -> testWork.id.toString,
          KubernetesService.META_FQON_KEY -> "root",
          KubernetesService.META_PROVIDER_KEY -> testProvider.id.toString
        )
      ))(any,meq(client.secretKind))
    }

    "provision namespace with the expected name and labels" in new FakeKube {
      val (newWork, newEnv) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment").get
      Entitlements.setNewEntitlements(dummyRootOrgId, newEnv.id, user, Some(newWork.id))
      testSetup.kubeClient.getOption[skuber.Namespace](newEnv.id.toString) returns Future.successful(None)
      testSetup.kubeClient.create[skuber.Namespace](argThat(
        ((_:skuber.Namespace).name) ^^ beEqualTo(newEnv.id.toString)
      ))(any,any) returns Future(mock[skuber.Namespace])
      val newNamespace = await(testSetup.kubeService.getNamespace(
        rc = testSetup.kubeClient,
        pc = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${newEnv.id}/containers"), testProvider.id, None),
        create = true
      ))
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.Namespace).metadata.labels) ^^ havePairs(
          KubernetesService.META_ENVIRONMENT_KEY -> newEnv.id.toString,
          KubernetesService.META_WORKSPACE_KEY -> newWork.id.toString,
          KubernetesService.META_FQON_KEY -> "root",
          KubernetesService.META_PROVIDER_KEY -> testProvider.id.toString
        )
          and
        ((_:skuber.Namespace).name) ^^ beEqualTo(newEnv.id.toString)
      ))(any,any)
    }

    "set PullPolicy Always when force_pull == true" in new FakeKubeCreate(force_pull = true) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.imagePullPolicy)) ^^ beSome(
          skuber.Container.PullPolicy.Always
        )
      ))(any,meq(skuber.ext.deploymentKind))
      there were two(testSetup.kubeClient).close
    }

    "set PullPolicy Always when force_pull == false" in new FakeKubeCreate(force_pull = false) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.imagePullPolicy)) ^^ beSome(
          skuber.Container.PullPolicy.IfNotPresent
        )
      ))(any,meq(skuber.ext.deploymentKind))
      there were two(testSetup.kubeClient).close
    }

    "pass args when specified" in new FakeKubeCreate(args = Some(Seq("echo","hello","world"))) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.args)) ^^ beSome(
          containTheSameElementsAs(Seq("echo","hello","world"))
        )
      ))(any,meq(skuber.ext.deploymentKind))
      there were two(testSetup.kubeClient).close
    }

    "pass no args when unspecified" in new FakeKubeCreate(args = None) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.args)) ^^ beSome(empty)
      ))(any,meq(skuber.ext.deploymentKind))
      there were two(testSetup.kubeClient).close
    }

    "pass simple cmd when specified" in new FakeKubeCreate(cmd = Some("""/usr/bin/python""")) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.command)) ^^ beSome(
          containTheSameElementsAs(Seq("/usr/bin/python"))
        )
      ))(any,meq(skuber.ext.deploymentKind))
      there were two(testSetup.kubeClient).close
    }

    "pass complicated cmd when specified" in new FakeKubeCreate(cmd = Some("python -m SimpleHTTPServer $PORT")) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.command)) ^^ beSome(
          containTheSameElementsAs(Seq("python","-m","SimpleHTTPServer","$PORT"))
        )
      ))(any,meq(skuber.ext.deploymentKind))
      there were two(testSetup.kubeClient).close
    }

    "provision cpu request and memory limit+request by default (dcos default)" in new FakeKubeCreate(cpu = 1.0, memory = 1024) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).flatMap(_.resources)) ^^ beSome(skuber.Resource.Requirements(
          limits = Map("memory" -> "1024.000M"),
          requests = Map("memory" -> "1024.000M", "cpu" -> "1000m")
        ))
      ))(any,meq(skuber.ext.deploymentKind))
      there were two(testSetup.kubeClient).close
    }

    "provision cpu limit if specified (tight mode)" in new FakeKubeCreate(cpu = 1.0, memory = 1024, providerConfig = Seq("cpu-requirement-type" -> "request,limit")) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).flatMap(_.resources)) ^^ beSome(skuber.Resource.Requirements(
          limits = Map("memory" -> "1024.000M", "cpu" -> "1000m"),
          requests = Map("memory" -> "1024.000M", "cpu" -> "1000m")
        ))
      ))(any,meq(skuber.ext.deploymentKind))
      there were two(testSetup.kubeClient).close
    }

    "provision memory request-only if specified (noisy-neighbor)" in new FakeKubeCreate(cpu = 1.0, memory = 1024, providerConfig = Seq("memory-requirement-type" -> "request")) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).flatMap(_.resources)) ^^ beSome(skuber.Resource.Requirements(
          limits = Map(),
          requests = Map("memory" -> "1024.000M", "cpu" -> "1000m")
        ))
      ))(any,meq(skuber.ext.deploymentKind))
      there were two(testSetup.kubeClient).close
    }

    "provision with no limits or resources if specified (wild-west)" in new FakeKubeCreate(cpu = 1.0, memory = 1024, providerConfig = Seq("cpu-requirement-type" -> "", "memory-requirement-type" -> "")) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).flatMap(_.resources)) ^^ beSome(skuber.Resource.Requirements(
          limits = Map(), requests = Map()
        ))
      ))(any,meq(skuber.ext.deploymentKind))
      there were two(testSetup.kubeClient).close
    }

    "pass complicated cmd with difficult bash-compatible spacing" in new FakeKubeCreate(cmd = Some("echo hello|wc")) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.command)) ^^ beSome(
          containTheSameElementsAs(Seq("echo","hello","|","wc"))
        )
      ))(any,meq(skuber.ext.deploymentKind))
      there was one(testSetup.kubeClient).close
    }.pendingUntilFixed("this is going to be hard")

    "pass no cmd when unspecified" in new FakeKubeCreate(cmd = None) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.command)) ^^ beSome(empty)
      ))(any,meq(skuber.ext.deploymentKind))
      there were two(testSetup.kubeClient).close
    }

    "orchestrate kube ingress for virtual hosts" in new FakeKubeCreate(port_mappings = Seq(
      ContainerSpec.PortMapping( protocol = "tcp", container_port = Some(80),   expose_endpoint = Some(true), name = Some("http"), virtual_hosts = Some(Seq("galacticfog.com","www.galacticfog.com")) ),
      ContainerSpec.PortMapping( protocol = "tcp", container_port = Some(443),  service_port = Some(8443), expose_endpoint = Some(true), name = Some("https"), virtual_hosts = Some(Seq("secure.galacticfog.com")) ),
      ContainerSpec.PortMapping( protocol = "tcp", container_port = Some(10000), expose_endpoint = Some(false), name = Some("not-exposed"), virtual_hosts = Some(Seq("no-exposure-no-vhost.galacticfog.com")) )
    )) {
      testSetup.kubeClient.create(any)(any,meq(skuber.ext.ingressKind)) returns Future(mock[skuber.ext.Ingress])

      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties

      import skuber.ext.Ingress
      import skuber.ext.Ingress._

      val serviceCaptor = ArgumentCaptor.forClass(classOf[skuber.Service])
      there was one(testSetup.kubeClient).create(serviceCaptor.capture())(any, meq(client.serviceKind))
      val createdService = serviceCaptor.getValue

      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
        ((_:Ingress).metadata.labels) ^^ havePairs(
          KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString,
          KubernetesService.META_ENVIRONMENT_KEY -> testEnv.id.toString,
          KubernetesService.META_WORKSPACE_KEY -> testWork.id.toString,
          KubernetesService.META_FQON_KEY -> "root",
          KubernetesService.META_PROVIDER_KEY -> testProvider.id.toString
        )
          and
        ((_:Ingress).spec.map(_.rules).getOrElse(Nil)) ^^ containTheSameElementsAs(Seq(
          Rule("www.galacticfog.com",    HttpRule(List(Path("", Backend(createdService.name, 80))))),
          Rule("galacticfog.com",        HttpRule(List(Path("", Backend(createdService.name, 80))))),
          Rule("secure.galacticfog.com", HttpRule(List(Path("", Backend(createdService.name, 8443)))))
        ))
      ))(any,meq(skuber.ext.ingressKind))
      there were two(testSetup.kubeClient).close
    }

    "set appropriate host port on container tasks from kubernetes Service node port" in new FakeKubeCreate {

      import com.galacticfog.gestalt.meta.api.ContainerStats

      val Some(containerStats) = await(testSetup.kubeService.find(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      ))

      val Seq(containerStats2) = await(testSetup.kubeService.listInEnvironment(
        context = ProviderContext(play.api.test.FakeRequest("POST",s"/root/environments/${testEnv.id}/containers"), testProvider.id, None)
      ))

      containerStats.tasksRunning must_== 2
      containerStats.taskStats must beSome(containTheSameElementsAs(Seq(
        ContainerStats.TaskStat(
          id = "test-container-hash-a",
          host = "host-a",
          ipAddresses = Some(Seq(ContainerStats.TaskStat.IPAddress(
            ipAddress = "10.10.10.1",
            protocol = "IPv4"
          ))),
          ports = Seq(30784,31374,0),
          startedAt = Some(startA.toString)
        ),
        ContainerStats.TaskStat(
          id = "test-container-hash-b",
          host = "host-b",
          ipAddresses = Some(Seq(ContainerStats.TaskStat.IPAddress(
            ipAddress = "10.10.10.2",
            protocol = "IPv4"
          ))),
          ports = Seq(30784,31374,0),
          startedAt = Some(startB.toString)
        )
      )))

      containerStats must_== containerStats2
      there were two(testSetup.kubeClient).close
    }

    "prefer to update cpu/mem from limit instead of request" in new FakeKube {
      val containerId = uuid()
      val metaContainer = mock[GestaltResourceInstance]
      metaContainer.id returns containerId
      val lbls = Map(KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString)
      val mockDepl = skuber.ext.Deployment(
        metadata = skuber.ObjectMeta(
          name = "test-container",
          namespace = testEnv.id.toString,
          labels = lbls,
          creationTimestamp = Some(ZonedDateTime.now(ZoneOffset.UTC))
        )
      ).withTemplate(
        skuber.Pod.Template.Spec().addContainer(
          skuber.Container(
            name = "test-container",
            image = "nginx",
            resources = Some(skuber.Resource.Requirements(
              limits = Map(
                skuber.Resource.cpu -> "1000m",
                skuber.Resource.memory -> "128000k"
              ),
              requests = Map(
                skuber.Resource.cpu -> "100m",
                skuber.Resource.memory -> "64000k"
              )
            ))
          )
        )
      )
      testSetup.kubeClient.list()(any,meq(skuber.ext.deplListKind)) returns Future.successful(skuber.ext.DeploymentList(items = List(mockDepl)))
      testSetup.kubeClient.list()(any,meq(client.serviceListKind)) returns Future.successful(skuber.ServiceList())
      testSetup.kubeClient.list()(any,meq(client.podListKind)) returns Future.successful(skuber.PodList())

      val Some(containerStats) = await(testSetup.kubeService.find(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      ))

      containerStats.cpus must_== 1.0
      containerStats.memory must_== 128.0
    }

    "fallback on getting cpu/mem from request" in new FakeKube {
      val containerId = uuid()
      val metaContainer = mock[GestaltResourceInstance]
      metaContainer.id returns containerId
      val lbls = Map(KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString)
      val mockDepl = skuber.ext.Deployment(
        metadata = skuber.ObjectMeta(
          name = "test-container",
          namespace = testEnv.id.toString,
          labels = lbls,
          creationTimestamp = Some(ZonedDateTime.now(ZoneOffset.UTC))
        )
      ).withTemplate(
        skuber.Pod.Template.Spec().addContainer(
          skuber.Container(
            name = "test-container",
            image = "nginx",
            resources = Some(skuber.Resource.Requirements(
              limits = Map(),
              requests = Map(
                skuber.Resource.cpu -> "100m",
                skuber.Resource.memory -> "64000k"
              )
            ))
          )
        )
      )
      testSetup.kubeClient.list()(any,meq(skuber.ext.deplListKind)) returns Future.successful(skuber.ext.DeploymentList(items = List(mockDepl)))
      testSetup.kubeClient.list()(any,meq(client.serviceListKind)) returns Future.successful(skuber.ServiceList())
      testSetup.kubeClient.list()(any,meq(client.podListKind)) returns Future.successful(skuber.PodList())

      val Some(containerStats) = await(testSetup.kubeService.find(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      ))

      containerStats.cpus must_== 0.1
      containerStats.memory must_== 64.0
    }

    "fallback on to 0 cpu/mem if neither request nor limit " in new FakeKube {
      val containerId = uuid()
      val metaContainer = mock[GestaltResourceInstance]
      metaContainer.id returns containerId
      val lbls = Map(KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString)
      val mockDepl = skuber.ext.Deployment(
        metadata = skuber.ObjectMeta(
          name = "test-container",
          namespace = testEnv.id.toString,
          labels = lbls,
          creationTimestamp = Some(ZonedDateTime.now(ZoneOffset.UTC))
        )
      ).withTemplate(
        skuber.Pod.Template.Spec().addContainer(
          skuber.Container(
            name = "test-container",
            image = "nginx",
            resources = Some(skuber.Resource.Requirements(
              limits = Map(),
              requests = Map()
            ))
          )
        )
      )
      testSetup.kubeClient.list()(any,meq(skuber.ext.deplListKind)) returns Future.successful(skuber.ext.DeploymentList(items = List(mockDepl)))
      testSetup.kubeClient.list()(any,meq(client.serviceListKind)) returns Future.successful(skuber.ServiceList())
      testSetup.kubeClient.list()(any,meq(client.podListKind)) returns Future.successful(skuber.PodList())

      val Some(containerStats) = await(testSetup.kubeService.find(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      ))

      containerStats.cpus must_== 0.0
      containerStats.memory must_== 0.0
    }

    "delete service and any ingress on container delete" in new FakeKube {
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
          "network" -> "default"
        ))
      )

      val label = KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString
      val mockDep = skuber.ext.Deployment(metadata = skuber.ObjectMeta(
        name = metaContainer.name,
        namespace = testEnv.id.toString,
        labels = Map(label)
      ))
      val mockIngress = skuber.ext.Ingress(metadata = skuber.ObjectMeta(
        name = metaContainer.name,
        namespace = testEnv.id.toString,
        labels = Map(label)
      ))
      val mockRS  = skuber.ext.ReplicaSet("test-container-hash").addLabel( label )

      val mockService = skuber.Service(metaContainer.name).withSelector( label ).addLabel( label )
      testSetup.kubeClient.list()(any, meq(skuber.ext.deplListKind)) returns Future.successful(skuber.ext.DeploymentList(items = List(mockDep)))
      testSetup.kubeClient.list()(any, meq(skuber.ext.replsetListKind)) returns Future.successful(skuber.ext.ReplicaSetList(items = List(mockRS)))
      testSetup.kubeClient.list()(any, meq(client.podListKind)) returns Future.successful(skuber.PodList())
      testSetup.kubeClient.list()(any, meq(client.serviceListKind)) returns Future.successful(skuber.ServiceList(items = List(mockService)))
      testSetup.kubeClient.list()(any, meq(skuber.ext.ingressListKind)) returns Future.successful(skuber.ext.IngressList(items = List(mockIngress)))
      testSetup.kubeClient.delete(mockDep.name,0)(skuber.ext.deploymentKind) returns Future.successful(())
      testSetup.kubeClient.delete(mockRS.name, 0)(skuber.ext.replsetsKind) returns Future.successful(())
      testSetup.kubeClient.delete(mockService.name, 0)(client.serviceKind) returns Future.successful(())
      testSetup.kubeClient.delete(mockIngress.name, 0)(skuber.ext.ingressKind) returns Future.successful(())

      await(testSetup.kubeService.destroy(metaContainer))
      there was one(testSetup.kubeClient).delete(mockDep.name,0)(skuber.ext.deploymentKind)
      there was one(testSetup.kubeClient).delete(mockRS.name,0)(skuber.ext.replsetsKind)
      there was one(testSetup.kubeClient).delete(mockService.name,0)(client.serviceKind)
      there was one(testSetup.kubeClient).delete(mockIngress.name,0)(skuber.ext.ingressKind)
      there was one(testSetup.kubeClient).close
    }

    "not fail if no service or ingress to delete on container delete" in new FakeKube {
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
          "network" -> "default"
        ))
      )

      val label = KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString
      val mockDep = skuber.ext.Deployment(metadata = skuber.ObjectMeta(
        name = metaContainer.name,
        namespace = testEnv.id.toString,
        labels = Map(label)
      ))
      val mockRS  = skuber.ext.ReplicaSet(s"${metaContainer.name}-hash").addLabel( label )
      testSetup.kubeClient.list()(any, meq(skuber.ext.deplListKind)) returns Future.successful(skuber.ext.DeploymentList(items = List(mockDep)))
      testSetup.kubeClient.list()(any, meq(skuber.ext.replsetListKind)) returns Future.successful(skuber.ext.ReplicaSetList(items = List(mockRS)))
      testSetup.kubeClient.list()(any, meq(client.podListKind)) returns Future.failed(new skuber.api.client.K8SException(skuber.api.client.Status(reason = Some("test failure"))))
      testSetup.kubeClient.list()(any, meq(client.serviceListKind)) returns Future.failed(new skuber.api.client.K8SException(skuber.api.client.Status(reason = Some("test failure"))))
      testSetup.kubeClient.list()(any, meq(skuber.ext.ingressListKind)) returns Future.failed(new skuber.api.client.K8SException(skuber.api.client.Status(reason = Some("test failure"))))
      testSetup.kubeClient.delete(mockDep.name,0)(skuber.ext.deploymentKind) returns Future.successful(())
      testSetup.kubeClient.delete(mockRS.name, 0)(skuber.ext.replsetsKind) returns Future.successful(())

      await(testSetup.kubeService.destroy(metaContainer))
      there were no(testSetup.kubeClient).delete(any,any)(meq(client.serviceKind))
      there was one(testSetup.kubeClient).close
    }

    "scale appropriately using skuber PATCH" in new FakeKube {
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
          "network" -> "default"
        ))
      )

      val testScale = 0
      val label = KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString
      val testDepl = skuber.ext.Deployment(metadata = skuber.ObjectMeta(
        name = metaContainer.name,
        namespace = testEnv.id.toString,
        labels = Map(label)
      ))
      val mockRS  = skuber.ext.ReplicaSet(s"${metaContainer.name}-hash").addLabel( label )
      testSetup.kubeClient.getOption(meq(metaContainer.name))(any, meq(skuber.ext.deploymentKind)) returns Future.successful(Some(testDepl))
      testSetup.kubeClient.update(any)(any, meq(skuber.ext.deploymentKind)) answers {
        (a: Any) =>
          val arr = a.asInstanceOf[Array[Object]]
          val depl = arr(0).asInstanceOf[skuber.ext.Deployment]
          Future.successful(depl.withReplicas(testScale))
      }

      val Some(updatedContainerProps) = await(testSetup.kubeService.scale(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers/${metaContainer.id}"), testProvider.id, None),
        container = metaContainer,
        numInstances = testScale
      )).properties

      there was one(testSetup.kubeClient).update(argThat(
        ((depl: skuber.ext.Deployment) => (depl.spec.map(_.replicas).getOrElse(-1) must_== testScale)) and haveName("test-container")
      ))(any,meq(skuber.ext.deploymentKind))

      updatedContainerProps must havePair(
        "num_instances" -> testScale.toString
      )
      there was one(testSetup.kubeClient).close
    }

    "throw a 400 on container rename" in new FakeKubeWithPrexistingContainer(Seq.empty) {
      await(testSetup.kubeService.update(
        context = ProviderContext(play.api.test.FakeRequest("PATCH", s"/root/environments/${testEnv.id}/containers/${metaContainer.id}"), testProvider.id, None),
        container = metaContainer.copy(
          name = "updated-name"
        )
      )) must throwAn[BadRequestException]("renaming containers is not supported")
    }

    "update support using kube PUT" in new FakeKubeWithPrexistingContainer(Seq(
          ContainerSpec.PortMapping(
            protocol = "tcp",
            container_port = Some(80),
            name = Some("remove-service"),
            expose_endpoint = Some(true),
            virtual_hosts = Some(Seq("port80.test.com"))
          ),
          ContainerSpec.PortMapping(
            protocol = "tcp",
            container_port = Some(81),
            name = Some("add-service"),
            expose_endpoint = Some(false)
          ),
          ContainerSpec.PortMapping(
            protocol = "tcp",
            container_port = Some(443),
            service_port = Some(8443),
            name = Some("remove-port"),
            expose_endpoint = Some(true),
            virtual_hosts = Some(Seq("port8443.test.com"))
          )
        )) {

      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(skuber.ext.ingressKind)) returns Future.successful(Some(mock[skuber.ext.Ingress]))
      testSetup.kubeClient.list()(any, meq(skuber.ext.deplListKind)) returns Future.successful(skuber.ext.DeploymentList(items = List(mock[skuber.ext.Deployment])))
      testSetup.kubeClient.update(any)(any,meq(skuber.ext.ingressKind)) returns Future.successful(mock[skuber.ext.Ingress])
      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(client.serviceKind)) returns Future.successful(None)
      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(client.serviceKind)) returns Future.successful(Some(mock[skuber.Service]))
      testSetup.kubeClient.update(any)(any,meq(client.serviceKind)) returns Future.successful(mock[skuber.Service])

      val newPortMappings = Seq(
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(80),
          name = Some("remove-service"),
          expose_endpoint = Some(false)
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(81),
          name = Some("add-service"),
          expose_endpoint = Some(true),
          virtual_hosts = Some(Seq("port81.test.com"))
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(444),
          service_port = Some(8444),
          name = Some("add-port"),
          expose_endpoint = Some(true),
          virtual_hosts = Some(Seq("port8444.test.com"))
        )
      )
      val updatedContainer = await(testSetup.kubeService.update(
        context = ProviderContext(play.api.test.FakeRequest("PATCH", s"/root/environments/${testEnv.id}/containers/${metaContainer.id}"), testProvider.id, None),
        container = metaContainer.copy(
          properties = metaContainer.properties.map(
            _ ++ Map(
              "image" -> "nginx:updated",
              "port_mappings" -> Json.toJson(newPortMappings).toString
            )
          )
        )
      ))
      val Some(updatedContainerProps) = updatedContainer.properties
      there was one(testSetup.kubeClient).update(argThat(
        inNamespace(testSetup.testNS.name)
          and haveName(metaContainer.name)
          and (((_:skuber.ext.Deployment).getPodSpec.get.containers.head.image) ^^ be_==("nginx:updated"))
          and ((((_:skuber.ext.Deployment).spec.get.selector.get.requirements) ^^ contain(
            skuber.LabelSelector.IsEqualRequirement( "meta/container", metaContainer.id.toString )
          )))
      ))(any,meq(skuber.ext.deploymentKind))
      there was one(testSetup.kubeClient).update(argThat(
        inNamespace(testSetup.testNS.name)
          and haveName(metaContainer.name)
          and (((_: skuber.ext.Ingress).spec.get.rules.map(_.host)) ^^ containTheSameElementsAs(Seq("port81.test.com","port8444.test.com")))
          and (((_: skuber.ext.Ingress).spec.get.rules.flatMap(_.http.paths).map(_.backend.serviceName).distinct) ^^ containTheSameElementsAs(Seq(metaContainer.name)))
      ))(any,meq(skuber.ext.ingressKind))
      there was one(testSetup.kubeClient).update(argThat(
        inNamespace(testSetup.testNS.name)
          and (((_: skuber.Service).name) ^^ be_==(metaContainer.name))
      ))( any, meq(client.serviceKind) )

      updatedContainerProps must havePair(
        "image" -> "nginx:updated"
      )
      updatedContainerProps("port_mappings") must /#(0) and not /#(0) /("service_address")
      updatedContainerProps("port_mappings") must /#(1) /("service_address") /("host" -> (metaContainer.name + "." + testEnv.id + ".svc.cluster.local"))
      updatedContainerProps("port_mappings") must /#(2) /("service_address") /("host" -> (metaContainer.name + "." + testEnv.id + ".svc.cluster.local"))
      there were two(testSetup.kubeClient).close
    }

    "delete empty ingress on container PUT" in new FakeKubeWithPrexistingContainer(Seq(
          ContainerSpec.PortMapping(
            protocol = "tcp",
            container_port = Some(80),
            name = Some("remove-service"),
            expose_endpoint = Some(true),
            virtual_hosts = Some(Seq("port80.test.com"))
          ),
          ContainerSpec.PortMapping(
            protocol = "tcp",
            container_port = Some(81),
            name = Some("no-service"),
            expose_endpoint = Some(false)
          )
        )) {

      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(skuber.ext.ingressKind)) returns Future.successful(Some(mock[skuber.ext.Ingress]))
      testSetup.kubeClient.list()(any, meq(skuber.ext.deplListKind)) returns Future.successful(skuber.ext.DeploymentList(items = List(mock[skuber.ext.Deployment])))
      testSetup.kubeClient.delete(metaContainer.name, 0)(skuber.ext.ingressKind) returns Future.successful(())
      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(client.serviceKind)) returns Future.successful(None)
      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(client.serviceKind)) returns Future.successful(Some(mock[skuber.Service]))
      testSetup.kubeClient.update(any)(any,meq(client.serviceKind)) returns Future.successful(mock[skuber.Service])

      val newPortMappings = Seq(
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(80),
          name = Some("removed-service"),
          expose_endpoint = Some(false)
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(81),
          name = Some("no-service"),
          expose_endpoint = Some(false)
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(8881),
          name = Some("new-service"),
          expose_endpoint = Some(true)
        )
      )

      val updatedContainer = await(testSetup.kubeService.update(
        context = ProviderContext(play.api.test.FakeRequest("PATCH", s"/root/environments/${testEnv.id}/containers/${metaContainer.id}"), testProvider.id, None),
        container = metaContainer.copy(
          properties = metaContainer.properties.map(
            _ ++ Map(
              "port_mappings" -> Json.toJson(newPortMappings).toString
            )
          )
        )
      ))
      val Some(updatedContainerProps) = updatedContainer.properties
      there was one(testSetup.kubeClient).delete( meq("test-container"), any )(meq(skuber.ext.ingressKind))
      there was one(testSetup.kubeClient).update( any )( any, meq(client.serviceKind) )
      there was one(testSetup.kubeClient).update( any )( any, meq(skuber.ext.deploymentKind) )
      there were two(testSetup.kubeClient).close
    }

    "delete empty service on container PUT" in new FakeKubeWithPrexistingContainer(Seq(
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(80),
          name = Some("remove-service"),
          expose_endpoint = Some(true),
          virtual_hosts = Some(Seq("port80.test.com"))
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(81),
          name = Some("no-service"),
          expose_endpoint = Some(false)
        )
      )) {

      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(skuber.ext.ingressKind)) returns Future.successful(None)
      testSetup.kubeClient.list()(any, meq(skuber.ext.deplListKind)) returns Future.successful(skuber.ext.DeploymentList(items = List(mock[skuber.ext.Deployment])))
      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(client.serviceKind)) returns Future.successful(None)
      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(client.serviceKind)) returns Future.successful(Some(mock[skuber.Service]))
      testSetup.kubeClient.delete(meq(metaContainer.name), any)(meq(client.serviceKind)) returns Future.successful(())

      val newPortMappings = Seq(
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(80),
          name = Some("removed-service"),
          expose_endpoint = Some(false)
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(81),
          name = Some("no-service"),
          expose_endpoint = Some(false)
        )
      )

      val updatedContainer = await(testSetup.kubeService.update(
        context = ProviderContext(play.api.test.FakeRequest("PATCH", s"/root/environments/${testEnv.id}/containers/${metaContainer.id}"), testProvider.id, None),
        container = metaContainer.copy(
          properties = metaContainer.properties.map(
            _ ++ Map(
              "port_mappings" -> Json.toJson(newPortMappings).toString
            )
          )
        )
      ))
      val Some(updatedContainerProps) = updatedContainer.properties
      there was one(testSetup.kubeClient).delete( meq(metaContainer.name), any )(meq(client.serviceKind))
      there was one(testSetup.kubeClient).update( any )( any, meq(skuber.ext.deploymentKind) )
      there were two(testSetup.kubeClient).close
    }

    "create service and ingress on container update" in new FakeKubeWithPrexistingContainer(Seq(
      ContainerSpec.PortMapping(
        protocol = "tcp",
        container_port = Some(80),
        name = Some("update-port"),
        expose_endpoint = Some(false),
        virtual_hosts = None
      )
    )) {

      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(skuber.ext.ingressKind)) returns Future.successful(None)
      testSetup.kubeClient.list()(any, meq(skuber.ext.deplListKind)) returns Future.successful(skuber.ext.DeploymentList(items = List(mock[skuber.ext.Deployment])))
      testSetup.kubeClient.create(any)(any,meq(skuber.ext.ingressKind)) returns Future.successful(mock[skuber.ext.Ingress])
      testSetup.kubeClient.getOption(any)(any,meq(client.serviceKind)) returns Future.successful(None)
      testSetup.kubeClient.create(any)(any,meq(client.serviceKind)) returns Future.successful(mock[skuber.Service])

      val newPortMappings = Seq(
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(80),
          name = Some("update-port"),
          expose_endpoint = Some(true),
          virtual_hosts = Some(Seq("port80.test.com"))
        )
      )
      val updatedContainer = await(testSetup.kubeService.update(
        context = ProviderContext(play.api.test.FakeRequest("PATCH", s"/root/environments/${testEnv.id}/containers/${metaContainer.id}"), testProvider.id, None),
        container = metaContainer.copy(
          properties = metaContainer.properties.map(
            _ ++ Map(
              "image" -> "nginx:updated",
              "port_mappings" -> Json.toJson(newPortMappings).toString
            )
          )
        )
      ))
      val Some(updatedContainerProps) = updatedContainer.properties
      there was one(testSetup.kubeClient).update(argThat(
        inNamespace(testSetup.testNS.name)
          and haveName(metaContainer.name)
          and (((_:skuber.ext.Deployment).getPodSpec.get.containers.head.image) ^^ be_==("nginx:updated"))
      ))(any,meq(skuber.ext.deploymentKind))
      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and haveName(metaContainer.name)
          and (((_: skuber.ext.Ingress).spec.get.rules.toSeq.map(_.host)) ^^ containTheSameElementsAs(Seq("port80.test.com")))
      ))(any,meq(skuber.ext.ingressKind))
      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and haveName(metaContainer.name)
          and (((_: skuber.Service).spec.get.ports.map(_.targetPort.get))) ^^ containTheSameElementsAs(Seq(skuber.portNumToNameablePort(80)))
      ))(any,meq(client.serviceKind))

      updatedContainerProps must havePair(
        "image" -> "nginx:updated"
      )
      there were two(testSetup.kubeClient).close
    }

    "not persist secret values in meta" in new FakeKubeCreate() {
      val Some(updatedSecretProps) = await(testSetup.kubeService.createSecret(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/secrets"), testProvider.id, None),
        secret = metaSecret,
        items = metaSecretItems
      )).properties

      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
          (
            ((_: skuber.Secret).data.map({
              case (key, arrayByte) => SecretSpec.Item(key, Some(new String(arrayByte, Ascii.DEFAULT_CHARSET)))
            }).toSeq) ^^ containTheSameElementsAs(metaSecretItems)
          )
      ))(any,meq(client.secretKind))

      Json.parse(updatedSecretProps("items")).as[Seq[SecretSpec.Item]] must containTheSameElementsAs(
        metaSecretItems.map(_.copy(value = None))
      )

      val persistedProps = ResourceFactory.findById(ResourceIds.Secret, metaSecret.id).get.properties.get
      Json.parse(persistedProps("items")).as[Seq[SecretSpec.Item]] must containTheSameElementsAs(
        metaSecretItems.map(_.copy(value = None))
      )
    }

    "delete secret" in new FakeKube {
      val Success(metaSecret) = createInstance(
        ResourceIds.Secret,
        "test-secret",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> Output.renderInstance(testProvider).toString,
          "items" -> "[]"
        ))
      )

      val label = KubernetesService.META_SECRET_KEY -> metaSecret.id.toString
      val mockSecret = skuber.Secret(metadata = skuber.ObjectMeta(
        name = metaSecret.name,
        namespace = testEnv.id.toString,
        labels = Map(label)
      ))

      testSetup.kubeClient.list()(any, meq(client.secretListKind)) returns Future.successful(skuber.SecretList(items = List(mockSecret)))
      testSetup.kubeClient.delete(mockSecret.name,0)(client.secretKind) returns Future.successful(())

      await(testSetup.kubeService.destroySecret(metaSecret))
      there was one(testSetup.kubeClient).delete(mockSecret.name,0)(client.secretKind)
      there was one(testSetup.kubeClient).close
    }

    "mount specified secrets on container create" in new FakeKubeCreate(
      secrets = Seq(
        SecretEnvMount(null, "SOME_ENV_VAR", "part-a"),
//        SecretFileMount(null, "/dir/file-part-a", "part-a"),
//        SecretFileMount(null, "/dir/file-part-b", "part-b"),
        SecretDirMount(null, "/dir")
      )
    ) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties

      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
          (((_: skuber.ext.Deployment).spec.get.template.get.spec.get.volumes) ^^ containAllOf(Seq(
            skuber.Volume(
              name = metaSecret.id.toString,
              source = skuber.Volume.Secret(metaSecret.name)
            )
          )))
          and
          (((_: skuber.ext.Deployment).spec.get.template.get.spec.get.containers.head.volumeMounts) ^^ containAllOf(Seq(
            skuber.Volume.Mount(metaSecret.id.toString, "/dir", true)
          )))
          and
          (((_: skuber.ext.Deployment).spec.get.template.get.spec.get.containers.head.env) ^^ containAllOf(Seq(
            skuber.EnvVar("SOME_ENV_VAR", skuber.EnvVar.SecretKeyRef(metaSecret.properties.get("external_id"), "part-a"))
          )))
      ))(any,meq(skuber.ext.deploymentKind))

      val serviceCaptor = ArgumentCaptor.forClass(classOf[skuber.Service])
      there was one(testSetup.kubeClient).create(serviceCaptor.capture())(any, meq(client.serviceKind))
      val createdService = serviceCaptor.getValue
      createdService must inNamespace(testSetup.testNS.name) and
        hasExactlyServicePorts(
          skuber.Service.Port("http",  skuber.Protocol.TCP,   80, Some(skuber.portNumToNameablePort(80))),
          skuber.Service.Port("https", skuber.Protocol.TCP, 8443, Some(skuber.portNumToNameablePort(443)))
        )  and hasSelector(KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString) and
        (((_: skuber.Service).name) ^^ be_==(metaContainer.name))
      createdService.metadata.labels must havePairs(
        KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString,
        KubernetesService.META_ENVIRONMENT_KEY -> testEnv.id.toString,
        KubernetesService.META_WORKSPACE_KEY -> testWork.id.toString,
        KubernetesService.META_FQON_KEY -> "root",
        KubernetesService.META_PROVIDER_KEY -> testProvider.id.toString
      )

      import ContainerSpec.PortMapping
      import ContainerSpec.ServiceAddress

      val svcHost = s"${createdService.name}.${testEnv.id}.svc.cluster.local"
      updatedContainerProps.get("status") must beSome("LAUNCHED")
      val mappings = Json.parse(updatedContainerProps("port_mappings")).as[Seq[ContainerSpec.PortMapping]]
      mappings must containTheSameElementsAs(Seq(
        PortMapping("tcp", Some(80), None, None, Some("http"), None, Some(true), Some(ServiceAddress(svcHost, 80, Some("tcp"), None)), None),
        PortMapping("tcp", Some(443), None, Some(8443), Some("https"), None, Some(true), Some(ServiceAddress(svcHost, 8443, Some("tcp"), None)), None),
        PortMapping("udp", Some(9999), Some(9999), None, Some("debug"), None, None, None, None)
      ))
      there were two(testSetup.kubeClient).close
    }.pendingUntilFixed("needs implementation")

  }

}
