package services

import java.time.{ZoneOffset, ZonedDateTime}

import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.ContainerSpec.ServiceAddress
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.google.inject.AbstractModule
import com.mohiva.play.silhouette.impl.util.SecureRandomIDGenerator
import controllers.SecurityResources
import controllers.util.GestaltSecurityMocking
import org.junit.runner.RunWith
import org.mockito.ArgumentCaptor
import org.specs2.mock.Mockito
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.specs2.specification.{BeforeAfterEach, BeforeAll, Scope}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.PlaySpecification
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.{JsonMatchers, Matcher}
import skuber.api.client
import skuber.json.format._
import skuber.json.ext.format._

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.util.Success
import play.api.inject.bind

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
      mockSkuber.get[skuber.Namespace]("default") returns Future.successful(skDefaultNs)
      mockSkuberFactory.initializeKube(meq(testProvider.id), meq(testEnv.id.toString))(any) returns Future.successful(mockSkuber)
      mockSkuber.get[skuber.Namespace](testEnv.id.toString) returns Future.successful(skTestNs)

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
      mockSkuberFactory.initializeKube(meq(testProvider.id), meq("default")         )(any) returns Future.successful(mockSkuber)
      mockSkuber.get[skuber.Namespace]("default") returns Future.successful(skDefaultNs)
      mockSkuberFactory.initializeKube(meq(testProvider.id), meq(testEnv.id.toString))(any) returns Future.successful(mockSkuber)
      mockSkuber.get[skuber.Namespace](testEnv.id.toString) returns Future.successful(skTestNs)

      mockSkuber.getOption(meq(metaContainer.name))(any,meq(skuber.ext.deploymentKind)) returns Future.successful(Some(mock[skuber.ext.Deployment]))
      mockSkuber.update(any)(any,meq(skuber.ext.deploymentKind)) returns Future.successful(mock[skuber.ext.Deployment])
      mockSkuber.list()(any,meq(client.persistentVolumeClaimListKind)) returns Future.successful(skuber.PersistentVolumeClaimList(items = Nil))

      val ks = new KubernetesService(mockSkuberFactory)
      TestSetup(ks, mockSkuber, mockSkuberFactory, skTestNs, Some(metaContainer))
    }
  }

  abstract class FakeKubeCreate( force_pull: Boolean = true,
                                 args: Option[Seq[String]] = None,
                                 cmd: Option[String] = None,
                                 port_mappings: Seq[ContainerSpec.PortMapping] = Seq.empty,
                                 labels: Map[String,String] = Map.empty ) extends Scope {

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
      name = "",
      container_type = "DOCKER",
      image = "nginx",
      provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
      port_mappings = port_mappings,
      cpus = 1.0,
      memory = 128,
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

    lazy val lbls = Map(KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString)

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
    ).addLabels(lbls)

    lazy val mockDepl = skuber.ext.Deployment(
      metadata = skuber.ObjectMeta(
        name = metaContainer.name,
        namespace = testEnv.id.toString,
        labels = lbls,
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
        labels = lbls
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
        labels = lbls
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
      mockSkuber.get[skuber.Namespace]("default") returns Future.successful(skDefaultNs)
      mockSkuberFactory.initializeKube(meq(testProvider.id), meq(testEnv.id.toString))(any) returns Future.successful(mockSkuber)
      mockSkuber.get[skuber.Namespace](testEnv.id.toString) returns Future.successful(skTestNs)

      mockSkuber.list()(any,meq(client.persistentVolumeClaimListKind)) returns Future.successful(skuber.PersistentVolumeClaimList(items = Nil))
      mockSkuber.list()(any,meq(skuber.ext.deplListKind)) returns Future.successful(skuber.ext.DeploymentList(items = List(mockDepl)))
      mockSkuber.list()(any,meq(client.serviceListKind)) returns Future.successful(skuber.ServiceList(items = List(mockService)))
      mockSkuber.list()(any,meq(client.podListKind)) returns Future.successful(skuber.PodList(items = List(mockPodA,mockPodB)))

      mockSkuber.create(any)(any,meq(skuber.ext.deploymentKind)) returns Future.successful(mock[skuber.ext.Deployment])
      mockSkuber.create(any)(any,meq(client.serviceKind)) returns Future.successful(mockService)

      mockSkuber.getOption(meq(metaContainer.name))(any,meq(client.serviceKind)) returns Future.successful(Some(mockService))

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

      there was two(testSetup.skuberFactory).initializeKube(meq(testProvider.id), any)(any)
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
        ) and
        hasPair(KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString) and
        hasSelector(KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString) and
        (((_: skuber.Service).name) ^^ be_==(metaContainer.name))

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
    }

    "provision with the expected external_id property" in new FakeKubeCreate() {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      updatedContainerProps must havePair(
        "external_id" -> s"/namespaces/${testEnv.id}/deployments/test-container"
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
    }

    "provision with the requested labels" in new FakeKubeCreate(labels = Map(
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
          "labelb" -> "value b"
        )
      ))(any,meq(skuber.ext.deploymentKind))
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
    }

    "pass no args when unspecified" in new FakeKubeCreate(args = None) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.args)) ^^ beSome(empty)
      ))(any,meq(skuber.ext.deploymentKind))
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
    }.pendingUntilFixed("this is going to be hard")

    "pass no cmd when unspecified" in new FakeKubeCreate(cmd = None) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.command)) ^^ beSome(empty)
      ))(any,meq(skuber.ext.deploymentKind))
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
        hasPair(KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString)
          and
        ((_:Ingress).spec.map(_.rules).getOrElse(Nil)) ^^ containTheSameElementsAs(Seq(
          Rule("www.galacticfog.com",    HttpRule(List(Path("", Backend(createdService.name, 80))))),
          Rule("galacticfog.com",        HttpRule(List(Path("", Backend(createdService.name, 80))))),
          Rule("secure.galacticfog.com", HttpRule(List(Path("", Backend(createdService.name, 8443)))))
        ))
      ))(any,meq(skuber.ext.ingressKind))
    }

    "set appropriate host port on container tasks from kubernetes Service node port" in new FakeKubeCreate {
      import com.galacticfog.gestalt.marathon.ContainerStats

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
            name = Some("remove_service"),
            expose_endpoint = Some(true),
            virtual_hosts = Some(Seq("port80.test.com"))
          ),
          ContainerSpec.PortMapping(
            protocol = "tcp",
            container_port = Some(81),
            name = Some("add_service"),
            expose_endpoint = Some(false)
          ),
          ContainerSpec.PortMapping(
            protocol = "tcp",
            container_port = Some(443),
            service_port = Some(8443),
            name = Some("remove_port"),
            expose_endpoint = Some(true),
            virtual_hosts = Some(Seq("port8443.test.com"))
          )
        )) {

      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(skuber.ext.ingressKind)) returns Future.successful(Some(mock[skuber.ext.Ingress]))
      testSetup.kubeClient.update(any)(any,meq(skuber.ext.ingressKind)) returns Future.successful(mock[skuber.ext.Ingress])
      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(client.serviceKind)) returns Future.successful(None)
      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(client.serviceKind)) returns Future.successful(Some(mock[skuber.Service]))
      testSetup.kubeClient.update(any)(any,meq(client.serviceKind)) returns Future.successful(mock[skuber.Service])

      val newPortMappings = Seq(
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(80),
          name = Some("remove_service"),
          expose_endpoint = Some(false)
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(81),
          name = Some("add_service"),
          expose_endpoint = Some(true),
          virtual_hosts = Some(Seq("port81.test.com"))
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(444),
          service_port = Some(8444),
          name = Some("add_port"),
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
    }

    "delete empty ingress on container PUT" in new FakeKubeWithPrexistingContainer(Seq(
          ContainerSpec.PortMapping(
            protocol = "tcp",
            container_port = Some(80),
            name = Some("remove_service"),
            expose_endpoint = Some(true),
            virtual_hosts = Some(Seq("port80.test.com"))
          ),
          ContainerSpec.PortMapping(
            protocol = "tcp",
            container_port = Some(81),
            name = Some("no_service"),
            expose_endpoint = Some(false)
          )
        )) {

      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(skuber.ext.ingressKind)) returns Future.successful(Some(mock[skuber.ext.Ingress]))
      testSetup.kubeClient.delete(metaContainer.name, 0)(skuber.ext.ingressKind) returns Future.successful(())
      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(client.serviceKind)) returns Future.successful(None)
      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(client.serviceKind)) returns Future.successful(Some(mock[skuber.Service]))
      testSetup.kubeClient.update(any)(any,meq(client.serviceKind)) returns Future.successful(mock[skuber.Service])

      val newPortMappings = Seq(
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(80),
          name = Some("removed_service"),
          expose_endpoint = Some(false)
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(81),
          name = Some("no_service"),
          expose_endpoint = Some(false)
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(8881),
          name = Some("new_service"),
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
    }

    "delete empty service on container PUT" in new FakeKubeWithPrexistingContainer(Seq(
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(80),
          name = Some("remove_service"),
          expose_endpoint = Some(true),
          virtual_hosts = Some(Seq("port80.test.com"))
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(81),
          name = Some("no_service"),
          expose_endpoint = Some(false)
        )
      )) {

      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(skuber.ext.ingressKind)) returns Future.successful(None)
      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(client.serviceKind)) returns Future.successful(None)
      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(client.serviceKind)) returns Future.successful(Some(mock[skuber.Service]))
      testSetup.kubeClient.delete(meq(metaContainer.name), any)(meq(client.serviceKind)) returns Future.successful(())

      val newPortMappings = Seq(
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(80),
          name = Some("removed_service"),
          expose_endpoint = Some(false)
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(81),
          name = Some("no_service"),
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
    }

    "create service and ingress on container update" in new FakeKubeWithPrexistingContainer(Seq(
      ContainerSpec.PortMapping(
        protocol = "tcp",
        container_port = Some(80),
        name = Some("update_port"),
        expose_endpoint = Some(false),
        virtual_hosts = None
      )
    )) {

      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(skuber.ext.ingressKind)) returns Future.successful(None)
      testSetup.kubeClient.create(any)(any,meq(skuber.ext.ingressKind)) returns Future.successful(mock[skuber.ext.Ingress])
      testSetup.kubeClient.getOption(any)(any,meq(client.serviceKind)) returns Future.successful(None)
      testSetup.kubeClient.create(any)(any,meq(client.serviceKind)) returns Future.successful(mock[skuber.Service])

      val newPortMappings = Seq(
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(80),
          name = Some("update_port"),
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
    }

  }

}
