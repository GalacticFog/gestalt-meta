package services

import java.time.{ZoneOffset, ZonedDateTime}
import java.util.Base64
import scala.collection.JavaConversions._

import scala.concurrent.Future
import scala.util.Success
import scala.language.implicitConversions

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.ContainerSpec.{SecretDirMount, SecretEnvMount, SecretFileMount}
import com.galacticfog.gestalt.meta.api.{ContainerSpec, SecretSpec}
import com.galacticfog.gestalt.meta.api.errors.{BadRequestException, UnprocessableEntityException}
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
import play.api.libs.json.Json.JsValueWrapper
import skuber.{PersistentVolumeClaim, Pod, Secret, Service}
import skuber.api.client
import skuber.ext.{Deployment, Ingress, ReplicaSet}
import skuber.json.format._
import ContainerSpec.HealthCheck._

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

  def mockSvc(resourceVersion: String = "", clusterIP: String = "", name: String = "test-container"): skuber.Service = {
    skuber.Service(
      name = name
    ).withClusterIP(clusterIP)
      .withResourceVersion(resourceVersion)
  }

  // A scope for testing with a mocked skuber client and the necessary namespaces for a test environment
  abstract class FakeKube() extends Scope {
    lazy val testAuthResponse = GestaltSecurityMocking.dummyAuthResponseWithCreds()
    lazy val testCreds = testAuthResponse.creds
    lazy val user = AuthAccountWithCreds(testAuthResponse.account, Seq.empty, Seq.empty, testCreds, dummyRootOrgId)

    lazy val (testWork, testEnv) = {
      val (tw, te) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment").get
      Entitlements.setNewResourceEntitlements(dummyRootOrgId, te.id, user, Some(tw.id))
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
      mockSkuber.getOption(meq("default"))(any,meq(skuber.Namespace.namespaceDef)) returns Future.successful(Some(skDefaultNs))
      mockSkuberFactory.initializeKube(meq(testProvider.id), meq(testEnv.id.toString))(any) returns Future.successful(mockSkuber)
      mockSkuber.getOption(meq(testEnv.id.toString))(any,meq(skuber.Namespace.namespaceDef)) returns Future.successful(Some(skTestNs))

      val ks = new KubernetesService(mockSkuberFactory)
      TestSetup(ks, mockSkuber, mockSkuberFactory, skTestNs, None)
    }
  }

  // A scope for testing with a mocked skuber client and a ton of mocking around depl, svc, etc. based on the input arguments
  abstract class FakeKubeCreate( force_pull: Boolean = true,
                                 cpu: Double = 1.0,
                                 memory: Double = 128,
                                 args: Option[Seq[String]] = None,
                                 cmd: Option[String] = None,
                                 port_mappings: Seq[ContainerSpec.PortMapping] = Seq.empty,
                                 labels: Map[String,String] = Map.empty,
                                 providerConfig: Seq[(String,JsValueWrapper)] = Seq.empty,
                                 secrets: Seq[ContainerSpec.SecretMount] = Seq.empty,
                                 volumes: Seq[ContainerSpec.Volume] = Seq.empty,
                                 health_checks: Seq[ContainerSpec.HealthCheck] = Seq.empty
                               ) extends Scope {

    lazy val testAuthResponse = GestaltSecurityMocking.dummyAuthResponseWithCreds()
    lazy val testCreds = testAuthResponse.creds
    lazy val user = AuthAccountWithCreds(testAuthResponse.account, Seq.empty, Seq.empty, testCreds, dummyRootOrgId)

    lazy val (testWork, testEnv) = {
      val (tw, te) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment").get
      Entitlements.setNewResourceEntitlements(dummyRootOrgId, te.id, user, Some(tw.id))
      (tw,te)
    }

    lazy val testProvider = createKubernetesProvider(testEnv.id, "test-provider", providerConfig).get

    lazy val metaSecretItems = Seq(
      SecretSpec.Item("part-a", Some("dmFsdWUtYQ==")),   // "value-a"
      SecretSpec.Item("part-b", Some("dmFsdWUtYg=="))    // "value-b"
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

    val secretsWithId = secrets.collect {
      case env: SecretEnvMount  => env.copy(secret_id = metaSecret.id)
      case fil: SecretFileMount => fil.copy(secret_id = metaSecret.id)
      case dir: SecretDirMount  => dir.copy(secret_id = metaSecret.id)
    }

    lazy val initProps = ContainerSpec(
      name = "test-container",
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
      env = Map(),
      user = None,
      secrets = secretsWithId
    )

    lazy val origExtId = s"/namespaces/${testEnv.id}/deployments/${initProps.name}"

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
        "labels" -> Json.toJson(labels).toString,
        "volumes" -> Json.toJson(initProps.volumes).toString,
        "secrets" -> Json.toJson(initProps.secrets).toString,
        "health_checks" -> Json.toJson(initProps.health_checks).toString,
        "external_id" -> origExtId
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

    import skuber.portNumToNameablePort
    lazy val baseNodePort = 31000
    lazy val assignedNodePortsNP = port_mappings.filter(pm => pm.expose_endpoint.contains(true) && pm.`type`.exists(Set("external","loadBalancer").contains)).zipWithIndex.map({
      case (pm,i) => (pm.name.getOrElse("") , pm.service_port.filter(_ != 0).getOrElse(31000+i))
    }) toMap
    lazy val assignedNodePortsLB = port_mappings.filter(pm => pm.expose_endpoint.contains(true) && pm.`type`.contains("loadBalancer")).zipWithIndex.map({
      case (pm,i) => (pm.name.getOrElse("") , pm.service_port.filter(_ != 0).getOrElse(32000+i))
    }) toMap
    lazy val mockService = skuber.Service(
      name = metaContainer.name,
      spec = skuber.Service.Spec(
        clusterIP = "10.0.161.84",
        ports = port_mappings.filter(_.expose_endpoint.contains(true)).map(
          pm => skuber.Service.Port(
            name = pm.name.getOrElse(""),
            protocol = skuber.Protocol.TCP,
            port = pm.lb_port.filter(_ != 0).getOrElse(pm.container_port.get),
            targetPort = Some(pm.container_port.get),
            nodePort = 0
          )
        ).toList,
        _type = skuber.Service.Type.ClusterIP
      )
    ).addLabels(containerLbls)
    lazy val mockService2 = skuber.Service(
      name = metaContainer.name+"-ext",
      spec = skuber.Service.Spec(
        clusterIP = "10.0.161.84",
        ports = port_mappings.filter(pm => pm.expose_endpoint.contains(true) && pm.`type`.exists(Set("external","loadBalancer").contains)).map(
          pm => skuber.Service.Port(
            name = pm.name.getOrElse(""),
            protocol = skuber.Protocol.TCP,
            port = pm.lb_port.filter(_ != 0).getOrElse(pm.container_port.get),
            targetPort = Some(pm.container_port.get),
            nodePort = assignedNodePortsNP(pm.name.getOrElse(""))
          )
        ).toList,
        _type = skuber.Service.Type.NodePort
      )
    ).addLabels(containerLbls)
    lazy val mockService3 = skuber.Service(
      name = metaContainer.name+"-lb",
      spec = skuber.Service.Spec(
        clusterIP = "10.0.161.85",
        ports = port_mappings.filter(pm => pm.expose_endpoint.contains(true) && pm.`type`.contains("loadBalancer")).map(
          pm => skuber.Service.Port(
            name = pm.name.getOrElse(""),
            protocol = skuber.Protocol.TCP,
            port = pm.lb_port.filter(_ != 0).getOrElse(pm.container_port.get),
            targetPort = Some(pm.container_port.get),
            nodePort = assignedNodePortsLB(pm.name.getOrElse(""))
          )
        ).toList,
        _type = skuber.Service.Type.LoadBalancer
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
          ports = port_mappings.zipWithIndex.map({
            case (pm,i) => skuber.Container.Port(pm.container_port.get, skuber.Protocol.TCP, pm.name.getOrElse(""), s"10.100.0.$i", pm.host_port)
          }).toList
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
      mockSkuber.getOption(meq("default"))(any,meq(skuber.Namespace.namespaceDef)) returns Future.successful(Some(skDefaultNs))
      mockSkuberFactory.initializeKube(meq(testProvider.id), meq(testEnv.id.toString))(any) returns Future.successful(mockSkuber)
      mockSkuber.getOption(meq(testEnv.id.toString))(any,meq(skuber.Namespace.namespaceDef)) returns Future.successful(Some(skTestNs))

      mockSkuber.list()(any,meq(skuber.PersistentVolumeClaim.pvcListDef)) returns Future.successful(new skuber.PersistentVolumeClaimList("","",None,Nil))
      mockSkuber.list()(any,meq(skuber.ext.Deployment.deployListDef)) returns Future.successful(new skuber.ext.DeploymentList("","",None,List(mockDepl)))
      mockSkuber.list(any)(any,meq(skuber.ext.Deployment.deployListDef)) returns Future.successful(new skuber.ext.DeploymentList("","",None,List(mockDepl)))

      def answerId[T](a: Any): Future[T] = Future.successful(a.asInstanceOf[Array[Object]](0).asInstanceOf[T])

      mockSkuber.update(any)(any,meq(Deployment.deployDef)) answers {(x: Any) => answerId[skuber.ext.Deployment](x)}
      mockSkuber.update(any)(any,meq(Ingress.ingDef)) answers {(x: Any) => answerId[skuber.ext.Ingress](x)}
      mockSkuber.update(any)(any,meq(Service.svcDef)) answers {(x: Any) => answerId[skuber.Service](x)}

      val nonEmptySvcs = List( mockService, mockService2, mockService3 ).filter(_.spec.map(_.ports.nonEmpty).contains(true))
      mockSkuber.list()(any,meq(skuber.Service.svcListDef)) returns Future.successful(new skuber.ServiceList("","",None,nonEmptySvcs))
      mockSkuber.list(any)(any,meq(skuber.Service.svcListDef)) returns Future.successful(new skuber.ServiceList("","",None,nonEmptySvcs))
      mockSkuber.list()(any,meq(skuber.Pod.poListDef)) returns Future.successful(new skuber.PodList("","",None,List(mockPodA,mockPodB)))
      mockSkuber.list(any)(any,meq(skuber.Pod.poListDef)) returns Future.successful(new skuber.PodList("","",None,List(mockPodA,mockPodB)))
      mockSkuber.list()(any,meq(skuber.Secret.secListDef)) returns Future.successful(new skuber.SecretList("","",None,List(mockSecret)))
      mockSkuber.list(any)(any,meq(skuber.Secret.secListDef)) returns Future.successful(new skuber.SecretList("","",None,List(mockSecret)))

      mockSkuber.create(any)(any,meq(skuber.ext.Deployment.deployDef)) returns Future.successful(mock[skuber.ext.Deployment])

      mockSkuber.create(argThat( (_:Service).name == metaContainer.name        ))(any,meq(skuber.Service.svcDef)) returns Future.successful(mockService)
      mockSkuber.create(argThat( (_:Service).name == metaContainer.name+"-ext" ))(any,meq(skuber.Service.svcDef)) returns Future.successful(mockService2)
      mockSkuber.create(argThat( (_:Service).name == metaContainer.name+"-lb"  ))(any,meq(skuber.Service.svcDef)) returns Future.successful(mockService3)
      mockSkuber.delete(meq(metaContainer.name       ), any)(meq(skuber.Service.svcDef)) returns Future.successful(())
      mockSkuber.delete(meq(metaContainer.name+"-ext"), any)(meq(skuber.Service.svcDef)) returns Future.successful(())
      mockSkuber.delete(meq(metaContainer.name+"-lb" ), any)(meq(skuber.Service.svcDef)) returns Future.successful(())

      mockSkuber.create(any)(any,meq(skuber.Secret.secDef)) returns Future.successful(mockSecret)

      mockSkuber.create(argThat( (_:Ingress).name == metaContainer.name))(any,meq(Ingress.ingDef)) answers {(x: Any) => answerId[skuber.ext.Ingress](x)}

      mockSkuber.getOption(meq(metaSecret.name))(any,meq(skuber.Secret.secDef)) returns Future.successful(Some(mockSecret))

      val ks = new KubernetesService(mockSkuberFactory)
      TestSetup(ks, mockSkuber, mockSkuberFactory, skTestNs, Some(metaContainer))
    }
  }

  def hasExactlyContainerPorts(ps: skuber.Container.Port*) = ((_: skuber.ext.Deployment).getPodSpec.map(_.containers.flatMap(_.ports)).getOrElse(List())) ^^ containTheSameElementsAs(Seq(ps:_*))

  def hasServiceType(tp: Service.Type.Value) = ((_: skuber.Service).spec.map(_._type) must beSome(tp))

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
      testSetup.kubeClient.create(any)(any,meq(Deployment.deployDef)) returns Future.successful(mock[skuber.ext.Deployment])
      testSetup.kubeClient.list()(any,meq(PersistentVolumeClaim.pvcListDef)) returns Future.successful(new skuber.PersistentVolumeClaimList("","",None,Nil))

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
      ))(any,meq(Deployment.deployDef))
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
      testSetup.kubeClient.create(any)(any,meq(Deployment.deployDef)) returns Future.successful(mock[skuber.ext.Deployment])
      testSetup.kubeClient.list()(any,meq(PersistentVolumeClaim.pvcListDef)) returns Future.successful(new skuber.PersistentVolumeClaimList("","",None,Nil))

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
      ))(any,meq(Deployment.deployDef))
      there were two(testSetup.kubeClient).close
    }

    "deploy services for exposed port mappings and set service addresses and host port" in new FakeKubeCreate(
      port_mappings = Seq(
        // automatically assigned lb_port, default type == "internal"
        ContainerSpec.PortMapping( protocol = "tcp", container_port = Some(80),                          expose_endpoint = Some(true), name = Some("http"),   `type` = None),
        // automatically assigned lb_port, test type == external, user-specified nodePort/service_port
        ContainerSpec.PortMapping( protocol = "tcp", container_port = Some(443),   lb_port = Some(0),    expose_endpoint = Some(true), name = Some("https"),  `type` = Some("external"),     service_port = Some(32000)),
        // user-specified lb_port for type == "internal", ignore and nerf the nodePort/service_port
        ContainerSpec.PortMapping( protocol = "tcp", container_port = Some(444),   lb_port = Some(8444), expose_endpoint = Some(true), name = Some("https2"), `type` = Some("internal"),    service_port = Some(32001)),
        // test type ==  "loadBalancer", test that assigned nodePort is set on service_port
        ContainerSpec.PortMapping( protocol = "tcp", container_port = Some(445),   lb_port = Some(8445), expose_endpoint = Some(true), name = Some("https3"), `type` = Some("loadBalancer"), service_port = None),
        // non-exposed port, with host_port and "udp". test that lb_port is nerfed.
        ContainerSpec.PortMapping( protocol = "udp", container_port = Some(10000), lb_port = Some(0),    expose_endpoint = Some(false), name = Some("debug"), `type` = Some("internal"),    host_port = Some(10000))
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
          skuber.Container.Port(80,    skuber.Protocol.TCP, "http"),
          skuber.Container.Port(443,   skuber.Protocol.TCP, "https"),
          skuber.Container.Port(444,   skuber.Protocol.TCP, "https2"),
          skuber.Container.Port(445,   skuber.Protocol.TCP, "https3"),
          skuber.Container.Port(10000, skuber.Protocol.UDP, "debug", hostPort = Some(10000))
        )
      ))(any,meq(skuber.ext.Deployment.deployDef))

      val haveExpectedLabels = ((_:skuber.ObjectResource).metadata.labels) ^^ havePairs(
        KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString,
        KubernetesService.META_ENVIRONMENT_KEY -> testEnv.id.toString,
        KubernetesService.META_WORKSPACE_KEY -> testWork.id.toString,
        KubernetesService.META_FQON_KEY -> "root",
        KubernetesService.META_PROVIDER_KEY -> testProvider.id.toString
      )

      val serviceCaptor = ArgumentCaptor.forClass(classOf[skuber.Service])
      there were three(testSetup.kubeClient).create(serviceCaptor.capture())(any,meq(Service.svcDef))
      val createdServices = serviceCaptor.getAllValues.toSeq
      createdServices.size must_== 3
      createdServices must contain(eachOf(
        inNamespace(testSetup.testNS.name) and haveName(metaContainer.name) and
          hasServiceType(Service.Type.ClusterIP) and
          hasExactlyServicePorts(
            skuber.Service.Port("http",    skuber.Protocol.TCP,    80, Some(Left(80)),  0),
            skuber.Service.Port("https",   skuber.Protocol.TCP,   443, Some(Left(443)), 0),
            skuber.Service.Port("https2",  skuber.Protocol.TCP,  8444, Some(Left(444)), 0),
            skuber.Service.Port("https3",  skuber.Protocol.TCP,  8445, Some(Left(445)), 0)
          ) and
          hasSelector(KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString) and haveExpectedLabels,
        //
        inNamespace(testSetup.testNS.name) and haveName(metaContainer.name + "-ext") and
          hasServiceType(Service.Type.NodePort) and
          hasExactlyServicePorts(
            skuber.Service.Port("https",  skuber.Protocol.TCP,  443, Some(Left(443)), 32000),
            skuber.Service.Port("https3", skuber.Protocol.TCP, 8445, Some(Left(445)), 0)
          ) and
          hasSelector(KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString),
        //
        inNamespace(testSetup.testNS.name) and haveName(metaContainer.name + "-lb") and
          hasServiceType(Service.Type.LoadBalancer) and
          hasExactlyServicePorts(
            skuber.Service.Port("https3",   skuber.Protocol.TCP, 8445, Some(Left(445)), 0)
          )  and
          hasSelector(KubernetesService.META_CONTAINER_KEY -> metaContainer.id.toString)
      ))

      import ContainerSpec.PortMapping
      import ContainerSpec.ServiceAddress

      val svcHost = s"${metaContainer.name}.${testEnv.id}.svc.cluster.local"
      updatedContainerProps.get("status") must beSome("LAUNCHED")
      val mappings = Json.parse(updatedContainerProps("port_mappings")).as[Seq[ContainerSpec.PortMapping]]
      mappings must containTheSameElementsAs(Seq(
        PortMapping("tcp", name = Some("http"),   container_port = Some(80),    lb_port = Some(80),   host_port = None,        service_port = None,                                expose_endpoint = Some(true),  `type` = Some("internal"),    service_address = Some(ServiceAddress(svcHost,   80, Some("tcp"), None))),
        PortMapping("tcp", name = Some("https"),  container_port = Some(443),   lb_port = Some(443),  host_port = None,        service_port = Some(32000),                         expose_endpoint = Some(true),  `type` = Some("external"),     service_address = Some(ServiceAddress(svcHost,  443, Some("tcp"), None))),
        PortMapping("tcp", name = Some("https2"), container_port = Some(444),   lb_port = Some(8444), host_port = None,        service_port = None,                                expose_endpoint = Some(true),  `type` = Some("internal"),    service_address = Some(ServiceAddress(svcHost, 8444, Some("tcp"), None))),
        PortMapping("tcp", name = Some("https3"), container_port = Some(445),   lb_port = Some(8445), host_port = None,        service_port = Some(assignedNodePortsNP("https3")), expose_endpoint = Some(true),  `type` = Some("loadBalancer"), service_address = Some(ServiceAddress(svcHost, 8445, Some("tcp"), None))),
        PortMapping("udp", name = Some("debug"),  container_port = Some(10000), lb_port = None,       host_port = Some(10000), service_port = None,                                expose_endpoint = Some(false), `type` = None,                 service_address = None)
      ))
      there were two(testSetup.kubeClient).close
    }

    "throw exception if there are multiple health checks" in new FakeKubeCreate(
      health_checks = Seq(
        ContainerSpec.HealthCheck(TCP,     path = None,                      command = None, 32, 17, 7, 3, None, Some(8888)),
        ContainerSpec.HealthCheck(HTTP,    path = Some("/youGood"),          command = None, 30, 15, 5, 1, None, Some(8080))
      )
    ) {
      await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )) must throwAn[UnprocessableEntityException]("Kubernetes supports at most one health check/liveness probe")
    }

    "create containers with HTTP health checks" in new FakeKubeCreate(
      port_mappings = Seq(
        ContainerSpec.PortMapping("tcp", Some(8080), None, None, Some("service"))
      ),
      health_checks = Seq(
        ContainerSpec.HealthCheck(HTTP,    path = Some("/youGood"),          command = None, 30, 15, 5, 1, None, Some(8080))
      )
    ) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
          (((_:skuber.ext.Deployment).getPodSpec.get.containers.head.livenessProbe) ^^ beSome(
            skuber.Probe(skuber.HTTPGetAction(Left(8080), "", "/youGood", "HTTP"), 30, 5)
          ))
      ))(any,meq(Deployment.deployDef))
    }

    "create containers with HTTPS health checks" in new FakeKubeCreate(
      port_mappings = Seq(
        ContainerSpec.PortMapping("tcp", Some(8443), None, None, Some("service"))
      ),
      health_checks = Seq(
        ContainerSpec.HealthCheck(HTTPS,   path = Some("/youGoodAndSecure"), command = None, 31, 16, 6, 2, None, Some(8443))
      )
    ) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
          (((_:skuber.ext.Deployment).getPodSpec.get.containers.head.livenessProbe) ^^ beSome(
            skuber.Probe(skuber.HTTPGetAction(Left(8443), "", "/youGoodAndSecure", "HTTPS"), 31, 6)
          ))
      ))(any,meq(Deployment.deployDef))
    }

    "create containers with TCP health checks" in new FakeKubeCreate(
      port_mappings = Seq(
        ContainerSpec.PortMapping("tcp", Some(8888), None, None, Some("service"))
      ),
      health_checks = Seq(
        ContainerSpec.HealthCheck(TCP,     path = None,                      command = None, 32, 17, 7, 3, None, Some(8888))
      )
    ) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
          (((_:skuber.ext.Deployment).getPodSpec.get.containers.head.livenessProbe) ^^ beSome(
            skuber.Probe(skuber.TCPSocketAction(Left(8888)), 32, 7)
          ))
      ))(any,meq(Deployment.deployDef))
    }

    "create containers with COMMAND health checks" in new FakeKubeCreate(
      health_checks = Seq(
        ContainerSpec.HealthCheck(COMMAND, path = None,                      command = Some("curl localhost:8888"), 33, 18, 8, 4)
      )
    ) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
          (((_:skuber.ext.Deployment).getPodSpec.get.containers.head.livenessProbe) ^^ beSome(
            skuber.Probe(skuber.ExecAction(List("/bin/sh", "curl", "localhost:8888")), 33, 8)
          ))
      ))(any,meq(Deployment.deployDef))
    }

    "create containers with HTTP health checks (port_index)" in new FakeKubeCreate(
      port_mappings = Seq(
        ContainerSpec.PortMapping("tcp", Some(9999), None, None, Some("debug")),
        ContainerSpec.PortMapping("tcp", Some(8080), None, None, Some("web"))
      ),
      health_checks = Seq(
        ContainerSpec.HealthCheck(HTTP,    path = Some("/youGood"),          command = None, 30, 15, 5, 1, Some(1), None)
      )
    ) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
          (((_:skuber.ext.Deployment).getPodSpec.get.containers.head.livenessProbe) ^^ beSome(
            skuber.Probe(skuber.HTTPGetAction(Left(8080), "", "/youGood", "HTTP"), 30, 5)
          ))
      ))(any,meq(Deployment.deployDef))
    }

    "create containers with HTTPS health checks (port_index)" in new FakeKubeCreate(
      port_mappings = Seq(
        ContainerSpec.PortMapping("tcp", Some(9999), None, None, Some("debug")),
        ContainerSpec.PortMapping("tcp", Some(8443), None, None, Some("secureweb"))
      ),
      health_checks = Seq(
        ContainerSpec.HealthCheck(HTTPS,   path = Some("/youGoodAndSecure"), command = None, 31, 16, 6, 2, Some(1), None)
      )
    ) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
          (((_:skuber.ext.Deployment).getPodSpec.get.containers.head.livenessProbe) ^^ beSome(
            skuber.Probe(skuber.HTTPGetAction(Left(8443), "", "/youGoodAndSecure", "HTTPS"), 31, 6)
          ))
      ))(any,meq(Deployment.deployDef))
    }

    "throw exception for invalid port_index in health checks" in new FakeKubeCreate(
      port_mappings = Seq(
        ContainerSpec.PortMapping("tcp", Some(8888), None, None, Some("svc"))
      ),
      health_checks = Seq(
        ContainerSpec.HealthCheck(TCP,     path = None,                      command = None, 32, 17, 7, 3, Some(1), None)
      )
    ) {
      await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )) must throwA[UnprocessableEntityException]("index '.*' was out of bounds")
    }

    "create containers with TCP health checks (port_index)" in new FakeKubeCreate(
      port_mappings = Seq(
        ContainerSpec.PortMapping("tcp", Some(9999), None, None, Some("debug")),
        ContainerSpec.PortMapping("tcp", Some(8888), None, None, Some("svc"))
      ),
      health_checks = Seq(
        ContainerSpec.HealthCheck(TCP,     path = None,                      command = None, 32, 17, 7, 3, Some(1), None)
      )
    ) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
          (((_:skuber.ext.Deployment).getPodSpec.get.containers.head.livenessProbe) ^^ beSome(
            skuber.Probe(skuber.TCPSocketAction(Left(8888)), 32, 7)
          ))
      ))(any,meq(Deployment.deployDef))
    }

    "422 on non-white-listed hostPath volumes" in new FakeKubeCreate(
      volumes= Seq(
        ContainerSpec.Volume("/mnt/someContainerPath1", Some("/mnt/not-allowed/someHostPath1"), None, Some("RO"), Some("my-volume"))
      )
    ) {
      testSetup.kubeClient.list()(any,meq(PersistentVolumeClaim.pvcListDef)) returns Future.successful(new skuber.PersistentVolumeClaimList("","",None,Nil))
      testSetup.kubeClient.create(any)(any,meq(PersistentVolumeClaim.pvcDef)) returns Future.successful(mock[skuber.PersistentVolumeClaim])

      await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )) must throwAn[UnprocessableEntityException]("host_path is not in provider's white-list")

      there were no(testSetup.kubeClient).create(any)(any,meq(Deployment.deployDef))
    }

    "create and mount white-listed hostPath volumes" in new FakeKubeCreate(
      volumes= Seq(
        ContainerSpec.Volume("/mnt/someContainerPath1", Some("/mnt/for_containers/someHostPath"), None, Some("RO"), Some("my-volume-1")),
        ContainerSpec.Volume("/mnt/someContainerPath2", Some("/mnt/also_for_containers/wow/someReallyDeepHostPath"), None, Some("RW"), Some("my-volume-2"))
      ),
      providerConfig = Seq(
        "host_volume_whitelist" -> Json.arr("/mnt/for_containers", "/mnt/also_for_containers")
      )
    ) {
      testSetup.kubeClient.list()(any,meq(PersistentVolumeClaim.pvcListDef)) returns Future.successful(new skuber.PersistentVolumeClaimList("","",None,Nil))
      testSetup.kubeClient.create(any)(any,meq(PersistentVolumeClaim.pvcDef)) returns Future.successful(mock[skuber.PersistentVolumeClaim])

      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties

      val deploymentCaptor = ArgumentCaptor.forClass(classOf[skuber.ext.Deployment])
      there was one(testSetup.kubeClient).create(deploymentCaptor.capture())(any,meq(Deployment.deployDef))
      val createdDepl = deploymentCaptor.getValue

      createdDepl.getPodSpec.get.volumes must containTheSameElementsAs(Seq(
        skuber.Volume("my-volume-1", skuber.Volume.HostPath("/mnt/for_containers/someHostPath")),
        skuber.Volume("my-volume-2", skuber.Volume.HostPath("/mnt/also_for_containers/wow/someReallyDeepHostPath"))
      ))

      createdDepl.getPodSpec.get.containers.head.volumeMounts must containTheSameElementsAs(Seq(
        skuber.Volume.Mount(
          "my-volume-1",
          mountPath = "/mnt/someContainerPath1",
          readOnly = true
        ),
        skuber.Volume.Mount(
          "my-volume-2",
          mountPath = "/mnt/someContainerPath2",
          readOnly = false
        )
      ))
    }

    "create and mount persistent volume claims when mounting into container" in new FakeKubeCreate(
      volumes = Seq(
        ContainerSpec.Volume("/mnt/path1", None, Some(ContainerSpec.Volume.PersistentVolumeInfo(100)), Some("ReadOnlyMany"), Some("my-volume-1"))
      )
    ) {
      testSetup.kubeClient.list()(any,meq(PersistentVolumeClaim.pvcListDef)) returns Future.successful(new skuber.PersistentVolumeClaimList("","",None,Nil))
      testSetup.kubeClient.create(any)(any,meq(PersistentVolumeClaim.pvcDef)) returns Future.successful(mock[skuber.PersistentVolumeClaim])

      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties

      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
          (((_:skuber.ext.Deployment).spec.get.template.get.spec.get.volumes) ^^ containTheSameElementsAs(Seq(
            skuber.Volume("my-volume-1", skuber.Volume.PersistentVolumeClaimRef("my-volume-1", true))
          )))
      ))(any,meq(Deployment.deployDef))

      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
          (((_:skuber.PersistentVolumeClaim).name) ^^ beEqualTo("my-volume-1"))
      ))(any,meq(PersistentVolumeClaim.pvcDef))
    }

    "fail to create container if persistent volume claim creation fails" in new FakeKubeCreate(
      volumes = Seq(
        ContainerSpec.Volume("/mnt/path1", None, Some(ContainerSpec.Volume.PersistentVolumeInfo(100)), Some("ReadOnlyMany"), Some("my-volume-1"))
      )
    ) {
      testSetup.kubeClient.list()(any,meq(PersistentVolumeClaim.pvcListDef)) returns Future.successful(new skuber.PersistentVolumeClaimList("","",None,Nil))
      testSetup.kubeClient.create(any)(any,meq(PersistentVolumeClaim.pvcDef)) returns Future.failed(new skuber.K8SException(mock[client.Status]))

      await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )) must throwAn[UnprocessableEntityException]("Failed creating PVC for volume")

      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
          (((_:skuber.PersistentVolumeClaim).name) ^^ beEqualTo("my-volume-1"))
      ))(any,meq(PersistentVolumeClaim.pvcDef))
    }

    "using existing persistent volume claims when mounting into container" in new FakeKubeCreate(
      volumes = Seq(
        ContainerSpec.Volume("/mnt/path1", None, Some(ContainerSpec.Volume.PersistentVolumeInfo(100)), Some("ReadOnlyMany"), Some("my-volume-1")),
        ContainerSpec.Volume("/mnt/path2", None, Some(ContainerSpec.Volume.PersistentVolumeInfo(100)), Some("ReadWriteOnce"), Some("my-volume-2"))
      )
    ) {

      testSetup.kubeClient.list()(any,meq(PersistentVolumeClaim.pvcListDef)) returns Future.successful(new skuber.PersistentVolumeClaimList(
        "","",None,List(
          skuber.PersistentVolumeClaim(metadata = skuber.ObjectMeta("my-volume-1")),
          skuber.PersistentVolumeClaim(metadata = skuber.ObjectMeta("my-volume-2"))
        )
      ))

      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties

      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
          (((_:skuber.ext.Deployment).spec.get.template.get.spec.get.volumes) ^^ containTheSameElementsAs(Seq(
            skuber.Volume("my-volume-1", skuber.Volume.PersistentVolumeClaimRef("my-volume-1", true)),
            skuber.Volume("my-volume-2", skuber.Volume.PersistentVolumeClaimRef("my-volume-2", false))
          )))
      ))(any,meq(Deployment.deployDef))
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
      ))(any,meq(Deployment.deployDef))

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
      ))(any,meq(Secret.secDef))
    }

    "provision namespace with the expected name and labels" in new FakeKube {
      val (newWork, newEnv) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment").get
      Entitlements.setNewResourceEntitlements(dummyRootOrgId, newEnv.id, user, Some(newWork.id))
      testSetup.kubeClient.getOption(meq(newEnv.id.toString))(any,meq(skuber.Namespace.namespaceDef)) returns Future.successful(None)
      testSetup.kubeClient.create(argThat(
        ((_:skuber.Namespace).name) ^^ beEqualTo(newEnv.id.toString)
      ))(any,meq(skuber.Namespace.namespaceDef)) returns Future(mock[skuber.Namespace])
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
      ))(any,meq(skuber.Namespace.namespaceDef))
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
      ))(any,meq(Deployment.deployDef))
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
      ))(any,meq(Deployment.deployDef))
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
      ))(any,meq(Deployment.deployDef))
      there were two(testSetup.kubeClient).close
    }

    "pass no args when unspecified" in new FakeKubeCreate(args = None) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.args)) ^^ beSome(empty)
      ))(any,meq(Deployment.deployDef))
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
      ))(any,meq(Deployment.deployDef))
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
      ))(any,meq(Deployment.deployDef))
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
      ))(any,meq(Deployment.deployDef))
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
      ))(any,meq(Deployment.deployDef))
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
      ))(any,meq(Deployment.deployDef))
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
      ))(any,meq(Deployment.deployDef))
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
      ))(any,meq(Deployment.deployDef))
      there was one(testSetup.kubeClient).close
    }.pendingUntilFixed("this is going to be hard")

    "pass no cmd when unspecified" in new FakeKubeCreate(cmd = None) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        ((_:skuber.ext.Deployment).spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.command)) ^^ beSome(empty)
      ))(any,meq(Deployment.deployDef))
      there were two(testSetup.kubeClient).close
    }

    "orchestrate kube ingress for virtual hosts" in new FakeKubeCreate(port_mappings = Seq(
      ContainerSpec.PortMapping( protocol = "tcp", container_port = Some(80),                          expose_endpoint = Some(true), name = Some("http"), virtual_hosts = Some(Seq("galacticfog.com","www.galacticfog.com")) ),
      ContainerSpec.PortMapping( protocol = "tcp", container_port = Some(443),   lb_port = Some(8443), expose_endpoint = Some(true), name = Some("https"), virtual_hosts = Some(Seq("secure.galacticfog.com")) ),
      ContainerSpec.PortMapping( protocol = "tcp", container_port = Some(10000),                       expose_endpoint = Some(false), name = Some("not-exposed"), virtual_hosts = Some(Seq("no-exposure-no-vhost.galacticfog.com")) )
    )) {
      testSetup.kubeClient.create(any)(any,meq(Ingress.ingDef)) returns Future(mock[Ingress])

      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties

      import Ingress._

      val serviceCaptor = ArgumentCaptor.forClass(classOf[skuber.Service])
      there was one(testSetup.kubeClient).create(serviceCaptor.capture())(any,meq(Service.svcDef))
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
      ))(any,meq(Ingress.ingDef))
      there were two(testSetup.kubeClient).close
    }

    "set appropriate host port on container tasks when host port is requested" in new FakeKubeCreate(
      port_mappings = Seq(ContainerSpec.PortMapping("web", Some(9000), Some(80)))
    ) {
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
          ports = Seq(80),
          startedAt = Some(startA.toString)
        ),
        ContainerStats.TaskStat(
          id = "test-container-hash-b",
          host = "host-b",
          ipAddresses = Some(Seq(ContainerStats.TaskStat.IPAddress(
            ipAddress = "10.10.10.2",
            protocol = "IPv4"
          ))),
          ports = Seq(80),
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
      testSetup.kubeClient.list(any)(any,meq(Deployment.deployListDef)) returns Future.successful(new skuber.ext.DeploymentList("","",None,List(mockDepl)))
      testSetup.kubeClient.list(any)(any,meq(Service.svcListDef)) returns Future.successful(new skuber.ServiceList("","",None,Nil))
      testSetup.kubeClient.list(any)(any,meq(Pod.poListDef)) returns Future.successful(new skuber.PodList("","",None,Nil))

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
      testSetup.kubeClient.list(any)(any,meq(Deployment.deployListDef)) returns Future.successful(new skuber.ext.DeploymentList("","",None,List(mockDepl)))
      testSetup.kubeClient.list(any)(any,meq(Service.svcListDef)) returns Future.successful(new skuber.ServiceList("","",None,Nil))
      testSetup.kubeClient.list(any)(any,meq(Pod.poListDef)) returns Future.successful(new skuber.PodList("","",None,Nil))

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
      testSetup.kubeClient.list(any)(any,meq(Deployment.deployListDef)) returns Future.successful(new skuber.ext.DeploymentList("","",None,List(mockDepl)))
      testSetup.kubeClient.list(any)(any,meq(Service.svcListDef)) returns Future.successful(new skuber.ServiceList("","",None,Nil))
      testSetup.kubeClient.list(any)(any,meq(Pod.poListDef)) returns Future.successful(new skuber.PodList("","",None,Nil))

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
      testSetup.kubeClient.list(any)(any,meq(Deployment.deployListDef)) returns Future.successful(new skuber.ext.DeploymentList("","",None,items = List(mockDep)))
      testSetup.kubeClient.list(any)(any,meq(ReplicaSet.rsListDef)) returns Future.successful(new skuber.ext.ReplicaSetList("","",None,items = List(mockRS)))
      testSetup.kubeClient.list(any)(any,meq(Pod.poListDef)) returns Future.successful(new skuber.PodList("","",None,Nil))
      testSetup.kubeClient.list(any)(any,meq(Service.svcListDef)) returns Future.successful(new skuber.ServiceList("","",None,items = List(mockService)))
      testSetup.kubeClient.list(any)(any,meq(Ingress.ingListDef)) returns Future.successful(new skuber.ext.IngressList("","",None,items = List(mockIngress)))
      testSetup.kubeClient.delete(mockDep.name,0)(Deployment.deployDef) returns Future.successful(())
      testSetup.kubeClient.delete(mockRS.name,0)(ReplicaSet.rsDef) returns Future.successful(())
      testSetup.kubeClient.delete(mockService.name,0)(Service.svcDef) returns Future.successful(())
      testSetup.kubeClient.delete(mockIngress.name,0)(Ingress.ingDef) returns Future.successful(())

      await(testSetup.kubeService.destroy(metaContainer))
      there was one(testSetup.kubeClient).delete(mockDep.name,0)(Deployment.deployDef)
      there was one(testSetup.kubeClient).delete(mockRS.name,0)(ReplicaSet.rsDef)
      there was one(testSetup.kubeClient).delete(mockService.name,0)(Service.svcDef)
      there was one(testSetup.kubeClient).delete(mockIngress.name,0)(Ingress.ingDef)
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
      testSetup.kubeClient.list(any)(any,meq(Deployment.deployListDef)) returns Future.successful(new skuber.ext.DeploymentList("","",None,List(mockDep)))
      testSetup.kubeClient.list(any)(any,meq(ReplicaSet.rsListDef)) returns Future.successful(new skuber.ext.ReplicaSetList("","",None,List(mockRS)))
      testSetup.kubeClient.list(any)(any,meq(Pod.poListDef)) returns Future.failed(new skuber.api.client.K8SException(skuber.api.client.Status(reason = Some("test failure"))))
      testSetup.kubeClient.list(any)(any,meq(Service.svcListDef)) returns Future.failed(new skuber.api.client.K8SException(skuber.api.client.Status(reason = Some("test failure"))))
      testSetup.kubeClient.list(any)(any,meq(Ingress.ingListDef)) returns Future.failed(new skuber.api.client.K8SException(skuber.api.client.Status(reason = Some("test failure"))))
      testSetup.kubeClient.delete(mockDep.name,0)(Deployment.deployDef) returns Future.successful(())
      testSetup.kubeClient.delete(mockRS.name,0)(ReplicaSet.rsDef) returns Future.successful(())

      await(testSetup.kubeService.destroy(metaContainer))
      there were no(testSetup.kubeClient).delete(any,any)(meq(Service.svcDef))
      there were no(testSetup.kubeClient).delete(any,any)(meq(Pod.poDef))
      there were no(testSetup.kubeClient).delete(any,any)(meq(Ingress.ingDef))
      there was one(testSetup.kubeClient).close
    }

    "scale appropriately using skuber update" in new FakeKube {
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
      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(skuber.ext.Deployment.deployDef)) returns Future.successful(Some(testDepl))
      testSetup.kubeClient.update(any)(any,meq(Deployment.deployDef)) answers {
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
        ((depl: skuber.ext.Deployment) => (depl.spec.flatMap(_.replicas).getOrElse(-1) must_== testScale)) and haveName("test-container")
      ))(any,any)

      updatedContainerProps must havePair(
        "num_instances" -> testScale.toString
      )
      there was one(testSetup.kubeClient).close
    }

    "throw a 400 on container rename" in new FakeKubeCreate() {
      await(testSetup.kubeService.update(
        context = ProviderContext(play.api.test.FakeRequest("PATCH", s"/root/environments/${testEnv.id}/containers/${metaContainer.id}"), testProvider.id, None),
        container = metaContainer.copy(
          name = "updated-name"
        )
      )) must throwAn[BadRequestException]("renaming containers is not supported")
    }

    "update support using kube PUT" in new FakeKubeCreate(port_mappings = Seq(
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
      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(Ingress.ingDef)) returns Future.successful(Some(mock[skuber.ext.Ingress]))

      val newPortMappings = Seq(
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(80),
          name = Some("remove-service"),
          expose_endpoint = Some(false)
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(9000),
          name = Some("add-service"),
          lb_port = Some(80),
          expose_endpoint = Some(true),
          virtual_hosts = Some(Seq("port81.test.com"))
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(444),
          service_port = Some(30001),
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
      ))(any,meq(Deployment.deployDef))
      there was one(testSetup.kubeClient).update(argThat(
        inNamespace(testSetup.testNS.name)
          and haveName(metaContainer.name)
          and (((_: skuber.ext.Ingress).spec.get.rules.map(_.host)) ^^ containTheSameElementsAs(Seq("port81.test.com","port8444.test.com")))
          and (((_: skuber.ext.Ingress).spec.get.rules.flatMap(_.http.paths).map(_.backend.serviceName).distinct) ^^ containTheSameElementsAs(Seq(metaContainer.name)))
      ))(any,meq(Ingress.ingDef))
      there was one(testSetup.kubeClient).update(argThat(
        inNamespace(testSetup.testNS.name)
          and (((_: skuber.Service).name) ^^ be_==(metaContainer.name))
      ))(any,meq(Service.svcDef))

      updatedContainerProps must havePair(
        "image" -> "nginx:updated"
      )
      println(updatedContainerProps("port_mappings"))
      updatedContainerProps("port_mappings") must /#(0) and not /#(0) /("service_address")
      updatedContainerProps("port_mappings") must /#(1) /("service_address") /("host" -> (metaContainer.name + "." + testEnv.id + ".svc.cluster.local"))
      updatedContainerProps("port_mappings") must /#(2) /("service_address") /("host" -> (metaContainer.name + "." + testEnv.id + ".svc.cluster.local"))
      there were two(testSetup.kubeClient).close
    }

    "add service_port from kube Service on port_mappings update, ignore it on non-external types" in new FakeKubeCreate() {

      val assignedNodePort81 = 33334

      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(Ingress.ingDef)) returns Future.successful(None)
      testSetup.kubeClient.create(argThat((_:Service).name == metaContainer.name))(any,meq(Service.svcDef)) returns Future.successful({
        mockSvc().setPorts(List(
          skuber.Service.Port("web",    skuber.Protocol.TCP, 80,  Some(Left(80)),     0),
          skuber.Service.Port("api",    skuber.Protocol.TCP, 81,  Some(Left(81)),     0),
          skuber.Service.Port("secure", skuber.Protocol.TCP, 443, Some(Left(443)), 8443)
        )).withType(Service.Type.ClusterIP)
      })
      testSetup.kubeClient.create(argThat((_:Service).name == metaContainer.name + "-ext"))(any,meq(Service.svcDef)) returns Future.successful({
        mockSvc().setPorts(List(
          skuber.Service.Port("api",    skuber.Protocol.TCP,  81, Some(Left(81)),  assignedNodePort81),
          skuber.Service.Port("secure", skuber.Protocol.TCP, 443, Some(Left(443)),               8443)
        )).withType(Service.Type.NodePort)
      })

      val newPortMappings = Seq(
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(8888),
          name = Some("no-service"),
          expose_endpoint = Some(false)
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(80),
          service_port = Some(32000), // ignored
          name = Some("web"),
          expose_endpoint = Some(true)
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(81),
          service_port = Some(0),     // assigned
          name = Some("api"),
          `type` = Some("external"),
          expose_endpoint = Some(true)
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(443),
          service_port = Some(8443),
          name = Some("secure"),
          `type` = Some("external"),
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

      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
        and haveName(metaContainer.name)
        and (((_: skuber.Service).spec.get.ports) ^^ containTheSameElementsAs(List(
          skuber.Service.Port(name = "web",    protocol = skuber.Protocol.TCP, port = 80,  targetPort = Some(Left(80)),  nodePort = 0),
          skuber.Service.Port(name = "api",    protocol = skuber.Protocol.TCP, port = 81,  targetPort = Some(Left(81)),  nodePort = 0),
          skuber.Service.Port(name = "secure", protocol = skuber.Protocol.TCP, port = 443, targetPort = Some(Left(443)), nodePort = 0)
        )))
        and (((_: skuber.Service).spec.get._type) ^^ be_==(Service.Type.ClusterIP))
      ))(any,meq(Service.svcDef))

      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
        and haveName(metaContainer.name + "-ext")
        and (((_: skuber.Service).spec.get.ports) ^^ containTheSameElementsAs(List(
          skuber.Service.Port(name = "api",    protocol = skuber.Protocol.TCP, port = 81,  targetPort = Some(Left(81)),  nodePort = 0),
          skuber.Service.Port(name = "secure", protocol = skuber.Protocol.TCP, port = 443, targetPort = Some(Left(443)), nodePort = 8443)
        )))
        and (((_: skuber.Service).spec.get._type) ^^ be_==(Service.Type.NodePort))
      ))(any,meq(Service.svcDef))

      updatedContainerProps("port_mappings") must /#(0) and not /#(0) /("service_address")
      updatedContainerProps("port_mappings") must /#(1) /("service_address") /("host" -> (metaContainer.name + "." + testEnv.id + ".svc.cluster.local"))
      updatedContainerProps("port_mappings") must /#(1) /("container_port" -> 80)
      updatedContainerProps("port_mappings") must not /#(1) /("service_port")
      updatedContainerProps("port_mappings") must /#(2) /("service_address") /("host" -> (metaContainer.name + "." + testEnv.id + ".svc.cluster.local"))
      updatedContainerProps("port_mappings") must /#(2) /("container_port" -> 81)
      updatedContainerProps("port_mappings") must /#(2) /("service_port"   -> assignedNodePort81)
      updatedContainerProps("port_mappings") must /#(3) /("service_address") /("host" -> (metaContainer.name + "." + testEnv.id + ".svc.cluster.local"))
      updatedContainerProps("port_mappings") must /#(3) /("container_port" -> 443)
      updatedContainerProps("port_mappings") must /#(3) /("service_port"   -> 8443)
      there were two(testSetup.kubeClient).close
    }

    "delete empty ingress on container PUT" in new FakeKubeCreate(port_mappings = Seq(
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

      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(Ingress.ingDef)) returns Future.successful(Some(skuber.ext.Ingress(metaContainer.name)))
      testSetup.kubeClient.delete(metaContainer.name,0)(Ingress.ingDef) returns Future.successful(())

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
      there was one(testSetup.kubeClient).delete("test-container",0)(Ingress.ingDef)
      there was one(testSetup.kubeClient).update(any)(any,meq(Service.svcDef))
      there was one(testSetup.kubeClient).update(any)(any,meq(Deployment.deployDef))
      there were two(testSetup.kubeClient).close
    }

    "delete empty services on container PUT" in new FakeKubeCreate(port_mappings = Seq(
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(80),
          name = Some("remove-services"),
          expose_endpoint = Some(true),
          `type` = Some("loadBalancer"),  // this will create three Service objects
          virtual_hosts = None
        ),
        ContainerSpec.PortMapping(
          protocol = "tcp",
          container_port = Some(81),
          name = Some("no-service"),
          expose_endpoint = Some(false)
        )
      )) {

      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(Ingress.ingDef)) returns Future.successful(None)

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
      there was one(testSetup.kubeClient).update(any)(any,meq(Deployment.deployDef))
      there was one(testSetup.kubeClient).delete(metaContainer.name,       0)(Service.svcDef)
      there was one(testSetup.kubeClient).delete(metaContainer.name+"-ext",0)(Service.svcDef)
      there was one(testSetup.kubeClient).delete(metaContainer.name+"-lb", 0)(Service.svcDef)
      there were two(testSetup.kubeClient).close
    }

    "create service and ingress on container update" in new FakeKubeCreate(port_mappings = Seq(
      ContainerSpec.PortMapping(
        protocol = "tcp",
        container_port = Some(80),
        name = Some("update-port"),
        expose_endpoint = Some(false),
        virtual_hosts = None
      )
    )) {

      testSetup.kubeClient.getOption(meq(metaContainer.name))(any,meq(Ingress.ingDef)) returns Future.successful(None)

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
      ))(any,meq(Deployment.deployDef))
      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and haveName(metaContainer.name)
          and (((_: skuber.ext.Ingress).spec.get.rules.toSeq.map(_.host)) ^^ containTheSameElementsAs(Seq("port80.test.com")))
      ))(any,meq(Ingress.ingDef))
      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and haveName(metaContainer.name)
          and (((_: skuber.Service).spec.get.ports.map(_.targetPort.get))) ^^ containTheSameElementsAs(Seq(skuber.portNumToNameablePort(80)))
      ))(any,meq(Service.svcDef))

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

      // this also checks that "base64-decode secrets before sending to skuber"
      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
          (
            ((_: skuber.Secret).data.map({
              case (key, bytes) => SecretSpec.Item(key, Some(Base64.getEncoder.encodeToString(bytes)))
            }).toSeq) ^^ containTheSameElementsAs(metaSecretItems)
          )
      ))(any,meq(Secret.secDef))

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

      testSetup.kubeClient.list(any)(any,meq(skuber.Secret.secListDef)) returns Future.successful(new skuber.SecretList("","",None,List(mockSecret)))
      testSetup.kubeClient.delete(mockSecret.name,0)(skuber.Secret.secDef) returns Future.successful(())

      await(testSetup.kubeService.destroySecret(metaSecret))
      there was one(testSetup.kubeClient).delete(mockSecret.name,0)(skuber.Secret.secDef)
      there was one(testSetup.kubeClient).close
    }

    "mount specified secrets on container create" in new FakeKubeCreate(
      secrets = Seq(
        SecretEnvMount(null, "SOME_ENV_VAR", "part-a"),
        SecretFileMount(null, "/mnt/secrets/files/file-a", "part-a"),
        SecretFileMount(null, "/mnt/secrets/files/file-b", "part-b"),
        SecretFileMount(null, "/mnt/secrets/files/sub/file-c", "part-b"),
        SecretDirMount(null, "/mnt/secrets/dir")
      )
    ) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties

      val deploymentCaptor = ArgumentCaptor.forClass(classOf[skuber.ext.Deployment])
      there was one(testSetup.kubeClient).create(deploymentCaptor.capture())(any,meq(Deployment.deployDef))
      val createdDeployment = deploymentCaptor.getValue
      createdDeployment must inNamespace(testSetup.testNS.name)
      // deployment internal volume names are created on-demand, need to figure out what these were
      val deplVolumes = createdDeployment.spec.get.template.get.spec.get.volumes
      val dirVolName   = deplVolumes.find(v => v.source.isInstanceOf[skuber.Volume.Secret] && !v.source.asInstanceOf[skuber.Volume.Secret].items.exists(_.nonEmpty)).map(_.name).getOrElse("")
      val partsVolName = deplVolumes.find(v => v.source.isInstanceOf[skuber.Volume.Secret] &&  v.source.asInstanceOf[skuber.Volume.Secret].items.exists(_.nonEmpty)).map(_.name).getOrElse("")
      deplVolumes must containAllOf(Seq(
        skuber.Volume(partsVolName, skuber.Volume.Secret(secretName = metaSecret.name, items = Some(List(
          skuber.Volume.KeyToPath(key = "part-a", path = "file-a"),
          skuber.Volume.KeyToPath(key = "part-b", path = "file-b"),
          skuber.Volume.KeyToPath(key = "part-b", path = "sub/file-c")
        )))),
        skuber.Volume(dirVolName, skuber.Volume.Secret(secretName = metaSecret.name))
      ))
      createdDeployment.spec.get.template.get.spec.get.containers.head.volumeMounts must containTheSameElementsAs(Seq(
        skuber.Volume.Mount(partsVolName, "/mnt/secrets/files", true),
        skuber.Volume.Mount(dirVolName, "/mnt/secrets/dir", true)
      ))
      createdDeployment.spec.get.template.get.spec.get.containers.head.env must containAllOf(Seq(
        skuber.EnvVar("SOME_ENV_VAR", skuber.EnvVar.SecretKeyRef("part-a", metaSecret.name))
      ))
    }

    "mount a single secret file during container create (#338)" in new FakeKubeCreate(
      // https://gitlab.com/galacticfog/gestalt-meta/issues/338
      secrets = Seq(
        SecretFileMount(null, "/mnt/secrets/files/file-a", "part-a")
      )
    ) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties

      val deploymentCaptor = ArgumentCaptor.forClass(classOf[skuber.ext.Deployment])
      there was one(testSetup.kubeClient).create(deploymentCaptor.capture())(any,meq(Deployment.deployDef))
      val createdDeployment = deploymentCaptor.getValue
      createdDeployment must inNamespace(testSetup.testNS.name)
      // deployment internal volume names are created on-demand, need to figure out what these were
      val deplVolumes = createdDeployment.spec.get.template.get.spec.get.volumes
      val dirVolName   = deplVolumes.find(v => v.source.isInstanceOf[skuber.Volume.Secret] && !v.source.asInstanceOf[skuber.Volume.Secret].items.exists(_.nonEmpty)).map(_.name).getOrElse("")
      val partsVolName = deplVolumes.find(v => v.source.isInstanceOf[skuber.Volume.Secret] &&  v.source.asInstanceOf[skuber.Volume.Secret].items.exists(_.nonEmpty)).map(_.name).getOrElse("")
      deplVolumes must containAllOf(Seq(
        skuber.Volume(partsVolName, skuber.Volume.Secret(secretName = metaSecret.name, items = Some(List(
          skuber.Volume.KeyToPath(key = "part-a", path = "file-a")
        ))))
      ))
      createdDeployment.spec.get.template.get.spec.get.containers.head.volumeMounts must containTheSameElementsAs(Seq(
        skuber.Volume.Mount(partsVolName, "/mnt/secrets/files", true)
      ))
    }


    "mount a single secret file during container create (#338) in root" in new FakeKubeCreate(
      // https://gitlab.com/galacticfog/gestalt-meta/issues/338
      secrets = Seq(
        SecretFileMount(null, "/file-a", "part-a")
      )
    ) {
      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties

      val deploymentCaptor = ArgumentCaptor.forClass(classOf[skuber.ext.Deployment])
      there was one(testSetup.kubeClient).create(deploymentCaptor.capture())(any,meq(Deployment.deployDef))
      val createdDeployment = deploymentCaptor.getValue
      createdDeployment must inNamespace(testSetup.testNS.name)
      // deployment internal volume names are created on-demand, need to figure out what these were
      val deplVolumes = createdDeployment.spec.get.template.get.spec.get.volumes
      val dirVolName   = deplVolumes.find(v => v.source.isInstanceOf[skuber.Volume.Secret] && !v.source.asInstanceOf[skuber.Volume.Secret].items.exists(_.nonEmpty)).map(_.name).getOrElse("")
      val partsVolName = deplVolumes.find(v => v.source.isInstanceOf[skuber.Volume.Secret] &&  v.source.asInstanceOf[skuber.Volume.Secret].items.exists(_.nonEmpty)).map(_.name).getOrElse("")
      deplVolumes must containAllOf(Seq(
        skuber.Volume(partsVolName, skuber.Volume.Secret(secretName = metaSecret.name, items = Some(List(
          skuber.Volume.KeyToPath(key = "part-a", path = "file-a")
        ))))
      ))
      createdDeployment.spec.get.template.get.spec.get.containers.head.volumeMounts must containTheSameElementsAs(Seq(
        skuber.Volume.Mount(partsVolName, "/", true)
      ))
    }

    "secret mounts must be unique" in new FakeKubeCreate(
      secrets = Seq(
        SecretFileMount(null, "/mnt/secrets/files/file", "part-a"),
        SecretFileMount(null, "/mnt/secrets/files/file", "part-b")
      )
    ) {
      await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )) must throwA[BadRequestException]("secrets must have unique paths")
    }

    "deployment should not have affinity if not configured in the provider" in new FakeKube() {
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
          "env" -> "{}",
          "network" -> ""
        ))
      )
      testSetup.kubeClient.create(any)(any,meq(Deployment.deployDef)) returns Future.successful(mock[skuber.ext.Deployment])
      testSetup.kubeClient.list()(any,meq(PersistentVolumeClaim.pvcListDef)) returns Future.successful(new skuber.PersistentVolumeClaimList("","",None,Nil))

      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
          (((_: skuber.ext.Deployment).getPodSpec.get.affinity) ^^ beNone)
      ))(any,meq(Deployment.deployDef))
      there were two(testSetup.kubeClient).close
    }

    "deployment should have affinity if configured in the provider" in new FakeKube() {
      val affinityCfg =
        Json.parse("""{
          |  "nodeAffinity": {
          |    "preferredDuringSchedulingIgnoredDuringExecution": [
          |      {
          |        "preference": {
          |          "matchExpressions": [
          |            {
          |              "key": "another-node-label-key",
          |              "operator": "In",
          |              "values": [
          |                "another-node-label-value"
          |              ]
          |            }
          |          ]
          |        },
          |        "weight": 1
          |      }
          |    ],
          |    "requiredDuringSchedulingIgnoredDuringExecution": {
          |      "nodeSelectorTerms": [
          |        {
          |          "matchExpressions": [
          |            {
          |              "key": "kubernetes.io/e2e-az-name",
          |              "operator": "In",
          |              "values": [
          |                "e2e-az1",
          |                "e2e-az2"
          |              ]
          |            }
          |          ]
          |        }
          |      ]
          |    }
          |  }
          |}
        """.stripMargin)

      val providerWithAffinity = createKubernetesProvider(
        parent = testEnv.id,
        name = "test-provider-with-affinity",
        config = Seq(
          "affinity" -> affinityCfg
        )
      ).get
      testSetup.skuberFactory.initializeKube(meq(providerWithAffinity.id), meq("default")          )(any)  returns Future.successful(testSetup.kubeClient)
      testSetup.skuberFactory.initializeKube(meq(providerWithAffinity.id), meq(testEnv.id.toString))(any) returns Future.successful(testSetup.kubeClient)

      val Success(metaContainer) = createInstance(
        ResourceIds.Container,
        "test-container",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> providerWithAffinity.id
          ).toString,
          "cpus" -> "2.0",
          "memory" -> "768.0",
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> "[]",
          "env" -> "{}",
          "network" -> ""
        ))
      )
      testSetup.kubeClient.create(any)(any,meq(Deployment.deployDef)) returns Future.successful(mock[skuber.ext.Deployment])
      testSetup.kubeClient.list()(any,meq(PersistentVolumeClaim.pvcListDef)) returns Future.successful(new skuber.PersistentVolumeClaimList("","",None,Nil))

      val affinity = affinityCfg.as[skuber.Pod.Affinity]

      val Some(updatedContainerProps) = await(testSetup.kubeService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), providerWithAffinity.id, None),
        container = metaContainer
      )).properties
      there was one(testSetup.kubeClient).create(argThat(
        inNamespace(testSetup.testNS.name)
          and
          (((_: skuber.ext.Deployment).getPodSpec.get.affinity) ^^ beSome(affinity))
      ))(any,meq(Deployment.deployDef))
      there were two(testSetup.kubeClient).close
    }

  }

}
