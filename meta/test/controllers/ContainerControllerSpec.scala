package controllers

import java.util.UUID

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.{ContainerSpec, SecretSpec, VolumeSpec}
import com.galacticfog.gestalt.meta.genericactions.GenericProviderManager
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.fakes.FakeGestaltSecurityModule
import com.galacticfog.gestalt.util.Error
import controllers.util._
import modules._
import org.junit.runner.RunWith
import org.mockito.Matchers.{eq => meq}
import org.specs2.execute.{AsResult, Result}
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.matcher.{JsonMatchers, Matcher}
import org.specs2.runner.JUnitRunner
import play.api.inject.bind
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.test.PlaySpecification
import services._

import scala.concurrent.Future
import scala.util.{Success, Try}
import com.galacticfog.gestalt.meta.api.sdk.GestaltConfigurationManager
import com.galacticfog.gestalt.data.PostgresConfigManager



@RunWith(classOf[JUnitRunner])
class ContainerControllerSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    pristineDatabase()
    val Success(_) = Ents.createNewMetaUser(user, dummyRootOrgId, rootOwnerLink(), user.account,
      Some(Map(
        "firstName" -> user.account.firstName,
        "lastName" -> user.account.lastName,
        "email" -> user.account.email.getOrElse(""),
        "phoneNumber" -> user.account.phoneNumber.getOrElse("")
      )),
      user.account.description
    )
  }

  sequential

  def appWithMocks() = {
    val defaultDisabled = Seq(
      classOf[MetaDefaultDocker],
      classOf[MetaDefaultDCOS],
      classOf[MetaDefaultSkuber],
      classOf[ProdSecurityModule],
      classOf[MetaDefaultServices],
      classOf[HealthModule]
    )

    val sc: Seq[GuiceableModule] = Seq(
      FakeGestaltSecurityModule(fakeSecurityEnvironment()),
      new SystemConfigModule,
      bind[SecureController].toInstance(mockSecureController),
      bind[SecurityClientProvider].toInstance(mock[SecurityClientProvider]),
      bind[SecurityKeyInit].toInstance(mock[SecurityKeyInit]),
      bind[MetaHealth].toInstance(mock[MetaHealth]),
      bind[MetaServiceStatus].toInstance(mock[MetaServiceStatus]),
      bind[UpgraderService].toInstance(mock[UpgraderService]),
      bind[ContainerService].toInstance(mock[ContainerService]),
      bind[ProviderManager].toInstance(mock[ProviderManager]),
      bind[MarathonClientFactory].toInstance(mock[MarathonClientFactory]),
      bind[kubernetes.SkuberFactory].toInstance(mock[kubernetes.SkuberFactory]),
      bind[DockerClientFactory].toInstance(mock[DockerClientFactory]),
      bind[GenericProviderManager].toInstance(mock[GenericProviderManager]),
      bind[GenericResourceMethods].toInstance(mock[GenericResourceMethods]),
      bind(classOf[GestaltConfigurationManager]).toInstance(PostgresConfigManager)
    )

    new GuiceApplicationBuilder()
      .disable(defaultDisabled: _*)
      .bindings(sc: _*)
      .build
  }

  abstract class TestContainerController extends WithDb(appWithMocks()) {
    lazy val (testWork,testEnv) = {
      val Success((tW,tE)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
      Ents.setNewResourceEntitlements(dummyRootOrgId, tE.id, user, Some(tW.id))
      (tW,tE)
    }
    lazy val containerService = app.injector.instanceOf[ContainerService]
    lazy val providerManager = app.injector.instanceOf[ProviderManager]

    lazy val testProvider = createKubernetesProvider(testEnv.id, "test-kube-provider").get
    lazy val mockCaasService = {
      val m = mock[CaasService]
      providerManager.getProviderImpl(testProvider.typeId) returns Success(m)
      m
    }

    override def around[T: AsResult](t: => T): Result = super.around {
      scalikejdbc.config.DBs.setupAll()
      t
    }
  }

  def matchesProviderContext(provider: GestaltResourceInstance, workspace: GestaltResourceInstance, environment: GestaltResourceInstance): Matcher[ProviderContext] =
    ((_: ProviderContext).workspace.id == workspace.id, (_: ProviderContext).workspace.id.toString + " does not contain the expected workspace resource " + workspace.id) and
    ((_: ProviderContext).environment.id == environment.id, (_: ProviderContext).environment.id.toString + " does not contain the expected environment resource " + environment.id) and
    ((_: ProviderContext).environmentId == environment.id, (_: ProviderContext).environmentId.toString + " does not contain the expected environmentId " + environment.id) and
    ((_: ProviderContext).provider.id == provider.id, (_: ProviderContext).provider.id.toString + " does not contain the expected provider resource " + provider.id) and
    ((_: ProviderContext).providerId == provider.id, (_: ProviderContext).providerId.toString + " does not contain the expected providerId " + provider.id)

  def hasName(name: => String): Matcher[GestaltResourceInstance] =
    ((_: GestaltResourceInstance).name) ^^ be_==(name)

  def hasDescription(desc: => String): Matcher[GestaltResourceInstance] =
    ((_: GestaltResourceInstance).description) ^^ beSome(desc)

  def hasProperties(props: (String,String)*): Matcher[GestaltResourceInstance] =
    ((_: GestaltResourceInstance).properties.get) ^^ havePairs(props:_*)

  def hasId(id: => UUID): Matcher[GestaltResourceInstance] =
    ((_: GestaltResourceInstance).id) ^^ be_==(id)

  "ContainerController" should {

    import com.galacticfog.gestalt.data.EnvironmentType
    
    "assertCompatibleEnvType" should {

      "succeed when the environment is compatible with Provider declared env types" in new TestContainerController {
        val env = createInstance(ResourceIds.Environment, uuid.toString, 
            properties = Some(Map("environment_type" -> EnvironmentType.id("test").toString)))
        env must beSuccessfulTry
        
        val prv = createInstance(ResourceIds.KubeProvider, uuid.toString, 
            properties = Some(Map("environment_types" -> Json.stringify(Json.arr("development", "test")))))        
        prv must beSuccessfulTry
        
        val cc = app.injector.instanceOf[ContainerController]
        cc.assertCompatibleEnvType(prv.get, env.get) must_== Right(())
      }
      
      "fail when the environment is incompatible with Provider declared env types" in new TestContainerController {
        val env = createInstance(ResourceIds.Environment, uuid.toString, 
            properties = Some(Map("environment_type" -> EnvironmentType.id("production").toString)))
        env must beSuccessfulTry
        
        val prv = createInstance(ResourceIds.KubeProvider, uuid.toString, 
            properties = Some(Map("environment_types" -> Json.stringify(Json.arr("development", "test")))))        
        prv must beSuccessfulTry
        
        val cc = app.injector.instanceOf[ContainerController]
        cc.assertCompatibleEnvType(prv.get, env.get) must_== Left(Error.Conflict("Incompatible Environment type. Expected one of (development,test). found: 'production'"))
      }
      
    }
    
    "get a container via the ContainerService interface" in new TestContainerController {

      val testProps = ContainerSpec(
        name = "",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp", Some(80), None, None, None, None)),
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
        user = None)
      val testContainerName = "test-container"
      val testContainer = createInstance(
        ResourceIds.Container,
        testContainerName,
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
          "network" -> testProps.network.get))).get

      containerService.getEnvironmentContainer("root", testEnv.id, testContainer.id) returns Future(Some(testContainer -> Seq.empty))

      // Set entitlements on new environment for container creation
      val ces = Try(Ents.setNewResourceEntitlements(dummyRootOrgId, testContainer.id, user, Some(testEnv.id)))
      ces.isFailure must beFalse

      val path = s"/root/environments/${testEnv.id}/containers/${testContainer.id}"
      val request = fakeAuthRequest(GET, path, testCreds)

      val Some(result) = route(app,request)

      contentAsString(result) must /("id" -> testContainer.id.toString)
      status(result) must equalTo(OK)
    }

    "return 400 if updating a container with a new name" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        cpus = .2,
        memory = 512,
        num_instances = 1,
        provider = ContainerSpec.InputProvider(id = testProvider.id)
      )
      val extId = s"/${testEnv.id}/test-container"
      val Success(createdResource) = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "disk" -> testProps.disk.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "external_id" -> s"${extId}"
        ))
      )

      val request = fakeAuthRequest(PUT,
        s"/root/containers/${createdResource.id}", testCreds
      ).withBody(
        Json.obj(
          "name" -> "updated-name",
          "description" -> "updated description",
          "properties" -> Json.obj(
            "env" ->  Json.obj(),
            "labels" ->  Json.obj(),
            "volumes" -> Json.arr(),
            "port_mappings" ->  Json.arr(),
            "health_checks" ->  Json.arr(),
            "container_type" ->  "DOCKER",
            "cpus" ->  2.0,
            "memory" ->  2048.0,
            "num_instances" -> 4,
            "network" -> "NEW-NETWORK",
            "image" -> "nginx:updated"
          )
        )
      )
      val Some(result) = route(app,request)
      contentAsJson(result) must_== Json.obj(
        "code" -> 400,
        "message" -> "renaming containers is not supported"
      )
      status(result) must equalTo(BAD_REQUEST)
    }

    "update a container via the ContainerService interface with minimal input" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        cpus = .2,
        memory = 512,
        num_instances = 1,
        provider = ContainerSpec.InputProvider(id = testProvider.id)
      )
      val extId = s"/${testEnv.id}/test-container"
      val Success(createdResource) = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "disk" -> testProps.disk.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "external_id" -> s"${extId}"
        ))
      )
      containerService.updateContainer(
        any,
        hasId(createdResource.id),
        any,
        any
      ) answers {
        (a: Any) =>
          val arr = a.asInstanceOf[Array[Object]]
          val r = arr(1).asInstanceOf[GestaltResourceInstance]
          Future.successful(r)
      }

      val request = fakeAuthRequest(PUT,
        s"/root/containers/${createdResource.id}", testCreds
      ).withBody(
        Json.obj(
          "name" -> testContainerName,
          "description" -> "updated description",
          "properties" -> Json.obj(
            "env" ->  Json.obj(),
            "labels" ->  Json.obj(),
            "volumes" -> Json.arr(),
            "port_mappings" ->  Json.arr(),
            "health_checks" ->  Json.arr(),
            "container_type" ->  "DOCKER",
            "cpus" ->  2.0,
            "memory" ->  2048.0,
            "num_instances" -> 4,
            "network" -> "NEW-NETWORK",
            "image" -> "nginx:updated"
          )
        )
      )
      val Some(result) = route(app,request)
      (contentAsJson(result) \ "properties").as[JsValue] must_== Json.obj(
        "network" -> "NEW-NETWORK",
        "num_instances" -> 4,
        // "provider" -> Output.renderInstance(testProvider),
        "provider" -> Json.obj("id" -> testProvider.id.toString, "name" -> testProvider.name),
        "secrets" -> Json.arr(),
        "disk" -> 0,
        "image" -> "nginx:updated",
        "health_checks" -> Json.arr(),
        "force_pull" -> false,
        "volumes" -> Json.arr(),
        "container_type" -> "DOCKER",
        "labels" -> Json.obj(),
        "constraints" -> Json.arr(),
        "cpus" -> 2,
        "port_mappings" -> Json.arr(),
        "env" -> Json.obj(),
        "memory" -> 2048,
        "external_id" -> s"${extId}"
      )
      status(result) must equalTo(OK)
      there was one(containerService).updateContainer(
        any,
        (
          hasId(createdResource.id) and
          hasName(testContainerName) and
          hasDescription("updated description") and
          hasProperties(
            "image" -> "nginx:updated",
            "cpus" -> "2",
            "memory" -> "2048",
            "num_instances" -> "4"
          )
        ),
        any,
        any
      )
    }.pendingUntilFixed("figure out why this is broken")

    "ignore system fields on container patch" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id)
      )
      val extId = s"/${testEnv.id}/test-container"
      val Success(createdResource) = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "disk" -> testProps.disk.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "external_id" -> s"${extId}"
        ))
      )
      containerService.updateContainer(
        any,
        hasId(createdResource.id),
        any,
        any
      ) answers {
        (a: Any) =>
          val arr = a.asInstanceOf[Array[Object]]
          val r = arr(1).asInstanceOf[GestaltResourceInstance]
          Future.successful(r)
      }

      val request = fakeAuthRequest(PUT,
        s"/root/containers/${createdResource.id}", testCreds
      ).withBody(
        Json.obj(
          "id" -> UUID.randomUUID().toString,
          "name" ->  testContainerName,
          "description" -> "new description",
          "properties" -> Json.obj(
            "image" -> "nginx:updated",
            "container_type" -> "DOCKER",
            "external_id" -> "cannot-update-external_id",
            "provider" -> Json.obj(
              "id" -> UUID.randomUUID().toString,
              "name" -> "cannot-update-provider"
            )
          )
        )
      )
      val Some(result) = route(app,request)
      status(result) must equalTo(OK)
      there was one(containerService).updateContainer(
        any,
        (
          hasId(createdResource.id) and
          hasProperties(
            "external_id" -> extId,
            // "provider" -> Output.renderInstance(testProvider).toString
            "provider" -> Json.obj("id" -> testProvider.id.toString, "name" -> testProvider.name).toString
          )
        ),
        any,
        any
      )
    }

    "list containers via the ContainerService interface" in new TestContainerController {

      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = "",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp", Some(80), None, None, None, None)),
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
        user = None)
      val testContainer = createInstance(ResourceIds.Container, testContainerName,
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
          "network" -> testProps.network.get))).get

      val ces = Try(Ents.setNewResourceEntitlements(
        dummyRootOrgId, testContainer.id, user, Some(testEnv.id)))
      ces.isFailure must beFalse

      containerService.listEnvironmentContainers(
        "root",
        testEnv.id) returns Future(Seq(testContainer -> Seq.empty))

      val path = s"/root/environments/${testEnv.id}/containers?expand=true"
      val request = fakeAuthRequest(GET, path, testCreds)

      val Some(result) = route(app,request)

      contentAsString(result) must /#(0) / ("id" -> testContainer.id.toString)
      status(result) must equalTo(OK)

    }

    "create containers with non-existent provider should 400" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = UUID.randomUUID())
      )

      val newResource = newInstance(
        typeId = ResourceIds.Container,
        name = testContainerName,
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Json.toJson(testProps.provider).toString
        ))
      )

      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/containers", testCreds).withBody(
        Output.renderInstance(newResource)
      )

      val Some(result) = route(app,request)

      contentAsJson(result) must_== Json.obj(
        "code" -> 404,
        "message" -> s"CaasProvider with ID '${testProps.provider.id}' not found"
      )
      status(result) must equalTo(NOT_FOUND)
    }

    "create containers using CaaSService interface" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp", Some(80), None, None, None, None)),
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

      val newResource = newInstance(
        typeId = ResourceIds.Container,
        name = testContainerName,
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Json.toJson(testProps.provider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "disk" -> testProps.disk.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "network" -> testProps.network.get)
        )
      )

      containerService.createContainer(any,any,any,any) returns Future.successful(newResource)

      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/containers", testCreds).withBody(
        Output.renderInstance(newResource)
      )

      val Some(result) = route(app,request)

      (contentAsJson(result) \ "created").as[JsValue] must not equalTo(null)
      (contentAsJson(result) \ "modified").as[JsValue] must not equalTo(null)
      (contentAsJson(result) \ "owner").as[JsValue] must not equalTo(null)
      status(result) must equalTo(CREATED)

      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome(newResource.id)
      (json \ "name").asOpt[String] must beSome(testContainerName)
      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Container")
      (json \ "properties").asOpt[ContainerSpec] must beSome(testProps.copy(name = ""))

      there was one(containerService).createContainer(
        context = matchesProviderContext(
          provider = testProvider,
          workspace = testWork,
          environment = testEnv
        ),
        any,any,any
      )
    }

    "create containers using the ContainerService interface with specific ID" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),None,None,None,None)),
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

      val userSpecificUUID = uuid()
      val newResource = newInstance(
        typeId = ResourceIds.Container,
        id = userSpecificUUID,
        name = testContainerName,
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Json.toJson(testProps.provider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "disk" -> testProps.disk.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "network" -> testProps.network.get
        ))
      )

      containerService.createContainer(any,any,any,any) returns Future.successful(newResource)

      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/containers", testCreds).withBody(
        Output.renderInstance(newResource)
      )
      val Some(result) = route(app,request)
      status(result) must equalTo(CREATED)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome(userSpecificUUID)
      (json \ "name").asOpt[String] must beSome(testContainerName)
      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Container")
      (json \ "properties").asOpt[ContainerSpec] must beSome(testProps.copy(name = ""))

      there was one(containerService).createContainer(
        context = matchesProviderContext(
          provider = testProvider,
          workspace = testWork,
          environment = testEnv
        ),
        user = argThat( (id: AuthAccountWithCreds) => id.account.id == testAuthResponse.account.id),
        containerSpec = any,
        userRequestedId = beSome(userSpecificUUID)
      )
    }

    "delete containers using the ContainerService interface" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),None,None,None,None)),
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
      val extId = s"/${testEnv.id}/test-container"
      val createdResource = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "disk" -> testProps.disk.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "network" -> testProps.network.get,
          "external_id" -> s"${extId}"
        ))
      ).get

      mockCaasService.destroy(
        hasId(createdResource.id)
      ) returns Future(())

      val request = fakeAuthRequest(DELETE,
        s"/root/environments/${testEnv.id}/containers/${createdResource.id}", testCreds)

      val Some(result) = route(app,request)

      status(result) must equalTo(NO_CONTENT)

      there was one(mockCaasService).destroy(
        hasId(createdResource.id) and hasProperties("external_id" -> extId)
      )
      there was atLeastOne(providerManager).getProviderImpl(ResourceIds.KubeProvider)
    }

    "scale containers using the ContainerService and CaaSService interfaces" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),None,None,None,None)),
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
      val extId = s"/${testEnv.id}/test-container"
      val createdResource = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "disk" -> testProps.disk.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "network" -> testProps.network.get,
          "external_id" -> s"${extId}"
        ))
      ).get

      mockCaasService.scale(
        any, any, any
      ) returns Future(createdResource.copy(
        properties = Some(createdResource.properties.get ++ Map("num_instances" -> "4"))
      ))

      val request = fakeAuthRequest(POST,
        s"/root/containers/${createdResource.id}/scale?numInstances=4", testCreds
      )

      val Some(result) = route(app,request)

      status(result) must equalTo(ACCEPTED)

      there was one(mockCaasService).scale(
        context = matchesProviderContext(
          provider = testProvider,
          workspace = testWork,
          environment = testEnv
        ),
        container = hasId(createdResource.id),
        numInstances = meq(4)
      )
      there was atLeastOne(providerManager).getProviderImpl(ResourceIds.KubeProvider)
    }

    "import containers using the GenericResourceMethods interface" in new TestContainerController {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),None,None,None,None)),
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
        user = None,
        external_id = Some(s"/${testEnv.id}/test-container")
      )
      val extId = s"/${testEnv.id}/test-container"
      val Success(createdResource) = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "disk" -> testProps.disk.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "network" -> testProps.network.get,
          "external_id" -> testProps.external_id.get
        ))
      )

      val genericMethods = app.injector.instanceOf[GenericResourceMethods]
      genericMethods.createProviderBackedResource(any,any,any,any,any,any,any)(any) returns Future.successful(createdResource)

      val request = fakeAuthRequest(POST,
        s"/root/environments/${testEnv.id}/containers?action=import",
        testCreds
      ).withBody(Json.obj(
        "name" -> "imported-container",
        "properties" -> Json.obj(
          "provider" -> Json.obj(
            "id" -> testProvider.id
          ),
          "external_id" -> extId
        )
      ))

      val Some(result) = route(app,request)
      // println(contentAsString(result))
      status(result) must equalTo(CREATED)
      val json = contentAsJson(result)
      (json \ "name").asOpt[String] must beSome(testContainerName)
      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Container")
      (json \ "properties").asOpt[ContainerSpec] must beSome(testProps.copy(name = ""))
    }

    /************
      *  Secrets
      */

    "create secrets with missing provider payload should 400" in new TestContainerController {
      val newResource = newInstance(
        typeId = ResourceIds.Secret,
        name = "test-secret",
        properties = Some(Map(
          // "provider" -> Json.toJson(testProps.provider).toString
        ))
      )
      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/secrets", testCreds).withBody(
        Output.renderInstance(newResource)
      )

      val Some(result) = route(app,request)
      status(result) must equalTo(BAD_REQUEST)
      contentAsJson(result).as[JsValue] must_== Json.obj(
        "code" -> 400,
        "message" -> "Failed to parse payload: missing value at /provider; missing value at /items"
      )
    }

    "create secrets with non-existent provider should 400" in new TestContainerController {
      val newResource = newInstance(
        typeId = ResourceIds.Secret,
        name = "test-secret",
        properties = Some(Map(
          "provider" -> Json.toJson(
            ContainerSpec.InputProvider(id = UUID.randomUUID())
          ).toString
        ))
      )
      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/secrets", testCreds).withBody(
        Output.renderInstance(newResource)
      )

      val Some(result) = route(app,request)
      contentAsJson(result).as[JsValue] must_== Json.obj(
        "code" -> 400,
        "message" -> "Failed to parse payload: missing value at /items"
      )
      status(result) must equalTo(BAD_REQUEST)
    }

    "create secrets using CaaSService interface" in new TestContainerController {
      val testSecretName = "test-secret"
      val testItems = Seq(
          SecretSpec.Item("item-a", Some("c2hoaGho")),
          SecretSpec.Item("item-b", Some("dGhpcyBpcyBhIHNlY3JldA=="))
        )
      val testProps = SecretSpec(
        name = testSecretName,
        provider = ContainerSpec.InputProvider(id = testProvider.id),
        items = testItems
      )

      val newResource = newInstance(
        typeId = ResourceIds.Secret,
        name = testSecretName,
        properties = Some(Map(
          "provider" -> Json.toJson(testProps.provider).toString,
          "items" -> Json.toJson(testProps.items).toString
        ))
      )

      val createdResource = newResource.copy(
        properties = Some(Map(
          "provider" -> Json.toJson(testProps.provider).toString,
          "items" -> Json.toJson(testProps.items.map(_.copy(value = None))).toString
        ))
      )

      containerService.createSecret(any,any,any,any) returns Future.successful(createdResource)

      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/secrets", testCreds).withBody(
        Output.renderInstance(newResource)
      )

      val Some(result) = route(app,request)
      status(result) must equalTo(CREATED)

      there was one(containerService).createSecret(
        context = matchesProviderContext(
          provider = testProvider,
          workspace = testWork,
          environment = testEnv
        ),
        user = any,
        secretSpec = argThat(
          ((_: SecretSpec).name) ^^ be_==(testSecretName)
            and
            ((_: SecretSpec).items) ^^ containTheSameElementsAs(testItems)
        ),
        userRequestedId = any
      )

      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome(createdResource.id)
      (json \ "name").asOpt[String] must beSome(testSecretName)
      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Secret")
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "items").as[Seq[SecretSpec.Item]] must containTheSameElementsAs(
        testItems.map(_.copy(value = None))
      )
    }

    "delete secrets using the ContainerService interface" in new TestContainerController {
      val testSecretName = "test-secret"
      val testItems = Seq(
        SecretSpec.Item("item-a", Some("c2hoaGho")),
        SecretSpec.Item("item-b", Some("dGhpcyBpcyBhIHNlY3JldA=="))
      )
      val testProps = SecretSpec(
        name = testSecretName,
        provider = ContainerSpec.InputProvider(id = testProvider.id),
        items = testItems
      )
      val extId = s"/${testEnv.id}/$testSecretName"
      val Success(createdResource) = createInstance(ResourceIds.Secret, testSecretName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> Json.toJson(testProps.provider).toString,
          "items" -> Json.toJson(testProps.items).toString,
          "external_id" -> s"${extId}"
        ))
      )

      mockCaasService.destroySecret(
        hasId(createdResource.id)
      ) returns Future(())

      val request = fakeAuthRequest(DELETE,
        s"/root/environments/${testEnv.id}/secrets/${createdResource.id}", testCreds)

      val Some(result) = route(app,request)

      status(result) must equalTo(NO_CONTENT)

      there was one(mockCaasService).destroySecret(
        hasId(createdResource.id) and hasProperties("external_id" -> extId)
      )
      there was atLeastOne(providerManager).getProviderImpl(ResourceIds.KubeProvider)
    }

    "import secrets using the GenericResourceMethods interface" in new TestContainerController {
      val testSecretName = "test-secret"
      val testItems = Seq(
        SecretSpec.Item("item-a", Some("c2hoaGho")),
        SecretSpec.Item("item-b", Some("dGhpcyBpcyBhIHNlY3JldA=="))
      )
      val testProps = SecretSpec(
        name = testSecretName,
        description = None,
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
        items = testItems
        // external_id = Some(s"/${testEnv.id}/test-container")
      )
      // val extId = s"/${testEnv.id}/test-container"
      val Success(createdResource) = createInstance(ResourceIds.Secret, testSecretName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> Output.renderInstance(testProvider).toString,
          "items" -> Json.toJson(testProps.items).toString
          // "external_id" -> testProps.external_id.get
        ))
      )

      val genericMethods = app.injector.instanceOf[GenericResourceMethods]
      genericMethods.createProviderBackedResource(any,any,any,any,any,any,any)(any) returns Future.successful(createdResource)

      val request = fakeAuthRequest(POST,
        s"/root/environments/${testEnv.id}/secrets?action=import",
        testCreds
      ).withBody(Json.obj(
        "name" -> "imported-secret",
        "properties" -> Json.obj(
          "provider" -> Json.obj(
            "id" -> testProvider.id
          )//,
          // "external_id" -> extId
        )
      ))

      val Some(result) = route(app,request)
      // println(contentAsString(result))
      status(result) must equalTo(CREATED)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome(createdResource.id)
      (json \ "name").asOpt[String] must beSome(testSecretName)
      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Secret")
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "items").as[Seq[SecretSpec.Item]] must containTheSameElementsAs(
        testItems.map(_.copy(value = None))
      )
    }


    /************
      *  Volumes
      */

    "create volumes with missing provider payload should 400" in new TestContainerController {
      val newResource = newInstance(
        typeId = migrations.V13.VOLUME_TYPE_ID,
        name = "test-volume",
        properties = Some(Map(
          "type" -> "host_path",
          "provider" -> "{}",
          "size" -> "100",
          "access_mode" -> "ReadWriteOnce",
          "config" -> "{}"
        ))
      )
      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/volumes", testCreds).withBody(
        Output.renderInstance(newResource)
      )

      val Some(result) = route(app,request)
      contentAsJson(result).as[JsValue] must_== Json.obj(
        "code" -> 400,
        "message" -> "Failed to parse payload: missing value at /provider/id"
      )
      status(result) must equalTo(BAD_REQUEST)
    }

    "create volumes with non-existent provider should 400" in new TestContainerController {
      val providerId = UUID.randomUUID()
      val newResource = newInstance(
        typeId = migrations.V13.VOLUME_TYPE_ID,
        name = "test-volume",
        properties = Some(Map(
          "type" -> "host_path",
          "provider" -> Json.toJson(
            ContainerSpec.InputProvider(id = providerId)
          ).toString,
          "size" -> "100",
          "access_mode" -> "ReadWriteOnce",
          "config" -> "{}"
        ))
      )
      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/volumes", testCreds).withBody(
        Output.renderInstance(newResource)
      )

      val Some(result) = route(app,request)
      contentAsJson(result).as[JsValue] must_== Json.obj(
        "code" -> 404,
        "message" -> s"CaasProvider with ID '${providerId}' not found"
      )
      status(result) must equalTo(NOT_FOUND)
    }

    "create volumes using CaaSService interface" in new TestContainerController {
      val testVolumeName = "test-volume"
      val testProps = VolumeSpec(
        name = testVolumeName,
        provider = Some(ContainerSpec.InputProvider(id = testProvider.id)),
        `type` = VolumeSpec.HostPath,
        size = 1000,
        access_mode = VolumeSpec.ReadWriteOnce,
        config = Json.obj(
          "host_path" -> "/tmp"
        )
      )

      val newResource = newInstance(
        typeId = migrations.V13.VOLUME_TYPE_ID,
        name = testVolumeName,
        properties = Some(Map(
          "provider" -> Json.toJson(testProps.provider).toString,
          "type" -> testProps.`type`.label,
          "size" -> testProps.size.toString,
          "access_mode" -> testProps.access_mode.toString,
          "config" -> testProps.config.toString
        ))
      )

      val extId = s"/namespaces/${testEnv.id}/volumes/${testVolumeName}"

      val createdResource = newResource.copy(
        properties = Some(Map(
          "provider" -> Json.toJson(testProps.provider).toString,
          "type" -> testProps.`type`.label,
          "config" -> testProps.config.toString,
          "size" -> testProps.size.toString,
          "access_mode" -> testProps.access_mode.toString,
          "external_id" -> extId
        ))
      )

      containerService.createVolume(any,any,any,any) returns Future.successful(createdResource)

      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/volumes", testCreds).withBody(
        Output.renderInstance(newResource)
      )

      val Some(result) = route(app,request)
      status(result) must equalTo(CREATED)

      there was one(containerService).createVolume(
        context = matchesProviderContext(
          provider = testProvider,
          workspace = testWork,
          environment = testEnv
        ),
        user = any,
        volumeSpec = argThat(
          ((_: VolumeSpec).name) ^^ be_==(testVolumeName)
            and
            ((_: VolumeSpec).config) ^^ be_==(testProps.config)
            and
            ((_: VolumeSpec).`type`) ^^ be_==(testProps.`type`)
        ),
        userRequestedId = any
      )

      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome(createdResource.id)
      (json \ "name").asOpt[String] must beSome(testVolumeName)
      (json \ "resource_type").asOpt[String] must beSome(migrations.V13.VOLUME_TYPE_NAME)
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "type").as[VolumeSpec.Type] must_== VolumeSpec.HostPath
      (json \ "properties" \ "config").as[JsValue] must_== testProps.config
      (json \ "properties" \ "external_id").as[String] must_== extId
    }

    "import volumes using the GenericResourceMethods interface" in new TestContainerController {
      val testVolumeName = "test-volume"
      val testProps = VolumeSpec(
        name = testVolumeName,
        provider = Some(ContainerSpec.InputProvider(id = testProvider.id)),
        `type` = VolumeSpec.HostPath,
        size = 1000,
        access_mode = VolumeSpec.ReadWriteOnce,
        config = Json.obj(
          "host_path" -> "/tmp"
        )
      )

      val newResource = newInstance(
        typeId = migrations.V13.VOLUME_TYPE_ID,
        name = testVolumeName,
        properties = Some(Map(
          "provider" -> Json.toJson(testProps.provider).toString,
          "type" -> testProps.`type`.label,
          "size" -> testProps.size.toString,
          "access_mode" -> testProps.access_mode.toString,
          "config" -> testProps.config.toString
        ))
      )

      val extId = s"/namespaces/${testEnv.id}/volumes/${testVolumeName}"

      val createdResource = newResource.copy(
        properties = Some(Map(
          "provider" -> Json.toJson(testProps.provider).toString,
          "type" -> testProps.`type`.label,
          "config" -> testProps.config.toString,
          "size" -> testProps.size.toString,
          "access_mode" -> testProps.access_mode.toString,
          "external_id" -> extId
        ))
      )

      val genericMethods = app.injector.instanceOf[GenericResourceMethods]
      genericMethods.createProviderBackedResource(any,any,any,any,any,any,any)(any) returns Future.successful(createdResource)

      val request = fakeAuthRequest(POST,
        s"/root/environments/${testEnv.id}/volumes?action=import",
        testCreds
      ).withBody(Json.obj(
        "name" -> "imported-volume",
        "properties" -> Json.obj(
          "provider" -> Json.obj(
            "id" -> testProvider.id
          ),
          "external_id" -> extId
        )
      ))

      val Some(result) = route(app,request)
      // println(contentAsString(result))
      status(result) must equalTo(CREATED)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome(createdResource.id)
      (json \ "name").asOpt[String] must beSome(testVolumeName)
      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Volume")
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      // (json \ "properties").as[JsValue] must equalTo(Json.obj())
    }

    "delete volumes using the ContainerService interface" in new TestContainerController {
      val testVolumeName = "test-volume"
      val testProps = VolumeSpec(
        name = testVolumeName,
        provider = Some(ContainerSpec.InputProvider(id = testProvider.id)),
        `type` = VolumeSpec.HostPath,
        size = 1000,
        access_mode = VolumeSpec.ReadWriteOnce,
        config = Json.obj(
          "host_path" -> "/tmp"
        )
      )
      val extId = s"/${testEnv.id}/$testVolumeName"
      val Success(createdResource) = createInstance(migrations.V13.VOLUME_TYPE_ID, testVolumeName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> Json.toJson(testProps.provider).toString,
          "type" -> testProps.`type`.label,
          "size" -> testProps.size.toString,
          "access_mode" -> testProps.access_mode.toString,
          "config" -> testProps.config.toString,
          "external_id" -> s"${extId}"
        ))
      )

      mockCaasService.destroyVolume(
        hasId(createdResource.id)
      ) returns Future(())

      val request = fakeAuthRequest(DELETE,
        s"/root/environments/${testEnv.id}/volumes/${createdResource.id}", testCreds)

      val Some(result) = route(app,request)

      status(result) must equalTo(NO_CONTENT)

      there was one(mockCaasService).destroyVolume(
        hasId(createdResource.id) and hasProperties("external_id" -> extId)
      )
    }

  }

}