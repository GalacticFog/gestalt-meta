package controllers.util

import java.util.UUID

import actors.SystemConfigActor
import akka.actor.ActorRef
import akka.pattern.ask
import com.galacticfog.gestalt.data.{EnvironmentType, ResourceFactory}
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.providers.ProviderMap
import com.galacticfog.gestalt.meta.test._
import controllers._
// import org.mockito.ArgumentCaptor
import org.specs2.matcher.JsonMatchers
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification.BeforeAll
import play.api.inject.{BindingKey, bind}
import play.api.libs.json._
import services.CaasService

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

class UpgraderServiceSpec extends GestaltProviderMocking with BeforeAll with JsonMatchers {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    pristineDatabase()
  }

  sequential

  stopOnFail

  abstract class FakeUpgraderScope extends WithDb(
    containerApp(
      additionalBindings = Seq(
        bind[GatewayMethods].toInstance(mockGatewayMethods)
      )
    )
  )

  trait TestApplication extends FakeUpgraderScope {
    
    val upgrader = application.injector.instanceOf[DefaultUpgraderService]

    val dbProviderId = uuid
    val secProviderId = uuid
    val gwmProviderId = uuid
    val kongProviderId = uuid

    val testProviderEnvId = uuid

    val systemConfigActor = application.injector.instanceOf(BindingKey(classOf[ActorRef]).qualifiedWith(SystemConfigActor.name))

    implicit val ec = application.injector.instanceOf[ExecutionContext]

    val Success(testGatewayProvider) = createInstance(id = gwmProviderId, typeId = ResourceIds.GatewayManager, name = "test-gateway-provider", properties = Some(Map(
      "config" ->
        """{
          |  "env": {
          |     "public": {
          |       "SERVICE_HOST": "gateway.service",
          |       "SERVICE_PORT": "6473"
          |     }
          |  }
          |}""".stripMargin
    )))

    val Success(testKongProvider) = createInstance(id = kongProviderId, typeId = ResourceIds.KongGateway, name = "test-kong-provider", properties = Some(Map(
      "config" ->
      """{
        |  "external_protocol": "https",
        |  "env": {
        |    "public": {
        |      "PUBLIC_URL_VHOST_0": "test-kong.mycompany.com"
        |    }
        |  }
        |}
      """.stripMargin
    )))

    var Success(testCaasProvider) = createKubernetesProvider(parent = dummyRootOrgId, name = "test-caas-provider")
    val mockCaasService = mock[CaasService]
    mockProviderManager.getProviderImpl(testCaasProvider.typeId) returns Success(mockCaasService)

  }

  "DefaultUpgraderService" should {

    "create provider on launch and save config" in new TestApplication {
      mockProviderManager.getOrCreateProviderEnvironment(any, any) answers {
        (a: Any) => ResourceFactory.findById(ResourceIds.Environment, testProviderEnvId).get
      }
      var testContainerId: UUID = null
      mockProviderManager.triggerProvider(any, any) answers {
        (a: Any) =>
          val arr = a.asInstanceOf[Array[Object]]
          // val pm = arr(0).asInstanceOf[ProviderMap]
          val pm = ProviderMap(arr(0).asInstanceOf[GestaltResourceInstance])
          val Success(env) = createInstance(
            typeId = ResourceIds.Environment,
            id = testProviderEnvId,
            name = "services",
            parent = Option(pm.root.id),
            properties = Option(Map(
              "environment_type" -> EnvironmentType.id("other").toString
            ))
          )
          val Success(testContainer) = createInstance(
            typeId = ResourceIds.Container,
            name = "test-container",
            properties = Some(Map(
              "cpus" -> "0.1",
              "memory" -> "128",
              "image" -> "nginx",
              "container_type" -> "DOCKER",
              "port_mappings" -> Json.toJson(Seq(
                ContainerSpec.PortMapping("tcp",
                  name = Some("api"),
                  expose_endpoint = Some(true),
                  service_address = Some(ContainerSpec.ServiceAddress(
                    host = "my-nginx.service-address",
                    port = 9000,
                    protocol = Some("tcp")
                  ))
                )
              )).toString,
              "provider" -> Json.obj(
                "id" -> testCaasProvider.id,
                "name" -> testCaasProvider.name
              ).toString
            )),
            parent = Some(testProviderEnvId)
          )
          testContainerId = testContainer.id
          // Future.successful(pm -> Seq(testContainer))
          Future.successful(Seq(testContainer))
      }

      var apiArgs: Array[Object] = null
      var endpointArgs: Array[Object] = null
      var testApiId: UUID = null

      mockGatewayMethods.createApi(any, any, any, any) answers { a: Any =>
        apiArgs = a.asInstanceOf[Array[Object]]

        val Success(testApi) = createInstance(ResourceIds.Api, (apiArgs(2).asInstanceOf[JsValue] \ "name").as[String], properties = Some(Map(
          "provider" -> Json.obj(
            "id" -> testGatewayProvider.id.toString,
            "locations" -> Json.arr(testKongProvider.id.toString)
          ).toString
        )), parent = Some(testProviderEnvId))
        testApiId = testApi.id
        Future.successful(testApi)
      }
      mockGatewayMethods.createEndpoint(any, any, any, any) answers { a: Any =>
        endpointArgs = a.asInstanceOf[Array[Object]]

        val Success(testEndpoint) = createInstance(ResourceIds.ApiEndpoint, "test-endpoint", properties = Some(Map(
          "resource" -> "/original/path",
          "upstream_url" -> "http://original-upstream-url-is-irrelevant:1234/blah/blah/blah",
          "methods" -> Json.toJson(Seq("GET")).toString,
          "implementation_type" -> "container",
          "implementation_id" -> testContainerId.toString,
          "location_id" -> testKongProvider.id.toString,
          "parent" -> testApiId.toString,
          "provider" -> Json.obj(
            "id" -> testGatewayProvider.id.toString,
            "locations" -> Json.arr(testKongProvider.id.toString)
          ).toString
        )), parent = Some(testApiId))
        
        Future.successful(testEndpoint)
      }
      
      mockGatewayMethods.getPublicUrl(any) answers { _ => Some("public url") }

      val status = await(upgrader.launchUpgrader(adminUser, UpgraderService.UpgradeLaunch(
        image = "galacticfog/upgrader-image:version",
        dbProviderId = dbProviderId,
        secProviderId = secProviderId,
        caasProviderId = testCaasProvider.id,
        gwmProviderId = gwmProviderId,
        kongProviderId = kongProviderId
      ), "http://meta.test"))

      there was one(mockGatewayMethods).createApi(any, any, any, any)
      there was one(mockGatewayMethods).createEndpoint(any, any, any, any)

      (apiArgs(2).asInstanceOf[JsValue] \ "properties" \ "provider" \ "id").as[UUID] must beEqualTo(gwmProviderId)
      endpointArgs(1).asInstanceOf[GestaltResourceInstance].id must beEqualTo(testApiId)

      val maybeProviderId = (systemConfigActor ? SystemConfigActor.GetKey("upgrade_provider")).mapTo[Option[String]]
      await(maybeProviderId) must beSome
      status.active must beTrue
      // status.endpoint must beSome(s"https://test-kong.mycompany.com/${(apiArgs(2).asInstanceOf[JsValue] \ "name").as[String]}/original/path")
      status.endpoint must beSome("public url")
    }

    "delete provider on launch and delete config" in new TestApplication {
      mockCaasService.destroy(any) returns Future.successful(())
      mockGatewayMethods.deleteApiHandler(any) returns Future.successful(())
      mockGatewayMethods.deleteEndpointHandler(any) returns Future.successful(())

      val Some(providerId) = await((systemConfigActor ? SystemConfigActor.GetKey("upgrade_provider")).mapTo[Option[String]]).map(UUID.fromString(_))
      ResourceFactory.findById(providerId) must beSome
      val status = await(upgrader.deleteUpgrader(adminUser))
      status.active must beFalse
      status.endpoint must beNone
      ResourceFactory.findById(providerId) must beNone

      await((systemConfigActor ? SystemConfigActor.GetKey("upgrade_provider")).mapTo[Option[String]]) must beNone
      await((systemConfigActor ? SystemConfigActor.GetKey("upgrade_lock")).mapTo[Option[String]]) must beNone or beSome("false")

      there was one(mockCaasService).destroy(any)
      there was one(mockGatewayMethods).deleteApiHandler(any)
      there was one(mockGatewayMethods).deleteEndpointHandler(any)
    }

  }

}