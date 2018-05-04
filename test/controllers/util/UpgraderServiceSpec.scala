package controllers.util

import java.util.UUID

import actors.SystemConfigActor
import akka.actor.ActorRef
import akka.pattern.ask
import com.galacticfog.gestalt.data.{EnvironmentType, ResourceFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.providers.ProviderMap
import com.galacticfog.gestalt.meta.test._
import controllers._
import org.mockito.ArgumentCaptor
import org.specs2.matcher.JsonMatchers
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification.BeforeAll
import play.api.inject.{BindingKey, bind}
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import services.CaasService

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

class UpgraderServiceSpec extends GestaltProviderMocking with BeforeAll with JsonMatchers {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    pristineDatabase()
  }

  sequential

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
      mockProviderManager.processProvider(any, any) answers {
        (a: Any) =>
          val arr = a.asInstanceOf[Array[Object]]
          val pm = arr(0).asInstanceOf[ProviderMap]
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
          Future.successful(pm -> Seq(testContainer))
      }
      val apiCaptor = ArgumentCaptor.forClass(classOf[GestaltResourceInstance])
      val endpointCaptor = ArgumentCaptor.forClass(classOf[GestaltResourceInstance])
      mockGatewayMethods.createEndpoint(apiCaptor.capture(), endpointCaptor.capture(), any) answers {
        (a: Any) => Future.successful(a.asInstanceOf[Array[Object]](1).asInstanceOf[GestaltResourceInstance])
      }
      val status = await(upgrader.launchUpgrader(adminUser, UpgraderService.UpgradeLaunch(
        image = "galacticfog/upgrader-image:version",
        dbProviderId = dbProviderId,
        secProviderId = secProviderId,
        caasProviderId = testCaasProvider.id,
        gwmProviderId = gwmProviderId,
        kongProviderId = kongProviderId
      )))

      val maybeProviderId = (systemConfigActor ? SystemConfigActor.GetKey("upgrade_provider")).mapTo[Option[String]]
      await(maybeProviderId) must beSome
      status.active must beTrue
      status.endpoint must beSome(s"https://test-kong.mycompany.com/${apiCaptor.getValue.name}/${endpointCaptor.getValue.name}")
    }

    "delete provider on launch and delete config" in new TestApplication {
      mockCaasService.destroy(any) returns Future.successful(())
      mockGatewayMethods.deleteApiHandler(any) returns Success(())
      mockGatewayMethods.deleteEndpointHandler(any) returns Success(())

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