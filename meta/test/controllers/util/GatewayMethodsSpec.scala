package controllers.util

import java.util.UUID
import scala.util.Success
import scala.concurrent.{Future,Await}
import scala.concurrent.duration._
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.inject.bind
import play.api.test.PlaySpecification
import play.api.libs.json._
import play.api.libs.ws.WSClient
import org.specs2.specification.{BeforeAll, Scope}
import org.specs2.matcher.JsonMatchers
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.meta.providers.gwm._
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import controllers.SecurityResources

class GatewayMethodsSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll
 with DbShutdown with JsonMatchers {

  object Ents extends AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    pristineDatabase()
    val Success(createdUser) = Ents.createNewMetaUser(user, dummyRootOrgId, rootOwnerLink(), user.account,
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

  trait TestApplication extends Scope {
    val mockGestaltSecurityConfig = mock[GestaltSecurityConfig]
    val appBuilder = new GuiceApplicationBuilder()
      .disable[modules.ProdSecurityModule]
      .disable[modules.MetaDefaultSkuber]
      .disable[modules.MetaDefaultServices]
      .disable[modules.HealthModule]
      .bindings(
        bind(classOf[GestaltSecurityConfig]).toInstance(mockGestaltSecurityConfig)//,
        // bind(classOf[ProviderMethods]).toInstance(mockProviderMethods)
      )
    val injector = appBuilder.injector()
    setInjector(injector)

    val providerMethods = injector.instanceOf[ProviderMethods]
    val ws = injector.instanceOf[WSClient]

    var Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    Entitlements.setNewResourceEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))
    
    // val gatewayMethods = application.injector.instanceOf[GatewayMethods]
    // val mockProviderMethods = application.injector.instanceOf[ProviderMethods]

    // val patchController = application.injector.instanceOf[PatchController]
    // val apiController = application.injector.instanceOf[ApiController]
    
    val Success(testLambdaProvider) = createInstance(ResourceIds.LambdaProvider, "test-lambda-provider", properties = Some(Map(
      "config" ->
        """{
          |  "env": {
          |     "public": {
          |       "SERVICE_HOST": "laser.service",
          |       "SERVICE_PORT": "1111"
          |     }
          |  }
          |}""".stripMargin
    )))
    val Success(testGatewayProvider) = createInstance(ResourceIds.GatewayManager, "test-gateway-provider", properties = Some(Map(
      "config" ->
        """{
          |  "env": {
          |     "public": {
          |       "SERVICE_HOST": "gateway.service",
          |       "SERVICE_PORT": "6473",
          |       "GESTALT_SECURITY_KEY": "",
          |       "GESTALT_SECURITY_SECRET": ""
          |     }
          |  }
          |}""".stripMargin
    )))
    val Success(testKongProvider) = createInstance(ResourceIds.KongGateway, "test-kong-provider", properties = None)
    val Success(testLambda) = createInstance(ResourceIds.Lambda, "test-lambda", properties = Some(Map(
      "public" -> "true",
      "cpus" -> "0.1",
      "memory" -> "128",
      "code_type" -> "inline",
      "timeout" -> "30",
      "handler" -> "blah;blah",
      "runtime" -> "custom",
      "provider" -> Json.obj(
        "name" -> testLambdaProvider.name,
        "id" -> testLambdaProvider.id.toString
      ).toString
    )))
    val Success(testContainer) = createInstance(ResourceIds.Container, "test-container", properties = Some(Map(
      "cpus" -> "0.1",
      "memory" -> "128",
      "image" -> "nginx",
      "container_type" -> "DOCKER",
      "port_mappings" -> Json.toJson(Seq(
        ContainerSpec.PortMapping("tcp").copy(
          name = Some("web"),
          expose_endpoint = Some(true),
          service_address = Some(ContainerSpec.ServiceAddress(
            host = "my-nginx.service-address",
            port = 80,
            protocol = Some("tcp")
          ))
        ),
        ContainerSpec.PortMapping("tcp").copy(
          name = Some("not-exposed")
        )
      )).toString,
      "provider" -> Json.obj(
        "id" -> UUID.randomUUID().toString,
        "name" -> "nonexistent-provider-does-not-matter"
      ).toString
    )))

    val testApi = newInstance(ResourceIds.Api, "test-api", properties = Some(Map(
      "provider" -> Json.obj(
        "id" -> testGatewayProvider.id.toString,
        "locations" -> Json.arr(testKongProvider.id.toString)
      ).toString
    )))

    val Success(createdApi) = createInstance(ResourceIds.Api, "test-api", properties = Some(Map(
      "provider" -> Json.obj(
        "id" -> testGatewayProvider.id.toString,
        "locations" -> Json.arr(testKongProvider.id.toString)
      ).toString
    )))

    val testEndpoint = newInstance(ResourceIds.ApiEndpoint, "test-endpoint", properties = Some(Map(
      "resource" -> "/original/path",
      "upstream_url" -> "http://original-upstream-url-is-irrelevant:1234/blah/blah/blah",
      "methods" -> Json.toJson(Seq("GET")).toString,
      "implementation_type" -> "lambda",
      "implementation_id" -> testLambda.id.toString,
      "location_id" -> testKongProvider.id.toString,
      "parent" -> createdApi.id.toString,
      "provider" -> Json.obj(
        "id" -> testGatewayProvider.id.toString,
        "locations" -> Json.arr(testKongProvider.id.toString)
      ).toString
    )))

    val Success(createdEndpoint) = createInstance(ResourceIds.ApiEndpoint, "test-endpoint", properties = Some(Map(
      "resource" -> "/original/path",
      "upstream_url" -> "http://original-upstream-url-is-irrelevant:1234/blah/blah/blah",
      "methods" -> Json.toJson(Seq("GET")).toString,
      "implementation_type" -> "lambda",
      "implementation_id" -> testLambda.id.toString,
      "location_id" -> testKongProvider.id.toString,
      "parent" -> createdApi.id.toString,
      "provider" -> Json.obj(
        "id" -> testGatewayProvider.id.toString,
        "locations" -> Json.arr(testKongProvider.id.toString)
      ).toString
    )), parent = Some(createdApi.id))


    val Success(testUser) = createInstance(
      typeId = ResourceIds.User,
      name = uuid().toString,
      properties = Some(Map(
        "firstName" -> "Testy",
        "lastName" -> "McTestFace"
      ))
    )
    val Success(testGroup) = createInstance(
      typeId = ResourceIds.Group,
      name = uuid().toString
    )
  }

  def trimGri(gri: GestaltResourceInstance): GestaltResourceInstance = gri.copy(created=None, modified=None)

  "GatewayMethods" should {
    "create api" in new TestApplication {
      val mockAwsapiImpl = mock[AWSGatewayManagerProvider]
      val mockGwmImpl = mock[GatewayManagerProvider]

      var r: GestaltResourceInstance = null
      mockGwmImpl.createApi(any, any) answers { a: Any =>
        val args = a.asInstanceOf[Array[Any]]
        r = args(1).asInstanceOf[GestaltResourceInstance]
        Future.successful(testApi)
      }

      val gatewayMethods = new GatewayMethods(ws, providerMethods, mockAwsapiImpl, mockGwmImpl)

      val _ = Await.result(gatewayMethods.createApi(dummyRootOrgId, testEnv.id, Json.toJson(testApi), testUser), 10 .seconds)
      trimGri(r) must beEqualTo(testApi)
    }
    "delete api" in new TestApplication {
      val mockAwsapiImpl = mock[AWSGatewayManagerProvider]
      val mockGwmImpl = mock[GatewayManagerProvider]

      mockGwmImpl.deleteApi(any, any) answers { a: Any =>
        Future.successful(())
      }

      val gatewayMethods = new GatewayMethods(ws, providerMethods, mockAwsapiImpl, mockGwmImpl)

      val _ = Await.result(gatewayMethods.deleteApiHandler(testApi), 10 .seconds)
    }
    "create endpoint" in new TestApplication {
      val mockAwsapiImpl = mock[AWSGatewayManagerProvider]
      val mockGwmImpl = mock[GatewayManagerProvider]

      var r: GestaltResourceInstance = null
      mockGwmImpl.createEndpoint(any, any) answers { a: Any =>
        val args = a.asInstanceOf[Array[Any]]
        r = args(1).asInstanceOf[GestaltResourceInstance]
        Future.successful(testEndpoint)
      }

      val gatewayMethods = new GatewayMethods(ws, providerMethods, mockAwsapiImpl, mockGwmImpl)

      val _ = Await.result(gatewayMethods.createEndpoint(dummyRootOrgId, createdApi, Json.toJson(testEndpoint), testUser), 10 .seconds)
      trimGri(r) must beEqualTo(testEndpoint)
    }
    "create endpoint with plugins" in new TestApplication {
      val mockAwsapiImpl = mock[AWSGatewayManagerProvider]
      val mockGwmImpl = mock[GatewayManagerProvider]

      var r: GestaltResourceInstance = null
      mockGwmImpl.createEndpoint(any, any) answers { a: Any =>
        val args = a.asInstanceOf[Array[Any]]
        r = args(1).asInstanceOf[GestaltResourceInstance]
        Future.successful(testEndpoint)
      }

      val gatewayMethods = new GatewayMethods(ws, providerMethods, mockAwsapiImpl, mockGwmImpl)

      val newTestEndpoint = testEndpoint.copy(
        properties = Some(testEndpoint.properties.get ++ Map[String,String](
          "plugins" -> Json.obj(
            "gestaltSecurity" -> Json.obj("enabled" -> true, "users" -> Json.arr(), "groups" -> Json.arr()),
            "rateLimit" -> Json.obj("enabled" -> true, "perMinute" -> 60)
          ).toString
        ))
      )

      val _ = Await.result(gatewayMethods.createEndpoint(dummyRootOrgId, createdApi, Json.toJson(newTestEndpoint), testUser), 10 .seconds)
      trimGri(r) must beEqualTo(newTestEndpoint)
    }
    "update endpoint" in new TestApplication {
      val mockAwsapiImpl = mock[AWSGatewayManagerProvider]
      val mockGwmImpl = mock[GatewayManagerProvider]

      var r: GestaltResourceInstance = null
      mockGwmImpl.updateEndpoint(any, any, any) answers { a: Any =>
        val args = a.asInstanceOf[Array[Any]]
        r = args(1).asInstanceOf[GestaltResourceInstance]
        Future.successful(r)
      }

      val patch = PatchDocument(
        PatchOp.Replace("/properties/handler", "blahblah"),
        PatchOp.Replace("/properties/memory", 64)
      )

      val gatewayMethods = new GatewayMethods(ws, providerMethods, mockAwsapiImpl, mockGwmImpl)

      val _ = Await.result(gatewayMethods.updateEndpoint(createdEndpoint, patch), 10 .seconds)
      r must beEqualTo(createdEndpoint.copy(
        properties = Some(testEndpoint.properties.get ++ Map[String,String](
          "handler" -> "blahblah",
          "memory" -> "64"
        ))
      ))
    }
    "delete endpoint" in new TestApplication {
      val mockAwsapiImpl = mock[AWSGatewayManagerProvider]
      val mockGwmImpl = mock[GatewayManagerProvider]

      mockGwmImpl.deleteEndpoint(any, any) answers { a: Any =>
        Future.successful(())
      }

      val gatewayMethods = new GatewayMethods(ws, providerMethods, mockAwsapiImpl, mockGwmImpl)

      val _ = Await.result(gatewayMethods.deleteEndpointHandler(createdEndpoint), 10 .seconds)
    }
  }
}