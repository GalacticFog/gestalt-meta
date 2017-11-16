package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.laser.LaserEndpoint
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.{HostConfig, JsonClient, ResourceIds}
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import controllers.{ContainerController, DeleteController, SecurityResources}

import mockws.{MockWS, Route}
import org.joda.time.DateTime
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.matcher.{JsonMatchers, Matcher}
import org.specs2.specification.{BeforeAll, Scope}

import play.api.http.HttpVerbs
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.{JsValue, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.ws.WSResponse
import play.api.mvc._
import play.api.mvc.Results._
import play.api.test.{FakeRequest, PlaySpecification}
import play.api.mvc.BodyParsers.parse

import scala.concurrent.Future
import scala.util.Success

class GatewayMethodsSpec extends GestaltProviderMocking with BeforeAll with JsonMatchers {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    pristineDatabase()
    val Success(createdUser) = Ents.createNewMetaUser(user, dummyRootOrgId, user.account,
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

  abstract class FakeGatewayScope extends WithDb(
    containerApp(
      additionalBindings = Seq(
        bind(classOf[ProviderMethods]).toInstance(mock[ProviderMethods])
      )
    )
  )

  trait TestApplication extends FakeGatewayScope {
    
    var Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    Entitlements.setNewEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))
    
    val gatewayMethods = application.injector.instanceOf[GatewayMethods]
    val mockProviderMethods = application.injector.instanceOf[ProviderMethods]
    
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
          |       "SERVICE_PORT": "6473"
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

    val Success(testApi) = createInstance(
      ResourceIds.Api,
      "test-api",
      properties = Some(Map(
        "provider" -> Json.obj(
          "id" -> testGatewayProvider.id.toString,
          "locations" -> Json.arr(testKongProvider.id.toString).toString
        ).toString
      ))
    )

    val Success(testEndpoint) = createInstance(
      ResourceIds.ApiEndpoint,
      "test-endpoint",
      properties = Some(Map(
        "resource"     -> "/original/path",
        "upstream_url" -> "http://original-upstream-url-is-irrelevant:1234/blah/blah/blah",
        "methods" -> Json.toJson(Seq("GET")).toString,
        "implementation_type" -> "lambda",
        "implementation_id" -> testLambda.id.toString,
        "provider" -> Json.obj(
          "id" -> testGatewayProvider.id.toString,
          "locations" -> Json.arr(testKongProvider.id.toString).toString
        ).toString
      )),
      parent = Some(testApi.id)
    )

    val routeGetEndpoint = Route {
      case (GET, uri) if uri == s"http://gateway.service:6473/endpoints/${testEndpoint.id}" => Action {
        Ok(
          Json.toJson(LaserEndpoint(
            id = Some(testEndpoint.id.toString),
            apiId = testApi.id.toString,
            upstreamUrl = "http://original-upstream-url-is-irrelevant:1234/blah/blah/blah",
            path = "/original/path"
          ))
        )
      }
    }
    var putBody: JsValue = null
    val routePutEndpoint = Route {
      case (PUT, uri) if uri == s"http://gateway.service:6473/endpoints/${testEndpoint.id}" => Action(parse.json) { implicit request =>
        putBody = request.body
        Ok(
          Json.obj("matters" -> "not")
        )
      }
    }
    val routeDeleteEndpoint = Route {
      case (DELETE, uri) if uri == s"http://gateway.service:6473/endpoints/${testEndpoint.id}" => Action {
        Ok( Json.obj() )
      }
    }
    val routeDeleteApi = Route {
      case (DELETE, uri) if uri == s"http://gateway.service:6473/apis/${testApi.id}" => Action {
        Ok( Json.obj() )
      }
    }
    val ws = MockWS (routeGetEndpoint orElse routePutEndpoint orElse routeDeleteApi orElse routeDeleteEndpoint)
    mockProviderMethods.configureWebClient(argThat(
      (provider: GestaltResourceInstance) => provider.id == testGatewayProvider.id
    ), any) returns new JsonClient(HostConfig("http", "gateway.service", Some(6473)), Some(ws))

  }

  "GatewayMethods" should {

    "properly create upstream_url for synchronous lambda-bound endpoints" in new TestApplication {
      val endpointId = UUID.randomUUID()
      val apiId = UUID.randomUUID()
      gatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> endpointId.toString,
        "properties" -> Json.obj(
          "implementation_type" -> "lambda",
          "synchronous" -> true,
          "implementation_id" -> testLambda.id.toString,
          "resource" -> "/some-path"
        )
      ), apiId) must beSuccessfulTry(
        (e: LaserEndpoint) => (e.upstreamUrl must_== s"http://laser.service:1111/lambdas/${testLambda.id}/invokeSync") and
          (e.id must beSome(endpointId.toString)) and
          (e.apiId must_== apiId.toString)
      )
    }

    "properly create upstream_url for asynchronous lambda-bound endpoints" in new TestApplication {
      val endpointId = UUID.randomUUID()
      val apiId = UUID.randomUUID()
      gatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> endpointId.toString,
        "properties" -> Json.obj(
          "implementation_type" -> "lambda",
          "synchronous" -> false,
          "implementation_id" -> testLambda.id.toString,
          "resource" -> "/some-path"
        )
      ), apiId) must beSuccessfulTry(
        (e: LaserEndpoint) => (e.upstreamUrl must_== s"http://laser.service:1111/lambdas/${testLambda.id}/invoke") and
          (e.id must beSome(endpointId.toString)) and
          (e.apiId must_== apiId.toString)
      )
    }

    "properly infer implementation_type from implementation_id for backwards compatibility for lambda-bound endpoints and overwrite upstream_url" in new TestApplication {
      val endpointId = UUID.randomUUID()
      val apiId = UUID.randomUUID()
      gatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> endpointId.toString,
        "properties" -> Json.obj(
          "implementation_id" -> testLambda.id.toString,
          "resource" -> "/some-path",
          "upstream_url" -> "please-overwrite-me"
        )
      ), apiId) must beSuccessfulTry(
        (e: LaserEndpoint) => (e.upstreamUrl must_== s"http://laser.service:1111/lambdas/${testLambda.id}/invokeSync") and
          (e.id must beSome(endpointId.toString)) and
          (e.apiId must_== apiId.toString)
      )
    }

    "properly create upstream_url for container-bound endpoints" in new TestApplication {
      val endpointId = UUID.randomUUID()
      val apiId = UUID.randomUUID()
      gatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> endpointId.toString,
        "properties" -> Json.obj(
          "implementation_type" -> "container",
          "implementation_id" -> testContainer.id.toString,
          "container_port_name" -> "web",
          "resource" -> "/some-path"
        )
      ), apiId) must beSuccessfulTry(
        (e: LaserEndpoint) => (e.upstreamUrl must_== s"http://my-nginx.service-address:80") and
          (e.id must beSome(endpointId.toString)) and
          (e.apiId must_== apiId.toString)
      )
    }

    "properly create with /properties/methods given" in new TestApplication {
      val endpointId = UUID.randomUUID()
      val apiId = UUID.randomUUID()
      val methods = Seq("PATCH", "PUT")
      
      gatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> endpointId.toString,
        "properties" -> Json.obj(
          "implementation_type" -> "container",
          "implementation_id" -> testContainer.id.toString,
          "container_port_name" -> "web",
          "resource" -> "/some-path",
          "methods" -> Json.toJson(methods)
        )
      ), apiId) must beSuccessfulTry(
        (e: LaserEndpoint) => (e.methods must beSome) and (e.methods.get === methods)
      )
    }    
    
    "properly create with /properties/rateLimit given" in new TestApplication {
      val endpointId = UUID.randomUUID()
      val apiId = UUID.randomUUID()
      val limit = Json.obj("perMinute" -> 2)
      
      val pluginJson = Json.obj("rateLimit" -> Json.obj("perMinute" -> 2))
      
      gatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> endpointId.toString,
        "properties" -> Json.obj(
          "implementation_type" -> "container",
          "implementation_id" -> testContainer.id.toString,
          "container_port_name" -> "web",
          "resource" -> "/some-path",
          "plugins" -> pluginJson
        )
      ), apiId) must beSuccessfulTry(
        (e: LaserEndpoint) => (e.plugins must beSome) and (e.plugins.get === pluginJson)
      )
    }        
    
    "BadRequest for container-bound endpoints with un-exposed port mapping" in new TestApplication {
      gatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> UUID.randomUUID().toString,
        "properties" -> Json.obj(
          "implementation_type" -> "container",
          "implementation_id" -> testContainer.id.toString,
          "container_port_name" -> "not-exposed",
          "resource" -> "/some-path"
        )
      ), UUID.randomUUID()) must beFailedTry(
        (e: Throwable) => (e must beAnInstanceOf[BadRequestException]) and (e.getMessage must contain("port mapping with the specified name was not exposed or did not contain service address"))
      )
    }

    "BadRequest for container-bound endpoints with missing port mapping" in new TestApplication {
      gatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> UUID.randomUUID().toString,
        "properties" -> Json.obj(
          "implementation_type" -> "container",
          "implementation_id" -> testContainer.id.toString,
          "container_port_name" -> "missing",
          "resource" -> "/some-path"
        )
      ), UUID.randomUUID()) must beFailedTry(
        (e: Throwable) => (e must beAnInstanceOf[BadRequestException]) and (e.getMessage must contain("no port mapping with the specified name"))
      )
    }

    "BadRequest for missing lambda on lambda-bound endpoints" in new TestApplication {
      gatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> UUID.randomUUID().toString,
        "properties" -> Json.obj(
          "implementation_type" -> "lambda",
          "synchronous" -> true,
          "implementation_id" -> UUID.randomUUID().toString,
          "resource" -> "/some-path"
        )
      ), UUID.randomUUID()) must beFailedTry(
        (e: Throwable) => (e must beAnInstanceOf[BadRequestException]) and (e.getMessage must contain("no lambda with id matching"))
      )
    }

    "BadRequest for missing container on container-bound endpoints" in new TestApplication {
      gatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> UUID.randomUUID().toString,
        "properties" -> Json.obj(
          "implementation_type" -> "container",
          "implementation_id" -> UUID.randomUUID().toString,
          "container_port_name" -> "web",
          "resource" -> "/some-path"
        )
      ), UUID.randomUUID()) must beFailedTry(
        (e: Throwable) => (e must beAnInstanceOf[BadRequestException]) and (e.getMessage must contain("no container with id matching"))
      )
    }

    "patch against api-gateway provider to an async lambda" in new TestApplication {
      val newPath = "/new/path"
      val expUpstreamUrl = s"http://laser.service:1111/lambdas/${testLambda.id}/invoke"

      val updatedEndpoint = await(gatewayMethods.patchEndpointHandler(
        r = testEndpoint,
        patch = PatchDocument(
          PatchOp.Replace("/properties/resource",    newPath),
          PatchOp.Replace("/properties/synchronous", false),
          PatchOp.Replace("/properties/implementation_type", "lambda"),
          PatchOp.Replace("/properties/implementation_id", testLambda.id.toString)
        ),
        user = user,
        request = FakeRequest(HttpVerbs.PATCH, s"/root/endpoints/${testLambda.id}")
      ))

      routeGetEndpoint.timeCalled must_== 1
      routePutEndpoint.timeCalled must_== 1

      (putBody \ "apiId").as[String] must_== testApi.id.toString
      (putBody \ "path").as[String] must_== newPath
      (putBody \ "upstreamUrl").as[String] must_== expUpstreamUrl

      updatedEndpoint.properties.get("upstream_url") must_== expUpstreamUrl
      updatedEndpoint.properties.get("resource") must_== newPath
      updatedEndpoint.properties.get("synchronous") must_== "false"
      updatedEndpoint.properties.get("implementation_type") must_== "lambda"
      updatedEndpoint.properties.get("implementation_id") must_== testLambda.id.toString
    }

    "patch against api-gateway provider to a synchro lambda" in new TestApplication {

      val newPath = "/new/path"
      val expUpstreamUrl = s"http://laser.service:1111/lambdas/${testLambda.id}/invokeSync"

      val updatedEndpoint = await(gatewayMethods.patchEndpointHandler(
        r = testEndpoint,
        patch = PatchDocument(
          PatchOp.Replace("/properties/resource",    newPath),
          PatchOp.Replace("/properties/synchronous", true),
          PatchOp.Replace("/properties/implementation_type", "lambda"),
          PatchOp.Replace("/properties/implementation_id", testLambda.id.toString)
        ),
        user = user,
        request = FakeRequest(HttpVerbs.PATCH, s"/root/endpoints/${testLambda.id}")
      ))

      routeGetEndpoint.timeCalled must_== 1
      routePutEndpoint.timeCalled must_== 1

      (putBody \ "apiId").as[String] must_== testApi.id.toString
      (putBody \ "path").as[String] must_== newPath
      (putBody \ "upstreamUrl").as[String] must_== expUpstreamUrl

      updatedEndpoint.properties.get("upstream_url") must_== expUpstreamUrl
      updatedEndpoint.properties.get("resource") must_== newPath
      updatedEndpoint.properties.get("synchronous") must_== "true"
      updatedEndpoint.properties.get("implementation_type") must_== "lambda"
      updatedEndpoint.properties.get("implementation_id") must_== testLambda.id.toString
    }

    "patch against api-gateway provider to modify methods" in new TestApplication {
      val newMethods = Json.arr(
        "GET", "POST", "OPTION"
      )

      val updatedEndpoint = await(gatewayMethods.patchEndpointHandler(
        r = testEndpoint,
        patch = PatchDocument(
          PatchOp.Replace("/properties/methods", newMethods)
        ),
        user = user,
        request = FakeRequest(HttpVerbs.PATCH, s"/root/endpoints/${testLambda.id}")
      ))

      routeGetEndpoint.timeCalled must_== 1
      routePutEndpoint.timeCalled must_== 1
      (putBody \ "apiId").as[String] must_== testApi.id.toString
      (putBody \ "methods").as[JsValue] must_== newMethods
      Json.parse(updatedEndpoint.properties.get("methods")) must_== newMethods
    }

    "deep patch against api-gateway provider to modify methods" in new TestApplication {
      val newMethods = Json.arr(
        "GET", "PUT"
      )

      val updatedEndpoint = await(gatewayMethods.patchEndpointHandler(
        r = testEndpoint,
        patch = PatchDocument(
          PatchOp.Add("/properties/methods/1","PUT")
        ),
        user = user,
        request = FakeRequest(HttpVerbs.PATCH, s"/root/endpoints/${testLambda.id}")
      ))

      routeGetEndpoint.timeCalled must_== 1
      routePutEndpoint.timeCalled must_== 1
      (putBody \ "apiId").as[String] must_== testApi.id.toString
      (putBody \ "methods").asOpt[JsValue] must beSome(newMethods)
      Json.parse(updatedEndpoint.properties.get("methods")) must_== newMethods
    }

    "patch against api-gateway provider to modify plugins" in new TestApplication {
      val newPlugins = Json.obj(
        "plugin1" -> Json.obj("do" -> "not care"),
        "plugin2" -> Json.obj("very" -> "opaque")
      )

      val updatedEndpoint = await(gatewayMethods.patchEndpointHandler(
        r = testEndpoint,
        patch = PatchDocument(
          PatchOp.Replace("/properties/plugins",    newPlugins)
        ),
        user = user,
        request = FakeRequest(HttpVerbs.PATCH, s"/root/endpoints/${testLambda.id}")
      ))

      routeGetEndpoint.timeCalled must_== 1
      routePutEndpoint.timeCalled must_== 1
      (putBody \ "apiId").as[String] must_== testApi.id.toString
      (putBody \ "plugins").as[JsValue] must_== newPlugins
      Json.parse(updatedEndpoint.properties.get("plugins")) must_== newPlugins
    }

    "deep patch against api-gateway provider to modify gestaltSecurity plugin" in new TestApplication {
      assert(testEndpoint.properties.get.get("plugins").isEmpty, "test assumes that plugins block is empty")
      val newPlugins = Json.obj(
        "gestaltSecurity" -> Json.obj(
          "enabled" -> true
        )
      )

      val updatedEndpoint = await(gatewayMethods.patchEndpointHandler(
        r = testEndpoint,
        patch = PatchDocument(
          PatchOp.Replace("/properties/plugins/gestaltSecurity/enabled", true)
        ),
        user = user,
        request = FakeRequest(HttpVerbs.PATCH, s"/root/endpoints/${testLambda.id}")
      ))

      routeGetEndpoint.timeCalled must_== 1
      routePutEndpoint.timeCalled must_== 1
      (putBody \ "apiId").as[String] must_== testApi.id.toString
      (putBody \ "plugins").as[JsValue] must_== newPlugins
      Json.parse(updatedEndpoint.properties.get("plugins")) must_== newPlugins
    }

    "deep patch against api-gateway provider to modify rateLimit plugin" in new TestApplication {
      assert(testEndpoint.properties.get.get("plugins").isEmpty, "test assumes that plugins block is empty")
      val newPlugins = Json.obj(
        "rateLimit" -> Json.obj(
          "perMinute" -> 100
        )
      )

      val updatedEndpoint = await(gatewayMethods.patchEndpointHandler(
        r = testEndpoint,
        patch = PatchDocument(
          PatchOp.Replace("/properties/plugins/rateLimit/perMinute", 100)
        ),
        user = user,
        request = FakeRequest(HttpVerbs.PATCH, s"/root/endpoints/${testLambda.id}")
      ))

      routeGetEndpoint.timeCalled must_== 1
      routePutEndpoint.timeCalled must_== 1
      (putBody \ "apiId").as[String] must_== testApi.id.toString
      (putBody \ "plugins").as[JsValue] must_== newPlugins
      Json.parse(updatedEndpoint.properties.get("plugins")) must_== newPlugins
    }

    "patch against api-gateway provider to a container" in new TestApplication {

      val expUpstreamUrl = "http://my-nginx.service-address:80"
      val newPath = "/new/path"

      val updatedEndpoint = await(gatewayMethods.patchEndpointHandler(
        r = testEndpoint,
        patch = PatchDocument(
          PatchOp.Replace("/properties/resource",    newPath),
          PatchOp.Replace("/properties/implementation_type", "container"),
          PatchOp.Replace("/properties/implementation_id", testContainer.id.toString),
          PatchOp.Replace("/properties/container_port_name", "web")
        ),
        user = user,
        request = FakeRequest(HttpVerbs.PATCH, s"/root/endpoints/${testLambda.id}")
      ))

      routeGetEndpoint.timeCalled must_== 1
      routePutEndpoint.timeCalled must_== 1

      (putBody \ "apiId").as[String] must_== testApi.id.toString
      (putBody \ "path").as[String] must_== newPath
      (putBody \ "upstreamUrl").as[String] must_== expUpstreamUrl

      updatedEndpoint.properties.get("upstream_url") must_== expUpstreamUrl
      updatedEndpoint.properties.get("resource") must_== newPath
      updatedEndpoint.properties.get("implementation_type") must_== "container"
      updatedEndpoint.properties.get("implementation_id") must_== testContainer.id.toString
    }

    "delete against GatewayMethods deletes apis" in new TestApplication {
      val Success(_) = gatewayMethods.deleteApiHandler(
        r = testApi,
        user = user
      )
      routeDeleteApi.timeCalled must_== 1
    }

    "delete against GatewayMethods deletes apiendpoints" in new TestApplication {
      val Success(_) = gatewayMethods.deleteEndpointHandler(
        r = testEndpoint,
        user = user
      )
      routeDeleteEndpoint.timeCalled must_== 1
    }

  }

}