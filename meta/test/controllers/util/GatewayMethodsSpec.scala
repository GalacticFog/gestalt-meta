package controllers.util

import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.{HostConfig, JsonClient, ResourceIds}
import com.galacticfog.gestalt.meta.auth.Entitlement
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import controllers._
import mockws.{MockWS, Route}
import org.specs2.matcher.JsonMatchers
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification.BeforeAll
import play.api.http.HttpVerbs
import play.api.inject.bind
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.BodyParsers.parse
import play.api.mvc.Results._
import play.api.mvc._
import play.api.test.FakeRequest

import scala.util.Success
import scala.concurrent.Await
import scala.concurrent.duration._

class GatewayMethodsSpec extends GestaltProviderMocking with BeforeAll with JsonMatchers {

  implicit val actorSystem = ActorSystem("test-actor-system")
  implicit val mat = ActorMaterializer()

  import GatewayMethods._

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

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

  abstract class FakeGatewayScope extends WithDb(
    containerApp(
      additionalBindings = Seq(
        bind(classOf[ProviderMethods]).toInstance(mock[ProviderMethods])
      )
    )
  )

  trait TestApplication extends FakeGatewayScope {

    var Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    Entitlements.setNewResourceEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))
    
    val gatewayMethods = application.injector.instanceOf[GatewayMethods]
    val mockProviderMethods = application.injector.instanceOf[ProviderMethods]

    val patchController = application.injector.instanceOf[PatchController]
    val apiController = application.injector.instanceOf[ApiController]
    
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

    val Success(testApi) = apiController.CreateWithEntitlements(
      dummyRootOrgId, user, Json.obj(
        "name" -> "test-api",
        "properties" -> Json.obj(
          "provider" -> Json.obj(
            "id" -> testGatewayProvider.id.toString,
            "locations" -> Json.arr(testKongProvider.id.toString)
          ).toString
        )
      ), ResourceIds.Api, Some(testEnv.id)
    )


    val Success(testEndpoint) = apiController.CreateWithEntitlements(
      dummyRootOrgId, user, Json.obj(
        "name" -> "test-endpoint",
        "properties" -> Json.obj(
          "resource"     -> "/original/path",
          "upstream_url" -> "http://original-upstream-url-is-irrelevant:1234/blah/blah/blah",
          "methods" -> Json.toJson(Seq("GET")).toString,
          "implementation_type" -> "lambda",
          "implementation_id" -> testLambda.id.toString,
          "provider" -> Json.obj(
            "id" -> testGatewayProvider.id.toString,
            "locations" -> Json.arr(testKongProvider.id.toString).toString
          ).toString
        )
      ), ResourceIds.ApiEndpoint, Some(testApi.id)
    )

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

    // update the apiendpoint.invoke entitlement on the parent API for a richer test
    val (expectedUserIds, expectedGroupIds) = {
      val apiEnt = Entitlement.make(ResourceFactory.findDescendantEntitlements(testEndpoint.id, "apiendpoint.invoke").head)
      val Success(newApiEnt) = ResourceFactory.update(Entitlement.toGestalt(
        user.account.id,
        apiEnt.copy(
          properties = apiEnt.properties.addIdentities(testUser.id, testGroup.id)
        )
      ), user.account.id)
      Entitlement.make(newApiEnt).properties.identities.getOrElse(Seq.empty).partition(id => ResourceFactory.findById(ResourceIds.User, id).isDefined)
    }

    val routeGetEndpoint = Route {
      case (GET, uri) if uri == s"http://gateway.service:6473/endpoints/${testEndpoint.id}" => Action {
        Ok(
          Json.toJson(LaserEndpoint(
            id = Some(testEndpoint.id.toString),
            apiId = testApi.id.toString,
            upstreamUrl = "http://original-upstream-url-is-irrelevant:1234/blah/blah/blah",
            path = Some("/original/path")
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
    var apiPostBody: JsValue = null
    val routePostApi = Route {
      case (POST, uri) if uri == s"http://gateway.service:6473/apis" => Action(parse.json) { implicit request =>
        apiPostBody = request.body
        Ok( Json.obj() )
      }
    }
    var endpointPostBody: JsValue = null
    val routePostEndpoint = Route {
      case (POST, uri) if uri == s"http://gateway.service:6473/apis/${testApi.id}/endpoints" => Action(parse.json) { implicit request =>
        endpointPostBody = request.body
        Ok( Json.obj() )
      }
    }
    val ws = MockWS (routeGetEndpoint orElse routePutEndpoint orElse routeDeleteApi orElse routeDeleteEndpoint orElse routePostApi orElse routePostEndpoint)
    mockProviderMethods.configureWebClient(argThat(
      (provider: GestaltResourceInstance) => provider.id == testGatewayProvider.id
    ), any) returns new JsonClient(HostConfig("http", "gateway.service", Some(6473)), Some(ws))

  }

  "GatewayMethods" should {

    "properly create upstream_url for synchronous lambda-bound endpoints" in new TestApplication {
      val endpointId = UUID.randomUUID()
      val apiId = UUID.randomUUID()
      GatewayMethods.toGatewayEndpoint(Json.obj(
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
      GatewayMethods.toGatewayEndpoint(Json.obj(
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
      GatewayMethods.toGatewayEndpoint(Json.obj(
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
      GatewayMethods.toGatewayEndpoint(Json.obj(
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

    "properly create with /properties/hosts given" in new TestApplication {
      val endpointId = UUID.randomUUID()
      val apiId = UUID.randomUUID()
      val hosts = Seq("host1.com", "host2.company.com")

      GatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> endpointId.toString,
        "properties" -> Json.obj(
          "implementation_type" -> "container",
          "implementation_id" -> testContainer.id.toString,
          "container_port_name" -> "web",
          "hosts" -> hosts
        )
      ), apiId) must beSuccessfulTry(
        (e: LaserEndpoint) => (e.hosts must beSome(hosts)) and (e.path must beNone)
      )
    }

    "ignore /properties/hosts if empty" in new TestApplication {
      val endpointId = UUID.randomUUID()
      val apiId = UUID.randomUUID()

      GatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> endpointId.toString,
        "properties" -> Json.obj(
          "implementation_type" -> "container",
          "implementation_id" -> testContainer.id.toString,
          "container_port_name" -> "web",
          "hosts" -> "[]",
          "resource" -> "/some-path"
        )
      ), apiId) must beSuccessfulTry(
        (e: LaserEndpoint) => (e.hosts must beNone) and (e.path must beSome("/some-path"))
      )
    }

    "throw if missing both /properties/hosts and /properties/resources" in new TestApplication {
      val endpointId = UUID.randomUUID()
      val apiId = UUID.randomUUID()

      GatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> endpointId.toString,
        "properties" -> Json.obj(
          "implementation_type" -> "container",
          "implementation_id" -> testContainer.id.toString,
          "container_port_name" -> "web"
        )
      ), apiId) must beFailedTry.withThrowable[BadRequestException]("ApiEndpoint must contain exactly one of '/properties/resource' or non-empty '/properties/hosts'")
    }

    "properly create with /properties/methods given" in new TestApplication {
      val endpointId = UUID.randomUUID()
      val apiId = UUID.randomUUID()
      val methods = Seq("PATCH", "PUT")

      GatewayMethods.toGatewayEndpoint(Json.obj(
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

      GatewayMethods.toGatewayEndpoint(Json.obj(
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
      GatewayMethods.toGatewayEndpoint(Json.obj(
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
      GatewayMethods.toGatewayEndpoint(Json.obj(
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
      GatewayMethods.toGatewayEndpoint(Json.obj(
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
      GatewayMethods.toGatewayEndpoint(Json.obj(
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

    "post against api-gateway provider to create api" in new TestApplication {
      val testApiName = uuid().toString
      val testApiId   = uuid()
      val Some(createdApiResponse) = route(app,fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/apis", testCreds).withBody(Json.obj(
        "id" -> testApiId,
        "name" -> testApiName,
        "properties" -> Json.obj(
          "provider" -> Json.obj(
            "id" -> testGatewayProvider.id,
            "locations" -> Seq(testKongProvider.id)
          )
        )
      )))
      status(createdApiResponse) must_== CREATED
      routePostApi.timeCalled must_== 1
      (apiPostBody \ "name").asOpt[String] must beSome(testApiName)
      (apiPostBody \ "id").asOpt[UUID] must beSome(testApiId)
    }

    "post against api-gateway provider to create apiendpoint" in new TestApplication {
      val testEndpointName = uuid().toString
      val testEndpointId   = uuid()
      val Some(createEndpointResult) = route(app,fakeAuthRequest(POST, s"/root/apis/${testApi.id}/apiendpoints", testCreds).withBody(Json.obj(
        "id" -> testEndpointId,
        "name" -> testEndpointName,
        "properties" -> Json.obj(
          "resource" -> "blah",
          "provider" -> Json.obj(
            "id" -> testGatewayProvider.id,
            "locations" -> Seq(testKongProvider.id)
          ),
          "implementation_id" -> testLambda.id,
          "implementation_type" -> "lambda"
        )
      )))
      status(createEndpointResult) must_== CREATED
      routePostEndpoint.timeCalled must_== 1
      (endpointPostBody \ "id").asOpt[UUID] must beSome(testEndpointId)
    }

    "patch against api-gateway provider to an async lambda" in new TestApplication {
      val newPath = "/new/path"
      val expUpstreamUrl = s"http://laser.service:1111/lambdas/${testLambda.id}/invoke"

      val updatedEndpoint = await(patchController.patchEndpointHandler(
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

      val updatedEndpoint = await(patchController.patchEndpointHandler(
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

      val updatedEndpoint = await(patchController.patchEndpointHandler(
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

      val updatedEndpoint = await(patchController.patchEndpointHandler(
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

      val updatedEndpoint = await(patchController.patchEndpointHandler(
        r = testEndpoint,
        patch = PatchDocument(
          PatchOp.Replace("/properties/plugins", newPlugins)
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

      val updatedEndpoint = await(patchController.patchEndpointHandler(
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
      updatedEndpoint.properties.get("plugins") must /("gestaltSecurity") /("enabled" -> true)
    }

    "deep patch against api-gateway provider to modify rateLimit plugin" in new TestApplication {
      assert(testEndpoint.properties.get.get("plugins").isEmpty, "test assumes that plugins block is empty")
      val newPlugins = Json.obj(
        "rateLimit" -> Json.obj(
          "perMinute" -> 100
        )
      )

      val updatedEndpoint = await(patchController.patchEndpointHandler(
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

      val updatedEndpoint = await(patchController.patchEndpointHandler(
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

    "populate gestaltSecurity plugin with entitlements on update" in new TestApplication {
      val updatedEndpoint = await(patchController.patchEndpointHandler(
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
      (putBody \ "plugins" \ "gestaltSecurity" \ "users").asOpt[Seq[UUID]] must beSome(containTheSameElementsAs(expectedUserIds))
      (putBody \ "plugins" \ "gestaltSecurity" \ "groups").asOpt[Seq[UUID]] must beSome(containTheSameElementsAs(expectedGroupIds))

      val updatedPlugins = updatedEndpoint.properties.get.get("plugins")
      updatedPlugins must beNone or beSome(not /("gestaltSecurity") /("users"))
      updatedPlugins must beNone or beSome(not /("gestaltSecurity") /("groups"))
    }

    "do not populate gestaltSecurity plugin with entitlements on update if it is not enabled" in new TestApplication {
      val updatedEndpoint = await(patchController.patchEndpointHandler(
        r = testEndpoint,
        patch = PatchDocument(
          PatchOp.Replace("/properties/plugins/gestaltSecurity/enabled", false)
        ),
        user = user,
        request = FakeRequest(HttpVerbs.PATCH, s"/root/endpoints/${testLambda.id}")
      ))

      routeGetEndpoint.timeCalled must_== 1
      routePutEndpoint.timeCalled must_== 1

      (putBody \ "apiId").as[String] must_== testApi.id.toString
      (putBody \ "plugins" \ "gestaltSecurity" \ "users").asOpt[Seq[UUID]] must beNone
      (putBody \ "plugins" \ "gestaltSecurity" \ "groups").asOpt[Seq[UUID]] must beNone

      val updatedPlugins = updatedEndpoint.properties.get("plugins")
      updatedPlugins must not /("gestaltSecurity") /("users")
      updatedPlugins must not /("gestaltSecurity") /("groups")
    }

    "do not populate gestaltSecurity plugin with entitlements on update if it is absent" in new TestApplication {
      // .properties.plugins.gestaltSecurity does not exist
      val updatedEndpoint = await(patchController.patchEndpointHandler(
        r = testEndpoint,
        patch = PatchDocument(
          PatchOp.Replace("/description", "new description is sufficient")
        ),
        user = user,
        request = FakeRequest(HttpVerbs.PATCH, s"/root/endpoints/${testLambda.id}")
      ))

      routeGetEndpoint.timeCalled must_== 1
      routePutEndpoint.timeCalled must_== 1

      (putBody \ "apiId").as[String] must_== testApi.id.toString
      (putBody \ "plugins" \ "gestaltSecurity").asOpt[JsValue] must beNone

      val updatedPlugins = updatedEndpoint.properties.get.get("plugins")
      updatedPlugins must beNone or beSome(not /("gestaltSecurity") /("users"))
      updatedPlugins must beNone or beSome(not /("gestaltSecurity") /("groups"))
    }

    "delete against GatewayMethods deletes apis" in new TestApplication {
      Await.ready(gatewayMethods.deleteApiHandler(testApi), 5 .seconds)
      routeDeleteApi.timeCalled must_== 1
    }

    "delete against GatewayMethods deletes apiendpoints" in new TestApplication {
      Await.ready(gatewayMethods.deleteEndpointHandler(testEndpoint), 5 .seconds)
      routeDeleteEndpoint.timeCalled must_== 1
    }

  }

}