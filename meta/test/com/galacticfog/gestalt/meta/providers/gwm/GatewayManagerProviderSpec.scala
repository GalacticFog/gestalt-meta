package com.galacticfog.gestalt.meta.providers.gwm

import java.util.UUID
import scala.util.{Try,Success}
import scala.concurrent.Await
import scala.concurrent.duration._
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.inject.bind
import play.api.test.PlaySpecification
import play.api.libs.json._
import play.api.mvc.Results._
import play.api.mvc._
import org.specs2.specification.{BeforeAll, Scope}
import org.specs2.matcher.JsonMatchers
import mockws.MockWS
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.patch.PatchInstance
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import com.galacticfog.gestalt.meta.test._
import controllers.util.{GestaltSecurityMocking,ProviderMethods}
import controllers.SecurityResources

class GatewayManagerProviderSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll
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
    val Success(testLambda2) = createInstance(ResourceIds.Lambda, "test-lambda", properties = Some(Map(
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

    val Success(testApi) = createInstance(ResourceIds.Api, "test-api", properties = Some(Map(
      "provider" -> Json.obj(
        "id" -> testGatewayProvider.id.toString,
        "locations" -> Json.arr(testKongProvider.id.toString)
      ).toString
    )))
    Ents.setNewResourceEntitlements(dummyRootOrgId, testApi.id, user, Some(testEnv.id))

    val Success(testEndpoint) = createInstance(ResourceIds.ApiEndpoint, "test-endpoint", properties = Some(Map(
      "resource" -> "/original/path",
      "upstream_url" -> "http://original-upstream-url-is-irrelevant:1234/blah/blah/blah",
      "methods" -> Json.toJson(Seq("GET")).toString,
      "implementation_type" -> "lambda",
      "implementation_id" -> testLambda.id.toString,
      "location_id" -> testKongProvider.id.toString,
      "parent" -> testApi.id.toString,
      "provider" -> Json.obj(
        "id" -> testGatewayProvider.id.toString,
        "locations" -> Json.arr(testKongProvider.id.toString)
      ).toString
    )))
    Ents.setNewResourceEntitlements(dummyRootOrgId, testEndpoint.id, user, Some(testApi.id))

    def newEndpoint(props: Map[String,String]) = {
      val Success(testEndpoint) = createInstance(ResourceIds.ApiEndpoint, "test-endpoint", properties = Some(Map(
        "parent" -> testApi.id.toString,
        "provider" -> Json.obj(
          "id" -> testGatewayProvider.id.toString,
          "locations" -> Json.arr(testKongProvider.id.toString)
        ).toString
      ) ++ props))
      Ents.setNewResourceEntitlements(dummyRootOrgId, testEndpoint.id, user, Some(testApi.id))
      testEndpoint
    }
  }

  "GatewayManagerProvider" should {
    "create api" in new TestApplication {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, "http://gateway.service:6473/apis") => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val gwmProvider = new GatewayManagerProvider(ws, providerMethods)

      val _ = Await.result(gwmProvider.createApi(testGatewayProvider, testApi), 10 .seconds)

      r must beEqualTo(Json.obj(
        "id" -> testApi.id.toString,
        "name" -> "test-api",
        "provider" -> Json.obj(
          "id" -> testKongProvider.id.toString,
          "location" -> testKongProvider.id.toString
        )
      ))
    }

    "delete api" in new TestApplication {
      val ws = MockWS {
        case (DELETE, _) => Action { request =>
          Ok("")
        }
      }
      val gwmProvider = new GatewayManagerProvider(ws, providerMethods)

      val _ = Await.result(gwmProvider.deleteApi(testGatewayProvider, testApi), 10 .seconds)
    }

    "create endpoint" in new TestApplication {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, _) => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val gwmProvider = new GatewayManagerProvider(ws, providerMethods)

      val _ = Await.result(gwmProvider.createEndpoint(testGatewayProvider, testEndpoint), 10 .seconds)

      r must beEqualTo(Json.obj(
        "id" -> testEndpoint.id,
        "apiId" -> testApi.id,
        "upstreamUrl" -> s"http://laser.service:1111/lambdas/${testLambda.id}/invokeSync",
        "path" -> "/original/path",
        "methods" -> Json.arr("GET")
      ))
    }

    "create endpoint with plugins" in new TestApplication {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, _) => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val gwmProvider = new GatewayManagerProvider(ws, providerMethods)

      val newTestEndpoint = testEndpoint.copy(
        properties = Some(testEndpoint.properties.get ++ Map[String,String](
          "plugins" -> Json.obj(
            "gestaltSecurity" -> Json.obj("enabled" -> true, "users" -> Json.arr(), "groups" -> Json.arr()),
            "rateLimit" -> Json.obj("enabled" -> true, "perMinute" -> 60)
          ).toString
        ))
      )
      val _ = Await.result(gwmProvider.createEndpoint(testGatewayProvider, newTestEndpoint), 10 .seconds)

      r must beEqualTo(Json.obj(
        "id" -> newTestEndpoint.id,
        "apiId" -> testApi.id,
        "upstreamUrl" -> s"http://laser.service:1111/lambdas/${testLambda.id}/invokeSync",
        "path" -> "/original/path",
        "methods" -> Json.arr("GET"),
        "plugins" -> Json.obj(
          "gestaltSecurity" -> Json.obj("enabled" -> true, "users" -> Json.arr(s"${user.account.id}"), "groups" -> Json.arr()),
          "rateLimit" -> Json.obj("enabled" -> true, "perMinute" -> 60)
        )
      ))
    }

    "properly create upstream_url for synchronous lambda-bound endpoints" in new TestApplication {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, _) => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val gwmProvider = new GatewayManagerProvider(ws, providerMethods)

      val newTestEndpoint = newEndpoint(Map[String,String](
        "implementation_type" -> "lambda",
        "synchronous" -> "true",
        "implementation_id" -> testLambda.id.toString,
        "resource" -> "/some-path"
      ))

      val _ = Await.result(gwmProvider.createEndpoint(testGatewayProvider, newTestEndpoint), 10 .seconds)

      r must beEqualTo(Json.obj(
        "id" -> newTestEndpoint.id,
        "apiId" -> testApi.id,
        "upstreamUrl" -> s"http://laser.service:1111/lambdas/${testLambda.id}/invokeSync",
        "path" -> "/some-path"
      ))
    }

    "properly create upstream_url for asynchronous lambda-bound endpoints" in new TestApplication {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, _) => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val gwmProvider = new GatewayManagerProvider(ws, providerMethods)

      val newTestEndpoint = newEndpoint(Map[String,String](
        "implementation_type" -> "lambda",
        "synchronous" -> "false",
        "implementation_id" -> testLambda.id.toString,
        "resource" -> "/some-path"
      ))

      val _ = Await.result(gwmProvider.createEndpoint(testGatewayProvider, newTestEndpoint), 10 .seconds)

      r must beEqualTo(Json.obj(
        "id" -> newTestEndpoint.id,
        "apiId" -> testApi.id,
        "upstreamUrl" -> s"http://laser.service:1111/lambdas/${testLambda.id}/invoke",
        "path" -> "/some-path"
      ))
    }

    // is this still needed?
    // "properly infer implementation_type from implementation_id for backwards compatibility for lambda-bound endpoints and overwrite upstream_url" in new TestApplication {
    //   var r: JsValue = null
    //   val ws = MockWS {
    //     case (POST, _) => Action { request =>
    //       r = request.body.asJson.get
    //       Ok("")
    //     }
    //   }
    //   val gwmProvider = new GatewayManagerProvider(ws, providerMethods)

    //   val newTestEndpoint = newEndpoint(Map[String,String](
    //     "synchronous" -> "false",
    //     "implementation_id" -> testLambda.id.toString,
    //     "resource" -> "/some-path"
    //   ))

    //   val _ = Await.result(gwmProvider.createEndpoint(testGatewayProvider, newTestEndpoint), 10 .seconds)

    //   r must beEqualTo(Json.obj(
    //     "id" -> testEndpoint.id,
    //     "apiId" -> testApi.id,
    //     "upstreamUrl" -> s"http://laser.service:1111/lambdas/${testLambda.id}/invoke",
    //     "path" -> "/some-path",
    //     "methods" -> Json.arr("GET")
    //   ))
    // }

    "properly create upstream_url for container-bound endpoints" in new TestApplication {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, _) => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val gwmProvider = new GatewayManagerProvider(ws, providerMethods)

      val newTestEndpoint = newEndpoint(Map[String,String](
        "implementation_type" -> "container",
        "implementation_id" -> testContainer.id.toString,
        "container_port_name" -> "web",
        "resource" -> "/some-path"
      ))

      val _ = Await.result(gwmProvider.createEndpoint(testGatewayProvider, newTestEndpoint), 10 .seconds)

      r must beEqualTo(Json.obj(
        "id" -> newTestEndpoint.id,
        "apiId" -> testApi.id,
        "upstreamUrl" -> s"http://my-nginx.service-address:80",
        "path" -> "/some-path"
      ))
    }

    "properly create with /properties/hosts given" in new TestApplication {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, _) => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val gwmProvider = new GatewayManagerProvider(ws, providerMethods)

      val newTestEndpoint = newEndpoint(Map[String,String](
        "implementation_type" -> "container",
        "implementation_id" -> testContainer.id.toString,
        "container_port_name" -> "web",
        "hosts" -> Json.arr("host1.com", "host2.company.com").toString
      ))

      val _ = Await.result(gwmProvider.createEndpoint(testGatewayProvider, newTestEndpoint), 10 .seconds)

      r must beEqualTo(Json.obj(
        "id" -> newTestEndpoint.id,
        "apiId" -> testApi.id,
        "upstreamUrl" -> s"http://my-nginx.service-address:80",
        "hosts" -> Json.arr("host1.com", "host2.company.com")
      ))
    }

    "ignore /properties/hosts if empty" in new TestApplication {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, _) => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val gwmProvider = new GatewayManagerProvider(ws, providerMethods)

      val newTestEndpoint = newEndpoint(Map[String,String](
        "implementation_type" -> "container",
        "implementation_id" -> testContainer.id.toString,
        "container_port_name" -> "web",
        "hosts" -> "[]",
        "resource" -> "/some-path"
      ))

      val _ = Await.result(gwmProvider.createEndpoint(testGatewayProvider, newTestEndpoint), 10 .seconds)

      r must beEqualTo(Json.obj(
        "id" -> newTestEndpoint.id,
        "apiId" -> testApi.id,
        "upstreamUrl" -> s"http://my-nginx.service-address:80",
        "path" -> "/some-path"
      ))
    }

    "throw if missing both /properties/hosts and /properties/resources" in new TestApplication {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, _) => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val gwmProvider = new GatewayManagerProvider(ws, providerMethods)

      val newTestEndpoint = newEndpoint(Map[String,String](
        "implementation_type" -> "container",
        "implementation_id" -> testContainer.id.toString,
        "container_port_name" -> "web"
      ))

      val t = Try(Await.result(gwmProvider.createEndpoint(testGatewayProvider, newTestEndpoint), 10 .seconds))
      t must beFailedTry.withThrowable[RuntimeException](s"Either `hosts` or `resource` field must be specified on resource ${newTestEndpoint.id}")
    }

    "BadRequest for container-bound endpoints with un-exposed port mapping" in new TestApplication {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, _) => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val gwmProvider = new GatewayManagerProvider(ws, providerMethods)

      val newTestEndpoint = newEndpoint(Map[String,String](
        "implementation_type" -> "container",
        "implementation_id" -> testContainer.id.toString,
        "container_port_name" -> "not-exposed",
        "resource" -> "/some-path"
      ))

      val t = Try(Await.result(gwmProvider.createEndpoint(testGatewayProvider, newTestEndpoint), 10 .seconds))
      t must beFailedTry.withThrowable[RuntimeException]("port mapping with the specified name was not exposed or did not contain service address")
    }

    "BadRequest for container-bound endpoints with missing port mapping" in new TestApplication {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, _) => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val gwmProvider = new GatewayManagerProvider(ws, providerMethods)

      val newTestEndpoint = newEndpoint(Map[String,String](
        "implementation_type" -> "container",
        "implementation_id" -> testContainer.id.toString,
        "container_port_name" -> "missing",
        "resource" -> "/some-path"
      ))

      val t = Try(Await.result(gwmProvider.createEndpoint(testGatewayProvider, newTestEndpoint), 10 .seconds))
      t must beFailedTry.withThrowable[RuntimeException]("no port mapping with the specified name on the specified container")
    }

    "BadRequest for missing lambda on lambda-bound endpoints" in new TestApplication {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, _) => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val gwmProvider = new GatewayManagerProvider(ws, providerMethods)

      val newTestEndpoint = newEndpoint(Map[String,String](
        "implementation_type" -> "lambda",
        "synchronous" -> "true",
        "implementation_id" -> UUID.randomUUID().toString,
        "resource" -> "/some-path"
      ))

      val t = Try(Await.result(gwmProvider.createEndpoint(testGatewayProvider, newTestEndpoint), 10 .seconds))
      t must beFailedTry.withThrowable[RuntimeException]("""no lambda with id matching ApiEndpoint "implementation_id"""")
    }

    "BadRequest for missing container on container-bound endpoints" in new TestApplication {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, _) => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val gwmProvider = new GatewayManagerProvider(ws, providerMethods)

      val newTestEndpoint = newEndpoint(Map[String,String](
        "implementation_type" -> "container",
        "implementation_id" -> UUID.randomUUID().toString,
        "container_port_name" -> "web",
        "resource" -> "/some-path"
      ))

      val t = Try(Await.result(gwmProvider.createEndpoint(testGatewayProvider, newTestEndpoint), 10 .seconds))
      t must beFailedTry.withThrowable[RuntimeException]("""no container with id matching ApiEndpoint "implementation_id"""")
    }

    "update endpoint" in new TestApplication {
      var r: JsValue = null
      val ws = MockWS {
        case (GET, _) => Action {
          Ok(Json.obj(
            "id" -> testEndpoint.id,
            "apiId" -> testApi.id,
            "upstreamUrl" -> s"http://laser.service:1111/lambdas/${testLambda.id}/invokeSync",
            "path" -> "/original/path",
            "methods" -> Json.arr("GET")
          ))
        }
        case (PUT, _) => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val gwmProvider = new GatewayManagerProvider(ws, providerMethods)

      val patch = PatchDocument(
        PatchOp.Replace("/properties/resource", "/new/path"),
        PatchOp.Replace("/properties/synchronous", false),
        PatchOp.Replace("/properties/implementation_type", "lambda"),
        PatchOp.Replace("/properties/implementation_id", testLambda2.id.toString)
      )
      val Success(newTestEndpoint) = PatchInstance.applyPatch(testEndpoint, patch)

      val _ = Await.result(gwmProvider.updateEndpoint(testGatewayProvider, newTestEndpoint, patch), 10 .seconds)

      r must beEqualTo(Json.obj(
        "id" -> testEndpoint.id,
        "apiId" -> testApi.id,
        "upstreamUrl" -> s"http://laser.service:1111/lambdas/${testLambda2.id}/invoke",
        "path" -> "/new/path",
        "methods" -> Json.arr("GET"),
        "plugins" -> Json.obj()
      ))
    }

    "delete endpoint" in new TestApplication {
      val ws = MockWS {
        case (DELETE, _) => Action { request =>
          Ok("")
        }
      }
      val gwmProvider = new GatewayManagerProvider(ws, providerMethods)

      val _ = Await.result(gwmProvider.deleteEndpoint(testGatewayProvider, testEndpoint), 10 .seconds)
    }
  }
}