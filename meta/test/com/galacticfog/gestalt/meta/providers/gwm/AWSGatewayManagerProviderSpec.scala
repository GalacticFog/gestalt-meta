package com.galacticfog.gestalt.meta.providers.gwm

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
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import com.galacticfog.gestalt.meta.test._
import controllers.util.{GestaltSecurityMocking,ProviderMethods}
import controllers.SecurityResources

class AWSGatewayManagerProviderSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll
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
    val Success(testLambdaProvider) = createInstance(migrations.V20.AWS_LAMBDA_PROVIDER_TYPE_ID, "test-gateway-provider", properties = Some(Map(
      "config" ->
        """{
          |  "env": {
          |     "public": {
          |       "AWS_API_KEY": "",
          |       "AWS_SECRET_KEY": "",
          |       "AWS_REGION": "us-east-2",
          |       "SERVICE_HOST": "gateway.service",
          |       "SERVICE_PORT": "6473",
          |       "GESTALT_SECURITY_KEY": "",
          |       "GESTALT_SECURITY_SECRET": ""
          |     }
          |  }
          |}""".stripMargin
    )))
    val Success(testGatewayProvider) = createInstance(migrations.V24.AWS_API_GATEWAY_PROVIDER_TYPE_ID, "test-gateway-provider", properties = Some(Map(
      "config" -> Json.obj(
        "env" -> Json.obj(
          "public" -> Json.obj(
            "AWS_APIGATEWAY_LOGLEVEL" -> "INFO",
            "AWS_APIGATEWAY_LOGREQUESTRESPONSE" -> "TRUE",
            "AWS_APIGATEWAY_ACCESSLOGGROUP" -> "test arn",
            "AWS_APIGATEWAY_ACCESSLOGFORMAT" -> "sample format"
          ),
          "private" -> Json.obj()
        )
      ).toString,
      "linked_providers" -> Json.arr(Json.obj(
        "name" -> "AWS",
        "type" -> "",
        "id" -> testLambdaProvider.id
      )).toString
    )))

    val Success(testLambda) = createInstance(ResourceIds.Lambda, "test-lambda", properties = Some(Map(
      "public" -> "true",
      "cpus" -> "0.1",
      "memory" -> "128",
      "code_type" -> "inline",
      "timeout" -> "30",
      "handler" -> "blah;blah",
      "runtime" -> "custom",
      "aws_function_id" -> "function id",
      "code" -> "sample code...",
      "provider" -> Json.obj(
        "name" -> testLambdaProvider.name,
        "id" -> testLambdaProvider.id.toString
      ).toString
    )))

    val Success(testApi) = createInstance(ResourceIds.Api, "test-api", properties = Some(Map(
      "provider" -> Json.obj(
        "id" -> testGatewayProvider.id.toString,
        "locations" -> Json.arr()
      ).toString
    )))
    Ents.setNewResourceEntitlements(dummyRootOrgId, testApi.id, user, Some(testEnv.id))

    val Success(testEndpoint) = createInstance(ResourceIds.ApiEndpoint, "test-endpoint", properties = Some(Map(
      "implementation_type" -> "lambda",
      "implementation_id" -> testLambda.id.toString,
      "container_port_name" -> "rest api id",
      "parent" -> testApi.id.toString,
      "provider" -> Json.obj(
        "id" -> testGatewayProvider.id.toString,
        "locations" -> Json.arr()
      ).toString
    )))
    Ents.setNewResourceEntitlements(dummyRootOrgId, testEndpoint.id, user, Some(testApi.id))
  }

  "AWSAPIGatewayProvider" should {
    "create endpoint" in new TestApplication {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, "http://gateway.service:6473/endpoint/create") => Action { request =>
          r = request.body.asJson.get
          Ok("\"rest api id\"")
        }
      }
      val gwmProvider = new AWSGatewayManagerProvider(ws, providerMethods)

      val resource = Await.result(gwmProvider.createEndpoint(testGatewayProvider, testEndpoint), 10 .seconds)

      resource must beEqualTo(testEndpoint.copy(
        properties = Some(testEndpoint.properties.get ++ Map[String,String](
          "container_port_name" -> "rest api id"
        ))
      ))

      r must beEqualTo(Json.obj(
        "name" -> s"${testEndpoint.id}",
        "aws" -> Json.obj(
          "functionArn" -> "function id"
        ),
        "errorLog" -> Json.obj(
          "level" -> "INFO",
          "logRequestResponseData" -> true
        ),
        "accessLog" -> Json.obj(
          "group" -> "test arn",
          "format" -> "sample format"
        )
      ))
    }
    "update endpoint" in new TestApplication {
      val ws = MockWS {
        case (DELETE, "") => Action {
          Ok("")
        }
      }
      val gwmProvider = new AWSGatewayManagerProvider(ws, providerMethods)

      val patch = PatchDocument(
        PatchOp.Replace("/properties/resource", "/new/path")
      )

      val t = Try(Await.result(gwmProvider.updateEndpoint(testGatewayProvider, testEndpoint, patch), 10 .seconds))
      t must beFailedTry.withThrowable[NotImplementedError]("an implementation is missing")
    }
    "delete endpoint" in new TestApplication {
      val ws = MockWS {
        case (DELETE, _) => Action {
          Ok("")
        }
      }
      val gwmProvider = new AWSGatewayManagerProvider(ws, providerMethods)

      val updatedTestEndpoint = testEndpoint.copy(
        properties = Some(testEndpoint.properties.get ++ Map[String,String](
          "container_port_name" -> "rest api id"
        ))
      )

      val _ = Await.result(gwmProvider.deleteEndpoint(testGatewayProvider, updatedTestEndpoint), 10 .seconds)
    }
    "getPublicUrl" in new TestApplication {
      val gwmProvider = new AWSGatewayManagerProvider(null, null)

      gwmProvider.getPublicUrl(testEndpoint) must beEqualTo(Some("https://rest api id.execute-api.us-east-2.amazonaws.com/default/path"))
    }
  }
}