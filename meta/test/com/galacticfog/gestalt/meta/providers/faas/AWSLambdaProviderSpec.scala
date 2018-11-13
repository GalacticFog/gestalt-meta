package com.galacticfog.gestalt.meta.providers.faas

import java.util.UUID
import scala.util.{Try,Success}
import scala.concurrent.Await
import scala.concurrent.duration._
import play.api.test.PlaySpecification
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.inject.bind
import play.api.libs.json._
import play.api.mvc._
import play.api.mvc.Results._
import org.specs2.specification.{BeforeAll, Scope}
import org.specs2.matcher.JsonMatchers
import mockws.MockWS
import com.galacticfog.gestalt.meta.test.{DbShutdown, ResourceScope}
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import controllers.util.{ProviderMethods,GestaltSecurityMocking}
import controllers.SecurityResources

class AWSLambdaProviderSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll with DbShutdown with JsonMatchers {

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

  abstract class FakeLambdaScope extends Scope {
    val Success(testOrg) = createOrg(name = uuid().toString)
    val Success((testWork, testEnv)) = createWorkEnv(wrkName = uuid().toString, envName = uuid().toString, org = testOrg.id)
    Entitlements.setNewResourceEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

    // val mockProviderMethods = mock[ProviderMethods]
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

    // val lambdaMethods = injector.instanceOf[LambdaMethods]
    val providerMethods = injector.instanceOf[ProviderMethods]
    // val laserProvider = injector.instanceOf[LaserProvider]

    val caasProviderId = uuid()

    val Success(testLambdaProvider) = createInstance(ResourceIds.LambdaProvider, "test-lambda-provider", properties = Some(Map(
      "config" ->
        s"""{
          |  "env": {
          |     "public": {
          |       "SERVICE_HOST": "laser.service",
          |       "SERVICE_PORT": "1111",
          |       "GESTALT_SECURITY_KEY": "key",
          |       "GESTALT_SECURITY_SECRET": "secret",
          |       "META_COMPUTE_PROVIDER_ID": "${caasProviderId}"
          |     }
          |  }
          |}""".stripMargin
    )))

    def createLambda(name: String,
                     props: Map[String,String],
                     orgId: UUID = testOrg.id,
                     envId: UUID = testEnv.id) =
      createInstance(ResourceIds.Lambda, name = name, properties = Some(props), org = orgId, parent = Some(envId))

    def createSecret(name: String,
                     props: Map[String,String],
                     orgId: UUID = testOrg.id,
                     envId: UUID = testEnv.id) =
      createInstance(ResourceIds.Secret, name = name, properties = Some(props), org = orgId, parent = Some(envId))

    val Success(testLambda) = createLambda("test-lambda", Map(
      "public" -> "true",
      "memory" -> "128",
      "cpus" -> "0.1",
      "code_type" -> "Inline",
      "timeout" -> "30",
      "handler" -> "blah",
      "runtime" -> "nodejs8.10",
      "code" -> "sample code...",
      "provider" -> Json.obj(
        "name" -> testLambdaProvider.name,
        "id" -> testLambdaProvider.id.toString//,
        //"locations" -> Json.arr()
      ).toString
    ))

    // val mockJsonClient = mock[JsonClient]
    // mockProviderMethods.configureWebClient(argThat(
    //   (provider: GestaltResourceInstance) => provider.id == testLambdaProvider.id
    // ), any) returns mockJsonClient
  }

  "createLambda" should {
    "create lambda" in new FakeLambdaScope {
      var r: JsValue = null
      val ws = MockWS {
        // case (POST, "http://laser.service:1111/function/create") => Action { request =>
        case (POST, _) => Action { request =>
          r = request.body.asJson.get
          Ok(Json.obj(
            "arn" -> "function arn",
            "config" -> Json.obj(
              "description" -> "description",
              "handler" -> "handler",
              "runtime" -> "runtime",
              "timeout" -> 30,
              "memorySize" -> 128,
              "role" -> "role arn"
            )
          ))
        }
      }
      val awslProvider = new AWSLambdaProvider(ws, providerMethods)

      val created = Await.result(awslProvider.createLambda(testLambdaProvider, testLambda), 10 .seconds)
      val removeCode = (__ \ 'code).json.prune
      val JsSuccess(rr, _) = r.transform(removeCode)
      rr must beEqualTo(Json.obj(
        "name" -> testLambda.id.toString,
        "config" -> Json.obj(
          "description" -> "Managed by Gestalt Platform",
          "handler" -> "index:blah",
          "runtime" -> "nodejs8.10",
          "timeout" -> 30,
          "memorySize" -> 128,
          "role" -> "auto"
        ),
        "publish" -> true
      ))

      created.properties must beEqualTo(Some(Map(
        "provider" -> s"""{"id":"${testLambdaProvider.id.toString}"}""",
        "public" -> "true",
        "code" -> "sample code...",
        "cpus" -> "0.1",
        "code_type" -> "Inline",
        "timeout" -> "30",
        "handler" -> "handler",
        "runtime" -> "runtime",
        "memory" -> "128",
        "aws_role_id" -> "role arn",
        "aws_function_id" -> "function arn"
      )))
    }
    "create lambda only if input is valid" in new FakeLambdaScope {
      var r: JsValue = null
      val ws = MockWS {
        // case (POST, "http://laser.service:1111/function/create") => Action { request =>
        case (POST, _) => Action { request =>
          r = request.body.asJson.get
          Ok(Json.obj(
            "arn" -> "function arn",
            "config" -> Json.obj(
              "description" -> "description",
              "handler" -> "handler",
              "runtime" -> "runtime",
              "timeout" -> 30,
              "memorySize" -> 128,
              "role" -> "role arn"
            )
          ))
        }
      }
      val awslProvider = new AWSLambdaProvider(ws, providerMethods)

      val newTestLambda = testLambda.copy(
        properties = Some(testLambda.properties.get -- Seq("handler"))
      )

      val res = Try(Await.result(awslProvider.createLambda(testLambdaProvider, newTestLambda), 10 .seconds))
      res must beFailedTry.withThrowable[RuntimeException]("Failed to parse payload: /handler: error.path.missing")
    }
  }
  "updateLambda" should {
    "update lambda configuration" in new FakeLambdaScope {
      var r: JsValue = null
      val ws = MockWS {
        case (PUT, url) if url.endsWith("/configuration") => Action { request =>
          r = request.body.asJson.get
          Ok(Json.obj(
            "description" -> "description",
            "handler" -> "handler",
            "runtime" -> "runtime",
            "timeout" -> 30,
            "memorySize" -> 128,
            "role" -> "role arn"
          ))
        }
      }
      val awslProvider = new AWSLambdaProvider(ws, providerMethods)

      val patch = PatchDocument(
        PatchOp.Replace("/properties/handler", "blahblah"),
        PatchOp.Replace("/properties/memory", 64)
      )

      val _ = Await.result(awslProvider.updateLambda(testLambdaProvider, testLambda, patch), 10 .seconds)
      r must beEqualTo(Json.obj(
        "description" -> "Managed by Gestalt Platform",
        "handler" -> "index:blahblah",
        "runtime" -> "nodejs8.10",
        "timeout" -> 30,
        "memorySize" -> 64,
        "role" -> "auto"
      ))
    }
    "update lambda code" in new FakeLambdaScope {
      val ws = MockWS {
        case (PUT, url) if url.endsWith("/code") => Action { request =>
          Ok("")
        }
      }
      val awslProvider = new AWSLambdaProvider(ws, providerMethods)

      val patch = PatchDocument(
        PatchOp.Add("/properties/code_type", "Inline"),
        PatchOp.Add("/properties/code", "sample")
      )

      val _ = Await.result(awslProvider.updateLambda(testLambdaProvider, testLambda, patch), 10 .seconds)
      1 must beEqualTo(1)
    }
    "update lambda configuration + code" in new FakeLambdaScope {
      var r: JsValue = null
      val ws = MockWS {
        case (PUT, url) if url.endsWith("/configuration") => Action { request =>
          r = request.body.asJson.get
          Ok(Json.obj(
            "description" -> "description",
            "handler" -> "handler",
            "runtime" -> "runtime",
            "timeout" -> 30,
            "memorySize" -> 128,
            "role" -> "role arn"
          ))
        }
        case (PUT, url) if url.endsWith("/code") => Action { request =>
          Ok("")
        }
      }
      val awslProvider = new AWSLambdaProvider(ws, providerMethods)

      val patch = PatchDocument(
        PatchOp.Replace("/properties/handler", "blahblah"),
        PatchOp.Replace("/properties/memory", 64),
        PatchOp.Add("/properties/code_type", "Inline"),
        PatchOp.Add("/properties/code", "sample")
      )

      val updated = Await.result(awslProvider.updateLambda(testLambdaProvider, testLambda, patch), 10 .seconds)
      r must beEqualTo(Json.obj(
        "description" -> "Managed by Gestalt Platform",
        "handler" -> "index:blahblah",
        "runtime" -> "nodejs8.10",
        "timeout" -> 30,
        "memorySize" -> 64,
        "role" -> "auto"
      ))

      updated.properties must beEqualTo(Some(Map(
        "provider" -> s"""{"id":"${testLambdaProvider.id.toString}"}""",
        "public" -> "true",
        "code" -> "sample",
        "cpus" -> "0.1",
        "code_type" -> "Inline",
        "timeout" -> "30",
        "handler" -> "handler",
        "runtime" -> "runtime",
        "memory" -> "128",
        "aws_role_id" -> "role arn"
      )))
    }
  }
  "deleteLambda" should {
    "delete lambda" in new FakeLambdaScope {
      val ws = MockWS {
        case (DELETE, _) => Action { Ok("") }
      }
      val awslProvider = new AWSLambdaProvider(ws, providerMethods)

      val deleted = Await.result(awslProvider.deleteLambda(testLambdaProvider, testLambda), 10 .seconds)
      deleted must beEqualTo(())
    }
  }
}