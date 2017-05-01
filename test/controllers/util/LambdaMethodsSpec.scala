package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.laser.{LaserEndpoint, LaserLambda}
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.{GestaltResourceInput, HostConfig, JsonClient, ResourceIds, ResourceStates}
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp, PatchOps}
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import controllers.SecurityResources
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.JsonMatchers
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification.{BeforeAll, Scope}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.{JsBoolean, JsValue, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.PlaySpecification
import play.api.mvc._
import play.api.mvc.Results._
import play.api.http.HttpVerbs._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.ws.WSResponse
import com.galacticfog.gestalt.laser._

import scala.concurrent.Future
import scala.util.Success

class LambdaMethodsSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll with JsonMatchers with JsonInput {

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

  abstract class FakeLambdaScope extends Scope {
    val Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    Entitlements.setNewEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

    val mockProviderMethods = mock[ProviderMethods]
    val injector =
      new GuiceApplicationBuilder()
        .disable[modules.ProdSecurityModule]
        .disable[modules.MetaDefaultSkuber]
        .disable[modules.MetaDefaultServices]
        .disable[modules.HealthModule]
        .bindings(
          bind(classOf[GestaltSecurityConfig]).toInstance(mock[GestaltSecurityConfig]),
          bind(classOf[ProviderMethods]).toInstance(mockProviderMethods)
        )
        .injector

    val lambdaMethods = injector.instanceOf[LambdaMethods]

    val Success(testLambdaProvider) = createInstance(ResourceIds.LambdaProvider, "test-lambda-provider", properties = Some(Map(
      "config" ->
        """{
          |  "env": {
          |     "public": {
          |       "SERVICE_HOST": "laser.service",
          |       "SERVICE_PORT": "1111",
          |       "GESTALT_SECURITY_KEY": "key",
          |       "GESTALT_SECURITY_SECRET": "secret"
          |     }
          |  }
          |}""".stripMargin
    )))
    val Success(testLambda) = createInstance(ResourceIds.Lambda, "test-lambda", properties = Some(Map(
      "public" -> "true",
      "cpus" -> "0.1",
      "memory" -> "128",
      "code_type" -> "inline",
      "timeout" -> "30",
      "handler" -> "blah;blah",
      "runtime" -> "custom",
      "periodic_info" -> "{}",
      "provider" -> Json.obj(
        "name" -> testLambdaProvider.name,
        "id" -> testLambdaProvider.id.toString
      ).toString
    )))

    val mockJsonClient = mock[JsonClient]
    mockProviderMethods.configureWebClient(argThat(
      (provider: GestaltResourceInstance) => provider.id == testLambdaProvider.id
    ), any) returns mockJsonClient
  }

  trait TestApplication extends FakeLambdaScope {
  }

  "LambdaMethods" should {

    "patch against url lambda provider" in new TestApplication {
      mockJsonClient.get(meq(s"/lambdas/${testLambda.id}"), any, any)(any) returns Future.successful({
        val mockResp = mock[WSResponse]
        mockResp.status returns 200
        mockResp.json returns Json.toJson(LaserLambda(
          Some(testLambda.id.toString),
          None,
          false,
          None,
          LaserArtifactDescription(None,"runtime","handler",0,0.0)
        ))
        mockResp
      })
      mockJsonClient.put(meq(s"/lambdas/${testLambda.id}"), any, any, any)(any) returns Future.successful({
        val mockResp = mock[WSResponse]
        mockResp.status returns 200
      })

      val Success(updatedLambda) = lambdaMethods.patchLambdaHandler(
        r = testLambda,
        patch = PatchDocument(
          PatchOp.Replace("/properties/public",        true),
          PatchOp.Replace("/properties/compressed",    true),
          PatchOp.Replace("/properties/package_url",   "http://code.com/hello.js"),
          PatchOp.Replace("/properties/handler",       "hello.js;hello"),
          PatchOp.Replace("/properties/timeout",       300),
          PatchOp.Replace("/properties/runtime",       "newruntime"),
          PatchOp.Replace("/properties/cpus",          2.0),
          PatchOp.Replace("/properties/memory",        2048),
          PatchOp.Replace("/properties/code_type",     "package"),
          PatchOp.Replace("/properties/periodic_info", Json.obj("new" -> "periodicity")),
          PatchOp.Replace("/properties/env",           Json.obj("new" -> "env"))
        ),
        user = user
      )

      updatedLambda.properties.get("public") must_== "true"
      updatedLambda.properties.get("compressed") must_== "true"
      updatedLambda.properties.get("package_url") must_== "http://code.com/hello.js"
      updatedLambda.properties.get("handler") must_== "hello.js;hello"
      updatedLambda.properties.get("timeout") must_== "300"
      updatedLambda.properties.get("runtime") must_== "newruntime"
      updatedLambda.properties.get("cpus").toDouble must_== 2.0
      updatedLambda.properties.get("memory") must_== "2048"
      updatedLambda.properties.get("code_type") must_== "package"
      updatedLambda.properties.get("periodic_info") must_== Json.obj("new" -> "periodicity").toString
      updatedLambda.properties.get("env") must_== Json.obj("new" -> "env").toString

      there was one(mockJsonClient).put(
        uri = meq(s"/lambdas/${testLambda.id}"),
        payload = argThat((js: Option[JsValue]) => {
          js.get.as[LaserLambda] must_== LaserLambda(
            id = Some(testLambda.id.toString),
            eventFilter = None,
            public = true,
            provider = None,
            artifactDescription = LaserArtifactDescription(
              cpus = 2.0,
              memorySize = 2048,
              timeoutSecs = 300,
              artifactUri = Some("http://code.com/hello.js"),
              code = None,
              runtime = "newruntime",
              handler = "hello.js;hello",
              description = None,
              compressed = true,
              publish = false,
              role = "none",
              periodicInfo = Some(Json.obj("new" -> "periodicity")),
              headers = Map.empty
            )
          )
        }),
        hdrs = any,
        timeout = any
      )(any)
    }

    "patch against code lambda provider removing periodic_info" in new TestApplication {
      mockJsonClient.get(meq(s"/lambdas/${testLambda.id}"), any, any)(any) returns Future.successful({
        val mockResp = mock[WSResponse]
        mockResp.status returns 200
        mockResp.json returns Json.toJson(LaserLambda(
          Some(testLambda.id.toString),
          None,
          false,
          None,
          LaserArtifactDescription(None,"runtime","handler",0,0.0,periodicInfo = Some(Json.obj()))
        ))
        mockResp
      })
      mockJsonClient.put(meq(s"/lambdas/${testLambda.id}"), any, any, any)(any) returns Future.successful({
        val mockResp = mock[WSResponse]
        mockResp.status returns 200
      })

      val Success(updatedLambda) = lambdaMethods.patchLambdaHandler(
        r = testLambda,
        patch = PatchDocument(
          PatchOp.Replace("/properties/public",        true),
          PatchOp.Replace("/properties/code",          "ZnVuY3Rpb24gaGVsbG8oKSB7cmV0dXJuICJoZWxsbyI7fQ=="),
          PatchOp.Replace("/properties/handler",       "hello"),
          PatchOp.Replace("/properties/timeout",       300),
          PatchOp.Replace("/properties/runtime",       "newruntime"),
          PatchOp.Replace("/properties/cpus",          2.0),
          PatchOp.Replace("/properties/memory",        2048),
          PatchOp.Replace("/properties/code_type",     "inline"),
          PatchOp.Remove("/properties/periodic_info"),
          PatchOp.Replace("/properties/env",           Json.obj("new" -> "env"))
        ),
        user = user
      )

      updatedLambda.properties.get("public") must_== "true"
      updatedLambda.properties.get("code") must_== "ZnVuY3Rpb24gaGVsbG8oKSB7cmV0dXJuICJoZWxsbyI7fQ=="
      updatedLambda.properties.get("handler") must_== "hello"
      updatedLambda.properties.get("timeout") must_== "300"
      updatedLambda.properties.get("runtime") must_== "newruntime"
      updatedLambda.properties.get("cpus").toDouble must_== 2.0
      updatedLambda.properties.get("memory") must_== "2048"
      updatedLambda.properties.get("code_type") must_== "inline"
      updatedLambda.properties.get.get("periodic_info") must beNone
      updatedLambda.properties.get("env") must_== Json.obj("new" -> "env").toString

      there was one(mockJsonClient).put(
        uri = meq(s"/lambdas/${testLambda.id}"),
        payload = argThat((js: Option[JsValue]) => {
          js.get.as[LaserLambda] must_== LaserLambda(
            id = Some(testLambda.id.toString),
            eventFilter = None,
            public = true,
            provider = None,
            artifactDescription = LaserArtifactDescription(
              cpus = 2.0,
              memorySize = 2048,
              timeoutSecs = 300,
              artifactUri = None,
              code = Some("ZnVuY3Rpb24gaGVsbG8oKSB7cmV0dXJuICJoZWxsbyI7fQ=="),
              runtime = "newruntime",
              handler = "hello",
              description = None,
              compressed = false,
              publish = false,
              role = "none",
              periodicInfo = None,
              headers = Map.empty
            )
          )
        }),
        hdrs = any,
        timeout = any
      )(any)
    }

    "delete against LambdaMethods deletes lambdas" in new TestApplication {
      mockJsonClient.delete(meq(s"/lambdas/${testLambda.id}"), any, any)(any) returns Future.successful({
        val mockResp = mock[WSResponse]
        mockResp.status returns 200
      })

      val Success(_) = lambdaMethods.deleteLambdaHandler(
        r = testLambda,
        user = user
      )

      there was one(mockJsonClient).delete(
        uri = meq(s"/lambdas/${testLambda.id}"),
        hdrs = any,
        timeout = any
      )(any)
    }

  }

}
