package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.laser.LaserEndpoint
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.{HostConfig, JsonClient, ResourceIds}
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.patch.PatchDocument
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import controllers.SecurityResources
//import mockws.MockWS
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.JsonMatchers
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification.{BeforeAll, Scope}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.PlaySpecification
import play.api.mvc._
import play.api.mvc.Results._
import play.api.http.HttpVerbs._

import scala.util.Success

class LambdaMethodsSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll with JsonMatchers {

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
    val Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    Entitlements.setNewEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))
  }

  sequential

  abstract class FakeCaaSScope extends Scope {
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
      "provider" -> Json.obj(
        "name" -> testLambdaProvider.name,
        "id" -> testLambdaProvider.id.toString
      ).toString
    )))

//    val mockws = MockWS {
//      case (GET, lambdaUrl) if lambdaUrl == s"/lambdas/${testLambda.id}" =>
//        Ok(Json.obj())
//    }
//    val mockWebClient = new JsonClient(
//      config = HostConfig("tcp","whatever", Some(80)),
//      wsClient = Some(mockws)
//    )
//    mockProviderMethods.configureWebClient(argThat(
//      (provider: GestaltResourceInstance) => provider.id == testLambdaProvider.id
//    ), any) returns mockWebClient
  }
  
  trait TestApplication extends FakeCaaSScope {
  }

  "LambdaMethods" should {

    "patch against lambda provider" in new TestApplication {
      lambdaMethods.patchLambdaHandler(
        r = testLambda,
        patch = PatchDocument(
        ),
        user = user
      ) must beSuccessfulTry
    }.pendingUntilFixed("mockws not working, figure it out")

  }

}