package controllers

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.ResourcePath
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.patch._
import controllers.util._
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.JsonMatchers
import org.specs2.mock.Mockito
import org.specs2.specification._
import play.api.inject.bind
import play.api.libs.json._
import play.api.test._

import scala.util.{Success, Try}

import com.galacticfog.gestalt.meta.api.sdk.GestaltConfigurationManager
import com.galacticfog.gestalt.data.PostgresConfigManager


class DeleteControllerSpec extends PlaySpecification with GestaltProviderMocking with JsonMatchers with ResourceScope with BeforeAll with Mockito {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    pristineDatabase()
    val Success(_) = Ents.createNewMetaUser(user, dummyRootOrgId, rootOwnerLink(), user.account,
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

  abstract class FakeSecurity extends WithDb(containerApp(
    additionalBindings = Seq(
      bind(classOf[LambdaMethods]).toInstance(mockLambdaMethods),
      bind(classOf[GatewayMethods]).toInstance(mockGatewayMethods)
      
    )
  ))

  trait TestApplication extends FakeSecurity {
    val Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")

    Ents.setNewResourceEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

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
    val Success(testLambda) = createInstance(
      ResourceIds.Lambda,
      "test-lambda",
      properties = Some(Map(
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
      )),
      parent = Some(testEnv.id)
    )
    val Success(testApi) = createInstance(
      ResourceIds.Api,
      "test-api",
      properties = Some(Map(
      )),
      parent = Some(testEnv.id)
    )
    val Success(testEndpoint) = createInstance(
      ResourceIds.ApiEndpoint,
      "test-endpoint",
      properties = Some(Map(
        "resource" -> "/the/path"
      )),
      parent = Some(testEnv.id)
    )
  }

  "DeleteController" should {

    "use LambdaMethods for external lambda delete" in new TestApplication {

      mockLambdaMethods.deleteLambdaHandler(any) returns Try(())

      val Some(result) = route(app,fakeAuthRequest(
        DELETE,
        s"/root/environments/${testEnv.id}/lambdas/${testLambda.id}", testCreds
      ))

      status(result) must equalTo(NO_CONTENT)

      there was one(mockLambdaMethods).deleteLambdaHandler(
        r = argThat( (r: GestaltResourceInstance) => r.id == testLambda.id )
      )
    }

    "use GatewayMethods for external apiendpoint delete" in new TestApplication {

      mockGatewayMethods.deleteEndpointHandler(any) returns Try(())

      val Some(result) = route(app,fakeAuthRequest(
        DELETE,
        s"/root/environments/${testEnv.id}/apiendpoints/${testEndpoint.id}", testCreds
      ))

      status(result) must equalTo(NO_CONTENT)

      there was one(mockGatewayMethods).deleteEndpointHandler(
        r = argThat( (r: GestaltResourceInstance) => r.id == testEndpoint.id )
      )
    }

    "use GatewayMethods for external api delete" in new TestApplication {

      mockGatewayMethods.deleteApiHandler(any) returns Try(())

      val Some(result) = route(app,fakeAuthRequest(
        DELETE,
        s"/root/environments/${testEnv.id}/apis/${testApi.id}?force=true", testCreds
      ))

      status(result) must equalTo(NO_CONTENT)

      there was one(mockGatewayMethods).deleteApiHandler(
        r = argThat( (r: GestaltResourceInstance) => r.id == testApi.id )
      )
    }

  }
  
}

