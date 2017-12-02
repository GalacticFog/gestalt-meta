package controllers.util

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.{JsonClient, ResourceIds}
import com.galacticfog.gestalt.meta.genericactions._
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import controllers.SecurityResources
import mockws.{MockWS, Route}
import org.specs2.matcher.JsonMatchers
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification.{BeforeAll, Scope}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.libs.ws.WSClient
import play.api.mvc.Action
import play.api.mvc.Results.Ok
import play.api.test.{DefaultAwaitTimeout, FutureAwaits, PlaySpecification}
import play.api.mvc.BodyParsers.parse
import play.api.libs.json._
import play.api.libs.json.Reads._

import scala.util.Success

class GenericResourceMethodsSpec extends PlaySpecification
  with GestaltSecurityMocking with ResourceScope
  with BeforeAll with JsonMatchers with JsonInput with DefaultAwaitTimeout with FutureAwaits {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    pristineDatabase()
    val Success(_) = Ents.createNewMetaUser(user, dummyRootOrgId, user.account,
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

  abstract class TestScope extends Scope {
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

    val Some(rootOrg) = ResourceFactory.findById(ResourceIds.Org, dummyRootOrgId)

//    val lambdaMethods = injector.instanceOf[LambdaMethods]

//    val Success(testLambda) = createInstance(ResourceIds.Lambda, "test-lambda", properties = Some(Map(
//      "public" -> "true",
//      "cpus" -> "0.1",
//      "memory" -> "128",
//      "code_type" -> "inline",
//      "timeout" -> "30",
//      "handler" -> "blah;blah",
//      "headers" -> Json.obj(
//        "Existing-Header" -> "ExistingHeaderValue",
//        "Remove-Header" -> "Nobody Cares What's Here"
//      ).toString,
//      "runtime" -> "custom",
//      "periodic_info" -> "{}",
//      "provider" -> Json.obj(
//        "name" -> testLambdaProvider.name,
//        "id" -> testLambdaProvider.id.toString
//      ).toString
//    )))

//    val mockJsonClient = mock[JsonClient]
//    mockProviderMethods.configureWebClient(argThat(
//      (provider: GestaltResourceInstance) => provider.id == testLambdaProvider.id
//    ), any) returns mockJsonClient

  }

  trait TestApplication extends TestScope {
  }

  "DefaultGenericProviderManager" should {

    "appropriately instantiate HttpGenericProvider classes for configured provider" in new TestApplication {
      val testUrl = "http://some-laser.some-domain/lambdas/b2d51c3d-aaaf-4575-b29d-4f0cb52d53fc/invokeSync"

      val Success(testProviderResource) = createInstance(ResourceIds.Provider, "test-provider", properties = Some(Map(
        "config" -> Json.obj(
          "endpoint" -> Json.obj(
            "http" -> Json.obj(
              "url" -> testUrl
            )
          )
        ).toString
      )))

      val providerManager = new DefaultGenericProviderManager(mock[WSClient])
      providerManager.getProvider(testProviderResource) must beASuccessfulTry(
        beAnInstanceOf[HttpGenericProvider]
          and (((_:GenericProvider).asInstanceOf[HttpGenericProvider].url) ^^ be_==(testUrl))
      )
    }

  }


  "HttpGenericProvider" in {

    "post to lambdas appropriately and parse Resource update semantics" in new TestApplication {
      val testUrl = "http://some-laser.some-domain/lambdas/b2d51c3d-aaaf-4575-b29d-4f0cb52d53fc/invokeSync"
      val Success(testProviderResource) = createInstance(ResourceIds.Provider, "test-provider", properties = Some(Map(
        "config" -> Json.obj(
          "endpoint" -> Json.obj(
            "http" -> Json.obj(
              "url" -> testUrl
            )
          ),
          "providerSpecificConfig" -> Json.obj(
            "password" -> "monkey",
            "url" -> "whatever"
          )
        ).toString
      )))
      val Success(dummyResource) = createInstance(ResourceIds.Resource, "test-resource")
      val actionPayload = Json.obj(
        "foo" -> "bar"
      )

      var putBody: JsValue = null
      val routeInvoke = Route {
        case (POST, testUrl) => Action(parse.json) { request =>
          putBody = request.body
          val resource = (request.body \ "resource").as[JsObject]
          val updated = resource.transform(
            (__ \ 'properties).json.update(
              __.read[JsObject].map(o => o ++  Json.obj("foo" -> "new-bar"))
            )
          ).get
          Ok(updated)
        }
      }
      val ws = MockWS(routeInvoke)
      val httpProvider = new HttpGenericProvider(ws, testUrl)

      val inv = GenericActionInvocation(
        action = "noun.verb",
        context = GenericActionContext(
          org = rootOrg,
          workspace = None,
          environment = None,
          queryParams = Map.empty
        ),
        provider = testProviderResource,
        resource = Some(dummyResource),
        actionPayload = Some(actionPayload)
      )

      val response = await(httpProvider.invokeAction(inv))

      routeInvoke.timeCalled must_== 1

      // FINISH: more testing of the passed data
      (putBody \ "action").asOpt[String] must beSome("noun.verb")

      // FINISH: more testing of the response
      response must beLeft[GestaltResourceInstance](
        ((_: GestaltResourceInstance).properties.get) ^^ havePairs(
          "foo" -> "new-bar"
        )
      )
    }

  }

}
