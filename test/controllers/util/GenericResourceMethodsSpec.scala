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

    val testUrl = "http://some-laser.some-domain/lambdas/b2d51c3d-aaaf-4575-b29d-4f0cb52d53fc/invokeSync"
    val Success(providerWithDefaultEndpoint) = createInstance(ResourceIds.Provider, "test-provider", properties = Some(Map(
      "config" -> Json.obj(
        "endpoints" -> Json.arr(
          Json.obj(
            "default" -> true,
            "http" -> Json.obj(
              "url" -> testUrl
            )
          )
        ),
        "providerSpecificConfig" -> Json.obj(
          "password" -> "monkey",
          "url" -> "whatever"
        )
      ).toString
    )))

    val Some(rootOrg) = ResourceFactory.findById(ResourceIds.Org, dummyRootOrgId)
  }

  trait TestApplication extends TestScope {
  }

  "DefaultGenericProviderManager" should {

    "appropriately instantiate HttpGenericProvider classes for configured provider" in new TestApplication {
      val providerManager = new DefaultGenericProviderManager(mock[WSClient])
      providerManager.getProvider(providerWithDefaultEndpoint, "some-verb") must beASuccessfulTry(beSome(
        beAnInstanceOf[HttpGenericProvider]
          and (((_:GenericProvider).asInstanceOf[HttpGenericProvider].url) ^^ be_==(testUrl))
      ))
    }

    "use default when present" in new TestApplication {
      val defaultUrl = "http://default-url"
      val overrideUrl = "http://override-url"
      val Success(testProvider) = createInstance(ResourceIds.Provider, "test-provider", properties = Some(Map(
        "config" -> Json.obj(
          "endpoints" -> Json.arr(
            Json.obj(
              "actions" -> Seq("action1", "action2"),
              "http" -> Json.obj(
                "url" -> overrideUrl
              )
            ),
            Json.obj(
              "default" -> true,
              "http" -> Json.obj(
                "url" -> defaultUrl
              )
            )
          )
        ).toString
      )))

      val providerManager = new DefaultGenericProviderManager(mock[WSClient])
      providerManager.getProvider(testProvider, "some-action") must beASuccessfulTry(beSome(
        beAnInstanceOf[HttpGenericProvider]
          and (((_:GenericProvider).asInstanceOf[HttpGenericProvider].url) ^^ be_==(defaultUrl))
      ))
      providerManager.getProvider(testProvider, "action1") must beASuccessfulTry(beSome(
        beAnInstanceOf[HttpGenericProvider]
          and (((_:GenericProvider).asInstanceOf[HttpGenericProvider].url) ^^ be_==(overrideUrl))
      ))
      providerManager.getProvider(testProvider, "action2") must beASuccessfulTry(beSome(
        beAnInstanceOf[HttpGenericProvider]
          and (((_:GenericProvider).asInstanceOf[HttpGenericProvider].url) ^^ be_==(overrideUrl))
      ))
    }

    "return None when there is no match or default" in new TestApplication {
      val overrideUrl = "http://override-url"
      val Success(testProvider) = createInstance(ResourceIds.Provider, "test-provider", properties = Some(Map(
        "config" -> Json.obj(
          "endpoints" -> Json.arr(
            Json.obj(
              "actions" -> Seq("action1", "action2"),
              "http" -> Json.obj(
                "url" -> overrideUrl
              )
            )
          )
        ).toString
      )))

      val providerManager = new DefaultGenericProviderManager(mock[WSClient])
      providerManager.getProvider(testProvider, "some-action") must beASuccessfulTry(beNone)
    }

  }


  "HttpGenericProvider" in {

    "post to lambdas appropriately and parse Resource update semantics" in new TestApplication {
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
        provider = providerWithDefaultEndpoint,
        resource = Some(dummyResource),
        actionPayload = Some(actionPayload)
      )

      val response = await(httpProvider.invokeAction(inv))

      routeInvoke.timeCalled must_== 1

      // FINISH: more testing of the passed data


      // FINISH: more testing of the response
      response must beLeft[GestaltResourceInstance](
        ((_: GestaltResourceInstance).properties.get) ^^ havePairs(
          "foo" -> "new-bar"
        )
      )
    }

  }

}
