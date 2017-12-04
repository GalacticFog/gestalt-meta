package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.genericactions.GenericProvider.RawInvocationResponse
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
import play.api.mvc.Results.{Ok, Status, Unauthorized}
import play.api.test.{DefaultAwaitTimeout, FutureAwaits, PlaySpecification}
import play.api.mvc.BodyParsers.parse
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.http.HeaderNames

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

    "instantiate HttpGenericProvider with authentication support" in new TestApplication {
      val defaultUrl = "http://default-url"
      val authHeader = "Bearer magic-token"
      val Success(testProvider) = createInstance(ResourceIds.Provider, "test-provider", properties = Some(Map(
        "config" -> Json.obj(
          "endpoints" -> Json.arr(
            Json.obj(
              "default" -> true,
              "http" -> Json.obj(
                "url" -> defaultUrl,
                "authentication" -> authHeader
              )
            )
          )
        ).toString
      )))

      val providerManager = new DefaultGenericProviderManager(mock[WSClient])
      providerManager.getProvider(testProvider, "some-action") must beASuccessfulTry(beSome(
        beAnInstanceOf[HttpGenericProvider]
          and (((_:GenericProvider).asInstanceOf[HttpGenericProvider].url) ^^ be_==(defaultUrl))
          and (((_:GenericProvider).asInstanceOf[HttpGenericProvider].authHeader) ^^ beSome(authHeader))
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

    "post appropriately to endpoints and parse Resource update semantics" in new TestApplication {
      val Success(dummyResource) = createInstance(ResourceIds.Resource, "test-resource")
      val actionPayload = Json.obj(
        "foo" -> "bar"
      )

      var postBody: JsValue = null
      val routeInvoke = Route {
        case (POST, testUrl) => Action(parse.json) { request =>
          postBody = request.body
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
      (postBody \ "provider" \ "properties" \ "config" \ "providerSpecificConfig").asOpt[JsObject] must beSome(Json.obj(
        "password" -> "monkey",
        "url" -> "whatever"
      ))
      (postBody \ "action").asOpt[String] must beSome("noun.verb")
      (postBody \ "actionPayload").asOpt[JsObject] must beSome(actionPayload)
      (postBody \ "context" \ "org" \ "name").asOpt[String] must beSome("root")
      (postBody \ "resource" \ "id").asOpt[UUID] must beSome(dummyResource.id)

      response must beLeft[GestaltResourceInstance](
        ((_: GestaltResourceInstance).properties.get) ^^ havePairs(
          "foo" -> "new-bar"
        )
      )
    }

    "use authentication header if configured" in new TestApplication {
      val Success(dummyResource) = createInstance(ResourceIds.Resource, "test-resource")

      val testHeader = "Bearer some-magic-token"
      val routeInvoke = Route {
        case (POST, testUrl) => Action(parse.json) { request =>
          if (request.headers.get(HeaderNames.AUTHORIZATION).contains(testHeader)) {
            val resource = (request.body \ "resource").as[JsObject]
            Ok(resource)
          }
          else Unauthorized("")
        }
      }
      val ws = MockWS(routeInvoke)
      val httpProvider = new HttpGenericProvider(ws, testUrl, authHeader = Some(testHeader))

      val inv = GenericActionInvocation(
        action = "noun.verb",
        context = GenericActionContext(
          org = rootOrg,
          workspace = None,
          environment = None,
          queryParams = Map.empty
        ),
        provider = providerWithDefaultEndpoint,
        resource = Some(dummyResource)
      )

      await(httpProvider.invokeAction(inv)) must beLeft[GestaltResourceInstance]
    }

    "40x errors from endpoints should result in BadRequestException" in new TestApplication {
      var status: Int = 398
      val routeInvoke = Route {
        case _ => Action(parse.json) { request =>
          status = status + 1
          new Status(status)
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
        provider = providerWithDefaultEndpoint
      )

      await(httpProvider.invokeAction(inv)) must throwA[BadRequestException]("399")
      await(httpProvider.invokeAction(inv)) must throwA[BadRequestException]("400")
      await(httpProvider.invokeAction(inv)) must throwA[BadRequestException]("401")
      await(httpProvider.invokeAction(inv)) must throwA[BadRequestException]("402")
      await(httpProvider.invokeAction(inv)) must throwA[BadRequestException]("403")
    }

    "parse custom-response semantics from endpoints " in new TestApplication {
      val Success(dummyResource) = createInstance(ResourceIds.Resource, "test-resource")
      val actionPayload = Json.obj(
        "foo" -> "bar"
      )

      var postBody: JsValue = null
      val customResponse = Json.obj(
        "custom" -> "response"
      )
      val routeInvoke = Route {
        case (POST, testUrl) => Action(parse.json) { request =>
          postBody = request.body
          Ok(customResponse)
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

      response must beRight(RawInvocationResponse(
        Some(200), Some("application/json; charset=utf-8"), Some(customResponse.toString)
      ))
    }

    "parse failure semantics" in new TestApplication {
      val Success(dummyResource) = createInstance(ResourceIds.Resource, "test-resource")
      val failureMessage = "some failure message"
      val customResponse = Json.obj(
        "actionFailed" -> failureMessage
      )
      val routeInvoke = Route {
        case (POST, testUrl) => Action(parse.json) { response => Ok(customResponse) }
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
        actionPayload = Some(Json.obj(
          "foo" -> "bar"
        ))
      )

      await(httpProvider.invokeAction(inv)) must throwA[BadRequestException](failureMessage)
    }

  }

  "GenericActionContext" should {

    "render query parameters" in new TestApplication {
      val json = GenericActionContext(
        org = rootOrg,
        workspace = None,
        environment = None,
        queryParams = Map(
          "q1" -> Seq("1"),
          "q2" -> Seq("2a", "2b")
        )
      ).toJson
      (json \ "queryParams" \ "q1").as[Seq[String]] must containTheSameElementsAs(Seq("1"))
      (json \ "queryParams" \ "q2").as[Seq[String]] must containTheSameElementsAs(Seq("2a", "2b"))
    }

  }

}