package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.genericactions.GenericProvider.RawInvocationResponse
import com.galacticfog.gestalt.meta.genericactions._
import com.galacticfog.gestalt.meta.test.{DbShutdown, ResourceScope}
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import controllers.SecurityResources
import org.specs2.matcher.JsonMatchers
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification._
import play.api.http.HeaderNames
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.Reads._
import play.api.libs.json.{JsObject, JsValue, Json, _}
import play.api.libs.ws.WSClient
import play.api.mvc.Action
import play.api.mvc.BodyParsers.parse
import play.api.mvc.Results.{Ok, Status, Unauthorized}
import play.api.test.{DefaultAwaitTimeout, FutureAwaits, PlaySpecification}
import mockws.{MockWS, Route}

import scala.util.Success

class GenericResourceMethodsSpec extends PlaySpecification
  with GestaltSecurityMocking with ResourceScope
  with BeforeAll with DbShutdown with JsonMatchers with JsonInput with DefaultAwaitTimeout with FutureAwaits {
  
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
  
  abstract class TestScope extends Scope {
    val mockProviderMethods = mock[ProviderMethods]
    val mockGestaltSecurityConfig = mock[GestaltSecurityConfig]
    val appBuilder =
      new GuiceApplicationBuilder()
        .disable[modules.ProdSecurityModule]
        .disable[modules.MetaDefaultSkuber]
        .disable[modules.MetaDefaultServices]
        .disable[modules.HealthModule]
        .bindings(
          bind(classOf[GestaltSecurityConfig]).toInstance(mockGestaltSecurityConfig),
          bind(classOf[ProviderMethods]).toInstance(mockProviderMethods)
        )
    val injector = appBuilder.injector()
    setInjector(injector)

    val Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    Entitlements.setNewResourceEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

    val testUrl = "http://some-laser.some-domain/lambdas/b2d51c3d-aaaf-4575-b29d-4f0cb52d53fc/invokeSync"
    val Success(providerWithDefaultEndpoint) = createInstance(ResourceIds.Provider, "test-provider", properties = Some(Map(
      "config" -> Json.obj(
        "env" -> Json.obj(
          "public" -> Json.obj(
            "SERVICE_HOST" -> "some-laser.some-domain",
            "SERVICE_PORT" -> "9000"
          )
        ),
        "services" -> Seq(
          Json.obj(
            "container_spec" -> Json.obj(
              "name" -> "some-service",
              "properties" -> Json.obj(
                "image" -> "some:image"
              )
            )
          )
        ),
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

  sequential

  "DefaultGenericProviderManager" should {

    "appropriately instantiate HttpGenericProvider classes for configured provider" in new TestScope {
      val providerManager = new DefaultGenericProviderManager(mock[WSClient])
      providerManager.getProvider(providerWithDefaultEndpoint, "some-verb", callerAuth = "fakeCreds") must beASuccessfulTry(beSome(
        beAnInstanceOf[HttpGenericProvider]
          and (((_:GenericProvider).asInstanceOf[HttpGenericProvider].url) ^^ be_==(testUrl))
      ))
    }

    "instantiate HttpGenericProvider with authentication support" in new TestScope {
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
      providerManager.getProvider(testProvider, "some-action", callerAuth = "fakeCreds") must beASuccessfulTry(beSome(
        beAnInstanceOf[HttpGenericProvider]
          and (((_:GenericProvider).asInstanceOf[HttpGenericProvider].url) ^^ be_==(defaultUrl))
          and (((_:GenericProvider).asInstanceOf[HttpGenericProvider].authHeader) ^^ beSome(authHeader))
      ))
    }

    "use default when present" in new TestScope {
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
      providerManager.getProvider(testProvider, "some-action", callerAuth = "fakeCreds") must beASuccessfulTry(beSome(
        beAnInstanceOf[HttpGenericProvider]
          and (((_:GenericProvider).asInstanceOf[HttpGenericProvider].url) ^^ be_==(defaultUrl))
      ))
      providerManager.getProvider(testProvider, "action1", callerAuth = "fakeCreds") must beASuccessfulTry(beSome(
        beAnInstanceOf[HttpGenericProvider]
          and (((_:GenericProvider).asInstanceOf[HttpGenericProvider].url) ^^ be_==(overrideUrl))
      ))
      providerManager.getProvider(testProvider, "action2", callerAuth = "fakeCreds") must beASuccessfulTry(beSome(
        beAnInstanceOf[HttpGenericProvider]
          and (((_:GenericProvider).asInstanceOf[HttpGenericProvider].url) ^^ be_==(overrideUrl))
      ))
    }

    "use default when present (reverse order)" in new TestScope {
      val defaultUrl = "http://default-url"
      val overrideUrl = "http://override-url"
      val Success(testProvider) = createInstance(ResourceIds.Provider, "test-provider", properties = Some(Map(
        "config" -> Json.obj(
          "endpoints" -> Json.arr(
            Json.obj(
              "default" -> true,
              "http" -> Json.obj(
                "url" -> defaultUrl
              )
            ),
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
      providerManager.getProvider(testProvider, "some-action", callerAuth = "fakeCreds") must beASuccessfulTry(beSome(
        beAnInstanceOf[HttpGenericProvider]
          and (((_:GenericProvider).asInstanceOf[HttpGenericProvider].url) ^^ be_==(defaultUrl))
      ))
      providerManager.getProvider(testProvider, "action1", callerAuth = "fakeCreds") must beASuccessfulTry(beSome(
        beAnInstanceOf[HttpGenericProvider]
          and (((_:GenericProvider).asInstanceOf[HttpGenericProvider].url) ^^ be_==(overrideUrl))
      ))
      providerManager.getProvider(testProvider, "action2", callerAuth = "fakeCreds") must beASuccessfulTry(beSome(
        beAnInstanceOf[HttpGenericProvider]
          and (((_:GenericProvider).asInstanceOf[HttpGenericProvider].url) ^^ be_==(overrideUrl))
      ))
    }

    "return None when there is no match or default" in new TestScope {
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
      providerManager.getProvider(testProvider, "some-action", callerAuth = "fakeCreds") must beASuccessfulTry(beNone)
    }

  }


  "HttpGenericProvider" in {

    "post appropriately to endpoints and parse Resource update semantics" in new TestScope {
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
      val httpProvider = new HttpGenericProvider(ws, testUrl, "POST")

      val inv = GenericActionInvocation(
        action = "noun.verb",
        metaAddress = "http://example.com",
        context = GenericActionContext(
          org = rootOrg,
          workspace = None,
          environment = None
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

    "use authentication header if configured" in new TestScope {
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
      val httpProvider = new HttpGenericProvider(ws, testUrl, "POST", authHeader = Some(testHeader))

      val inv = GenericActionInvocation(
        action = "noun.verb",
        metaAddress = "http://example.com",
        context = GenericActionContext(
          org = rootOrg,
          workspace = None,
          environment = None
        ),
        provider = providerWithDefaultEndpoint,
        resource = Some(dummyResource)
      )

      await(httpProvider.invokeAction(inv)) must beLeft[GestaltResourceInstance]
    }

    "abide by provider 'method'" in new TestScope {
      val Success(dummyResource) = createInstance(ResourceIds.Resource, "test-resource")

      val testHeader = "Bearer some-magic-token"
      val routeInvoke = Route {
        case (GET, testUrl) => Action { request =>
          if (request.headers.get(HeaderNames.AUTHORIZATION).contains(testHeader)) {
            Ok("got")
          }
          else Unauthorized("")
        }
      }
      val ws = MockWS(routeInvoke)
      val httpProvider = new HttpGenericProvider(ws, testUrl, "GET", authHeader = Some(testHeader))

      val inv = GenericActionInvocation(
        action = "noun.verb",
        metaAddress = "http://example.com",
        context = GenericActionContext(
          org = rootOrg,
          workspace = None,
          environment = None
        ),
        provider = providerWithDefaultEndpoint,
        resource = Some(dummyResource)
      )

      await(httpProvider.invokeAction(inv)) must beRight(RawInvocationResponse(
        Some(200), Some("text/plain; charset=utf-8"), Some("got")
      ))
    }

    "expand url template" in new TestScope {
      val Success(dummyResource) = createInstance(ResourceIds.Resource, "test-resource")

      val templateUrl = "http://<provider.properties.config.env.public.SERVICE_HOST>:<provider.properties.config.env.public.SERVICE_PORT>/streams/<resource.id>/status"

      val testHeader = "Bearer some-magic-token"
      val routeInvoke = Route {
        case (GET, url) if url == s"http://some-laser.some-domain:9000/streams/${dummyResource.id}/status" => Action { request =>
          if (request.headers.get(HeaderNames.AUTHORIZATION).contains(testHeader)) {
            Ok("got")
          }
          else Unauthorized("")
        }
      }
      val ws = MockWS(routeInvoke)
      val httpProvider = new HttpGenericProvider(ws, templateUrl, "GET", authHeader = Some(testHeader))

      val inv = GenericActionInvocation(
        action = "noun.verb",
        metaAddress = "http://example.com",
        context = GenericActionContext(
          org = rootOrg,
          workspace = None,
          environment = None
        ),
        provider = providerWithDefaultEndpoint,
        resource = Some(dummyResource)
      )

      await(httpProvider.invokeAction(inv)) must beRight(RawInvocationResponse(
        Some(200), Some("text/plain; charset=utf-8"), Some("got")
      ))
    }

    "expand url template (query params)" in new TestScope {
      val Success(dummyResource) = createInstance(ResourceIds.Resource, "test-resource")

      val templateUrl = "http://host/resources/<queryParams.p1>"

      val testHeader = "Bearer some-magic-token"
      val routeInvoke = Route {
        case (GET, url) if url == s"http://host/resources/v1" => Action { request =>
          if (request.headers.get(HeaderNames.AUTHORIZATION).contains(testHeader)) {
            Ok("got")
          }
          else Unauthorized("")
        }
      }
      val ws = MockWS(routeInvoke)
      val httpProvider = new HttpGenericProvider(ws, templateUrl, "GET", authHeader = Some(testHeader))

      val inv = GenericActionInvocation(
        action = "noun.verb",
        metaAddress = "http://example.com",
        context = GenericActionContext(
          org = rootOrg,
          workspace = None,
          environment = None
        ),
        provider = providerWithDefaultEndpoint,
        resource = Some(dummyResource),
        queryParams = Map(
          "p1" -> Seq("v1","v1.2"),
          "p2" -> Seq("v2")
        )
      )

      await(httpProvider.invokeAction(inv)) must beRight(RawInvocationResponse(
        Some(200), Some("text/plain; charset=utf-8"), Some("got")
      ))
    }

    "40x errors from endpoints should result in BadRequestException" in new TestScope {
      var status: Int = 398
      val routeInvoke = Route {
        case _ => Action(parse.json) { request =>
          status = status + 1
          new Status(status)
        }
      }
      val ws = MockWS(routeInvoke)
      val httpProvider = new HttpGenericProvider(ws, testUrl, "POST")

      val inv = GenericActionInvocation(
        action = "noun.verb",
        metaAddress = "http://example.com",
        context = GenericActionContext(
          org = rootOrg,
          workspace = None,
          environment = None
        ),
        provider = providerWithDefaultEndpoint
      )

      await(httpProvider.invokeAction(inv)) must throwA[RuntimeException]("399")
      await(httpProvider.invokeAction(inv)) must throwA[RuntimeException]("400")
      await(httpProvider.invokeAction(inv)) must throwA[RuntimeException]("401")
      await(httpProvider.invokeAction(inv)) must throwA[RuntimeException]("402")
      await(httpProvider.invokeAction(inv)) must throwA[RuntimeException]("403")
    }

    "parse custom-response semantics from endpoints " in new TestScope {
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
      val httpProvider = new HttpGenericProvider(ws, testUrl, "POST")

      val inv = GenericActionInvocation(
        action = "noun.verb",
        metaAddress = "http://example.com",
        context = GenericActionContext(
          org = rootOrg,
          workspace = None,
          environment = None
        ),
        provider = providerWithDefaultEndpoint,
        resource = Some(dummyResource),
        actionPayload = Some(actionPayload)
      )

      val response = await(httpProvider.invokeAction(inv))

      routeInvoke.timeCalled must_== 1

      response must beRight(RawInvocationResponse(
        Some(200), Some("application/json"), Some(customResponse.toString)
      ))
    }

    "parse failure semantics" in new TestScope {
      val Success(dummyResource) = createInstance(ResourceIds.Resource, "test-resource")
      val failureMessage = "some failure message"
      val customResponse = Json.obj(
        "actionFailed" -> failureMessage
      )
      val routeInvoke = Route {
        case (POST, testUrl) => Action(parse.json) { response => Ok(customResponse) }
      }
      val ws = MockWS(routeInvoke)
      val httpProvider = new HttpGenericProvider(ws, testUrl, "POST")

      val inv = GenericActionInvocation(
        action = "noun.verb",
        metaAddress = "http://example.com",
        context = GenericActionContext(
          org = rootOrg,
          workspace = None,
          environment = None
        ),
        provider = providerWithDefaultEndpoint,
        resource = Some(dummyResource),
        actionPayload = Some(Json.obj(
          "foo" -> "bar"
        ))
      )

      await(httpProvider.invokeAction(inv)) must throwA[RuntimeException](failureMessage)
    }

  }

  import com.galacticfog.gestalt.data.TypeFactory
  import com.galacticfog.gestalt.data.bootstrap.SystemType
  
  //lookupProvider(payload: JsValue, resourceType: UUID, providerType: UUID)
  "lookupProvider" should {
    
    "succeed when given good information" in {
      
      val providerTypeId = uuid()

      SystemType(
        dummyRootOrgId, dummyOwner,
        typeId   = providerTypeId,
        typeName = providerTypeId.toString,
        extend = Some(ResourceIds.Provider)
      ).save()
      
      val tpe = TypeFactory.findById(providerTypeId)
      tpe must beSome      
      
      val provider = createInstance(providerTypeId, uuid.toString)
      provider must beSuccessfulTry
      
      val providerId = provider.get.id
      
      val impl = new GenericResourceMethodsImpl(new DefaultGenericProviderManager(mock[WSClient]))
      val payload = Json.obj("name" -> "foo", "properties" -> Json.obj("provider" -> providerId.toString))
      
      await(impl.lookupProvider(payload, uuid(), providerTypeId)).id === providerId
    }
    
    "fail when payload is missing `properties.provider`" in {
      val providerTypeId = uuid()

      SystemType(
        dummyRootOrgId, dummyOwner,
        typeId   = providerTypeId,
        typeName = providerTypeId.toString,
        extend = Some(ResourceIds.Provider)
      ).save()
      
      val tpe = TypeFactory.findById(providerTypeId)
      tpe must beSome      
      
      val provider = createInstance(providerTypeId, uuid.toString)
      provider must beSuccessfulTry
      
      val providerId = provider.get.id
      
      val impl = new GenericResourceMethodsImpl(new DefaultGenericProviderManager(mock[WSClient]))
      
      // Missing 'properties.provider'
      val payload = Json.obj("name" -> "foo")
      
      await(impl.lookupProvider(payload, uuid(), providerTypeId)) must throwA[BadRequestException]
    }
    
    "fail when there is no resource with the given provider ID" in {
      val providerTypeId = uuid()

      SystemType(
        dummyRootOrgId, dummyOwner,
        typeId   = providerTypeId,
        typeName = providerTypeId.toString,
        extend = Some(ResourceIds.Provider)
      ).save()
      
      val tpe = TypeFactory.findById(providerTypeId)
      tpe must beSome      
      
      val providerId = uuid()
      
      val impl = new GenericResourceMethodsImpl(new DefaultGenericProviderManager(mock[WSClient]))
      
      // Missing 'properties.provider'
      val payload = Json.obj("name" -> "foo")
      
      await(impl.lookupProvider(payload, uuid(), providerTypeId)) must throwA[BadRequestException]
    }
    
    "fail if providerType is not a sub-type of provider" in {
      val providerTypeId = uuid()

      // This type does NOT extend Provider
      SystemType(
        dummyRootOrgId, dummyOwner,
        typeId   = providerTypeId,
        typeName = providerTypeId.toString
      ).save()
      
      val tpe = TypeFactory.findById(providerTypeId)
      tpe must beSome      
      
      val notProviderInstance = createInstance(providerTypeId, uuid.toString)
      notProviderInstance must beSuccessfulTry
      
      // We have the ID of a valid, existing resource, but it is NOT a sub-type of Provider.
      val providerId = notProviderInstance.get.id
      
      val impl = new GenericResourceMethodsImpl(new DefaultGenericProviderManager(mock[WSClient]))
      val payload = Json.obj("name" -> "foo", "properties" -> Json.obj("provider" -> providerId.toString))
      
      await(impl.lookupProvider(payload, uuid(), providerTypeId)) must throwA[BadRequestException]
      
    }
    
  }
  
}
