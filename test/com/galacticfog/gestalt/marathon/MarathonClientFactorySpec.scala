package com.galacticfog.gestalt.marathon

import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.testkit.{TestActor, TestActorRef, TestKit, TestProbe}
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.specification.{BeforeAll, Scope}
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc.BodyParsers.parse
import play.api.mvc.Results._
import play.api.mvc._
import play.api.test.PlaySpecification
import services.DCOSAuthTokenActor.{DCOSAuthTokenRequest, DCOSAuthTokenResponse}
import services.MarathonService.Properties
import services.{DCOSAuthTokenActor, DefaultMarathonClientFactory, MarathonService}

import scala.language.reflectiveCalls
import scala.util.Success

@RunWith(classOf[JUnitRunner])
class MarathonClientFactorySpec extends PlaySpecification with ResourceScope with BeforeAll {

  val authToken = "eyJhbGciOiJSUzI1NiIsImtpZCI6InNlY3JldCIsInR5cCI6IkpXVCJ9.eyJ1aWQiOiJkZW1vX2dlc3RhbHRfbWV0YSJ9.RP5MhJPey2mDXOJlu1GFcQ_TlWZzYnr_6N7AwDbB0jqJC3bsLR8QxKZVbzk_JInwO5QN_-BVK5bvxP5zyo4KhVotsugH5eP_iTSDyyx7iKWOK4oPmFlJglaXGRE_KEuySAeZCNTnDIrfUWnB21WwS92MGr6B4rFZ-IzVSmygzO-LgxM-ZU9_b9kyKLOUXcQLgHFLY-qJMWou98dTv36lhjqx65iKQ5PT53KjGtL6OQ-1vqXse5ynCJGsXk3HBXV4P_w42RJBIAWiIbsUfgN85sGTVPvtHO-o-GJMknf7G0FiwfGtsYS3n05kirNIwsZS54RX03TNlxq0Vg48eWGZKQ"
  val testServiceId = "meta-dcos-provider"
  val testPrivateKey = "-----BEGIN PRIVATE KEY-----\nMIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC9OzC0iseKnsqd\nu82KvTav6q+j4MoSS3mGGPZIA2JaD/cMjpzBtaaOxIbcyLWt2M8hwdO3TLXCZiW2\nybz2Koeo3+vNphnO7U4ZggSIuM+RYfhUUnQ79yiYKmL3z93HRrvZBlulG3yOFo5y\n30IFKqyt2QKlPy3ObCtZYwT4opYNnkev/pubtOjsjdkU9/u088eiLfVHwSwpBxjG\n2wbpFVGyN3p55UHW3K6QUrUw8B7EOF2A5EXzgR5GmAgL6SjuzEdghumqdMcSxGoE\n4pL3Y6LHer391ITdxO819o0i3cfglvgXxFGZSsiRVV89X15n8pEbP73cD3sRxnwe\nIwW860ZnAgMBAAECggEAIKUXb+4JIobmWXPOr8KYrpyEFHdxJNrUaifgROggjXz3\nl7j6nghiZXrN8UTG4ujmQuKXTaX0LUdF9lSzPpxzrtSCb4XaKfKSaKAffB614FTQ\nbGuVFcs7u5SEYk//6KLxQS1xnfgx8qk9hd+yGgYUqCEp7awKkPPkPpVwhBw4WrzJ\nkYxJ3bIT7j3svTr5uhno7cFso5jhfFyMA7PruHGNfyOWLIgzgw5qwRUK1WLMyk88\nJivrDRbvuskWK7pxvLrRQ/VA34LvGKLroj9Gqw9HIDGbY526PPjFo/uDq8ErHBsQ\nBdoagN6VihX5YjXdi3eF8mIcaFYBOQj6zB+Kfmkc0QKBgQDjkIemfgpHEMcRsinm\ni0WLlZGD8hjFTNku1Pki5sFffXcHR+FImrEUXL/NqJr8iqIeJ+1cx3OAiBm8PHh4\nl+LYz4H2TlvIEEURmOwLiBbh49N4o7T9the+PluDGLsZ9ka3AGHP1LBcvwYJdf7v\nubK3eky1QQSI5Ce6+uayU76QFQKBgQDU4G4j2eAIVTDQ0xMfJYXFaIh2eVqTkv83\nPeskWhAQcPUKzyX7bPHSdSbx+91ZW5iL8GX4DFp+JBiQFSqNq1tqhLwz9xHTxYrj\nGvi6MUJ4LCOihbU+6JIYuOdxq3govxtnJ+lE4cmwr5Y4HM1wx2dxba9EsItLrzkj\nHGPNDJ6fiwKBgCXgPHO9rsA9TqTnXon8zEp7TokDlpPgQpXE5OKmPbFDFLilgh2v\ngaG9/j6gvYsjF/Ck/KDgoZzXClGGTxbjUOJ9R0hTqnsWGijfpwoUUJqwbNY7iThh\nQnprrpeXWizsDMEQ0zbgU6pcMQkKFrCX2+Ml+/Z/J94Q+3vnntY3khQxAoGAdUkh\n5cbI1E57ktJ4mpSF23n4la3O5bf7vWf0AhdM+oIBwG7ZMmmX4qiBSJnIHs+EgLV2\nuO+1fAJPNjMzOtLKjymKt+bMf607FF1r5Mn3IVbQW17nuT1SISTe/5XFok2Iv5ER\nyM3N3fcgANJ9rkFvEOOpyWKrnItyI5IkunjVfHkCgYEAjmAjQOQt5eCO9kGitL7X\ntQGn8TWWHRCjMm1w3ith7bPp11WrdeyfNuUAB7weQjk2qjAIKTOGWtIRqc36OLPA\nkwF1GDyFXvLqJej/2ZLfytyjhetLAQnRL0qOgCi7EU5+YLXuYnn7zPEJgrR3ogX4\n4rvG4NIQ8wG0sEUTnr06nck=\n-----END PRIVATE KEY-----"
  val testPublicKey  = "-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvTswtIrHip7KnbvNir02\nr+qvo+DKEkt5hhj2SANiWg/3DI6cwbWmjsSG3Mi1rdjPIcHTt0y1wmYltsm89iqH\nqN/rzaYZzu1OGYIEiLjPkWH4VFJ0O/comCpi98/dx0a72QZbpRt8jhaOct9CBSqs\nrdkCpT8tzmwrWWME+KKWDZ5Hr/6bm7To7I3ZFPf7tPPHoi31R8EsKQcYxtsG6RVR\nsjd6eeVB1tyukFK1MPAexDhdgORF84EeRpgIC+ko7sxHYIbpqnTHEsRqBOKS92Oi\nx3q9/dSE3cTvNfaNIt3H4Jb4F8RRmUrIkVVfPV9eZ/KRGz+93A97EcZ8HiMFvOtG\nZwIDAQAB\n-----END PUBLIC KEY-----"
  val testDcosUrl = "https://m1.dcos"
  val marathonBaseUrl = "https://m1.dcos/service/marathon"

  override def beforeAll(): Unit = pristineDatabase()

  abstract class WithProviderConfig( providerAuth: JsValue = JsNull,
                                     permissiveHttps: JsValue = JsNull,
                                     marathonUrl: Option[String] = Some(marathonBaseUrl),
                                     dcosUrl: Option[String] = None,
                                     secretUrl: Option[String] = None,
                                     secretSupport: Option[Boolean] = None,
                                     secretStore: Option[String] = None
                                   ) extends TestKit(ActorSystem("MySpec")) with Scope {
    val Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    val Success(testProvider) = createInstance(ResourceIds.DcosProvider, "test-provider",
      parent = Some(testEnv.id),
      properties = Some(Map(
        "config" -> Json.obj(
          Properties.AUTH_CONFIG -> providerAuth,
          Properties.MARATHON_BASE_URL -> marathonUrl.map(JsString(_)).getOrElse[JsValue](JsNull),
          Properties.DCOS_BASE_URL -> dcosUrl.map(JsString(_)).getOrElse[JsValue](JsNull),
          Properties.SECRET_BASE_URL -> secretUrl.map(JsString(_)).getOrElse[JsValue](JsNull),
          Properties.ACCEPT_ANY_CERT -> permissiveHttps,
          Properties.SECRET_SUPPORT  -> secretSupport.map(JsBoolean(_)).getOrElse[JsValue](JsNull),
          Properties.SECRET_STORE     -> secretStore.map(JsString(_)).getOrElse[JsValue](JsNull)
        ).toString
      ))
    )
    val strictClient = mock[WSClient]
    val permissiveClient = mock[WSClient]
  }

  "DefaultMarathonClientFactory" should {

    "not support secrets by default" in new WithProviderConfig(
      secretSupport = None
    ) {
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, mock[ActorRef])
      val client= await(mcf.getClient(testProvider))
      client.secretBaseUrl must beNone
    }

    "not support secrets if explicitly disabled" in new WithProviderConfig(
      secretSupport = Some(false)
    ) {
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, mock[ActorRef])
      val client= await(mcf.getClient(testProvider))
      client.secretBaseUrl must beNone
    }

    "provider default marathon URL if not explicitly configured" in new WithProviderConfig(
      marathonUrl = None,
      dcosUrl = Some("https://dcos-cluster.mycompany.com:443")
    ) {
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, mock[ActorRef])
      val client= await(mcf.getClient(testProvider))
      client.marathonBaseUrl must_== "https://dcos-cluster.mycompany.com:443/service/marathon"
    }

    "provider default marathon URL if not explicitly configured (with slash prefix)" in new WithProviderConfig(
      marathonUrl = None,
      dcosUrl = Some("https://dcos-cluster.mycompany.com/")
    ) {
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, mock[ActorRef])
      val client= await(mcf.getClient(testProvider))
      client.marathonBaseUrl must_== "https://dcos-cluster.mycompany.com/service/marathon"
    }

    "provide default secret URL and store if secrets are enabled" in new WithProviderConfig(
      secretSupport = Some(true),
      dcosUrl = Some("https://dcos-cluster.mycompany.com:443")
    ) {
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, mock[ActorRef])
      val client= await(mcf.getClient(testProvider))
      client.secretBaseUrl must beSome("https://dcos-cluster.mycompany.com:443/secrets/v1")
      client.secretStore must_== MarathonClient.DEFAULT_SECRET_STORE
    }

    "support overriding secret store" in new WithProviderConfig(
      secretSupport = Some(true),
      dcosUrl = Some("https://dcos-cluster.mycompany.com:443"),
      secretStore = Some("another-store")
    ) {
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, mock[ActorRef])
      val client= await(mcf.getClient(testProvider))
      client.secretBaseUrl must beSome("https://dcos-cluster.mycompany.com:443/secrets/v1")
      client.secretStore must_== "another-store"
    }

    "provide default secret URL if secrets are enabled" in new WithProviderConfig(
      secretSupport = Some(true),
      dcosUrl = Some("https://dcos-cluster.mycompany.com/")
    ) {
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, mock[ActorRef])
      val client= await(mcf.getClient(testProvider))
      client.secretBaseUrl must beSome("https://dcos-cluster.mycompany.com/secrets/v1")
    }

    "provide explicit secret URL if secrets are enabled" in new WithProviderConfig(
      secretSupport = Some(true),
      secretUrl = Some("https://master.mesos:1111")
    ) {
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, mock[ActorRef])
      val client= await(mcf.getClient(testProvider))
      client.secretBaseUrl must beSome("https://master.mesos:1111")
    }

    "not provide secret URL if missing secretUrl or dcosUrl" in new WithProviderConfig(
      secretSupport = Some(true),
      secretUrl = None,
      dcosUrl = None
    ) {
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, mock[ActorRef])
      val client= await(mcf.getClient(testProvider))
      client.secretBaseUrl must beNone
    }

    "request auth token from DCOSAuthTokenActor on getClient for authed provider" in new WithProviderConfig(
      providerAuth = Json.obj(
        "scheme" -> "acs",
        "service_account_id" -> testServiceId,
        "private_key" -> testPrivateKey,
        "dcos_base_url" -> testDcosUrl
      )
    ) {
      val probe = TestProbe()
      probe.setAutoPilot(new TestActor.AutoPilot {
        def run(sender: ActorRef, msg: Any): TestActor.AutoPilot = {
          sender ! DCOSAuthTokenResponse(authToken)
          TestActor.NoAutoPilot
        }
      })
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, probe.ref)
      val client = await(mcf.getClient(testProvider))
      probe.expectMsg(DCOSAuthTokenRequest(
        providerId = testProvider.id,
        acceptAnyCert = false,
        serviceAccountId = testServiceId,
        privateKey = testPrivateKey,
        dcosUrl = testDcosUrl
      ))
      client.acsToken must beSome(authToken)
      client.marathonBaseUrl must_== marathonBaseUrl
      client.client must_== strictClient
    }

    "not request auth token from DCOSAuthTokenActor on getClient for provider with no auth" in new WithProviderConfig(
      providerAuth = JsNull
    ) {
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, testActor)
      val client = await(mcf.getClient(testProvider))
      client.acsToken must beNone
      client.marathonBaseUrl must_== marathonBaseUrl
      client.client must_== strictClient
      expectNoMsg
    }

    "not request auth token from DCOSAuthTokenActor on getClient for provider with empty auth" in new WithProviderConfig(
      providerAuth = Json.obj()
    ) {
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, testActor)
      val client = await(mcf.getClient(testProvider))
      client.acsToken must beNone
      client.marathonBaseUrl must_== marathonBaseUrl
      client.client must_== strictClient
      expectNoMsg
    }

    "use permissive client for auth'd providers if specified" in new WithProviderConfig(
      providerAuth = Json.obj(
        "scheme" -> "acs",
        "service_account_id" -> testServiceId,
        "private_key" -> testPrivateKey,
        "dcos_base_url" -> testDcosUrl
      ),
      permissiveHttps = JsBoolean(true)
    ) {
      val probe = TestProbe()
      probe.setAutoPilot(new TestActor.AutoPilot {
        def run(sender: ActorRef, msg: Any): TestActor.AutoPilot = {
          sender ! DCOSAuthTokenResponse(authToken)
          TestActor.NoAutoPilot
        }
      })
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, probe.ref)
      val client = await(mcf.getClient(testProvider))
      probe.expectMsg(DCOSAuthTokenRequest(
        providerId = testProvider.id,
        acceptAnyCert = true,
        serviceAccountId = testServiceId,
        privateKey = testPrivateKey,
        dcosUrl = testDcosUrl
      ))
      client.acsToken must beSome(authToken)
      client.marathonBaseUrl must_== marathonBaseUrl
      client.client must_== permissiveClient
    }

    "use permissive client for unauth'd providers if specified" in new WithProviderConfig(
      permissiveHttps = JsBoolean(true)
    ) {
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, testActor)
      val client = await(mcf.getClient(testProvider))
      client.acsToken must beNone
      client.marathonBaseUrl must_== marathonBaseUrl
      client.client must_== permissiveClient
      expectNoMsg
    }

    "not use permissive client for unauth'd providers if specified in the negative" in new WithProviderConfig(
      permissiveHttps = JsBoolean(false)
    ) {
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, testActor)
      val client = await(mcf.getClient(testProvider))
      client.acsToken must beNone
      client.marathonBaseUrl must_== marathonBaseUrl
      client.client must_== strictClient
      expectNoMsg
    }

    "throw a helpful exception with bad acs config (missing service_account_id)" in new WithProviderConfig(
      providerAuth = Json.obj(
        "scheme" -> "acs",
        // "service_account_id" -> testServiceId,
        "private_key" -> testPrivateKey,
        "dcos_base_url" -> testDcosUrl
      )
    ) {
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, testActor)
      await(mcf.getClient(testProvider)) must throwAn[BadRequestException]("provider with 'acs' authentication was missing required fields")
    }

    "throw a helpful exception with bad acs config (missing private_key)" in new WithProviderConfig(
      providerAuth = Json.obj(
        "scheme" -> "acs",
        "service_account_id" -> testServiceId,
        // "private_key" -> testPrivateKey,
        "dcos_base_url" -> testDcosUrl
      )
    ) {
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, testActor)
      await(mcf.getClient(testProvider)) must throwAn[BadRequestException]("provider with 'acs' authentication was missing required fields")
    }

    "throw a helpful exception with bad acs config (missing dcos_base_url)" in new WithProviderConfig(
      providerAuth = Json.obj(
        "scheme" -> "acs",
        "service_account_id" -> testServiceId,
        "private_key" -> testPrivateKey
        // "dcos_base_url" -> testDcosUrl
      )
    ) {
      val mcf = new DefaultMarathonClientFactory(strictClient, permissiveClient, testActor)
      await(mcf.getClient(testProvider)) must throwAn[BadRequestException]("provider with 'acs' authentication was missing required fields")
    }


  }

  "DCOSAuthTokenActor" should {

    def mockAuth() = Route {
      case (POST,url) if url == testDcosUrl + "/acs/api/v1/auth/login" => Action(parse.json) {request =>
        val uid = (request.body \ "uid").as[String]
        val jws = io.jsonwebtoken.Jwts.parser().setSigningKey(DCOSAuthTokenActor.strToPublicKey(testPublicKey)).parseClaimsJws((request.body \ "token").as[String])
        if ( uid == testServiceId && jws.getBody.get("uid",classOf[String]) == testServiceId )
          Ok(Json.obj("token" -> authToken))
        else
          Unauthorized(Json.obj("message" -> "epic failure"))
      }
    }

    "get tokens from DCOS ACS and persist them" in new WithProviderConfig {
      val permissive = mockAuth()
      val strict = mockAuth()
      val tokenActor = TestActorRef(new DCOSAuthTokenActor(MockWS{strict}, MockWS{permissive}))
      val providerId = UUID.randomUUID()

      val token = await(tokenActor ? DCOSAuthTokenRequest(
        providerId = providerId,
        acceptAnyCert = false,
        serviceAccountId = testServiceId,
        privateKey = testPrivateKey,
        dcosUrl = testDcosUrl
      ))
      token must_== DCOSAuthTokenResponse(authToken)
      tokenActor.underlyingActor.providerTokens.get(providerId) must beSome(authToken)
    }

    "use permissive client for auth call if so instructed" in new WithProviderConfig {
      val permissive = mockAuth()
      val strict = mockAuth()
      val tokenActor = TestActorRef(new DCOSAuthTokenActor(MockWS{strict}, MockWS{permissive}))
      val providerId = UUID.randomUUID()

      val token = await(tokenActor ? DCOSAuthTokenRequest(
        providerId = providerId,
        acceptAnyCert = false,
        serviceAccountId = testServiceId,
        privateKey = testPrivateKey,
        dcosUrl = testDcosUrl
      ))
      token must_== DCOSAuthTokenResponse(authToken)
      tokenActor.underlyingActor.providerTokens.get(providerId) must beSome(authToken)
      permissive.timeCalled must_== 0
      strict.timeCalled must_== 1
    }

    "use strict client for auth call if so instructed" in new WithProviderConfig {
      val permissive = mockAuth()
      val strict = mockAuth()
      val tokenActor = TestActorRef(new DCOSAuthTokenActor(MockWS{strict}, MockWS{permissive}))
      val providerId = UUID.randomUUID()

      val token = await(tokenActor ? DCOSAuthTokenRequest(
        providerId = providerId,
        acceptAnyCert = true,
        serviceAccountId = testServiceId,
        privateKey = testPrivateKey,
        dcosUrl = testDcosUrl
      ))
      token must_== DCOSAuthTokenResponse(authToken)
      tokenActor.underlyingActor.providerTokens.get(providerId) must beSome(authToken)
      permissive.timeCalled must_== 1
      strict.timeCalled must_== 0
    }

  }

}
