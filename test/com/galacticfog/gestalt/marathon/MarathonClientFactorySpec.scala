package com.galacticfog.gestalt.marathon

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestActor, TestActorRef, TestKit, TestProbe}
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import mockws.{MockWS, Route}
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.specification.{BeforeAll, Scope}
import play.api.test.PlaySpecification
import services.{DCOSAuthTokenActor, DefaultMarathonClientFactory}
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import services.DCOSAuthTokenActor.{DCOSAuthTokenRequest, DCOSAuthTokenResponse}
import play.api.mvc._
import play.api.mvc.Results._
import akka.pattern.ask
import play.api.mvc.BodyParsers.parse
import scala.util.Success
import scala.language.reflectiveCalls

@RunWith(classOf[JUnitRunner])
class MarathonClientFactorySpec extends PlaySpecification with ResourceScope with BeforeAll {

  override def beforeAll(): Unit = pristineDatabase()

  abstract class FakeDCOSTokenActor extends TestKit(ActorSystem("MySpec")) with Scope {
    val authToken = "eyJhbGciOiJSUzI1NiIsImtpZCI6InNlY3JldCIsInR5cCI6IkpXVCJ9.eyJ1aWQiOiJkZW1vX2dlc3RhbHRfbWV0YSJ9.RP5MhJPey2mDXOJlu1GFcQ_TlWZzYnr_6N7AwDbB0jqJC3bsLR8QxKZVbzk_JInwO5QN_-BVK5bvxP5zyo4KhVotsugH5eP_iTSDyyx7iKWOK4oPmFlJglaXGRE_KEuySAeZCNTnDIrfUWnB21WwS92MGr6B4rFZ-IzVSmygzO-LgxM-ZU9_b9kyKLOUXcQLgHFLY-qJMWou98dTv36lhjqx65iKQ5PT53KjGtL6OQ-1vqXse5ynCJGsXk3HBXV4P_w42RJBIAWiIbsUfgN85sGTVPvtHO-o-GJMknf7G0FiwfGtsYS3n05kirNIwsZS54RX03TNlxq0Vg48eWGZKQ"
    val Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    val testServiceId = "meta-dcos-provider"
    val testPrivateKey = "-----BEGIN PRIVATE KEY-----\nMIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC9OzC0iseKnsqd\nu82KvTav6q+j4MoSS3mGGPZIA2JaD/cMjpzBtaaOxIbcyLWt2M8hwdO3TLXCZiW2\nybz2Koeo3+vNphnO7U4ZggSIuM+RYfhUUnQ79yiYKmL3z93HRrvZBlulG3yOFo5y\n30IFKqyt2QKlPy3ObCtZYwT4opYNnkev/pubtOjsjdkU9/u088eiLfVHwSwpBxjG\n2wbpFVGyN3p55UHW3K6QUrUw8B7EOF2A5EXzgR5GmAgL6SjuzEdghumqdMcSxGoE\n4pL3Y6LHer391ITdxO819o0i3cfglvgXxFGZSsiRVV89X15n8pEbP73cD3sRxnwe\nIwW860ZnAgMBAAECggEAIKUXb+4JIobmWXPOr8KYrpyEFHdxJNrUaifgROggjXz3\nl7j6nghiZXrN8UTG4ujmQuKXTaX0LUdF9lSzPpxzrtSCb4XaKfKSaKAffB614FTQ\nbGuVFcs7u5SEYk//6KLxQS1xnfgx8qk9hd+yGgYUqCEp7awKkPPkPpVwhBw4WrzJ\nkYxJ3bIT7j3svTr5uhno7cFso5jhfFyMA7PruHGNfyOWLIgzgw5qwRUK1WLMyk88\nJivrDRbvuskWK7pxvLrRQ/VA34LvGKLroj9Gqw9HIDGbY526PPjFo/uDq8ErHBsQ\nBdoagN6VihX5YjXdi3eF8mIcaFYBOQj6zB+Kfmkc0QKBgQDjkIemfgpHEMcRsinm\ni0WLlZGD8hjFTNku1Pki5sFffXcHR+FImrEUXL/NqJr8iqIeJ+1cx3OAiBm8PHh4\nl+LYz4H2TlvIEEURmOwLiBbh49N4o7T9the+PluDGLsZ9ka3AGHP1LBcvwYJdf7v\nubK3eky1QQSI5Ce6+uayU76QFQKBgQDU4G4j2eAIVTDQ0xMfJYXFaIh2eVqTkv83\nPeskWhAQcPUKzyX7bPHSdSbx+91ZW5iL8GX4DFp+JBiQFSqNq1tqhLwz9xHTxYrj\nGvi6MUJ4LCOihbU+6JIYuOdxq3govxtnJ+lE4cmwr5Y4HM1wx2dxba9EsItLrzkj\nHGPNDJ6fiwKBgCXgPHO9rsA9TqTnXon8zEp7TokDlpPgQpXE5OKmPbFDFLilgh2v\ngaG9/j6gvYsjF/Ck/KDgoZzXClGGTxbjUOJ9R0hTqnsWGijfpwoUUJqwbNY7iThh\nQnprrpeXWizsDMEQ0zbgU6pcMQkKFrCX2+Ml+/Z/J94Q+3vnntY3khQxAoGAdUkh\n5cbI1E57ktJ4mpSF23n4la3O5bf7vWf0AhdM+oIBwG7ZMmmX4qiBSJnIHs+EgLV2\nuO+1fAJPNjMzOtLKjymKt+bMf607FF1r5Mn3IVbQW17nuT1SISTe/5XFok2Iv5ER\nyM3N3fcgANJ9rkFvEOOpyWKrnItyI5IkunjVfHkCgYEAjmAjQOQt5eCO9kGitL7X\ntQGn8TWWHRCjMm1w3ith7bPp11WrdeyfNuUAB7weQjk2qjAIKTOGWtIRqc36OLPA\nkwF1GDyFXvLqJej/2ZLfytyjhetLAQnRL0qOgCi7EU5+YLXuYnn7zPEJgrR3ogX4\n4rvG4NIQ8wG0sEUTnr06nck=\n-----END PRIVATE KEY-----"
    val testPublicKey  = "-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvTswtIrHip7KnbvNir02\nr+qvo+DKEkt5hhj2SANiWg/3DI6cwbWmjsSG3Mi1rdjPIcHTt0y1wmYltsm89iqH\nqN/rzaYZzu1OGYIEiLjPkWH4VFJ0O/comCpi98/dx0a72QZbpRt8jhaOct9CBSqs\nrdkCpT8tzmwrWWME+KKWDZ5Hr/6bm7To7I3ZFPf7tPPHoi31R8EsKQcYxtsG6RVR\nsjd6eeVB1tyukFK1MPAexDhdgORF84EeRpgIC+ko7sxHYIbpqnTHEsRqBOKS92Oi\nx3q9/dSE3cTvNfaNIt3H4Jb4F8RRmUrIkVVfPV9eZ/KRGz+93A97EcZ8HiMFvOtG\nZwIDAQAB\n-----END PUBLIC KEY-----"
    val testDcosUrl = "https://m1.dcos"
    val marathonBaseUrl = "https://m1.dcos/service/marathon"
    val Success(testProviderWithAuth) = createInstance(ResourceIds.DcosProvider, "test-provider-with-auth",
      parent = Some(testEnv.id),
      properties = Some(Map(
        "config" -> Json.obj(
          "auth" -> Json.obj(
            "scheme" -> "acs",
            "service_account_id" -> testServiceId,
            "private_key" -> testPrivateKey,
            "dcos_base_url" -> testDcosUrl
          ),
          "url" -> marathonBaseUrl
        ).toString
      ))
    )
    val Success(testProviderWithoutAuth) = createInstance(ResourceIds.DcosProvider, "test-provider-no-auth",
      parent = Some(testEnv.id),
      properties = Some(Map(
        "config" -> Json.obj(
          "auth" -> Json.obj(),
          "url" -> marathonBaseUrl
        ).toString
      ))
    )
    val Success(poorlyConfiguredProvider) = createInstance(ResourceIds.DcosProvider, "test-provider-with-bad-auth-config",
      parent = Some(testEnv.id),
      properties = Some(Map(
        "config" -> Json.obj(
          "auth" -> Json.obj(
            "scheme" -> "acs"
          ),
          "url" -> marathonBaseUrl
        ).toString
      ))
    )
  }

  "DefaultMarathonClientFactory" should {

    "request auth token from DCOSAuthTokenActor on getClient for authed provider" in new FakeDCOSTokenActor {
      val probe = TestProbe()
      probe.setAutoPilot(new TestActor.AutoPilot {
        def run(sender: ActorRef, msg: Any): TestActor.AutoPilot = {
          sender ! DCOSAuthTokenResponse(authToken)
          TestActor.NoAutoPilot
        }
      })
      val mcf = new DefaultMarathonClientFactory(mock[WSClient], probe.ref)
      val client = await(mcf.getClient(testProviderWithAuth))
      probe.expectMsg(DCOSAuthTokenRequest(
        serviceAccountId = testServiceId,
        privateKey = testPrivateKey,
        dcosUrl = testDcosUrl
      ))
      client.acsToken must beSome(authToken)
      client.marathonAddress must_== marathonBaseUrl
    }

    "throw a helpful exception with bad acs config" in new FakeDCOSTokenActor {
      val mcf = new DefaultMarathonClientFactory(mock[WSClient], testActor)
      await(mcf.getClient(poorlyConfiguredProvider)) must throwAn[BadRequestException]("provider with 'acs' authentication was missing required fields")
    }

    "not request auth token from DCOSAuthTokenActor on getClient for un-authed provider" in new FakeDCOSTokenActor {
      val mcf = new DefaultMarathonClientFactory(mock[WSClient], testActor)
      val client = await(mcf.getClient(testProviderWithoutAuth))
      client.acsToken must beNone
      client.marathonAddress must_== marathonBaseUrl
      expectNoMsg
    }

  }

  "DCOSAuthTokenActor" should {

    "get tokens from dcos" in new FakeDCOSTokenActor {
      val mockAuth = Route {
        case (POST,url) if url == testDcosUrl + "/acs/api/v1/auth/login" => Action(parse.json) {request =>
          val uid = (request.body \ "uid").as[String]
          val jws = io.jsonwebtoken.Jwts.parser().setSigningKey(DCOSAuthTokenActor.strToPublicKey(testPublicKey)).parseClaimsJws((request.body \ "token").as[String])
          println(jws)
          if ( uid == testServiceId && jws.getBody.get("uid",classOf[String]) == testServiceId )
            Ok(Json.obj("token" -> authToken))
          else
            Unauthorized(Json.obj("message" -> "epic failure"))
        }
      }
      val tokenActor = TestActorRef(new DCOSAuthTokenActor(MockWS(mockAuth)))
      val token = await(tokenActor ? DCOSAuthTokenRequest(
        serviceAccountId = testServiceId,
        privateKey = testPrivateKey,
        dcosUrl = testDcosUrl
      ))
      token must_== DCOSAuthTokenResponse(authToken)
    }

  }

}
