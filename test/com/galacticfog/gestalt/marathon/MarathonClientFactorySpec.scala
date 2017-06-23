package com.galacticfog.gestalt.marathon

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.specification.{BeforeAll, Scope}
import play.api.test.PlaySpecification
import services.DefaultMarathonClientFactory
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import services.DCOSAuthTokenActor.DCOSAuthTokenRequest

import scala.util.Success
import scala.language.reflectiveCalls
// import play.api.libs.concurrent.Execution.Implicits.defaultContext
// import org.mockito.Matchers.{eq => meq}

@RunWith(classOf[JUnitRunner])
class MarathonClientFactorySpec extends PlaySpecification with ResourceScope with BeforeAll {

  override def beforeAll(): Unit = pristineDatabase()

  abstract class FakeDCOSTokenActor extends TestKit(ActorSystem("MySpec")) with Scope {
    val Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    val testServiceId = "meta-dcos-provider"
    val testPrivateKey = "-----BEGIN PRIVATE KEY-----\nMIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC9OzC0iseKnsqd\nu82KvTav6q+j4MoSS3mGGPZIA2JaD/cMjpzBtaaOxIbcyLWt2M8hwdO3TLXCZiW2\nybz2Koeo3+vNphnO7U4ZggSIuM+RYfhUUnQ79yiYKmL3z93HRrvZBlulG3yOFo5y\n30IFKqyt2QKlPy3ObCtZYwT4opYNnkev/pubtOjsjdkU9/u088eiLfVHwSwpBxjG\n2wbpFVGyN3p55UHW3K6QUrUw8B7EOF2A5EXzgR5GmAgL6SjuzEdghumqdMcSxGoE\n4pL3Y6LHer391ITdxO819o0i3cfglvgXxFGZSsiRVV89X15n8pEbP73cD3sRxnwe\nIwW860ZnAgMBAAECggEAIKUXb+4JIobmWXPOr8KYrpyEFHdxJNrUaifgROggjXz3\nl7j6nghiZXrN8UTG4ujmQuKXTaX0LUdF9lSzPpxzrtSCb4XaKfKSaKAffB614FTQ\nbGuVFcs7u5SEYk//6KLxQS1xnfgx8qk9hd+yGgYUqCEp7awKkPPkPpVwhBw4WrzJ\nkYxJ3bIT7j3svTr5uhno7cFso5jhfFyMA7PruHGNfyOWLIgzgw5qwRUK1WLMyk88\nJivrDRbvuskWK7pxvLrRQ/VA34LvGKLroj9Gqw9HIDGbY526PPjFo/uDq8ErHBsQ\nBdoagN6VihX5YjXdi3eF8mIcaFYBOQj6zB+Kfmkc0QKBgQDjkIemfgpHEMcRsinm\ni0WLlZGD8hjFTNku1Pki5sFffXcHR+FImrEUXL/NqJr8iqIeJ+1cx3OAiBm8PHh4\nl+LYz4H2TlvIEEURmOwLiBbh49N4o7T9the+PluDGLsZ9ka3AGHP1LBcvwYJdf7v\nubK3eky1QQSI5Ce6+uayU76QFQKBgQDU4G4j2eAIVTDQ0xMfJYXFaIh2eVqTkv83\nPeskWhAQcPUKzyX7bPHSdSbx+91ZW5iL8GX4DFp+JBiQFSqNq1tqhLwz9xHTxYrj\nGvi6MUJ4LCOihbU+6JIYuOdxq3govxtnJ+lE4cmwr5Y4HM1wx2dxba9EsItLrzkj\nHGPNDJ6fiwKBgCXgPHO9rsA9TqTnXon8zEp7TokDlpPgQpXE5OKmPbFDFLilgh2v\ngaG9/j6gvYsjF/Ck/KDgoZzXClGGTxbjUOJ9R0hTqnsWGijfpwoUUJqwbNY7iThh\nQnprrpeXWizsDMEQ0zbgU6pcMQkKFrCX2+Ml+/Z/J94Q+3vnntY3khQxAoGAdUkh\n5cbI1E57ktJ4mpSF23n4la3O5bf7vWf0AhdM+oIBwG7ZMmmX4qiBSJnIHs+EgLV2\nuO+1fAJPNjMzOtLKjymKt+bMf607FF1r5Mn3IVbQW17nuT1SISTe/5XFok2Iv5ER\nyM3N3fcgANJ9rkFvEOOpyWKrnItyI5IkunjVfHkCgYEAjmAjQOQt5eCO9kGitL7X\ntQGn8TWWHRCjMm1w3ith7bPp11WrdeyfNuUAB7weQjk2qjAIKTOGWtIRqc36OLPA\nkwF1GDyFXvLqJej/2ZLfytyjhetLAQnRL0qOgCi7EU5+YLXuYnn7zPEJgrR3ogX4\n4rvG4NIQ8wG0sEUTnr06nck=\n-----END PRIVATE KEY-----"
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
            "dcos-base-url" -> testDcosUrl
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
  }

  "DefaultMarathonClientFactory" should {

    "request auth token from DCOSAuthTokenActor on getClient for authed provider" in new FakeDCOSTokenActor {
      val mcf = new DefaultMarathonClientFactory(mock[WSClient], testActor)
      val client = await(mcf.getClient(testProviderWithAuth))
      expectMsg(DCOSAuthTokenRequest(
        serviceAccountId = testServiceId,
        privateKey = testPrivateKey,
        dcosUrl = testDcosUrl
      ))
      client.acsToken must beSome
      client.marathonAddress must_== marathonBaseUrl
    }

    "not request auth token from DCOSAuthTokenActor on getClient for un-authed provider" in new FakeDCOSTokenActor {
      val mcf = new DefaultMarathonClientFactory(mock[WSClient], testActor)
      val client = await(mcf.getClient(testProviderWithoutAuth))
      client.acsToken must beNone
      client.marathonAddress must_== marathonBaseUrl
      expectNoMsg
    }

  }

}
