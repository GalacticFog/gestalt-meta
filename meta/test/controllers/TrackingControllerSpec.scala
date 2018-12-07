package controllers


import com.galacticfog.gestalt.meta.test._
import com.galacticfog.tracking.Credit.CreditType
import com.galacticfog.tracking.{Credit, CreditProvider}
import org.specs2.matcher.JsonMatchers
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.PlaySpecification


class TrackingControllerSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {

  abstract class TestTrackingController extends WithDb(containerApp(additionalBindings = Seq(
    bind[CreditProvider].toInstance(mock[CreditProvider])
  ))) {
    lazy val creditProviderMock = app.injector.instanceOf[CreditProvider]
  }

  sequential

  "TrackingController" should {

    "create credit" in new TestTrackingController {

      val payload = Json.obj(
        "name" -> "name",
        "metricName" -> "metricName",
        "period" -> 24,
        "amount" -> 100,
        "grantedOn" -> 0,
        "isGranted" -> true,
        "balance" -> 100,
        "creditType" -> "PERIODIC"
      )
      creditProviderMock.upsertCredit(any).returns(Some(true))

      val request = fakeAuthRequest(POST, s"/tracking/credits", testCreds).withBody(payload)
      val Some(result) = route(app, request)
      status(result) must equalTo(OK)
    }

    "delete credit" in new TestTrackingController {

      creditProviderMock.deleteCredit(any).returns(Some(1l))

      val request = fakeAuthRequest(DELETE, s"/tracking/credits/name", testCreds)
      val Some(result) = route(app, request)
      status(result) must equalTo(OK)
    }

    "get credit" in new TestTrackingController {

      val credit = Credit(
        name = "name",
        metricName = "metricName",
        period = 24,
        amount = 10,
        grantedOn = 0l,
        isGranted = true,
        balance = 10,
        creditType = CreditType.PERIODIC
      )
      creditProviderMock.getCredit(any).returns(Some(credit))

      val request = fakeAuthRequest(GET, s"/tracking/credits/name", testCreds)
      val Some(result) = route(app, request)
      status(result) must equalTo(OK)
    }

    "get credits" in new TestTrackingController {

      val credit = Credit(
        name = "name",
        metricName = "metricName",
        period = 24,
        amount = 10,
        grantedOn = 0l,
        isGranted = true,
        balance = 10,
        creditType = CreditType.PERIODIC
      )
      creditProviderMock.getCredits().returns(Some(List(credit)))

      val request = fakeAuthRequest(GET, s"/tracking/credits", testCreds)
      val Some(result) = route(app, request)
      status(result) must equalTo(OK)
    }
  }

}