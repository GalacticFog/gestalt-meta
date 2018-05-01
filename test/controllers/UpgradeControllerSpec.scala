package controllers

import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.genericactions.GenericProviderManager
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test._
import controllers.util.ContainerService
import org.specs2.execute.{AsResult, Result}
import org.specs2.matcher.JsonMatchers
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.{PlaySpecification, WithApplication}
import services.{MarathonClientFactory, SkuberFactory}

import scala.util.Success

class UpgradeControllerSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    val Success(_) = pristineDatabase()
  }

  sequential

  stopOnFail

  def appWithMocks() = application(additionalBindings = Seq(
    bind[ContainerService].toInstance(mock[ContainerService]),
    bind[ProviderManager].toInstance(mock[ProviderManager]),
    bind[MarathonClientFactory].toInstance(mock[MarathonClientFactory]),
    bind[SkuberFactory].toInstance(mock[SkuberFactory]),
    bind[GenericProviderManager].toInstance(mock[GenericProviderManager])
  ))

  abstract class TestUpgradeController extends WithApplication(appWithMocks()) {
    override def around[T: AsResult](t: => T): Result = super.around {
      scalikejdbc.config.DBs.setupAll()
      t
    }
  }

  "UpgradeController" should {

    "return 409 for unauthorized user" in new TestUpgradeController {
      val request = fakeAuthRequest(GET, s"/upgrade", testCreds)
      val Some(result) = route(request)
      status(result) must throwA[ConflictException]
    }

    "return 200 with appropriate payload when there is no active upgrade" in new TestUpgradeController {
      val request = fakeAuthRequest(GET, s"/upgrade", testAdminCreds)
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      contentAsString(result) must /("active" -> false)
      contentAsString(result) must not /("endpoint")
    }

    "return 201 on POST" in new TestUpgradeController {
      val pRequest = fakeAuthRequest(POST, s"/upgrade", testAdminCreds).withBody(Json.obj(
        // FINISH
      ))
      val Some(pResult) = route(pRequest)
      status(pResult) must equalTo(ACCEPTED)
      contentAsString(pResult) must /("active" -> true)
      contentAsString(pResult) must /("endpoint" -> "https://gtw1.test.galacticfog.com/test-upgrader")

      val gRequest = fakeAuthRequest(GET, s"/upgrade", testAdminCreds)
      val Some(gResult) = route(gRequest)
      status(gResult) must equalTo(OK)
      contentAsString(gResult) must_== contentAsString(pResult)
    }

    "return 400 on additional POST" in new TestUpgradeController {
      val request = fakeAuthRequest(POST, s"/upgrade", testAdminCreds).withBody(Json.obj(
        // FINISH
      ))
      val Some(result) = route(request)
      status(result) must equalTo(BAD_REQUEST)
      contentAsString(result) must /("message" -> contain("upgrade is already active"))
    }

    "return 201 on DELETE and reset to inactive" in new TestUpgradeController {
      val dRequest = fakeAuthRequest(DELETE, s"/upgrade", testAdminCreds)
      val Some(dResult) = route(dRequest)
      status(dResult) must equalTo(ACCEPTED)
      contentAsString(dResult) must /("active" -> false)
      contentAsString(dResult) must not /("endpoint")

      val gRequest = fakeAuthRequest(GET, s"/upgrade", testAdminCreds)
      val Some(gResult) = route(gRequest)
      status(gResult) must equalTo(OK)
      contentAsString(gResult) must_== contentAsString(dResult)
    }

    "then return 201 on POST" in new TestUpgradeController {
      val pRequest = fakeAuthRequest(POST, s"/upgrade", testAdminCreds).withBody(Json.obj(
        // FINISH
      ))
      val Some(pResult) = route(pRequest)
      status(pResult) must equalTo(ACCEPTED)
      contentAsString(pResult) must /("active" -> true)
      contentAsString(pResult) must /("endpoint" -> "https://gtw1.test.galacticfog.com/test-upgrader")

      val gRequest = fakeAuthRequest(GET, s"/upgrade", testAdminCreds)
      val Some(gResult) = route(gRequest)
      status(gResult) must equalTo(OK)
      contentAsString(gResult) must_== contentAsString(pResult)
    }

  }

}