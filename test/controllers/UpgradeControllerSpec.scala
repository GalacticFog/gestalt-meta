package controllers

import actors.SystemConfigActor
import akka.actor.ActorRef
import akka.pattern.ask
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.genericactions.GenericProviderManager
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test._
import controllers.util.{ContainerService, UpgraderService}
import org.specs2.execute.{AsResult, Result}
import org.specs2.matcher.JsonMatchers
import play.api.inject.{BindingKey, bind}
import play.api.libs.json.Json
import play.api.test.{PlaySpecification, WithApplication}
import services.{MarathonClientFactory, SkuberFactory}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.util.Success

class UpgradeControllerSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    val Success(_) = pristineDatabase()
  }

  sequential

  stopOnFail

  val testPayload = Json.obj(
    "image" -> "galacticfog/gestalt-upgrader:1.6.0",
    "dbProviderId" -> "48c24048-cb1d-45c4-806e-c888aed7ea42",
    "secProviderId" -> "a26d8342-09c9-4395-bd4a-f18473de6f36",
    "caasProviderId" -> "6ee3af52-a36e-4153-a642-a1d282d17c46",
    "kongProviderId" -> "5b835edf-bc93-4ce8-b018-401dfd5bf903"
  )

  val testEndpoint = "https://gtw1.test.galacticfog.com/test-upgrader"

  def appWithMocks() = application(additionalBindings = Seq(
    bind[ContainerService].toInstance(mock[ContainerService]),
    bind[ProviderManager].toInstance(mock[ProviderManager]),
    bind[MarathonClientFactory].toInstance(mock[MarathonClientFactory]),
    bind[SkuberFactory].toInstance(mock[SkuberFactory]),
    bind[GenericProviderManager].toInstance(mock[GenericProviderManager]),
    bind[UpgraderService].toInstance(mock[UpgraderService])
  ))

  abstract class TestUpgradeController extends WithApplication(appWithMocks()) {
    lazy val mockUpgraderService = app.injector.instanceOf[UpgraderService]
    lazy val configActor = app.injector.instanceOf(BindingKey(classOf[ActorRef]).qualifiedWith(SystemConfigActor.name))

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
      mockUpgraderService.launchUpgrader(any, any)(any) returns {
        val s = UpgraderService.UpgradeStatus(
          active = true,
          endpoint = Some(testEndpoint)
        )
        (configActor ? SystemConfigActor.SetKey(adminUserId,
          "upgrade_status" , Some(Json.toJson(s).toString)
        )).map { _ => s }
      }

      val pRequest = fakeAuthRequest(POST, s"/upgrade", testAdminCreds).withBody(testPayload)
      val Some(pResult) = route(pRequest)
      status(pResult) must equalTo(ACCEPTED)
      contentAsString(pResult) must /("active" -> true)
      contentAsString(pResult) must /("endpoint" -> testEndpoint)

      val gRequest = fakeAuthRequest(GET, s"/upgrade", testAdminCreds)
      val Some(gResult) = route(gRequest)
      status(gResult) must equalTo(OK)
      contentAsString(gResult) must_== contentAsString(pResult)
    }

    "return 400 on additional POST" in new TestUpgradeController {
      val request = fakeAuthRequest(POST, s"/upgrade", testAdminCreds).withBody(testPayload)
      val Some(result) = route(request)
      status(result) must equalTo(BAD_REQUEST)
      contentAsString(result) must /("message" -> contain("upgrade is already active"))
    }

    "return 201 on DELETE and reset to inactive" in new TestUpgradeController {
      mockUpgraderService.deleteUpgrader(any, any)(any) returns {
        val s = UpgraderService.UpgradeStatus.inactive
        (configActor ? SystemConfigActor.SetKeys(adminUserId,
          Map(
            "upgrade_status" -> Some(Json.toJson(s).toString),
            "upgrade_lock" -> Some("false")
          )
        )).map { _ => s }
      }

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
      mockUpgraderService.launchUpgrader(any, any)(any) returns {
        val s = UpgraderService.UpgradeStatus(
          active = true,
          endpoint = Some(testEndpoint)
        )
        (configActor ? SystemConfigActor.SetKey(adminUserId,
          "upgrade_status" , Some(Json.toJson(s).toString)
        )).map { _ => s }
      }

      val pRequest = fakeAuthRequest(POST, s"/upgrade", testAdminCreds).withBody(testPayload)
      val Some(pResult) = route(pRequest)
      status(pResult) must equalTo(ACCEPTED)
      contentAsString(pResult) must /("active" -> true)
      contentAsString(pResult) must /("endpoint" -> testEndpoint)

      val gRequest = fakeAuthRequest(GET, s"/upgrade", testAdminCreds)
      val Some(gResult) = route(gRequest)
      status(gResult) must equalTo(OK)
      contentAsString(gResult) must_== contentAsString(pResult)
    }

  }

}