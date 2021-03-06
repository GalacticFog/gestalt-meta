package controllers

import actors.SystemConfigActor
import akka.actor.ActorRef
import akka.pattern.ask
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.genericactions.GenericProviderManager
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test._
import controllers.util.{ContainerService, UpgraderService}
import org.specs2.matcher.JsonMatchers
import org.specs2.specification.BeforeAll
import play.api.inject.{BindingKey, bind}
import play.api.libs.json.Json
import play.api.test.PlaySpecification
import services.MarathonClientFactory
import services.kubernetes.SkuberFactory
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import com.galacticfog.gestalt.meta.api.sdk.GestaltConfigurationManager
import com.galacticfog.gestalt.data.PostgresConfigManager


class UpgradeControllerSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers with BeforeAll {

  sequential

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    pristineDatabase()
  }

  val testPayload = Json.obj(
    "image" -> "galacticfog/gestalt-upgrader:1.6.0",
    "dbProviderId" -> "48c24048-cb1d-45c4-806e-c888aed7ea42",
    "secProviderId" -> "a26d8342-09c9-4395-bd4a-f18473de6f36",
    "caasProviderId" -> "6ee3af52-a36e-4153-a642-a1d282d17c46",
    "gwmProviderId" -> "b22c704b-01d8-4bf0-b7d5-a8344119972c",
    "kongProviderId" -> "5b835edf-bc93-4ce8-b018-401dfd5bf903"
  )
  
  val testEndpoint = "https://gtw1.test.galacticfog.com/test-upgrader"
  
  def appWithMocks() = application(additionalBindings = Seq(
    bind[ContainerService].toInstance(mock[ContainerService]),
    bind[ProviderManager].toInstance(mock[ProviderManager]),
    bind[MarathonClientFactory].toInstance(mock[MarathonClientFactory]),
    bind[SkuberFactory].toInstance(mock[SkuberFactory]),
    bind[GenericProviderManager].toInstance(mock[GenericProviderManager]),
    bind[UpgraderService].toInstance(mock[UpgraderService]),
    bind(classOf[GestaltConfigurationManager]).toInstance(PostgresConfigManager)
  ))

  abstract class TestUpgradeController extends WithDb(appWithMocks()) {
    lazy val mockUpgraderService = app.injector.instanceOf[UpgraderService]
    lazy val configActor = app.injector.instanceOf(BindingKey(classOf[ActorRef]).qualifiedWith(SystemConfigActor.name))
  }

  "UpgradeController" should {

    "return 409 for unauthorized user" in new TestUpgradeController {
      val request = fakeAuthRequest(GET, s"/upgrade", testCreds)
      val Some(result) = route(app,request)
      status(result) must throwA[ConflictException]
    }

    "return 200 with appropriate payload when there is no active upgrade" in new TestUpgradeController {
      val request = fakeAuthRequest(GET, s"/upgrade", testAdminCreds)
      val Some(result) = route(app,request)
      status(result) must equalTo(OK)
      contentAsString(result) must /("active" -> false)
      contentAsString(result) must not /("endpoint")
    }

    "bad payload POST returns 400 but does not lock the upgrader" in new TestUpgradeController {
      val request = fakeAuthRequest(POST, s"/upgrade", testAdminCreds).withBody(Json.obj())
      val Some(result) = route(app,request)
      status(result) must equalTo(BAD_REQUEST)
      contentAsString(result) must /("message" -> "invalid payload")
      await(configActor ? SystemConfigActor.GetKey("upgrade_lock")).asInstanceOf[Option[String]] must beNone or beSome("false")
    }

    "return 400 on additional POST" in new TestUpgradeController {
      await(configActor ? SystemConfigActor.SetKey(adminUserId, "upgrade_lock", Some("true")))
      val request = fakeAuthRequest(POST, s"/upgrade", testAdminCreds).withBody(testPayload)
      val Some(result) = route(app,request)
      status(result) must equalTo(BAD_REQUEST)
      contentAsString(result) must /("message" -> contain("upgrade is already active"))
    }

    "return 201 on DELETE and reset to inactive" in new TestUpgradeController {
      mockUpgraderService.deleteUpgrader(any)(any) returns {
        val s = UpgraderService.UpgradeStatus.inactive
        (configActor ? SystemConfigActor.SetKeys(adminUserId,
          Map(
            "upgrade_status" -> Some(Json.toJson(s).toString),
            "upgrade_lock" -> Some("false")
          )
        )).map { _ => s }
      }

      val dRequest = fakeAuthRequest(DELETE, s"/upgrade", testAdminCreds)
      val Some(dResult) = route(app,dRequest)
      status(dResult) must equalTo(ACCEPTED)
      contentAsString(dResult) must /("active" -> false)
      contentAsString(dResult) must not /("endpoint")

      val gRequest = fakeAuthRequest(GET, s"/upgrade", testAdminCreds)
      val Some(gResult) = route(app,gRequest)
      status(gResult) must equalTo(OK)
      contentAsString(gResult) must_== contentAsString(dResult)
    }

//    "return 201 on POST" in new TestUpgradeController {
//      mockUpgraderService.launchUpgrader(any, any)(any) returns {
//        val s = UpgraderService.UpgradeStatus(
//          active = true,
//          endpoint = Some(testEndpoint)
//        )
//        (configActor ? SystemConfigActor.SetKey(adminUserId,
//          "upgrade_status" , Some(Json.toJson(s).toString)
//        )).map { _ => s }
//      }
//
//      val pRequest = fakeAuthRequest(POST, s"/upgrade", testAdminCreds).withBody(testPayload)
//      val Some(pResult) = route(app,pRequest)
//      status(pResult) must equalTo(ACCEPTED)
//      contentAsString(pResult) must /("active" -> true)
//      contentAsString(pResult) must /("endpoint" -> testEndpoint)
//
//      val gRequest = fakeAuthRequest(GET, s"/upgrade", testAdminCreds)
//      val Some(gResult) = route(app,gRequest)
//      status(gResult) must equalTo(OK)
//      contentAsString(gResult) must_== contentAsString(pResult)
//    }


  }

}