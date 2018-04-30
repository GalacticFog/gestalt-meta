package controllers

import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceStates}
import com.galacticfog.gestalt.meta.genericactions.GenericProviderManager
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test._
import controllers.util.ContainerService
import org.specs2.execute.{AsResult, Result}
import org.specs2.matcher.JsonMatchers
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.{FakeRequest, PlaySpecification, WithApplication}
import services.{CaasService, MarathonClientFactory, SkuberFactory}

import scala.concurrent.Await
import scala.util.Success

class UpgradeControllerSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    val Success(_) = pristineDatabase()
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

  def appWithMocks() = application(additionalBindings = Seq(
    bind[ContainerService].toInstance(mock[ContainerService]),
    bind[ProviderManager].toInstance(mock[ProviderManager]),
    bind[MarathonClientFactory].toInstance(mock[MarathonClientFactory]),
    bind[SkuberFactory].toInstance(mock[SkuberFactory]),
    bind[GenericProviderManager].toInstance(mock[GenericProviderManager])
  ))

  abstract class TestUpgradeController extends WithApplication(appWithMocks()) {
    var testWork: GestaltResourceInstance = null
    var testEnv: GestaltResourceInstance = null
    var containerService: ContainerService = null
    var providerManager: ProviderManager = null

    var testProvider: GestaltResourceInstance = null
    var mockCaasService: CaasService = null

    override def around[T: AsResult](t: => T): Result = super.around {
      scalikejdbc.config.DBs.setupAll()

      val Success(wrkEnv) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
      testWork = wrkEnv._1
      testEnv = wrkEnv._2

      Ents.setNewResourceEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

      val injector = app.injector
      containerService = injector.instanceOf[ContainerService]
      providerManager = injector.instanceOf[ProviderManager]

      testProvider = createKubernetesProvider(testEnv.id, "test-kube-provider").get
      mockCaasService = mock[CaasService]
      providerManager.getProviderImpl(testProvider.typeId) returns Success(mockCaasService)
      t
    }
  }

  stopOnFail

  "UpgradeController" should {

    "return 409 for unauthorized user" in new TestUpgradeController {
      val request = fakeAuthRequest(GET, s"/upgrade", testCreds)
      val Some(result) = route(request)
      status(result) must throwA[ConflictException]
    }

    "return 200 with appropriate payload when there is no upgrade" in new TestUpgradeController {
      val request = fakeAuthRequest(GET, s"/upgrade", testAdminCreds)

      val Some(result) = route(request)
      status(result) must equalTo(OK)
      contentAsJson(result) must_== Json.obj(
        "active" -> false
      )
    }

  }

}