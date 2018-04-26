package controllers

import java.util.UUID

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.{ContainerSpec, SecretSpec, sdk}
import com.galacticfog.gestalt.meta.genericactions.GenericProviderManager
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import controllers.util.ContainerService
import org.mockito.Matchers.{eq => meq}
import org.specs2.execute.{AsResult, Result}
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.matcher.{JsonMatchers, Matcher}
import play.api.inject.bind
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.{JsString, Json}
import play.api.test.{PlaySpecification, WithApplication}
import services.{CaasService, MarathonClientFactory, ProviderContext, SkuberFactory}

import scala.concurrent.Future
import scala.util.Success

class UpgradeControllerSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {

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

    import com.galacticfog.gestalt.data.EnvironmentType
    import com.galacticfog.gestalt.meta.api.errors._

    "ContainerService interface" in new TestUpgradeController {
      val testSecretName = "test-secret"
      val testItems = Seq(
        SecretSpec.Item("item-a", Some("c2hoaGho")),
        SecretSpec.Item("item-b", Some("dGhpcyBpcyBhIHNlY3JldA=="))
      )
      val testProps = SecretSpec(
        name = testSecretName,
        provider = ContainerSpec.InputProvider(id = testProvider.id),
        items = testItems
      )
      val extId = s"/${testEnv.id}/$testSecretName"
      val Success(createdResource) = createInstance(ResourceIds.Secret, testSecretName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> Json.toJson(testProps.provider).toString,
          "items" -> Json.toJson(testProps.items).toString,
          "external_id" -> s"${extId}"
        ))
      )

      mockCaasService.destroySecret(
        hasId(createdResource.id)
      ) returns Future(())

      val request = fakeAuthRequest(DELETE,
        s"/root/environments/${testEnv.id}/secrets/${createdResource.id}", testCreds)

      val Some(result) = route(request)

      status(result) must equalTo(NO_CONTENT)

      there was one(mockCaasService).destroySecret(
        hasId(createdResource.id) and hasProperties("external_id" -> extId)
      )
      there was atLeastOne(providerManager).getProviderImpl(ResourceIds.KubeProvider)
    }

  }

}