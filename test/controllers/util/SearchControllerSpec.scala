package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data.bootstrap.LineageInfo
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.{ResourceFactory, TypeFactory}
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.genericactions.GenericProvider.RawInvocationResponse
import com.galacticfog.gestalt.meta.genericactions.{GenericActionInvocation, GenericProvider, GenericProviderManager}
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import controllers.SecurityResources
import org.junit.runner._
import org.mockito.ArgumentCaptor
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.matcher.{JsonMatchers, Matcher}
import org.specs2.runner._
import play.api.inject.bind
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.{JsObject, JsString, Json}
import play.api.test.{PlaySpecification, WithApplication}
import services.{DockerClientFactory, MarathonClientFactory, SkuberFactory}

import scala.concurrent.Future
import scala.util.Success

@RunWith(classOf[JUnitRunner])
class SearchControllerSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources
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
    Ents.setNewEntitlements(dummyRootOrgId, dummyRootOrgId, user, None)
  }

  abstract class testApp extends WithApplication(application(additionalBindings = Seq(
    bind(classOf[ContainerService]).toInstance(mock[ContainerService]),
    bind(classOf[DockerClientFactory]).toInstance(mock[DockerClientFactory]),
    bind(classOf[MarathonClientFactory]).toInstance(mock[MarathonClientFactory]),
    bind(classOf[SkuberFactory]).toInstance(mock[SkuberFactory]),
    bind(classOf[GenericProviderManager]).toInstance(mock[GenericProviderManager])
  )))

  case class testAppWithEnv() extends testApp {

    lazy val (testOrg,testWork,testEnv) = {
      val Success(to) = createOrg(name = uuid().toString)
      val Success((tw,te)) = createWorkEnv(org = to.id)
      Ents.setNewEntitlements(dummyRootOrgId, te.id, user, Some(tw.id))
      Ents.setNewEntitlements(dummyRootOrgId, tw.id, user, Some(dummyRootOrgId))
      Ents.setNewEntitlements(dummyRootOrgId, to.id, user, None)
      (to,tw,te)
    }

  }

  sequential

  "SearchController" should {

    "support resource expansion" in new testAppWithEnv {
      // test with API endpoints, because they are the dominant use case and they have multiple, easily-searchable string fields
      val l1 = UUID.randomUUID()
      val l2 = UUID.randomUUID()
      val r1 = newInstance(ResourceIds.ApiEndpoint, "endpoint-1", org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l1.toString
      )))
      val r2 = newInstance(ResourceIds.ApiEndpoint, "endpoint-2", org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l2.toString
      )))
      val request = fakeAuthRequest(GET,
        s"/${testOrg.name}/resourcetypes/${ResourceIds.ApiEndpoint}/resources/search?implementation_type=lambda&expand=true",
        testCreds
      )
      val Some(result) = route(request)
      status(result) must beEqualTo(OK)
      val json = contentAsJson(result)
      json.as[Seq[JsObject]].flatMap(j => (j \ "id").asOpt[UUID]) must containTheSameElementsAs(Seq(r1.id, r2.id))
      json.as[Seq[JsObject]].flatMap(j => (j \ "properties" \ "implementation_id").asOpt[JsObject]) must containTheSameElementsAs(Seq(r1.id, r2.id))
    }

  }

}