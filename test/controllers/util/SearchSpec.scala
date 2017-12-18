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
class SearchSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {

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
      val l1 = uuid()
      val l2 = uuid()
      val Success(r1) = createInstance(ResourceIds.ApiEndpoint, "endpoint-1", org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l1.toString,
        "resource" -> ""
      )))
      val Success(r2) = createInstance(ResourceIds.ApiEndpoint, "endpoint-2", org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l2.toString,
        "resource" -> ""
      )))
      val request = fakeAuthRequest(GET,
        s"/${testOrg.name}/resourcetypes/${ResourceIds.ApiEndpoint}/resources/search?implementation_type=lambda&expand=true",
        testCreds
      )
      val Some(result) = route(request)
      status(result) must beEqualTo(OK)
      val json = contentAsJson(result)
      json.as[Seq[JsObject]].flatMap(j => (j \ "id").asOpt[UUID]) must containTheSameElementsAs(Seq(r1.id, r2.id))
      json.as[Seq[JsObject]].flatMap(j => (j \ "properties" \ "implementation_id").asOpt[UUID]) must containTheSameElementsAs(Seq(l1, l2))
    }

    "support multiple query parameters" in new testAppWithEnv {
      // test with API endpoints, because they are the dominant use case and they have multiple, easily-searchable string fields
      val l1 = uuid()
      val Success(r1) = createInstance(ResourceIds.ApiEndpoint, "endpoint-1", org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l1.toString,
        "resource" -> ""
      )))
      // we do not want this endpoint to come back in the search
      val Success(r2) = createInstance(ResourceIds.ApiEndpoint, "endpoint-2", org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> uuid().toString,
        "resource" -> ""
      )))
      val request = fakeAuthRequest(GET,
        s"/${testOrg.name}/resourcetypes/${ResourceIds.ApiEndpoint}/resources/search?implementation_type=lambda&implementation_id=${l1}&expand=true",
        testCreds
      )
      val Some(result) = route(request)
      status(result) must beEqualTo(OK)
      val json = contentAsJson(result)
      json.as[Seq[JsObject]].flatMap(j => (j \ "id").asOpt[UUID]) must containTheSameElementsAs(Seq(r1.id))
      json.as[Seq[JsObject]].flatMap(j => (j \ "properties" \ "implementation_id").asOpt[UUID]) must containTheSameElementsAs(Seq(l1))
      json.as[Seq[JsObject]].flatMap(j => (j \ "properties" \ "implementation_type").asOpt[String]) must containTheSameElementsAs(Seq("lambda"))
    }

    "support mixing name and properties in searches" in new testAppWithEnv {
      // test with API endpoints, because they are the dominant use case and they have multiple, easily-searchable string fields
      val l1 = uuid()
      val l2 = uuid()
      val Success(r1) = createInstance(ResourceIds.ApiEndpoint, "endpoint-1", org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l1.toString,
        "resource" -> ""
      )))
      // we do not want this endpoint to come back in the search
      val Success(r2) = createInstance(ResourceIds.ApiEndpoint, "endpoint-2", org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l2.toString,
        "resource" -> ""
      )))
      val request = fakeAuthRequest(GET,
        s"/${testOrg.name}/resourcetypes/${ResourceIds.ApiEndpoint}/resources/search?name=${r1.name}&implementation_id=${l1}&expand=true",
        testCreds
      )
      val Some(result) = route(request)
      status(result) must beEqualTo(OK)
      val json = contentAsJson(result)
      json.as[Seq[JsObject]].flatMap(j => (j \ "id").asOpt[UUID]) must containTheSameElementsAs(Seq(r1.id))
      json.as[Seq[JsObject]].flatMap(j => (j \ "name").asOpt[String]) must containTheSameElementsAs(Seq(r1.name))
      json.as[Seq[JsObject]].flatMap(j => (j \ "properties" \ "implementation_id").asOpt[UUID]) must containTheSameElementsAs(Seq(l1))
    }

    "support searching only by name" in new testAppWithEnv {
      // test with API endpoints, because they are the dominant use case and they have multiple, easily-searchable string fields
      val l1 = uuid()
      val Success(r1) = createInstance(ResourceIds.ApiEndpoint, uuid().toString, org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l1.toString,
        "resource" -> ""
      )))
      // we do not want this endpoint to come back in the search
      val Success(r2) = createInstance(ResourceIds.ApiEndpoint, "endpoint-2", org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> uuid().toString,
        "resource" -> ""
      )))
      val request = fakeAuthRequest(GET,
        s"/${testOrg.name}/resourcetypes/${ResourceIds.ApiEndpoint}/resources/search?name=${r1.name}&expand=true",
        testCreds
      )
      val Some(result) = route(request)
      status(result) must beEqualTo(OK)
      val json = contentAsJson(result)
      json.as[Seq[JsObject]].flatMap(j => (j \ "id").asOpt[UUID]) must containTheSameElementsAs(Seq(r1.id))
      json.as[Seq[JsObject]].flatMap(j => (j \ "name").asOpt[String]) must containTheSameElementsAs(Seq(r1.name))
      json.as[Seq[JsObject]].flatMap(j => (j \ "properties" \ "implementation_id").asOpt[UUID]) must containTheSameElementsAs(Seq(l1))
    }

    "return 400 for repeated query parameters" in new testAppWithEnv {
      val request = fakeAuthRequest(GET,
        s"/${testOrg.name}/resourcetypes/${ResourceIds.ApiEndpoint}/resources/search?implementation_type=lambda&implementation_type=lambda&expand=true",
        testCreds
      )
      val Some(result) = route(request)
      status(result) must beEqualTo(BAD_REQUEST)
      contentAsString(result) must contain("included multiple search terms")
    }

    "return 400 for no query parameters" in new testAppWithEnv {
      val request = fakeAuthRequest(GET,
        s"/${testOrg.name}/resourcetypes/${ResourceIds.ApiEndpoint}/resources/search?expand=true",
        testCreds
      )
      val Some(result) = route(request)
      status(result) must beEqualTo(BAD_REQUEST)
      contentAsString(result) must contain("endpoint requires at least one query parameter")
    }

  }

  "ResourceController l1 queries" should {

    "support expansion" in new testAppWithEnv {
      // test with API endpoints, because they are a primary use case and they have multiple, easily-searchable string fields
      val l1 = uuid()
      val l2 = uuid()
      val Success(r1) = createInstance(ResourceIds.ApiEndpoint, "endpoint-1", org = testOrg.id, parent = Some(testEnv.id), properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l1.toString,
        "resource" -> ""
      )))
      val Success(r2) = createInstance(ResourceIds.ApiEndpoint, "endpoint-2", org = testOrg.id, parent = Some(testEnv.id), properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l2.toString,
        "resource" -> ""
      )))
      val Success(r3) = createInstance(ResourceIds.ApiEndpoint, "endpoint-3", org = testOrg.id, parent = Some(testEnv.id), properties = Some(Map(
        "implementation_type" -> "not-lambda",
        "implementation_id" -> uuid().toString,
        "resource" -> ""
      )))
      val request = fakeAuthRequest(GET,
        s"/${testOrg.name}/apiendpoints?implementation_type=lambda&expand=true",
        testCreds
      )
      val Some(result) = route(request)
      status(result) must beEqualTo(OK)
      val json = contentAsJson(result)
      json.as[Seq[JsObject]].flatMap(j => (j \ "id").asOpt[UUID]) must containTheSameElementsAs(Seq(r1.id, r2.id))
      json.as[Seq[JsObject]].flatMap(j => (j \ "properties" \ "implementation_id").asOpt[UUID]) must containTheSameElementsAs(Seq(l1, l2))
    }

    "support multi-param with expand" in new testAppWithEnv {
      // test with API endpoints, because they are the dominant use case and they have multiple, easily-searchable string fields
      val l1 = uuid()
      val Success(r1) = createInstance(ResourceIds.ApiEndpoint, "endpoint-1", parent = Some(testEnv.id), org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l1.toString,
        "resource" -> ""
      )))
      // we do not want this endpoint to come back in the search
      val Success(r2) = createInstance(ResourceIds.ApiEndpoint, "endpoint-2", parent = Some(testEnv.id), org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> uuid().toString,
        "resource" -> ""
      )))
      val request = fakeAuthRequest(GET,
        s"/${testOrg.name}/apiendpoints?implementation_type=lambda&implementation_id=${l1}&expand=true",
        testCreds
      )
      val Some(result) = route(request)
      status(result) must beEqualTo(OK)
      val json = contentAsJson(result)
      json.as[Seq[JsObject]].flatMap(j => (j \ "id").asOpt[UUID]) must containTheSameElementsAs(Seq(r1.id))
      json.as[Seq[JsObject]].flatMap(j => (j \ "properties" \ "implementation_id").asOpt[UUID]) must containTheSameElementsAs(Seq(l1))
      json.as[Seq[JsObject]].flatMap(j => (j \ "properties" \ "implementation_type").asOpt[String]) must containTheSameElementsAs(Seq("lambda"))
    }

    "support mixing name and properties" in new testAppWithEnv {
      // test with API endpoints, because they are the dominant use case and they have multiple, easily-searchable string fields
      val l1 = uuid()
      val l2 = uuid()
      val Success(r1) = createInstance(ResourceIds.ApiEndpoint, "endpoint-1", parent = Some(testEnv.id), org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l1.toString,
        "resource" -> ""
      )))
      // we do not want this endpoint to come back in the search
      val Success(r2) = createInstance(ResourceIds.ApiEndpoint, "endpoint-2", parent = Some(testEnv.id), org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l2.toString,
        "resource" -> ""
      )))
      val request = fakeAuthRequest(GET,
        s"/${testOrg.name}/apiendpoints?name=${r1.name}&implementation_id=${l1}&expand=true",
        testCreds
      )
      val Some(result) = route(request)
      status(result) must beEqualTo(OK)
      val json = contentAsJson(result)
      json.as[Seq[JsObject]].flatMap(j => (j \ "id").asOpt[UUID]) must containTheSameElementsAs(Seq(r1.id))
      json.as[Seq[JsObject]].flatMap(j => (j \ "name").asOpt[String]) must containTheSameElementsAs(Seq(r1.name))
      json.as[Seq[JsObject]].flatMap(j => (j \ "properties" \ "implementation_id").asOpt[UUID]) must containTheSameElementsAs(Seq(l1))
    }

    "support queries with only name" in new testAppWithEnv {
      // test with API endpoints, because they are the dominant use case and they have multiple, easily-searchable string fields
      val l1 = uuid()
      val Success(r1) = createInstance(ResourceIds.ApiEndpoint, uuid().toString, parent = Some(testEnv.id), org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l1.toString,
        "resource" -> ""
      )))
      // we do not want this endpoint to come back in the search
      val Success(r2) = createInstance(ResourceIds.ApiEndpoint, "endpoint-2", parent = Some(testEnv.id), org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> uuid().toString,
        "resource" -> ""
      )))
      val request = fakeAuthRequest(GET,
        s"/${testOrg.name}/apiendpoints?name=${r1.name}&expand=true",
        testCreds
      )
      val Some(result) = route(request)
      status(result) must beEqualTo(OK)
      val json = contentAsJson(result)
      json.as[Seq[JsObject]].flatMap(j => (j \ "id").asOpt[UUID]) must containTheSameElementsAs(Seq(r1.id))
      json.as[Seq[JsObject]].flatMap(j => (j \ "name").asOpt[String]) must containTheSameElementsAs(Seq(r1.name))
      json.as[Seq[JsObject]].flatMap(j => (j \ "properties" \ "implementation_id").asOpt[UUID]) must containTheSameElementsAs(Seq(l1))
    }

    "return 400 for repeated query parameters" in new testAppWithEnv {
      val request = fakeAuthRequest(GET,
        s"/${testOrg.name}/apiendpoints?implementation_type=lambda&implementation_type=lambda&expand=true",
        testCreds
      )
      val Some(result) = route(request)
      status(result) must beEqualTo(BAD_REQUEST)
      contentAsString(result) must contain("included multiple search terms")
    }

  }

  "ResourceController l2 queries" should {

    "support expansion" in new testAppWithEnv {
      // test with API endpoints, because they are a primary use case and they have multiple, easily-searchable string fields
      val l1 = uuid()
      val l2 = uuid()
      val Success(r1) = createInstance(ResourceIds.ApiEndpoint, "endpoint-1", org = testOrg.id, parent = Some(testEnv.id), properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l1.toString,
        "resource" -> ""
      )))
      val Success(r2) = createInstance(ResourceIds.ApiEndpoint, "endpoint-2", org = testOrg.id, parent = Some(testEnv.id), properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l2.toString,
        "resource" -> ""
      )))
      val Success(r3) = createInstance(ResourceIds.ApiEndpoint, "endpoint-3", org = testOrg.id, parent = Some(testEnv.id), properties = Some(Map(
        "implementation_type" -> "not-lambda",
        "implementation_id" -> uuid().toString,
        "resource" -> ""
      )))
      val request = fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/apiendpoints?implementation_type=lambda&expand=true",
        testCreds
      )
      val Some(result) = route(request)
      status(result) must beEqualTo(OK)
      val json = contentAsJson(result)
      json.as[Seq[JsObject]].flatMap(j => (j \ "id").asOpt[UUID]) must containTheSameElementsAs(Seq(r1.id, r2.id))
      json.as[Seq[JsObject]].flatMap(j => (j \ "properties" \ "implementation_id").asOpt[UUID]) must containTheSameElementsAs(Seq(l1, l2))
    }

    "support multi-param with expand" in new testAppWithEnv {
      // test with API endpoints, because they are the dominant use case and they have multiple, easily-searchable string fields
      val l1 = uuid()
      val Success(r1) = createInstance(ResourceIds.ApiEndpoint, "endpoint-1", parent = Some(testEnv.id), org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l1.toString,
        "resource" -> ""
      )))
      // we do not want this endpoint to come back in the search
      val Success(r2) = createInstance(ResourceIds.ApiEndpoint, "endpoint-2", parent = Some(testEnv.id), org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> uuid().toString,
        "resource" -> ""
      )))
      val request = fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/apiendpoints?implementation_type=lambda&implementation_id=${l1}&expand=true",
        testCreds
      )
      val Some(result) = route(request)
      status(result) must beEqualTo(OK)
      val json = contentAsJson(result)
      json.as[Seq[JsObject]].flatMap(j => (j \ "id").asOpt[UUID]) must containTheSameElementsAs(Seq(r1.id))
      json.as[Seq[JsObject]].flatMap(j => (j \ "properties" \ "implementation_id").asOpt[UUID]) must containTheSameElementsAs(Seq(l1))
      json.as[Seq[JsObject]].flatMap(j => (j \ "properties" \ "implementation_type").asOpt[String]) must containTheSameElementsAs(Seq("lambda"))
    }

    "support mixing name and properties" in new testAppWithEnv {
      // test with API endpoints, because they are the dominant use case and they have multiple, easily-searchable string fields
      val l1 = uuid()
      val l2 = uuid()
      val Success(r1) = createInstance(ResourceIds.ApiEndpoint, "endpoint-1", parent = Some(testEnv.id), org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l1.toString,
        "resource" -> ""
      )))
      // we do not want this endpoint to come back in the search
      val Success(r2) = createInstance(ResourceIds.ApiEndpoint, "endpoint-2", parent = Some(testEnv.id), org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l2.toString,
        "resource" -> ""
      )))
      val request = fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/apiendpoints?name=${r1.name}&implementation_id=${l1}&expand=true",
        testCreds
      )
      val Some(result) = route(request)
      status(result) must beEqualTo(OK)
      val json = contentAsJson(result)
      json.as[Seq[JsObject]].flatMap(j => (j \ "id").asOpt[UUID]) must containTheSameElementsAs(Seq(r1.id))
      json.as[Seq[JsObject]].flatMap(j => (j \ "name").asOpt[String]) must containTheSameElementsAs(Seq(r1.name))
      json.as[Seq[JsObject]].flatMap(j => (j \ "properties" \ "implementation_id").asOpt[UUID]) must containTheSameElementsAs(Seq(l1))
    }

    "support queries with only name" in new testAppWithEnv {
      // test with API endpoints, because they are the dominant use case and they have multiple, easily-searchable string fields
      val l1 = uuid()
      val Success(r1) = createInstance(ResourceIds.ApiEndpoint, uuid().toString, parent = Some(testEnv.id), org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> l1.toString,
        "resource" -> ""
      )))
      // we do not want this endpoint to come back in the search
      val Success(r2) = createInstance(ResourceIds.ApiEndpoint, "endpoint-2", parent = Some(testEnv.id), org = testOrg.id, properties = Some(Map(
        "implementation_type" -> "lambda",
        "implementation_id" -> uuid().toString,
        "resource" -> ""
      )))
      val request = fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/apiendpoints?name=${r1.name}&expand=true",
        testCreds
      )
      val Some(result) = route(request)
      status(result) must beEqualTo(OK)
      val json = contentAsJson(result)
      json.as[Seq[JsObject]].flatMap(j => (j \ "id").asOpt[UUID]) must containTheSameElementsAs(Seq(r1.id))
      json.as[Seq[JsObject]].flatMap(j => (j \ "name").asOpt[String]) must containTheSameElementsAs(Seq(r1.name))
      json.as[Seq[JsObject]].flatMap(j => (j \ "properties" \ "implementation_id").asOpt[UUID]) must containTheSameElementsAs(Seq(l1))
    }

    "return 400 for repeated query parameters" in new testAppWithEnv {
      val request = fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/apiendpoints?implementation_type=lambda&implementation_type=lambda&expand=true",
        testCreds
      )
      val Some(result) = route(request)
      status(result) must beEqualTo(BAD_REQUEST)
      contentAsString(result) must contain("included multiple search terms")
    }

  }

}