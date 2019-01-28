package controllers

import java.util.UUID
import scala.util.Success
import play.api.libs.json._
import com.galacticfog.gestalt.data.ResourceFactory
import controllers.util._
import org.mockito.Matchers.{eq => _}
import org.specs2.matcher.JsonMatchers
import org.specs2.mock.Mockito
import org.specs2.specification._
import play.api.test._
import com.galacticfog.gestalt.meta.test._


class EnvironmentProviderMappingControllerSpec extends PlaySpecification with GestaltProviderMocking with JsonMatchers with ResourceScope with BeforeAll with Mockito {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    pristineDatabase()
    val Success(_) = Ents.createNewMetaUser(user, dummyRootOrgId, rootOwnerLink(), user.account,
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

  abstract class FakeSecurity extends WithDb(containerApp(
    // additionalBindings = Seq(
    //   bind(classOf[LambdaMethods]).toInstance(mockLambdaMethods),
    //   bind(classOf[GatewayMethods]).toInstance(mockGatewayMethods)
    // )
  )) {
    // lazy val mockSkuberFactory = app.injector.instanceOf[SkuberFactory]
  }

  trait TestApplication extends FakeSecurity {
  }

  "EnvironmentProviderMappingController" should {

    "getAll" in new TestApplication {
      val envId = s"test-environment-${UUID.randomUUID()}"
      val Success((testWork, testEnv)) = createWorkEnv(wrkName = s"test-workspace-${UUID.randomUUID()}", envName = envId)

      Ents.setNewResourceEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

      val providerId = s"${UUID.randomUUID()}"
      val newTestEnv = testEnv.copy(properties=Some(testEnv.properties.get ++ Map("provider_mapping" -> Json.obj(
        providerId -> Json.obj("namespaces" -> Json.arr(
          Json.obj("name" -> "test1", "default" -> true),
          Json.obj("name" -> "test2", "default" -> false)
        ))
      ).toString)))
      ResourceFactory.update(newTestEnv, user.account.id)

      val Some(result) = route(app, fakeAuthRequest(GET, s"/root/environments/${testEnv.id}/providermapping", testCreds))

      contentAsJson(result) must equalTo(Json.obj(
        providerId -> Json.obj("namespaces" -> Json.arr(
          Json.obj("name" -> "test1", "default" -> true),
          Json.obj("name" -> "test2", "default" -> false)
        ))
      ))
    }

    "get" in new TestApplication {
      val envId = s"test-environment-${UUID.randomUUID()}"
      val Success((testWork, testEnv)) = createWorkEnv(wrkName = s"test-workspace-${UUID.randomUUID()}", envName = envId)

      Ents.setNewResourceEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

      val providerId = s"${UUID.randomUUID()}"
      val newTestEnv = testEnv.copy(properties=Some(testEnv.properties.get ++ Map("provider_mapping" -> Json.obj(
        providerId -> Json.obj("namespaces" -> Json.arr(
          Json.obj("name" -> "test1", "default" -> true),
          Json.obj("name" -> "test2", "default" -> false)
        )),
        s"${UUID.randomUUID()}" -> Json.obj("namespaces" -> Json.arr(
          Json.obj("name" -> "test3", "default" -> true),
          Json.obj("name" -> "test4", "default" -> false)
        ))
      ).toString)))
      ResourceFactory.update(newTestEnv, user.account.id).get

      {
        val Some(result) = route(app, fakeAuthRequest(GET, s"/root/environments/${testEnv.id}/providermapping/${providerId}", testCreds))

        contentAsJson(result) must equalTo(Json.obj("namespaces" -> Json.arr(
          Json.obj("name" -> "test1", "default" -> true),
          Json.obj("name" -> "test2", "default" -> false)
        )))
      }
      {
        val Some(result) = route(app, fakeAuthRequest(GET, s"/root/environments/${testEnv.id}/providermapping/${UUID.randomUUID()}", testCreds))

        contentAsJson(result) must equalTo(Json.obj("namespaces" -> Json.arr(
          Json.obj("name" -> s"${testEnv.id}", "default" -> true)
        )))
      }
    }

    "post" in new TestApplication {
      val envId = s"test-environment-${UUID.randomUUID()}"
      val Success((testWork, testEnv)) = createWorkEnv(wrkName = s"test-workspace-${UUID.randomUUID()}", envName = envId)

      Ents.setNewResourceEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

      val providerId = s"${UUID.randomUUID()}"
      val newTestEnv = testEnv.copy(properties=Some(testEnv.properties.get ++ Map("provider_mapping" -> Json.obj(
        providerId -> Json.obj("namespaces" -> Json.arr(
          Json.obj("name" -> "test1", "default" -> true),
          Json.obj("name" -> "test2", "default" -> false)
        )),
        s"${UUID.randomUUID()}" -> Json.obj("namespaces" -> Json.arr(
          Json.obj("name" -> "test3", "default" -> true),
          Json.obj("name" -> "test4", "default" -> false)
        ))
      ).toString)))
      ResourceFactory.update(newTestEnv, user.account.id).get

      {
        val Some(result) = route(app, fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/providermapping/${providerId}", testCreds).withBody(
          "asdsadf"
        ))

        contentAsJson(result) must equalTo(Json.obj(
          "code" -> 400,
          "message" -> "Malformed request payload"
        ))
      }
      {
        val Some(result) = route(app, fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/providermapping/${providerId}", testCreds).withBody(
          Json.obj()
        ))

        contentAsJson(result) must equalTo(Json.obj(
          "code" -> 400,
          "message" -> "Failed to parse payload: /namespaces: error.path.missing"
        ))
      }
      {
        val Some(result) = route(app, fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/providermapping/${providerId}", testCreds).withBody(
          Json.obj("namespaces" -> Json.arr(
            Json.obj("name" -> "test3", "default" -> true),
            Json.obj("name" -> "test4", "default" -> false)
          ))
        ))

        contentAsJson(result) must equalTo(Json.obj("namespaces" -> Json.arr(
          Json.obj("name" -> "test3", "default" -> true),
          Json.obj("name" -> "test4", "default" -> false)
        )))
      }
      {
        val Some(result) = route(app, fakeAuthRequest(GET, s"/root/environments/${testEnv.id}/providermapping/${providerId}", testCreds))

        contentAsJson(result) must equalTo(Json.obj("namespaces" -> Json.arr(
          Json.obj("name" -> "test3", "default" -> true),
          Json.obj("name" -> "test4", "default" -> false)
        )))
      }
    }

    "delete" in new TestApplication {
      val envId = s"test-environment-${UUID.randomUUID()}"
      val Success((testWork, testEnv)) = createWorkEnv(wrkName = s"test-workspace-${UUID.randomUUID()}", envName = envId)

      Ents.setNewResourceEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

      val providerId = s"${UUID.randomUUID()}"
      val newTestEnv = testEnv.copy(properties=Some(testEnv.properties.get ++ Map("provider_mapping" -> Json.obj(
        providerId -> Json.obj("namespaces" -> Json.arr(
          Json.obj("name" -> "test1", "default" -> true),
          Json.obj("name" -> "test2", "default" -> false)
        )),
        s"${UUID.randomUUID()}" -> Json.obj("namespaces" -> Json.arr(
          Json.obj("name" -> "test3", "default" -> true),
          Json.obj("name" -> "test4", "default" -> false)
        ))
      ).toString)))
      ResourceFactory.update(newTestEnv, user.account.id).get

      {
        val Some(result) = route(app, fakeAuthRequest(DELETE, s"/root/environments/${testEnv.id}/providermapping/${providerId}", testCreds))

        contentAsJson(result) must equalTo(Json.obj())
      }
      {
        val Some(result) = route(app, fakeAuthRequest(GET, s"/root/environments/${testEnv.id}/providermapping/${providerId}", testCreds))

        contentAsJson(result) must equalTo(Json.obj("namespaces" -> Json.arr(
          Json.obj("name" -> s"${testEnv.id}", "default" -> true)
        )))
      }
      {
        val Some(result) = route(app, fakeAuthRequest(DELETE, s"/root/environments/${testEnv.id}/providermapping/${UUID.randomUUID()}", testCreds))

        // deleting at missing provider id is a noop
        contentAsJson(result) must equalTo(Json.obj())
      }
    }
  }
  
}

