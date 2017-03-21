package controllers

import com.galacticfog.gestalt.meta.api.sdk
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import controllers.util.{ContainerService, GestaltSecurityMocking}
import org.specs2.matcher.JsonMatchers
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification.BeforeAll
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.{JsObject, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.{PlaySpecification, WithApplication}
import play.api.inject.bind

import scala.util.Success

class MetaControllerSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll with JsonMatchers {

  override def beforeAll(): Unit = {
    pristineDatabase()
    val Success(createdUser) = Ents.createNewMetaUser(user, dummyRootOrgId, user.account,
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

  abstract class FakeSecurity extends WithApplication(application(
    additionalBindings = Seq(
      bind(classOf[ContainerService]).toInstance(mock[ContainerService])
    )
  ))

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  trait TestApplication extends FakeSecurity {
    Ents.setNewEntitlements(dummyRootOrgId, dummyRootOrgId, user, None)
  }

  //val in = new Loader(testAuthResponse, mock[GestaltSecurityConfig], mock[GestaltSecurityClient]) 

  "Kubernetes providers" should {

    "be created with a \"default\" network" in new TestApplication {
      val kubeProviderPayload = Json.parse(
        s"""
          |{
          |    "name": "test-kube",
          |    "description": "",
          |    "properties": {
          |        "config": {},
          |        "data": "RG9uJ3QgcHJvdmlkZXIgbWUsIGJyb1wh",
          |        "environments": [],
          |        "locations": []
          |    },
          |    "resource_state": "Gestalt::Resource::State::Active",
          |    "resource_type": "${sdk.ResourceName(ResourceIds.KubeProvider)}"
          |}
        """.stripMargin
      )

      val request = fakeAuthRequest(POST, s"/root/providers", testCreds).withBody(kubeProviderPayload)

      val Some(result) = route(request)

      status(result) must equalTo(CREATED)

      val json = contentAsJson(result)
      (json \ "properties" \ "config" \ "networks").asOpt[Seq[JsObject]] must beSome(Seq(Json.obj(
        "name" -> "default"
      )))
    }

  }

}