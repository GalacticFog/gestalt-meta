package controllers

import com.galacticfog.gestalt.meta.api.sdk
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.providers.{ProviderEnv, ProviderMap}
import com.galacticfog.gestalt.meta.test._
import controllers.util.{ContainerService, GestaltProviderMocking}
import org.specs2.matcher.JsonMatchers
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification.BeforeAll
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.{JsObject, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.{PlaySpecification, WithApplication}
import play.api.inject.bind
import services.{DockerClientFactory, MarathonClientFactory, SkuberFactory}

import scala.util.Success

class MetaSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {

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
    Ents.setNewEntitlements(dummyRootOrgId, dummyRootOrgId, user, None)
  }
  
  sequential
  
  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  abstract class TestApplication extends WithApplication(
    application(
      additionalBindings = Seq(
        bind(classOf[ContainerService]).toInstance(mock[ContainerService]),
        bind(classOf[SkuberFactory]).toInstance(mock[SkuberFactory]),
        bind(classOf[DockerClientFactory]).toInstance(mock[DockerClientFactory]),
        bind(classOf[MarathonClientFactory]).toInstance(mock[MarathonClientFactory])
      )
    )
  ) {
    import org.specs2.execute.{AsResult,Result}
    
    override def around[T: AsResult](t: => T): Result = super.around {
      println("******************AROUND")
      scalikejdbc.config.DBs.closeAll()
      scalikejdbc.config.DBs.setupAll()
      t
    }    
  }

  "Meta Controller" should {
    "preserve additional provider config in .properties.config when linking containers" in new TestApplication {
      val Success(testProvider) = createInstance(ResourceIds.DcosProvider, "test-provider",
        parent = Option(dummyRootOrgId),
        properties = Option(Map(
          "parent" -> "{}",
          "config" -> """{"env": "{}", "external_protocol": "https", "another_field": "blah"}"""
        )))
      val meta = app.injector.instanceOf[Meta]
      val Success(updatedProvider) = meta.saveProvider(ProviderMap(testProvider),ProviderEnv(None,None),adminUserId)
      updatedProvider.properties.get("config") must /("external_protocol" -> "https")
      updatedProvider.properties.get("config") must /("another_field" -> "blah")
    }
  }

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