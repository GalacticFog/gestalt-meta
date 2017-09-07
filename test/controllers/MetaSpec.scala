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
import com.galacticfog.gestalt.data.EnvironmentType
import com.galacticfog.gestalt.meta.api.errors._

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
    
    "normalizeProviderPayload" should {

      "accept payloads with valid environment_types" in new TestApplication {        
        val payload = Json.parse(
            """
            |{
            |  "name": "KubernetesProvider-1",
            |  "description": "A Kubernetes Cluster.",
            |  "resource_type": "Gestalt::Configuration::Provider::CaaS::Kubernetes",
            |  "properties": {
            |  	"environment_types": ["development", "test", "production"],
            |  	"config": {}
            |  }
            |}""".trim.stripMargin)
            
        val envtype = EnvironmentType.id("test")
        val parent = createInstance(
            ResourceIds.Environment, 
            uuid.toString, 
            properties = Some(Map("environment_type" -> envtype.toString)))
        parent must beSuccessfulTry
        
        val meta = app.injector.instanceOf[Meta]
        
        val result = meta.normalizeProviderPayload(payload, uuid(), ResourceIds.KubeProvider, parent.get, None)
        result must beSuccessfulTry
      }
      
      "reject payloads with invalid environment_types" in new TestApplication {
        val payload = Json.parse(
            """
            |{
            |  "name": "KubernetesProvider-1",
            |  "description": "A Kubernetes Cluster.",
            |  "resource_type": "Gestalt::Configuration::Provider::CaaS::Kubernetes",
            |  "properties": {
            |  	"environment_types": ["development", "test", "does_not_exist"],
            |  	"config": {}
            |  }
            |}""".trim.stripMargin)
        
        val envtype = EnvironmentType.id("test")
        val parent = createInstance(
            ResourceIds.Environment, 
            uuid.toString, 
            properties = Some(Map("environment_type" -> envtype.toString)))
        
        parent must beSuccessfulTry
        
        val meta = app.injector.instanceOf[Meta]
        
        val result = scala.util.Try {
          meta.normalizeProviderPayload(payload, uuid(), ResourceIds.KubeProvider, parent.get, None)
        }
        result must beFailedTry.withThrowable[UnprocessableEntityException]
      }
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