package controllers

import java.util.UUID

import com.galacticfog.gestalt.meta.api.sdk
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.providers.{ProviderEnv, ProviderMap}
import com.galacticfog.gestalt.meta.test._
import controllers.util.{ContainerService, GestaltProviderMocking}
import org.specs2.matcher.JsonMatchers
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification.BeforeAll
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.{PlaySpecification, WithApplication}
import play.api.inject.bind
import services.{DockerClientFactory, MarathonClientFactory, SkuberFactory}

import scala.util.Success
import com.galacticfog.gestalt.data.EnvironmentType
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}


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
  
  "updateLinkedProviders" should {
    "add empty linked_providers to Provider JSON if not given" in new TestApplication {
      val payload = Json.parse {
      s"""
      |{
      |  "name": "${uuid.toString}",
      |  "description": "Test security provider",
      |  "resource_type": "Gestalt::Configuration::Provider::Security",
      |  "properties": {
      |    "config": {
      |      "env": {
      |        "public": {
      |           "KEY": "123-456-789",
      |		        "SECRET": "thequickbrownfox",
      |		        "HOSTNAME": "10.10.10.10",
      |		        "PORT": "6543",
      |		        "PROTOCOL": "http"
      |        },
      |        "private": {
      |        }
      |      }
      |    },
      |    "services": []
      |  }
      |}
      """.trim.stripMargin        
      }
      
      val meta = app.injector.instanceOf[Meta]
      val result = meta.updateLinkedProviders(payload.as[JsObject])
      result must beSuccessfulTry
      
      val lps1 = Js.find(result.get, "/properties/linked_providers")
      lps1 must beSome(Json.arr())
    }
  }

  "defaultLinkedProviders" should {

    "leave Provider.properties.linked_providers untouched when given in the payload" in new TestApplication {
      val payload = Json.parse {
        s"""
        |{
        |  "name":"gateway-manager-1",
        |  "description":"gateway provider",
        |  "resource_type":"Gestalt::Configuration::Provider::GatewayManager",
        |  "properties":{
        |    "config":{
        |      "env":{
        |        "public":{
        |
        |         },
        |         "private":{
        |           "GATEWAY_DATABASE_NAME":"brad-gateway-provider2"
        |        }
        |      }
        |    },
        |    "linked_providers":[
        |      {
        |        "name":"KONG_0",
        |        "id":"1"
        |      },
        |      {
        |        "name":"GATEWAY_DATABASE",
        |        "id":"2"
        |      },
        |      {
        |        "name":"GESTALT_SECURITY",
        |        "id":"3"
        |      }
        |    ]
        |  }
        |}
        """.trim.stripMargin
      }.as[JsObject]

      val meta = app.injector.instanceOf[Meta]

      /* get array from original payload */
      val before1 = Js.find(payload, "/properties/linked_providers")
      before1 must beSome
      val before2 = Js.parse[Seq[JsValue]](before1.get)
      before2 must beSuccessfulTry

      /* get array from 'updated' payload */
      val updated = meta.defaultLinkedProviders(payload)
      val after1 = Js.find(payload, "/properties/linked_providers")
      after1 must beSome
      val after2 = Js.parse[Seq[JsValue]](after1.get)
      after2 must beSuccessfulTry

      before2.get.equals(after2.get) === true
    }

  }

  "Generic providers" should {

    "be linkable to self" in new TestApplication {
      val kubeProviderPayload1 = Json.parse(
        s"""
           |{
           |    "name": "test-kube-3",
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

      val request = fakeAuthRequest(POST, s"/root/providers", testCreds).withBody(kubeProviderPayload1)
      val Some(result) = route(request)
      status(result) must equalTo(CREATED)
      val providerId = (contentAsJson(result) \ "id").as[UUID]

      val request2 = fakeAuthRequest(PATCH, s"/root/providers/${providerId}", testCreds).withBody(
        PatchDocument(PatchOp.Replace("/properties/linked_providers", Json.arr(
          Json.obj(
            "name" -> "SELF_LINK",
            "id" -> providerId.toString
          )
        ))).toJson
      )

      val Some(result2) = route(request2)
      status(result2) must equalTo(OK)
      val lps = (contentAsJson(result2) \ "properties" \ "linked_providers").as[Seq[JsObject]]
      lps must haveSize(1)
      lps.head must_== Json.obj(
        "name" -> "SELF_LINK",
        "id" -> providerId.toString,
        "typeId" -> ResourceIds.KubeProvider.toString,
        "type" -> sdk.ResourceName(ResourceIds.KubeProvider)
      )
    }

    "have linked_providers be rendered type info" in new TestApplication {
      val kubeProviderPayload1 = Json.parse(
        s"""
           |{
           |    "name": "test-kube-1",
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

      val request = fakeAuthRequest(POST, s"/root/providers", testCreds).withBody(kubeProviderPayload1)
      val Some(result) = route(request)
      status(result) must equalTo(CREATED)
      val providerId = (contentAsJson(result) \ "id").as[UUID]

      val kubeProviderPayload2 = Json.parse(
        s"""
           |{
           |    "name": "test-kube-2",
           |    "description": "",
           |    "properties": {
           |        "config": {},
           |        "data": "RG9uJ3QgcHJvdmlkZXIgbWUsIGJyb1wh",
           |        "environments": [],
           |        "linked_providers": [
           |          {"id": "${providerId}", "name": "LINK"}
           |        ],
           |        "locations": []
           |    },
           |    "resource_state": "Gestalt::Resource::State::Active",
           |    "resource_type": "${sdk.ResourceName(ResourceIds.KubeProvider)}"
           |}
        """.stripMargin
      )


      val request2 = fakeAuthRequest(POST, s"/root/providers", testCreds).withBody(kubeProviderPayload2)
      val Some(result2) = route(request2)
      status(result2) must equalTo(CREATED)
      val lps = (contentAsJson(result2) \ "properties" \ "linked_providers").as[Seq[JsObject]]
      lps must haveSize(1)
      lps.head must_== Json.obj(
        "name" -> "LINK",
        "id" -> providerId.toString,
        "typeId" -> ResourceIds.KubeProvider.toString,
        "type" -> sdk.ResourceName(ResourceIds.KubeProvider)
      )
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