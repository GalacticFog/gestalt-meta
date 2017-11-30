package controllers

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.genericactions.{GenericActionInvocation, GenericProvider, GenericProviderManager}
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test._
import controllers.util.ContainerService
import org.mockito.ArgumentCaptor
import org.mockito.Matchers.{eq => meq}
import org.specs2.execute.{AsResult, Result}
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.matcher.{JsonMatchers, Matcher}
import play.api.inject.bind
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.{JsString, Json}
import play.api.test.{PlaySpecification, WithApplication}
import services.{MarathonClientFactory, SkuberFactory}

import scala.concurrent.Future
import scala.util.{Success, Try}

/*
    FINISH: this test is currently built around Blueprint resoruces, but it could easily be modified to be a test of generic provider-backed resource capability
    1) create new provider type
    2) create new resource type, backed by provider type from 1
    3) test just like below using that instead of Blueprint/BlueprintProvider

    this should be done as part of the work to remove the hard-coded route entries in favor of generic routing
 */
class BlueprintControllerSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  def hasName(name: => String): Matcher[GestaltResourceInstance] =
    ((_: GestaltResourceInstance).name) ^^ be_==(name)

  def hasProperties(props: (String,String)*): Matcher[GestaltResourceInstance] =
    ((_: GestaltResourceInstance).properties.get) ^^ havePairs(props:_*)

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
    bind(classOf[ContainerService]).toInstance(mock[ContainerService]),
    bind(classOf[ProviderManager]).toInstance(mock[ProviderManager]),
    bind(classOf[MarathonClientFactory]).toInstance(mock[MarathonClientFactory]),
    bind(classOf[SkuberFactory]).toInstance(mock[SkuberFactory]),
    bind(classOf[GenericProviderManager]).toInstance(mock[GenericProviderManager])
  ))

  abstract class TestActionProvider extends WithApplication(appWithMocks()) {
    var testWork: GestaltResourceInstance = null
    var testEnv: GestaltResourceInstance = null
    var providerManager: GenericProviderManager = null

    var testProvider: GestaltResourceInstance = null
    var mockActionProvider: GenericProvider = null

    override def around[T: AsResult](t: => T): Result = super.around {
      scalikejdbc.config.DBs.setupAll()

      val Success(wrkEnv) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
      testWork = wrkEnv._1
      testEnv = wrkEnv._2

      Ents.setNewEntitlements(dummyRootOrgId, testEnv.id,  user, Some(testWork.id))
      Ents.setNewEntitlements(dummyRootOrgId, testWork.id, user, Some(dummyRootOrgId))
      Ents.setNewEntitlements(dummyRootOrgId, dummyRootOrgId, user, None)

      val injector = app.injector
      providerManager = injector.instanceOf[GenericProviderManager]

      testProvider = createInstance(
        typeId = ResourceIds.BlueprintProvider,
        name = "test-provider",
        parent = Option(testEnv.id),
        properties = Option(Map(
          "parent" -> "{}",
          "config" -> "{}"
        ))
      ).get
      mockActionProvider = mock[GenericProvider]
      providerManager.getProvider(testProvider) returns Success(mockActionProvider)
      t
    }
  }

  stopOnFail

  "BlueprintController" should {

    import com.galacticfog.gestalt.meta.api.errors._

    "create containers without .properties.provider should 400" in new TestActionProvider {
      val request = fakeAuthRequest(POST, s"/root/blueprints", testCreds).withBody(
        Json.obj(
          "name" -> "test-blueprint",
          "properties" -> Json.obj(
            "blueprint_type" -> "docker-compose",
            "native_form" -> "",
            "canonical_form" -> ""
          )
        )
      )
      val Some(result) = route(request)
      contentAsString(result) must contain("requires a provider")
      status(result) must equalTo(BAD_REQUEST)
    }

    "create containers with non-existent provider should 400" in new TestActionProvider {
      val request = fakeAuthRequest(POST, s"/root/blueprints", testCreds).withBody(
        Json.obj(
          "name" -> "test-blueprint",
          "properties" -> Json.obj(
            "blueprint_type" -> "docker-compose",
            "native_form" -> "",
            "canonical_form" -> "",
            "provider" -> UUID.randomUUID().toString
          )
        )
      )
      val Some(result) = route(request)
      contentAsString(result) must contain("not found")
      status(result) must equalTo(BAD_REQUEST)
    }

    "create org blueprints using the ActionProvider interface" in new TestActionProvider {
      val testBlueprintName = "test-blueprint"
      mockActionProvider.invokeAction(any) answers {
        (a: Any) =>
          val invocation = a.asInstanceOf[GenericActionInvocation]
          Future.successful(Left(invocation.resource.get.copy(
            properties = Some(invocation.resource.get.properties.get ++ Map(
              "canonical_form" -> "meta canonical form"
            ))
          )))
      }

      val request = fakeAuthRequest(POST, s"/root/blueprints", testCreds).withBody(
        Json.obj(
          "name" -> testBlueprintName,
          "properties" -> Json.obj(
            "provider" -> testProvider.id,
            "blueprint_type" -> "docker-compose",
            "native_form" -> "blueprint data goes here"
          )
        )
      )
      val Some(result) = route(request)
      status(result) must equalTo(CREATED)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome
      (json \ "name").asOpt[String] must beSome(testBlueprintName)
      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Blueprint")
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "blueprint_type").asOpt[String] must beSome("docker-compose")
      (json \ "properties" \ "native_form").asOpt[String] must beSome("blueprint data goes here")
      (json \ "properties" \ "canonical_form").asOpt[String] must beSome("meta canonical form")

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(providerManager).getProvider(testProvider)
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== "blueprint.create"
      invocation.context.org.id must_== dummyRootOrgId
      invocation.context.workspace must beNone
      invocation.context.environment must beNone
      invocation.provider must_== testProvider
      invocation.resource must beSome(
        hasName(testBlueprintName)
          and
          hasProperties(
            "provider" -> testProvider.id.toString,
            "blueprint_type" -> "docker-compose",
            "native_form" -> "blueprint data goes here"
          )
      )
    }

    "create workspace blueprints using the ActionProvider interface" in new TestActionProvider {
      val testBlueprintName = "test-blueprint"
      mockActionProvider.invokeAction(any) answers {
        (a: Any) =>
          val invocation = a.asInstanceOf[GenericActionInvocation]
          Future.successful(Left(invocation.resource.get.copy(
            properties = Some(invocation.resource.get.properties.get ++ Map(
              "canonical_form" -> "meta canonical form"
            ))
          )))
      }

      val request = fakeAuthRequest(POST, s"/root/workspaces/${testWork.id}/blueprints", testCreds).withBody(
        Json.obj(
          "name" -> testBlueprintName,
          "properties" -> Json.obj(
            "provider" -> testProvider.id,
            "blueprint_type" -> "docker-compose",
            "native_form" -> "blueprint data goes here"
          )
        )
      )
      val Some(result) = route(request)
      status(result) must equalTo(CREATED)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome
      (json \ "name").asOpt[String] must beSome(testBlueprintName)
      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Blueprint")
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "blueprint_type").asOpt[String] must beSome("docker-compose")
      (json \ "properties" \ "native_form").asOpt[String] must beSome("blueprint data goes here")
      (json \ "properties" \ "canonical_form").asOpt[String] must beSome("meta canonical form")

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(providerManager).getProvider(testProvider)
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== "blueprint.create"
      invocation.context.org.id must_== dummyRootOrgId
      invocation.context.workspace must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testWork.id) )
      invocation.context.environment must beNone
      invocation.provider must_== testProvider
      invocation.resource must beSome(
        hasName(testBlueprintName)
          and
          hasProperties(
            "provider" -> testProvider.id.toString,
            "blueprint_type" -> "docker-compose",
            "native_form" -> "blueprint data goes here"
          )
      )
    }

    "create environment blueprints using the ActionProvider interface" in new TestActionProvider {
      val testBlueprintName = "test-blueprint"
      mockActionProvider.invokeAction(any) answers {
        (a: Any) =>
          val invocation = a.asInstanceOf[GenericActionInvocation]
          Future.successful(Left(invocation.resource.get.copy(
            properties = Some(invocation.resource.get.properties.get ++ Map(
              "canonical_form" -> "meta canonical form"
            ))
          )))
      }

      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/blueprints", testCreds).withBody(
        Json.obj(
          "name" -> testBlueprintName,
          "properties" -> Json.obj(
            "provider" -> testProvider.id,
            "blueprint_type" -> "docker-compose",
            "native_form" -> "blueprint data goes here"
          )
        )
      )
      val Some(result) = route(request)
      status(result) must equalTo(CREATED)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome
      (json \ "name").asOpt[String] must beSome(testBlueprintName)
      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Blueprint")
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "blueprint_type").asOpt[String] must beSome("docker-compose")
      (json \ "properties" \ "native_form").asOpt[String] must beSome("blueprint data goes here")
      (json \ "properties" \ "canonical_form").asOpt[String] must beSome("meta canonical form")

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(providerManager).getProvider(testProvider)
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== "blueprint.create"
      invocation.context.org.id must_== dummyRootOrgId
      invocation.context.workspace must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testWork.id) )
      invocation.context.environment must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testEnv.id) )
      invocation.provider must_== testProvider
      invocation.resource must beSome(
        hasName(testBlueprintName)
          and
          hasProperties(
            "provider" -> testProvider.id.toString,
            "blueprint_type" -> "docker-compose",
            "native_form" -> "blueprint data goes here"
          )
      )
    }

    "deploy blueprints using the ActionProvider interface with Json payload without update semantics" in new TestActionProvider {
      val testBlueprintName = "test-blueprint-deploy"
      val createdResource = createInstance(ResourceIds.Blueprint, testBlueprintName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> testProvider.id.toString,
          "blueprint_type" -> "docker-compose",
          "native_form" -> "native",
          "canonical_form" -> "canonical"
        ))
      ).get
      mockActionProvider.invokeAction(any) answers {
        (a: Any) =>
          val invocation = a.asInstanceOf[GenericActionInvocation]
          val name = invocation.payload.flatMap(j => (j \ "name").asOpt[String])
          Future.successful(Right(
            (Some(202),Some("text/plain"), Some(s"Hello, ${name.getOrElse("world")}"))
          ))
      }

      val payload = Json.obj(
        "name" -> "Chris"
      )
      val request = fakeAuthRequest(POST,
        s"/root/blueprints/${createdResource.id}?action=deploy", testCreds
      ).withBody(payload)
      val Some(result) = route(request)
      status(result) must equalTo(202)
      contentAsString(result) must_== "Hello, Chris"
      contentType(result) must beSome("text/plain")

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(providerManager).getProvider(testProvider)
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== "blueprint.deploy"
      invocation.context.org.id must_== dummyRootOrgId
      invocation.context.workspace must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testWork.id) )
      invocation.context.environment must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testEnv.id) )
      invocation.provider must_== testProvider
      invocation.resource must beSome(createdResource)
      invocation.payload must beSome(payload)
    }

    "deploy blueprints using the ActionProvider interface with text payload with update semantics" in new TestActionProvider {
      val testBlueprintName = "test-blueprint-deploy"
      val createdResource = createInstance(ResourceIds.Blueprint, testBlueprintName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> testProvider.id.toString,
          "blueprint_type" -> "docker-compose",
          "native_form" -> "native",
          "canonical_form" -> "canonical"
        ))
      ).get
      mockActionProvider.invokeAction(any) answers {
        (a: Any) =>
          val invocation = a.asInstanceOf[GenericActionInvocation]
          val updatedCF = invocation.payload.flatMap(_.asOpt[String]) getOrElse ""
          val resource = invocation.resource.get
          Future.successful(Left(
            resource.copy(
              properties = Some(resource.properties.get ++ Map(
                "canonical_form" -> updatedCF
              ))
            )
          ))
      }

      val payload = "updated canonical form during blueprint"

      val request = fakeAuthRequest(POST,
        s"/root/blueprints/${createdResource.id}?action=deploy", testCreds
      ).withBody(payload)
      val Some(result) = route(request)
      status(result) must equalTo(200)
      contentAsJson(result).toString must /("properties") /("canonical_form" -> payload)

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(providerManager).getProvider(testProvider)
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== "blueprint.deploy"
      invocation.context.org.id must_== dummyRootOrgId
      invocation.context.workspace must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testWork.id) )
      invocation.context.environment must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testEnv.id) )
      invocation.provider must_== testProvider
      invocation.resource must beSome(createdResource)
      invocation.payload must beSome(JsString(payload))

      val Some(updatedResource) = ResourceFactory.findById(createdResource.id)
      updatedResource.properties.get must havePairs(
        "provider" -> testProvider.id.toString,
        "blueprint_type" -> "docker-compose",
        "native_form" -> "native",
        "canonical_form" -> payload
      )
    }

    "delete blueprints using the ActionProvider interface" in new TestActionProvider {
      val testBlueprintName = "test-blueprint-delete"
      val createdResource = createInstance(ResourceIds.Blueprint, testBlueprintName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> testProvider.id.toString,
          "blueprint_type" -> "docker-compose",
          "native_form" -> "native",
          "canonical_form" -> "canonical"
        ))
      ).get
      mockActionProvider.invokeAction(any) returns Future.successful(Right(None,None,None))

      val request = fakeAuthRequest(DELETE,
        s"/root/environments/${testEnv.id}/blueprints/${createdResource.id}", testCreds
      )
      val Some(result) = route(request)
      println(contentAsString(result))
      status(result) must equalTo(204)

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(providerManager).getProvider(testProvider)
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== "blueprint.delete"
      invocation.context.org.id must_== dummyRootOrgId
      invocation.context.workspace must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testWork.id) )
      invocation.context.environment must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testEnv.id) )
      invocation.provider must_== testProvider
      invocation.resource must beSome(createdResource)
      invocation.payload must beNone

      ResourceFactory.findById(createdResource.id) must beNone
    }


  }


  "ActionProviderManager" should {

    "return the right class or something" in {
      ko("write me")
    }

  }


  "ActionProviderImpl" in {

    "post to lambdas appropriately" in {
      ko("write me")
    }

  }


}