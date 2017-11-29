package controllers

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.actions.{ActionContext, ActionInvocation, ActionProvider, ActionProviderManager}
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test._
import controllers.util.ContainerService
import org.mockito.ArgumentCaptor
import org.mockito.Matchers.{eq => meq}
import org.specs2.execute.{AsResult, Result}
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.matcher.JsonMatchers
import play.api.inject.bind
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.Json
import play.api.test.{PlaySpecification, WithApplication}
import services.{MarathonClientFactory, SkuberFactory}

import scala.concurrent.Future
import scala.util.{Success, Try}

class BlueprintControllerSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {

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
    bind(classOf[ContainerService]).toInstance(mock[ContainerService]),
    bind(classOf[ProviderManager]).toInstance(mock[ProviderManager]),
    bind(classOf[MarathonClientFactory]).toInstance(mock[MarathonClientFactory]),
    bind(classOf[SkuberFactory]).toInstance(mock[SkuberFactory]),
    bind(classOf[ActionProviderManager]).toInstance(mock[ActionProviderManager])
  ))

  abstract class TestActionProvider extends WithApplication(appWithMocks()) {
    var testWork: GestaltResourceInstance = null
    var testEnv: GestaltResourceInstance = null
    var providerManager: ActionProviderManager = null

    var testProvider: GestaltResourceInstance = null
    var mockActionProvider: ActionProvider = null

    override def around[T: AsResult](t: => T): Result = super.around {
      scalikejdbc.config.DBs.setupAll()

      val Success(wrkEnv) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
      testWork = wrkEnv._1
      testEnv = wrkEnv._2

      Ents.setNewEntitlements(dummyRootOrgId, testEnv.id,  user, Some(testWork.id))
      Ents.setNewEntitlements(dummyRootOrgId, testWork.id, user, Some(dummyRootOrgId))
      Ents.setNewEntitlements(dummyRootOrgId, dummyRootOrgId, user, None)

      val injector = app.injector
      providerManager = injector.instanceOf[ActionProviderManager]

      testProvider = createInstance(
        typeId = ResourceIds.BlueprintProvider,
        name = "test-provider",
        parent = Option(testEnv.id),
        properties = Option(Map(
          "parent" -> "{}",
          "config" -> "{}"
        ))
      ).get
      mockActionProvider = mock[ActionProvider]
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
          val invocation = a.asInstanceOf[ActionInvocation]
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
      println(contentAsString(result))
      status(result) must equalTo(CREATED)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome
      (json \ "name").asOpt[String] must beSome(testBlueprintName)
      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Blueprint")
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "blueprint_type").asOpt[String] must beSome("docker-compose")
      (json \ "properties" \ "native_form").asOpt[String] must beSome("blueprint data goes here")
      (json \ "properties" \ "canonical_form").asOpt[String] must beSome("meta canonical form")

      val invocationCaptor = ArgumentCaptor.forClass(classOf[ActionInvocation])
      there was atLeastOne(providerManager).getProvider(testProvider)
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== "blueprint.create"
      invocation.context.org.id must_== dummyRootOrgId
      invocation.context.workspace must beNone
      invocation.context.environment must beNone
      invocation.provider must_== testProvider
      invocation.resource must beSome
    }

    "create workspace blueprints using the ActionProvider interface" in new TestActionProvider {
      val testBlueprintName = "test-blueprint"
      mockActionProvider.invokeAction(any) answers {
        (a: Any) =>
          val invocation = a.asInstanceOf[ActionInvocation]
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
      println(contentAsString(result))
      status(result) must equalTo(CREATED)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome
      (json \ "name").asOpt[String] must beSome(testBlueprintName)
      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Blueprint")
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "blueprint_type").asOpt[String] must beSome("docker-compose")
      (json \ "properties" \ "native_form").asOpt[String] must beSome("blueprint data goes here")
      (json \ "properties" \ "canonical_form").asOpt[String] must beSome("meta canonical form")

      val invocationCaptor = ArgumentCaptor.forClass(classOf[ActionInvocation])
      there was atLeastOne(providerManager).getProvider(testProvider)
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== "blueprint.create"
      invocation.context.org.id must_== dummyRootOrgId
      invocation.context.workspace must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testWork.id) )
      invocation.context.environment must beNone
      invocation.provider must_== testProvider
      invocation.resource must beSome
    }

    "create environment blueprints using the ActionProvider interface" in new TestActionProvider {
      val testBlueprintName = "test-blueprint"
      mockActionProvider.invokeAction(any) answers {
        (a: Any) =>
          val invocation = a.asInstanceOf[ActionInvocation]
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
      println(contentAsString(result))
      status(result) must equalTo(CREATED)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome
      (json \ "name").asOpt[String] must beSome(testBlueprintName)
      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Blueprint")
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "blueprint_type").asOpt[String] must beSome("docker-compose")
      (json \ "properties" \ "native_form").asOpt[String] must beSome("blueprint data goes here")
      (json \ "properties" \ "canonical_form").asOpt[String] must beSome("meta canonical form")

      val invocationCaptor = ArgumentCaptor.forClass(classOf[ActionInvocation])
      there was atLeastOne(providerManager).getProvider(testProvider)
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== "blueprint.create"
      invocation.context.org.id must_== dummyRootOrgId
      invocation.context.workspace must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testWork.id) )
      invocation.context.environment must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testEnv.id) )
      invocation.provider must_== testProvider
      invocation.resource must beSome
    }

    "deploy blueprints using the ActionProvider interface without update semantics" in new TestActionProvider {
      val testBlueprintName = "test-blueprint"
      val createdResource = createInstance(ResourceIds.Blueprint, testBlueprintName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> testProvider.id.toString,
          "blueprint_type" -> "docker-compose",
          "native_form" -> "blueprint data goes here",
          "canonical_form" -> "canonical form is here"
        ))
      ).get
      mockActionProvider.invokeAction(any) answers {
        (a: Any) =>
          val invocation = a.asInstanceOf[ActionInvocation]
          val name = invocation.payload.flatMap(s => Try(Json.parse(s)).toOption).flatMap(j => (j \ "name").asOpt[String])
          Future.successful(Right(
            (Some(200),Some("text/plain"), Some(s"hello, ${name.getOrElse("world")}"))
          ))
      }

      val payload = Json.obj(
        "name" -> "Chris"
      )
      val request = fakeAuthRequest(POST,
        s"/root/blueprints/${createdResource.id}/deploy", testCreds
      ).withBody(payload)
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      there was one(mockActionProvider).invokeAction(meq(ActionInvocation(
        action = "blueprint.deploy",
        context = ActionContext(
          org = ResourceFactory.findById(dummyRootOrgId).get,
          workspace = Some(testWork),
          environment = Some(testEnv)
        ),
        provider = testProvider,
        resource = Some(createdResource),
        payload = Some(payload.toString)
      )))
      there was atLeastOne(providerManager).getProvider(testProvider)
    }

    "delete blueprints using the ActionProvider interface" in new TestActionProvider {
      val testBlueprintName = "test-blueprint"
      val createdResource = createInstance(ResourceIds.Blueprint, testBlueprintName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> testProvider.id.toString,
          "blueprint_type" -> "docker-compose",
          "native_form" -> "blueprint data goes here",
          "canonical_form" -> "canonical form is here"
        ))
      ).get
      mockActionProvider.invokeAction(any) returns Future.successful(Right(None,None,None))

      val request = fakeAuthRequest(DELETE,
        s"/root/environments/${testEnv.id}/blueprints/${createdResource.id}", testCreds
      )
      val Some(result) = route(request)
      status(result) must equalTo(NO_CONTENT)
      there was one(mockActionProvider).invokeAction(meq(ActionInvocation(
        action = "blueprint.delete",
        context = ActionContext(
          org = ResourceFactory.findById(dummyRootOrgId).get,
          workspace = Some(testWork),
          environment = Some(testEnv)
        ),
        provider = testProvider,
        resource = Some(createdResource)
      )))
      there was atLeastOne(providerManager).getProvider(testProvider)
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