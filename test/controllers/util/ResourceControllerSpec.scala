package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data.bootstrap.LineageInfo
import com.galacticfog.gestalt.data.{ResourceFactory, ResourceState, TypeFactory}
import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, GestaltResourceType}
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceStates}
import com.galacticfog.gestalt.meta.genericactions.GenericProvider.RawInvocationResponse
import com.galacticfog.gestalt.meta.genericactions.{GenericActionInvocation, GenericProvider, GenericProviderManager}
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import controllers.SecurityResources
import org.mockito.ArgumentCaptor
import org.mockito.Matchers.{eq => meq}
import org.specs2.execute.{AsResult, Result}
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.matcher.{JsonMatchers, Matcher}
import org.specs2.specification.{BeforeAll, Scope}
import play.api.inject.bind
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.{JsString, Json}
import play.api.test.{PlaySpecification, WithApplication}
import services.{DockerClientFactory, MarathonClientFactory, SkuberFactory}

import scala.concurrent.Future
import scala.util.Success
import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

@RunWith(classOf[JUnitRunner])
class ResourceControllerSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {

  def hasName(name: => String): Matcher[GestaltResourceInstance] =
    ((_: GestaltResourceInstance).name) ^^ be_==(name)

  def hasId(id: => UUID): Matcher[GestaltResourceInstance] =
    ((_: GestaltResourceInstance).id) ^^ be_==(id)

  def hasProperties(props: (String,String)*): Matcher[GestaltResourceInstance] =
    ((_: GestaltResourceInstance).properties.get) ^^ havePairs(props:_*)

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

  sequential

  stopOnFail

  "ResourceController" should {

    val providerTypeName = "Gestalt::Configuration::Provider::TestProvider"
    val providerTypeId = UUID.randomUUID()
    val resourceTypeName = "Gestalt::Configuration::Resource::TestResource"
    val resourceTypeId = UUID.randomUUID()
    val verb1 = "resourceVerb1"
    val verb2 = "resourceVerb2"
    val resourcePrefix = "test-resource-type"

    "support creating new provider types" in new testApp {
      val request = fakeAuthRequest(POST, "/root/resourcetypes", testCreds).withBody(
        Json.obj(
          "id" -> providerTypeId,
          "name" -> providerTypeName,
          "extend" -> ResourceIds.Provider,
          "description" -> "New provider type created during this test!",
          "properties" -> Json.obj(
            "abstract" -> false,
            "lineage" -> Json.obj(
              "parent_types" -> Json.arr(
                ResourceIds.Org,
                ResourceIds.Workspace,
                ResourceIds.Environment
              ),
              "child_types" -> Json.arr(
                ResourceIds.Entitlement
              )
            ),
            "api" -> Json.obj(
              "rest_name" -> s"providers"
            ),
            "actions" -> Json.obj(
              "prefix" -> "provider",
              "verbs" -> Json.arr()
            )
          )
        )
      )
      val Some(result) = route(request)
      (contentAsJson(result) \ "id").as[UUID] must_== providerTypeId
      TypeFactory.findById(providerTypeId) must beSome
    }

    "support creating new provider-backed resource types" in new testApp {
      val request = fakeAuthRequest(POST, "/root/resourcetypes", testCreds).withBody(
        Json.obj(
          "id" -> resourceTypeId,
          "name" -> resourceTypeName,
          "extend" -> ResourceIds.Resource,
          "description" -> "New provider-backed resource type created during this test!",
          "properties" -> Json.obj(
            "abstract" -> false,
            "is_provider_backed" -> true,
            "lineage" -> Json.obj(
              "parent_types" -> Json.arr(
                ResourceIds.Org,
                ResourceIds.Workspace,
                ResourceIds.Environment
              ),
              "child_types" -> Json.arr(
                ResourceIds.Entitlement
              )
            ),
            "api" -> Json.obj(
              "rest_name" -> s"${resourcePrefix}s"
            ),
            "actions" -> Json.obj(
              "prefix" -> resourcePrefix,
              "verbs" -> Json.arr(verb1, verb2)
            )
          ),
          "property_defs" -> Json.arr(
            Json.obj( "name" -> "provider", "data_type" -> "resource::uuid::link", "visibility_type" -> "plain", "requirement_type" -> "required", "refers_to" -> providerTypeId ),
            Json.obj( "name" -> "string_prop", "data_type" -> "string",  "visibility_type" -> "plain", "requirement_type" -> "required"),
            Json.obj( "name" -> "bool_prop",   "data_type" -> "boolean", "visibility_type" -> "plain", "requirement_type" -> "optional")
          )
        )
      )
      val Some(result) = route(request)
      (contentAsJson(result) \ "id").as[UUID] must_== resourceTypeId
      TypeFactory.findById(providerTypeId) must beSome
      def updateLineage(tid: UUID): Unit = {
        TypeFactory.findById(tid) foreach {
          tpe =>
            val newProps = for {
              props <- tpe.properties
              l <- props.get("lineage")
              lineage = Json.parse(l).as[LineageInfo]
              newLineage = lineage.copy(
                child_types = Some(lineage.child_types.getOrElse(Seq.empty) :+ resourceTypeId)
              )
            } yield props + ("lineage" -> Json.toJson(newLineage).toString)
            TypeFactory.update(tpe.copy(
              properties = newProps
            ), adminUserId)
        }
      }
      updateLineage(ResourceIds.Org)
      updateLineage(ResourceIds.Workspace)
      updateLineage(ResourceIds.Environment)
    }

    "create resources without .properties.provider should 400" in new testApp {
      val request = fakeAuthRequest(POST, s"/root/${resourcePrefix}s", testCreds).withBody(
        Json.obj(
          "name" -> "test-resource",
          "properties" -> Json.obj(
            "string_prop" -> "some-string-value",
            "bool_prop" -> false
          )
        )
      )
      val Some(result) = route(request)
      contentAsString(result) must contain("requires a provider")
      status(result) must equalTo(BAD_REQUEST)
    }

    "create resources with non-existent provider should 400" in new testApp {
      val request = fakeAuthRequest(POST, s"/root/${resourcePrefix}s", testCreds).withBody(
        Json.obj(
          "name" -> "test-resource",
          "properties" -> Json.obj(
            "string_prop" -> "some-string-value",
            "bool_prop" -> false,
            "provider" -> UUID.randomUUID().toString
          )
        )
      )
      val Some(result) = route(request)
      contentAsString(result) must contain("not found")
      status(result) must equalTo(BAD_REQUEST)
    }

    val testProviderName = "test-provider-name"
    lazy val testProvider = ResourceFactory.findChildByName(dummyRootOrgId, providerTypeId, testProviderName).get

    case class testAppWithProvider() extends testApp {

      lazy val mockProviderManager = {
        app.injector.instanceOf[GenericProviderManager]
      }

      lazy val mockActionProvider = {
        val map = mock[GenericProvider]
        mockProviderManager.getProvider(
          provider = meq(testProvider),
          action = any
        ) returns Success(Some(map))
        map
      }

      lazy val (testOrg,testWork,testEnv) = {
        val Some(to) = ResourceFactory.findById(ResourceIds.Org, dummyRootOrgId)
        val Success((tw,te)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
        Ents.setNewEntitlements(dummyRootOrgId, te.id, user, Some(tw.id))
        Ents.setNewEntitlements(dummyRootOrgId, tw.id, user, Some(dummyRootOrgId))
        Ents.setNewEntitlements(dummyRootOrgId, to.id, user, None)
        (to,tw,te)
      }

    }

    "create provider of new provider type" in new testApp {
      val request = fakeAuthRequest(POST, "/root/providers", testCreds).withBody(
        Json.obj(
          "name" -> testProviderName,
          "resource_type" -> providerTypeName,
          "properties" -> Json.obj(
            "config" -> Json.obj(
              "endpoints" -> Json.arr(
                Json.obj(
                  "default" -> true,
                  "http" -> Json.obj(
                    "url" -> "http://some-provider-endpoint"
                  )
                )
              )
            )
          )
        )
      )
      val Some(result) = route(request)
      println(contentAsString(result))
      status(result) must beEqualTo(CREATED)
      val id = (contentAsJson(result) \ "id").as[UUID]
      ResourceFactory.findById(id) must beSome(testProvider)
    }

    "failure from endpoint should prevent resource creation" in new testAppWithProvider {
      val testResourceName = "test-resource-failure"
      mockActionProvider.invokeAction(any) returns Future.failed(new BadRequestException("failure message from provider endpoint"))

      val request = fakeAuthRequest(POST, s"/${testOrg.name}/${resourcePrefix}s", testCreds).withBody(
        Json.obj(
          "name" -> testResourceName,
          "properties" -> Json.obj(
            "provider" -> testProvider.id,
            "string_prop" -> "some-string-value",
            "bool_prop" -> false
          )
        )
      )
      val Some(result) = route(request)
      (contentAsJson(result) \ "message").as[String] must contain("failure message from provider endpoint")
      status(result) must equalTo(BAD_REQUEST)

      ResourceFactory.findAllByName(dummyRootOrgId, resourceTypeId, testResourceName) must beEmpty
    }

    "create provider-backed resources using the ActionProvider interface under orgs" in new testAppWithProvider {
      val testResourceName = "test-resource"
      mockActionProvider.invokeAction(any) answers {
        (a: Any) =>
          val invocation = a.asInstanceOf[GenericActionInvocation]
          Future.successful(Left(invocation.resource.get.copy(
            properties = Some(invocation.resource.get.properties.get ++ Map(
              "bool_prop" -> "true"
            ))
          )))
      }

      val request = fakeAuthRequest(POST, s"/${testOrg.name}/${resourcePrefix}s", testCreds).withBody(
        Json.obj(
          "name" -> testResourceName,
          "properties" -> Json.obj(
            "provider" -> testProvider.id,
            "string_prop" -> "some string"
          )
        )
      )
      val Some(result) = route(request)
      status(result) must equalTo(CREATED)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome
      (json \ "name").asOpt[String] must beSome(testResourceName)
      (json \ "resource_type").asOpt[String] must beSome(resourceTypeName)
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "string_prop").asOpt[String] must beSome("some string")
      (json \ "properties" \ "bool_prop").asOpt[Boolean] must beSome(true)

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(mockProviderManager).getProvider(testProvider, s"${resourcePrefix}.create")
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== s"${resourcePrefix}.create"
      invocation.context.org.id must_== dummyRootOrgId
      invocation.context.workspace must beNone
      invocation.context.environment must beNone
      invocation.provider must_== testProvider
      invocation.resource must beSome(
        hasName(testResourceName)
          and
          hasProperties(
            "provider" -> testProvider.id.toString,
            "string_prop" -> "some string"
          )
      )

      val returnedId = (json \ "id").as[UUID]
      val Some(persisted) = ResourceFactory.findById(resourceTypeId, returnedId)
      persisted.properties.get must havePairs(
         "provider" -> testProvider.id.toString,
         "string_prop" -> "some string",
         "bool_prop" -> "true"
      )
    }

    // TODO: finish updating these from blueprint-tests to the generic tests like above

//    "create workspace blueprints using the ActionProvider interface" in new ExistingProvider {
//      val testBlueprintName = "test-blueprint"
//      mockActionProvider.invokeAction(any) answers {
//        (a: Any) =>
//          val invocation = a.asInstanceOf[GenericActionInvocation]
//          Future.successful(Left(invocation.resource.get.copy(
//            properties = Some(invocation.resource.get.properties.get ++ Map(
//              "canonical_form" -> "meta canonical form"
//            ))
//          )))
//      }
//
//      val request = fakeAuthRequest(POST, s"/root/workspaces/${testWork.id}/${resourcePrefix}s", testCreds).withBody(
//        Json.obj(
//          "name" -> testBlueprintName,
//          "properties" -> Json.obj(
//            "provider" -> testProvider.id,
//            "blueprint_type" -> "docker-compose",
//            "native_form" -> "blueprint data goes here"
//          )
//        )
//      )
//      val Some(result) = route(request)
//      status(result) must equalTo(CREATED)
//      val json = contentAsJson(result)
//      (json \ "id").asOpt[UUID] must beSome
//      (json \ "name").asOpt[String] must beSome(testBlueprintName)
//      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Blueprint")
//      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
//      (json \ "properties" \ "blueprint_type").asOpt[String] must beSome("docker-compose")
//      (json \ "properties" \ "native_form").asOpt[String] must beSome("blueprint data goes here")
//      (json \ "properties" \ "canonical_form").asOpt[String] must beSome("meta canonical form")
//
//      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
//      there was atLeastOne(providerManager).getProvider(testProvider, "blueprint.create")
//      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
//      val invocation = invocationCaptor.getValue
//      invocation.action must_== "blueprint.create"
//      invocation.context.org.id must_== dummyRootOrgId
//      invocation.context.workspace must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testWork.id) )
//      invocation.context.environment must beNone
//      invocation.provider must_== testProvider
//      invocation.resource must beSome(
//        hasName(testBlueprintName)
//          and
//          hasProperties(
//            "provider" -> testProvider.id.toString,
//            "blueprint_type" -> "docker-compose",
//            "native_form" -> "blueprint data goes here"
//          )
//      )
//
//      val returnedId = (json \ "id").as[UUID]
//      val Some(persisted) = ResourceFactory.findById(ResourceIds.Blueprint, returnedId)
//      persisted.properties.get must havePairs(
//        "provider" -> testProvider.id.toString,
//        "blueprint_type" -> "docker-compose",
//        "native_form" -> "blueprint data goes here",
//        "canonical_form" -> "meta canonical form"
//      )
//    }

//    "create environment blueprints using the ActionProvider interface" in new ExistingProvider {
//      val testBlueprintName = "test-blueprint"
//      mockActionProvider.invokeAction(any) answers {
//        (a: Any) =>
//          val invocation = a.asInstanceOf[GenericActionInvocation]
//          Future.successful(Left(invocation.resource.get.copy(
//            properties = Some(invocation.resource.get.properties.get ++ Map(
//              "canonical_form" -> "meta canonical form"
//            ))
//          )))
//      }
//
//      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/${testPrefix}", testCreds).withBody(
//        Json.obj(
//          "name" -> testBlueprintName,
//          "properties" -> Json.obj(
//            "provider" -> testProvider.id,
//            "blueprint_type" -> "docker-compose",
//            "native_form" -> "blueprint data goes here"
//          )
//        )
//      )
//      val Some(result) = route(request)
//      status(result) must equalTo(CREATED)
//      val json = contentAsJson(result)
//      (json \ "id").asOpt[UUID] must beSome
//      (json \ "name").asOpt[String] must beSome(testBlueprintName)
//      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Blueprint")
//      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
//      (json \ "properties" \ "blueprint_type").asOpt[String] must beSome("docker-compose")
//      (json \ "properties" \ "native_form").asOpt[String] must beSome("blueprint data goes here")
//      (json \ "properties" \ "canonical_form").asOpt[String] must beSome("meta canonical form")
//
//      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
//      there was atLeastOne(providerManager).getProvider(testProvider, "blueprint.create")
//      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
//      val invocation = invocationCaptor.getValue
//      invocation.action must_== "blueprint.create"
//      invocation.context.org.id must_== dummyRootOrgId
//      invocation.context.workspace must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testWork.id) )
//      invocation.context.environment must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testEnv.id) )
//      invocation.provider must_== testProvider
//      invocation.resource must beSome(
//        hasName(testBlueprintName)
//          and
//          hasProperties(
//            "provider" -> testProvider.id.toString,
//            "blueprint_type" -> "docker-compose",
//            "native_form" -> "blueprint data goes here"
//          )
//      )
//
//      val returnedId = (json \ "id").as[UUID]
//      val Some(persisted) = ResourceFactory.findById(ResourceIds.Blueprint, returnedId)
//      persisted.properties.get must havePairs(
//        "provider" -> testProvider.id.toString,
//        "blueprint_type" -> "docker-compose",
//        "native_form" -> "blueprint data goes here",
//        "canonical_form" -> "meta canonical form"
//      )
//    }

//    "create org blueprints using the import verb" in new ExistingProvider {
//      val testBlueprintName = "test-blueprint-import"
//      val metaRep = "a representation of the import target"
//      mockActionProvider.invokeAction(any) answers {
//        (a: Any) =>
//          val invocation = a.asInstanceOf[GenericActionInvocation]
//          Future.successful(Left(invocation.resource.get.copy(
//            properties = Some(invocation.resource.get.properties.get ++ Map(
//              "canonical_form" -> metaRep
//            ))
//          )))
//      }
//
//      val importTarget = UUID.randomUUID()
//      val request = fakeAuthRequest(POST, s"/root/${testPrefix}?action=import&p1=v1&p1=v1again&p2=v2", testCreds).withBody(
//        Json.obj(
//          "name" -> testBlueprintName,
//          "properties" -> Json.obj(
//            "provider" -> testProvider.id,
//            "blueprint_type" -> "meta-import",
//            "native_form" -> importTarget.toString
//          )
//        )
//      )
//      val Some(result) = route(request)
//      status(result) must equalTo(CREATED)
//      val json = contentAsJson(result)
//      (json \ "id").asOpt[UUID] must beSome
//      (json \ "name").asOpt[String] must beSome(testBlueprintName)
//      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Blueprint")
//      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
//      (json \ "properties" \ "blueprint_type").asOpt[String] must beSome("meta-import")
//      (json \ "properties" \ "canonical_form").asOpt[String] must beSome(metaRep)
//      (json \ "properties" \ "native_form").asOpt[UUID] must beSome(importTarget)
//
//      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
//      there was atLeastOne(providerManager).getProvider(testProvider, "blueprint.import")
//      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
//      val invocation = invocationCaptor.getValue
//      invocation.action must_== "blueprint.import"
//      invocation.context.org.id must_== dummyRootOrgId
//      invocation.context.workspace must beNone
//      invocation.context.environment must beNone
//      invocation.context.queryParams must_==(Map(
//        "p1" -> Seq("v1", "v1again"),
//        "p2" -> Seq("v2")
//      ))
//      invocation.provider must_== testProvider
//      invocation.resource must beSome(
//        hasName(testBlueprintName)
//          and
//          hasProperties(
//            "provider" -> testProvider.id.toString,
//            "blueprint_type" -> "meta-import",
//            "native_form" -> importTarget.toString
//          )
//      )
//
//      val returnedId = (json \ "id").as[UUID]
//      val Some(persisted) = ResourceFactory.findById(ResourceIds.Blueprint, returnedId)
//      persisted.properties.get must havePairs(
//        "provider" -> testProvider.id.toString,
//        "blueprint_type" -> "meta-import",
//        "native_form" -> importTarget.toString,
//        "canonical_form" -> metaRep
//      )
//    }

//    "deploy blueprints using the ActionProvider interface with Json payload without update semantics" in new ExistingProvider {
//      val testBlueprintName = "test-blueprint-deploy"
//      val createdResource = createInstance(ResourceIds.Blueprint, testBlueprintName,
//        parent = Some(testEnv.id),
//        properties = Some(Map(
//          "provider" -> testProvider.id.toString,
//          "blueprint_type" -> "docker-compose",
//          "native_form" -> "native",
//          "canonical_form" -> "canonical"
//        ))
//      ).get
//      mockActionProvider.invokeAction(any) answers {
//        (a: Any) =>
//          val invocation = a.asInstanceOf[GenericActionInvocation]
//          val name = invocation.actionPayload.flatMap(j => (j \ "name").asOpt[String])
//          Future.successful(Right(
//            RawInvocationResponse(Some(202),Some("text/plain"), Some(s"Hello, ${name.getOrElse("world")}"))
//          ))
//      }
//
//      val payload = Json.obj(
//        "name" -> "Chris"
//      )
//      val request = fakeAuthRequest(POST,
//        s"/root/${testPrefix}/${createdResource.id}?action=deploy&p1=v1", testCreds
//      ).withBody(payload)
//      val Some(result) = route(request)
//      status(result) must equalTo(202)
//      contentAsString(result) must_== "Hello, Chris"
//      contentType(result) must beSome("text/plain")
//
//      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
//      there was atLeastOne(providerManager).getProvider(testProvider, "blueprint.deploy")
//      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
//      val invocation = invocationCaptor.getValue
//      invocation.action must_== "blueprint.deploy"
//      invocation.context.org.id must_== dummyRootOrgId
//      invocation.context.workspace must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testWork.id) )
//      invocation.context.environment must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testEnv.id) )
//      invocation.context.queryParams must_==(Map(
//        "p1" -> Seq("v1")
//      ))
//      invocation.provider must_== testProvider
//      invocation.resource must beSome(createdResource)
//      invocation.actionPayload must beSome(payload)
//    }

//    "deploy blueprints using the ActionProvider interface with text payload with update semantics" in new ExistingProvider {
//      val testBlueprintName = "test-blueprint-deploy"
//      val createdResource = createInstance(ResourceIds.Blueprint, testBlueprintName,
//        parent = Some(testEnv.id),
//        properties = Some(Map(
//          "provider" -> testProvider.id.toString,
//          "blueprint_type" -> "docker-compose",
//          "native_form" -> "native",
//          "canonical_form" -> "canonical"
//        ))
//      ).get
//      mockActionProvider.invokeAction(any) answers {
//        (a: Any) =>
//          val invocation = a.asInstanceOf[GenericActionInvocation]
//          val updatedCF = invocation.actionPayload.flatMap(_.asOpt[String]) getOrElse ""
//          val resource = invocation.resource.get
//          Future.successful(Left(
//            resource.copy(
//              properties = Some(resource.properties.get ++ Map(
//                "canonical_form" -> updatedCF
//              ))
//            )
//          ))
//      }
//
//      val payload = "updated canonical form during blueprint"
//
//      val request = fakeAuthRequest(POST,
//        s"/root/${testPrefix}/${createdResource.id}?action=deploy&p1=v1", testCreds
//      ).withBody(payload)
//      val Some(result) = route(request)
//      status(result) must equalTo(200)
//      contentAsJson(result).toString must /("properties") /("canonical_form" -> payload)
//
//      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
//      there was atLeastOne(providerManager).getProvider(testProvider, "blueprint.deploy")
//      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
//      val invocation = invocationCaptor.getValue
//      invocation.action must_== "blueprint.deploy"
//      invocation.context.org.id must_== dummyRootOrgId
//      invocation.context.workspace must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testWork.id) )
//      invocation.context.environment must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testEnv.id) )
//      invocation.context.queryParams must_==(Map(
//        "p1" -> Seq("v1")
//      ))
//      invocation.provider must_== testProvider
//      invocation.resource must beSome(createdResource)
//      invocation.actionPayload must beSome(JsString(payload))
//
//      val Some(updatedResource) = ResourceFactory.findById(createdResource.id)
//      updatedResource.properties.get must havePairs(
//        "provider" -> testProvider.id.toString,
//        "blueprint_type" -> "docker-compose",
//        "native_form" -> "native",
//        "canonical_form" -> payload
//      )
//    }

//    "action failure should return bad request" in new ExistingProvider {
//      val testBlueprintName = "test-blueprint-deploy-failure"
//      val createdResource = createInstance(ResourceIds.Blueprint, testBlueprintName,
//        parent = Some(testEnv.id),
//        properties = Some(Map(
//          "provider" -> testProvider.id.toString,
//          "blueprint_type" -> "docker-compose",
//          "native_form" -> "native",
//          "canonical_form" -> "canonical"
//        ))
//      ).get
//      mockActionProvider.invokeAction(any) returns Future.failed(new BadRequestException("failure message"))
//
//      val request = fakeAuthRequest(POST,
//        s"/root/${testPrefix}/${createdResource.id}?action=deploy", testCreds
//      )
//      val Some(result) = route(request)
//      status(result) must equalTo(BAD_REQUEST)
//      contentAsString(result) must contain("failure message")
//    }

//    "delete blueprints using the ActionProvider interface" in new ExistingProvider {
//      val testBlueprintName = "test-blueprint-delete"
//      val createdResource = createInstance(ResourceIds.Blueprint, testBlueprintName,
//        parent = Some(testEnv.id),
//        properties = Some(Map(
//          "provider" -> testProvider.id.toString,
//          "blueprint_type" -> "docker-compose",
//          "native_form" -> "native",
//          "canonical_form" -> "canonical"
//        ))
//      ).get
//      mockActionProvider.invokeAction(any) returns Future.successful(Right(RawInvocationResponse(None,None,None)))
//
//      val request = fakeAuthRequest(DELETE,
//        s"/root/environments/${testEnv.id}/${testPrefix}/${createdResource.id}?p1=v1", testCreds
//      )
//      val Some(result) = route(request)
//      status(result) must equalTo(204)
//
//      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
//      there was atLeastOne(providerManager).getProvider(testProvider, "blueprint.delete")
//      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
//      val invocation = invocationCaptor.getValue
//      invocation.action must_== "blueprint.delete"
//      invocation.context.org.id must_== dummyRootOrgId
//      invocation.context.workspace must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testWork.id) )
//      invocation.context.environment must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testEnv.id) )
//      invocation.context.queryParams must_==(Map(
//        "p1" -> Seq("v1")
//      ))
//      invocation.provider must_== testProvider
//      invocation.resource must beSome(createdResource)
//      invocation.actionPayload must beNone
//
//      ResourceFactory.findById(createdResource.id) must beNone
//    }

//    "delete blueprints using the alternate verb against ActionProvider interface" in new ExistingProvider {
//      val testBlueprintName = "test-blueprint-delete"
//      val createdResource = createInstance(ResourceIds.Blueprint, testBlueprintName,
//        parent = Some(testEnv.id),
//        properties = Some(Map(
//          "provider" -> testProvider.id.toString,
//          "blueprint_type" -> "docker-compose",
//          "native_form" -> "native",
//          "canonical_form" -> "canonical"
//        ))
//      ).get
//      mockActionProvider.invokeAction(any) returns Future.successful(Right(RawInvocationResponse(None,None,None)))
//
//      val request = fakeAuthRequest(DELETE,
//        s"/root/environments/${testEnv.id}/${testPrefix}/${createdResource.id}?action=import", testCreds
//      )
//      val Some(result) = route(request)
//      status(result) must equalTo(204)
//
//      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
//      there was atLeastOne(providerManager).getProvider(testProvider, "blueprint.import")
//      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
//      val invocation = invocationCaptor.getValue
//      invocation.action must_== "blueprint.import"
//      invocation.context.org.id must_== dummyRootOrgId
//      invocation.context.workspace must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testWork.id) )
//      invocation.context.environment must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testEnv.id) )
//      invocation.provider must_== testProvider
//      invocation.resource must beSome(createdResource)
//      invocation.actionPayload must beNone
//
//      ResourceFactory.findById(createdResource.id) must beNone
//    }

//    "failure during invocation should prevent blueprint deletion" in new ExistingProvider {
//      val testBlueprintName = "test-blueprint-delete-failure"
//      val createdResource = createInstance(ResourceIds.Blueprint, testBlueprintName,
//        parent = Some(testEnv.id),
//        properties = Some(Map(
//          "provider" -> testProvider.id.toString,
//          "blueprint_type" -> "docker-compose",
//          "native_form" -> "native",
//          "canonical_form" -> "canonical"
//        ))
//      ).get
//      mockActionProvider.invokeAction(any) returns Future.failed(new BadRequestException("the failure message"))
//
//      val request = fakeAuthRequest(DELETE,
//        s"/root/environments/${testEnv.id}/${testPrefix}/${createdResource.id}?action=import", testCreds
//      )
//      val Some(result) = route(request)
//      status(result) must equalTo(BAD_REQUEST)
//      contentAsString(result) must contain("the failure message")
//
//      ResourceFactory.findById(createdResource.id) must beSome
//    }

//    "patch blueprints using the ActionProvider interface" in new ExistingProvider {
//      val testBlueprintName = "test-blueprint-patch"
//      val createdResource = createInstance(ResourceIds.Blueprint, testBlueprintName,
//        parent = Some(testEnv.id),
//        properties = Some(Map(
//          "provider" -> testProvider.id.toString,
//          "blueprint_type" -> "docker-compose",
//          "native_form" -> "original blueprint",
//          "canonical_form" -> "ORIGINAL BLUEPRINT"
//        ))
//      ).get
//
//      mockActionProvider.invokeAction(any) answers {
//        (a: Any) =>
//          val invocation = a.asInstanceOf[GenericActionInvocation]
//          val resource = invocation.resource.get
//          Future.successful(Left(resource.copy(
//            properties = Some(resource.properties.get ++ Map(
//              "canonical_form" -> resource.properties.get("native_form").toUpperCase()
//            ))
//          )))
//      }
//
//      val request = fakeAuthRequest(PATCH,
//        s"/root/environments/${testEnv.id}/${testPrefix}/${createdResource.id}", testCreds
//      ).withBody(
//        PatchDocument(
//          PatchOp.Replace("/properties/native_form", "updated blueprint")
//        ).toJson
//      )
//      val Some(result) = route(request)
//      status(result) must equalTo(OK)
//      val json = contentAsJson(result)
//      (json \ "id").asOpt[UUID] must beSome(createdResource.id)
//      (json \ "name").asOpt[String] must beSome(testBlueprintName)
//      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Blueprint")
//      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
//      (json \ "properties" \ "blueprint_type").asOpt[String] must beSome("docker-compose")
//      (json \ "properties" \ "native_form").asOpt[String] must beSome("updated blueprint")
//      (json \ "properties" \ "canonical_form").asOpt[String] must beSome("UPDATED BLUEPRINT")
//
//      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
//      there was atLeastOne(providerManager).getProvider(testProvider, "blueprint.update")
//      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
//      val invocation = invocationCaptor.getValue
//      invocation.action must_== "blueprint.update"
//      invocation.context.org.id must_== dummyRootOrgId
//      invocation.context.workspace must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testWork.id) )
//      invocation.context.environment must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testEnv.id) )
//      invocation.provider must_== testProvider
//      invocation.resource must beSome(
//        hasId(createdResource.id)
//          and
//          hasName(createdResource.name)
//          and
//          hasProperties(
//            "native_form"    -> "updated blueprint",  // updated in the patch op
//            "canonical_form" -> "ORIGINAL BLUEPRINT"  // not updated in the patch op, updated by the invocation
//          )
//      )
//      invocation.actionPayload must beNone
//
//      ResourceFactory.findById(createdResource.id) must beSome
//      val Some(updatedResource) = ResourceFactory.findById(createdResource.id)
//      updatedResource.properties.get must havePairs(
//        "provider" -> testProvider.id.toString,
//        "blueprint_type" -> "docker-compose",
//        "native_form" -> "updated blueprint",
//        "canonical_form" -> "UPDATED BLUEPRINT"
//      )
//    }

//    "patch blueprints with alternate verb using the ActionProvider interface" in new ExistingProvider {
//      val testBlueprintName = "test-blueprint-patch"
//      val createdResource = createInstance(ResourceIds.Blueprint, testBlueprintName,
//        parent = Some(testEnv.id),
//        properties = Some(Map(
//          "provider" -> testProvider.id.toString,
//          "blueprint_type" -> "docker-compose",
//          "native_form" -> "original blueprint",
//          "canonical_form" -> "ORIGINAL BLUEPRINT"
//        ))
//      ).get
//
//      mockActionProvider.invokeAction(any) answers {
//        (a: Any) =>
//          val invocation = a.asInstanceOf[GenericActionInvocation]
//          val resource = invocation.resource.get
//          Future.successful(Left(resource.copy(
//            properties = Some(resource.properties.get ++ Map(
//              "canonical_form" -> resource.properties.get("native_form").toUpperCase()
//            ))
//          )))
//      }
//
//      val request = fakeAuthRequest(PATCH,
//        s"/root/environments/${testEnv.id}/${testPrefix}/${createdResource.id}?action=import", testCreds
//      ).withBody(
//        PatchDocument(
//          PatchOp.Replace("/properties/native_form", "updated blueprint")
//        ).toJson
//      )
//      val Some(result) = route(request)
//      status(result) must equalTo(OK)
//      val json = contentAsJson(result)
//      (json \ "id").asOpt[UUID] must beSome(createdResource.id)
//      (json \ "name").asOpt[String] must beSome(testBlueprintName)
//      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Blueprint")
//      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
//      (json \ "properties" \ "blueprint_type").asOpt[String] must beSome("docker-compose")
//      (json \ "properties" \ "native_form").asOpt[String] must beSome("updated blueprint")
//      (json \ "properties" \ "canonical_form").asOpt[String] must beSome("UPDATED BLUEPRINT")
//
//      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
//      there was atLeastOne(providerManager).getProvider(testProvider, "blueprint.import")
//      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
//      val invocation = invocationCaptor.getValue
//      invocation.action must_== "blueprint.import"
//      invocation.context.org.id must_== dummyRootOrgId
//      invocation.context.workspace must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testWork.id) )
//      invocation.context.environment must beSome( ((_:GestaltResourceInstance).id) ^^ be_==(testEnv.id) )
//      invocation.provider must_== testProvider
//      invocation.resource must beSome(
//        hasId(createdResource.id)
//          and
//          hasName(createdResource.name)
//          and
//          hasProperties(
//            "native_form"    -> "updated blueprint",  // updated in the patch op
//            "canonical_form" -> "ORIGINAL BLUEPRINT"  // not updated in the patch op, updated by the invocation
//          )
//      )
//      invocation.actionPayload must beNone
//
//      ResourceFactory.findById(createdResource.id) must beSome
//      val Some(updatedResource) = ResourceFactory.findById(createdResource.id)
//      updatedResource.properties.get must havePairs(
//        "provider" -> testProvider.id.toString,
//        "blueprint_type" -> "docker-compose",
//        "native_form" -> "updated blueprint",
//        "canonical_form" -> "UPDATED BLUEPRINT"
//      )
//    }

//    "failure during patch should not update resource" in new ExistingProvider {
//      val testBlueprintName = "test-blueprint-patch-failure"
//      val createdResource = createInstance(ResourceIds.Blueprint, testBlueprintName,
//        parent = Some(testEnv.id),
//        properties = Some(Map(
//          "provider" -> testProvider.id.toString,
//          "blueprint_type" -> "docker-compose",
//          "native_form" -> "original blueprint",
//          "canonical_form" -> "ORIGINAL BLUEPRINT"
//        ))
//      ).get
//      mockActionProvider.invokeAction(any) returns Future.failed(new BadRequestException("failure"))
//
//      val request = fakeAuthRequest(PATCH,
//        s"/root/environments/${testEnv.id}/${testPrefix}/${createdResource.id}", testCreds
//      ).withBody(
//        PatchDocument(
//          PatchOp.Replace("/properties/native_form", "updated blueprint")
//        ).toJson
//      )
//      val Some(result) = route(request)
//      status(result) must equalTo(BAD_REQUEST)
//
//      ResourceFactory.findById(createdResource.id) must beSome
//      val Some(updatedResource) = ResourceFactory.findById(createdResource.id)
//      updatedResource.properties.get must havePairs(
//        "provider" -> testProvider.id.toString,
//        "blueprint_type" -> "docker-compose",
//        "native_form" -> "original blueprint",
//        "canonical_form" -> "ORIGINAL BLUEPRINT"
//      )
//    }

  }

}