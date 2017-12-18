package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data.bootstrap.LineageInfo
import com.galacticfog.gestalt.data.{ResourceFactory, TypeFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.genericactions.GenericProvider.RawInvocationResponse
import com.galacticfog.gestalt.meta.genericactions.{GenericActionInvocation, GenericProvider, GenericProviderManager}
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import controllers.SecurityResources
import org.mockito.ArgumentCaptor
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.matcher.{JsonMatchers, Matcher}
import play.api.inject.bind
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.{JsObject, JsString, Json}
import play.api.test.{PlaySpecification, WithApplication}
import services.{DockerClientFactory, MarathonClientFactory, SkuberFactory}

import scala.concurrent.Future
import scala.util.Success
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
        val Success(to) = createOrg(name = uuid().toString)
        val Success((tw,te)) = createWorkEnv(org = to.id)
        Ents.setNewEntitlements(dummyRootOrgId, te.id, user, Some(tw.id))
        Ents.setNewEntitlements(dummyRootOrgId, tw.id, user, Some(dummyRootOrgId))
        Ents.setNewEntitlements(dummyRootOrgId, to.id, user, None)
        (to,tw,te)
      }

    }

    "create provider of new provider type" in new testApp {
      val request = fakeAuthRequest(POST, s"/root/providers", testCreds).withBody(
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

    "create org provider-backed resources using the ActionProvider interface" in new testAppWithProvider {
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
      invocation.context.org.id must_== testOrg.id
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

    "create workspace provider-backed resources using the ActionProvider interface" in new testAppWithProvider {
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

      val request = fakeAuthRequest(POST, s"/${testOrg.name}/workspaces/${testWork.id}/${resourcePrefix}s", testCreds).withBody(
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
      invocation.context.org.id must_== testOrg.id
      invocation.context.workspace must beSome(testWork)
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

    "create environment provider-backed resources using the ActionProvider interface" in new testAppWithProvider {
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

      val request = fakeAuthRequest(POST, s"/${testOrg.name}/environments/${testEnv.id}/${resourcePrefix}s", testCreds).withBody(
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
      invocation.context.org.id must_== testOrg.id
      invocation.context.workspace must beSome(testWork)
      invocation.context.environment must beSome(testEnv)
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

    "create provider-backed resources using an alternative verb" in new testAppWithProvider {
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

      val request = fakeAuthRequest(POST, s"/${testOrg.name}/environments/${testEnv.id}/${resourcePrefix}s?action=${verb1}&qp1=v1&qp2=v2.1&qp2=v2.2", testCreds).withBody(
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
      there was atLeastOne(mockProviderManager).getProvider(testProvider, s"${resourcePrefix}.${verb1}")
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== s"${resourcePrefix}.${verb1}"
      invocation.context.org.id must_== testOrg.id
      invocation.context.workspace must beSome(testWork)
      invocation.context.environment must beSome(testEnv)
      invocation.context.queryParams must_== Map(
        "qp1" -> Seq("v1"),
        "qp2" -> Seq("v2.1", "v2.2")
      )
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

    "perform resource actions using the ActionProvider interface with Json payload without update semantics" in new testAppWithProvider {
      val testResourceName = "test-resource-action"
      val createdResource = createInstance(resourceTypeId, testResourceName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> testProvider.id.toString,
          "string_prop" -> "some string",
          "bool_prop" -> "false"
        ))
      ).get
      mockActionProvider.invokeAction(any) answers {
        (a: Any) =>
          val invocation = a.asInstanceOf[GenericActionInvocation]
          val name = invocation.actionPayload.flatMap(j => (j \ "name").asOpt[String])
          Future.successful(Right(
            RawInvocationResponse(Some(202),Some("text/plain"), Some(s"Hello, ${name.getOrElse("world")}"))
          ))
      }

      val payload = Json.obj(
        "name" -> "Chris"
      )
      val request = fakeAuthRequest(POST,
        s"/${testOrg.name}/${resourcePrefix}s/${createdResource.id}?action=${verb1}&p1=v1", testCreds
      ).withBody(payload)
      val Some(result) = route(request)
      status(result) must equalTo(202)
      contentAsString(result) must_== "Hello, Chris"
      contentType(result) must beSome("text/plain")

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(mockProviderManager).getProvider(testProvider, s"${resourcePrefix}.${verb1}")
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== s"${resourcePrefix}.${verb1}"
      invocation.context.org.id must_== testOrg.id
      invocation.context.workspace must beSome(testWork)
      invocation.context.environment must beSome(testEnv)
      invocation.context.queryParams must_==(Map(
        "p1" -> Seq("v1")
      ))
      invocation.provider must_== testProvider
      invocation.resource must beSome(createdResource)
      invocation.actionPayload must beSome(payload)
    }

    "perform resource actions using the ActionProvider interface with text payload with update semantics" in new testAppWithProvider {
      val testResourceName = "test-resource-action"
      val createdResource = createInstance(resourceTypeId, testResourceName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> testProvider.id.toString,
          "string_prop" -> "original string prop",
          "bool_prop" -> "false"
        ))
      ).get
      mockActionProvider.invokeAction(any) answers {
        (a: Any) =>
          val invocation = a.asInstanceOf[GenericActionInvocation]
          val update = invocation.actionPayload.flatMap(_.asOpt[String]) getOrElse ""
          val resource = invocation.resource.get
          Future.successful(Left(
            resource.copy(
              properties = Some(resource.properties.get ++ Map(
                "string_prop" -> update,
                "bool_prop" -> "true"
              ))
            )
          ))
      }

      val payload = "updated string prop"

      val request = fakeAuthRequest(POST,
        s"/${testOrg.name}/${resourcePrefix}s/${createdResource.id}?action=${verb2}&p1=v1", testCreds
      ).withBody(payload)
      val Some(result) = route(request)
      status(result) must equalTo(200)
      contentAsJson(result).toString must /("properties") /("string_prop" -> payload)
      contentAsJson(result).toString must /("properties") /("bool_prop" -> "true")

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(mockProviderManager).getProvider(testProvider, s"${resourcePrefix}.${verb2}")
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== s"${resourcePrefix}.${verb2}"
      invocation.context.org.id must_== testOrg.id
      invocation.context.workspace must beSome(testWork)
      invocation.context.environment must beSome(testEnv)
      invocation.context.queryParams must_==(Map(
        "p1" -> Seq("v1")
      ))
      invocation.provider must_== testProvider
      invocation.resource must beSome(createdResource)
      invocation.actionPayload must beSome(JsString(payload))

      val Some(updatedResource) = ResourceFactory.findById(createdResource.id)
      updatedResource.properties.get must havePairs(
        "provider" -> testProvider.id.toString,
        "string_prop" -> payload,
        "bool_prop" -> "true"
      )
    }

    "action failure should return bad request" in new testAppWithProvider {
      val testResourceName = "test-resource-action"
      val createdResource = createInstance(resourceTypeId, testResourceName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> testProvider.id.toString,
          "string_prop" -> "original string prop",
          "bool_prop" -> "false"
        ))
      ).get
      mockActionProvider.invokeAction(any) returns Future.failed(new BadRequestException("failure message"))

      val request = fakeAuthRequest(POST,
        s"/${testOrg.name}/${resourcePrefix}s/${createdResource.id}?action=${verb1}", testCreds
      )
      val Some(result) = route(request)
      status(result) must equalTo(BAD_REQUEST)
      contentAsString(result) must contain("failure message")
    }

    "delete resources using the ActionProvider interface" in new testAppWithProvider {
      val testResourceName = "test-resource-delete"
      val createdResource = createInstance(resourceTypeId, testResourceName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> testProvider.id.toString,
          "string_prop" -> ""
        ))
      ).get
      mockActionProvider.invokeAction(any) returns Future.successful(Right(RawInvocationResponse(None,None,None)))

      val request = fakeAuthRequest(DELETE,
        s"/${testOrg.name}/environments/${testEnv.id}/${resourcePrefix}s/${createdResource.id}?p1=v1", testCreds
      )
      val Some(result) = route(request)
      status(result) must equalTo(204)

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(mockProviderManager).getProvider(testProvider, s"${resourcePrefix}.delete")
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== s"${resourcePrefix}.delete"
      invocation.context.org.id must_== testOrg.id
      invocation.context.workspace must beSome(testWork)
      invocation.context.environment must beSome(testEnv)
      invocation.context.queryParams must_==(Map(
        "p1" -> Seq("v1")
      ))
      invocation.provider must_== testProvider
      invocation.resource must beSome(createdResource)
      invocation.actionPayload must beNone

      ResourceFactory.findById(createdResource.id) must beNone
    }


    "delete resources using the alternate verb against the ActionProvider interface" in new testAppWithProvider {
      val testResourceName = "test-resource-delete"
      val createdResource = createInstance(resourceTypeId, testResourceName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> testProvider.id.toString,
          "string_prop" -> ""
        ))
      ).get
      mockActionProvider.invokeAction(any) returns Future.successful(Right(RawInvocationResponse(None,None,None)))

      val request = fakeAuthRequest(DELETE,
        s"/${testOrg.name}/environments/${testEnv.id}/${resourcePrefix}s/${createdResource.id}?action=${verb2}&p1=v1", testCreds
      )
      val Some(result) = route(request)
      status(result) must equalTo(204)

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(mockProviderManager).getProvider(testProvider, s"${resourcePrefix}.${verb2}")
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== s"${resourcePrefix}.${verb2}"
      invocation.context.org.id must_== testOrg.id
      invocation.context.workspace must beSome(testWork)
      invocation.context.environment must beSome(testEnv)
      invocation.context.queryParams must_==(Map(
        "p1" -> Seq("v1")
      ))
      invocation.provider must_== testProvider
      invocation.resource must beSome(createdResource)
      invocation.actionPayload must beNone

      ResourceFactory.findById(createdResource.id) must beNone
    }

    "failure during action invocation should prevent resource deletion" in new testAppWithProvider {
      val testResourceName = "test-resource-delete"
      val createdResource = createInstance(resourceTypeId, testResourceName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> testProvider.id.toString,
          "string_prop" -> ""
        ))
      ).get
      mockActionProvider.invokeAction(any) returns Future.failed(new BadRequestException("the failure message"))

      val request = fakeAuthRequest(DELETE,
        s"/${testOrg.name}/environments/${testEnv.id}/${resourcePrefix}s/${createdResource.id}?action=${verb2}&p1=v1", testCreds
      )
      val Some(result) = route(request)
      status(result) must equalTo(BAD_REQUEST)
      contentAsString(result) must contain("the failure message")

      ResourceFactory.findById(createdResource.id) must beSome
    }

    "patch resources using the ActionProvider interface" in new testAppWithProvider {
      val testResourceName = "test-resource-patch"
      val createdResource = createInstance(resourceTypeId, testResourceName,
        parent = Some(testEnv.id),
        org = testOrg.id,
        properties = Some(Map(
          "provider" -> testProvider.id.toString,
          "string_prop" -> "original value",
          "bool_prop" -> "false"
        ))
      ).get

      mockActionProvider.invokeAction(any) answers {
        (a: Any) =>
          val invocation = a.asInstanceOf[GenericActionInvocation]
          val resource = invocation.resource.get
          Future.successful(Left(resource.copy(
            properties = Some(resource.properties.get ++ Map(
              "bool_prop" -> "true"
            ))
          )))
      }

      val request = fakeAuthRequest(PATCH,
        s"/${testOrg.name}/environments/${testEnv.id}/${resourcePrefix}s/${createdResource.id}", testCreds
      ).withBody(
        PatchDocument(
          PatchOp.Replace("/properties/string_prop", "updated value")
        ).toJson
      )
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome(createdResource.id)
      (json \ "name").asOpt[String] must beSome(testResourceName)
      (json \ "resource_type").asOpt[String] must beSome(resourceTypeName)
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "string_prop").asOpt[String] must beSome("updated value")
      (json \ "properties" \ "bool_prop").asOpt[Boolean] must beSome(true)

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(mockProviderManager).getProvider(testProvider, s"${resourcePrefix}.update")
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== s"${resourcePrefix}.update"
      invocation.context.org.id must_== testOrg.id
      invocation.context.workspace must beSome(testWork)
      invocation.context.environment must beSome(testEnv)
      invocation.provider must_== testProvider
      invocation.resource must beSome(
        hasId(createdResource.id)
          and
          hasName(createdResource.name)
          and
          hasProperties(
            "string_prop" -> "updated value",  // updated in the patch op
            "bool_prop"   -> "false"           // not updated in the patch op, updated by the invocation
          )
      )
      invocation.actionPayload must beNone

      ResourceFactory.findById(createdResource.id) must beSome
      val Some(updatedResource) = ResourceFactory.findById(createdResource.id)
      updatedResource.properties.get must havePairs(
        "provider" -> testProvider.id.toString,
        "string_prop" -> "updated value",
        "bool_prop" -> "true"
      )
    }

    "patch resources using an alternate verb against the ActionProvider interface" in new testAppWithProvider {
      val testResourceName = "test-resource-patch"
      val createdResource = createInstance(resourceTypeId, testResourceName,
        parent = Some(testEnv.id),
        org = testOrg.id,
        properties = Some(Map(
          "provider" -> testProvider.id.toString,
          "string_prop" -> "original value",
          "bool_prop" -> "false"
        ))
      ).get

      mockActionProvider.invokeAction(any) answers {
        (a: Any) =>
          val invocation = a.asInstanceOf[GenericActionInvocation]
          val resource = invocation.resource.get
          Future.successful(Left(resource.copy(
            properties = Some(resource.properties.get ++ Map(
              "bool_prop" -> "true"
            ))
          )))
      }

      val request = fakeAuthRequest(PATCH,
        s"/${testOrg.name}/environments/${testEnv.id}/${resourcePrefix}s/${createdResource.id}?action=${verb2}", testCreds
      ).withBody(
        PatchDocument(
          PatchOp.Replace("/properties/string_prop", "updated value")
        ).toJson
      )
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome(createdResource.id)
      (json \ "name").asOpt[String] must beSome(testResourceName)
      (json \ "resource_type").asOpt[String] must beSome(resourceTypeName)
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "string_prop").asOpt[String] must beSome("updated value")
      (json \ "properties" \ "bool_prop").asOpt[Boolean] must beSome(true)

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(mockProviderManager).getProvider(testProvider, s"${resourcePrefix}.${verb2}")
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== s"${resourcePrefix}.${verb2}"
      invocation.context.org.id must_== testOrg.id
      invocation.context.workspace must beSome(testWork)
      invocation.context.environment must beSome(testEnv)
      invocation.provider must_== testProvider
      invocation.resource must beSome(
        hasId(createdResource.id)
          and
          hasName(createdResource.name)
          and
          hasProperties(
            "string_prop" -> "updated value",  // updated in the patch op
            "bool_prop"   -> "false"           // not updated in the patch op, updated by the invocation
          )
      )
      invocation.actionPayload must beNone

      ResourceFactory.findById(createdResource.id) must beSome
      val Some(updatedResource) = ResourceFactory.findById(createdResource.id)
      updatedResource.properties.get must havePairs(
        "provider" -> testProvider.id.toString,
        "string_prop" -> "updated value",
        "bool_prop" -> "true"
      )
    }

    "failure during patch should not update resource" in new testAppWithProvider {
      val testResourceName = "test-resource-patch-failure"
      val createdResource = createInstance(resourceTypeId, testResourceName,
        parent = Some(testEnv.id),
        org = testOrg.id,
        properties = Some(Map(
          "provider" -> testProvider.id.toString,
          "string_prop" -> "original value"
        ))
      ).get
      mockActionProvider.invokeAction(any) returns Future.failed(new BadRequestException("failure"))

      val request = fakeAuthRequest(PATCH,
        s"/${testOrg.name}/environments/${testEnv.id}/${resourcePrefix}s/${createdResource.id}", testCreds
      ).withBody(
        PatchDocument(
          PatchOp.Replace("/properties/string_prop", "updated value")
        ).toJson
      )
      val Some(result) = route(request)
      status(result) must equalTo(BAD_REQUEST)

      ResourceFactory.findById(createdResource.id) must beSome
      val Some(updatedResource) = ResourceFactory.findById(createdResource.id)
      updatedResource.properties.get must havePairs(
        "provider" -> testProvider.id.toString,
        "string_prop" -> "original value"
      )
    }

  }

}