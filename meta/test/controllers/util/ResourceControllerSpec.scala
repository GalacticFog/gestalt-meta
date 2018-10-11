package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data.bootstrap.LineageInfo
import com.galacticfog.gestalt.data.{ResourceFactory, TypeFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.{ContainerSpec, output}
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
import play.api.libs.json._
import play.api.test.{PlaySpecification, WithApplication}
import services.{DockerClientFactory, MarathonClientFactory, SkuberFactory}

import scala.concurrent.Future
import scala.util.Success
import org.specs2.runner._
import org.junit.runner.RunWith
import org.specs2.specification.Tables

import com.galacticfog.gestalt.meta.api.sdk.GestaltConfigurationManager
import com.galacticfog.gestalt.data.PostgresConfigManager

@RunWith(classOf[JUnitRunner])
class ResourceControllerSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers with Tables {

  def hasName(name: => String): Matcher[GestaltResourceInstance] =
    ((_: GestaltResourceInstance).name) ^^ be_==(name)

  def hasId(id: => UUID): Matcher[GestaltResourceInstance] =
    ((_: GestaltResourceInstance).id) ^^ be_==(id)

  def hasProperties(props: (String,String)*): Matcher[GestaltResourceInstance] =
    ((_: GestaltResourceInstance).properties.get) ^^ havePairs(props:_*)

  def beUndefined = ((_:JsLookupResult).isInstanceOf[JsUndefined]) ^^ beTrue

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources
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
    Ents.setNewResourceEntitlements(dummyRootOrgId, dummyRootOrgId, user, None)

  }

  abstract class testAppMocks extends WithApplication(application(additionalBindings = Seq(
    bind[UpgraderService].toInstance(mock[UpgraderService]),
    bind[ContainerService].toInstance(mock[ContainerService]),
    bind[DockerClientFactory].toInstance(mock[DockerClientFactory]),
    bind[MarathonClientFactory].toInstance(mock[MarathonClientFactory]),
    bind[SkuberFactory].toInstance(mock[SkuberFactory]),
    bind[GenericProviderManager].toInstance(mock[GenericProviderManager]),
    bind[Security].toInstance(mock[Security]),
    bind(classOf[GestaltConfigurationManager]).toInstance(PostgresConfigManager)
  )))

  trait testApp extends testAppMocks {
    app.injector.instanceOf[Security].getOrgSyncTree2()(any) returns Success(dummyOrgSync)
  }

  sequential

  "ResourceController generic support" should {

    val providerTypeName = "Gestalt::Configuration::Provider::TestProvider"
    val providerTypeId = UUID.randomUUID()
    val resourceTypeName = "Gestalt::Configuration::Resource::TestResource"
    val resourceTypeId = UUID.randomUUID()
    val verb1 = "resourceVerb1"
    val verb2 = "resourceVerb2"
    val resourcePrefix = "test-resource-type"

    /*
     * Note: TypeController is the controller that actually handles creation of the new provider types and provider-backed resource type
     * But they are created here for use in the later tests in this spec
     * You can think of this as a test if you like, or just a precondition. But it needs to be complete or much of the later stuff will fail.
     */

    "[precondition] support creating new provider types" in new testApp {
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
      val Some(result) = route(app,request)
      status(result) must_== CREATED
      (contentAsJson(result) \ "id").as[UUID] must_== providerTypeId
      TypeFactory.findById(providerTypeId) must beSome
    }

    "[precondition] support creating new provider-backed resource types" in new testApp {
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
      val Some(result) = route(app,request)
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
      val Some(result) = route(app,request)
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
      val Some(result) = route(app,request)
      contentAsString(result) must contain("not found")
      status(result) must equalTo(BAD_REQUEST)
    }

    val testProviderName = "test-provider-name"
    lazy val testProvider = ResourceFactory.findChildByName(dummyRootOrgId, providerTypeId, testProviderName).get

    case class testAppWithEnv() extends testApp {

      lazy val (testOrg,testWork,testEnv) = {
        val Success(to) = createOrg(name = uuid().toString)
        val Success((tw,te)) = createWorkEnv(org = to.id)
        Ents.setNewResourceEntitlements(dummyRootOrgId, te.id, user, Some(tw.id))
        Ents.setNewResourceEntitlements(dummyRootOrgId, tw.id, user, Some(dummyRootOrgId))
        Ents.setNewResourceEntitlements(dummyRootOrgId, to.id, user, None)
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
          action = any,
          callerAuth = any
        ) returns Success(Some(map))
        map
      }

      lazy val (testOrg,testWork,testEnv) = {
        val Success(to) = createOrg(name = uuid().toString)
        val Success((tw,te)) = createWorkEnv(org = to.id)
        Ents.setNewResourceEntitlements(dummyRootOrgId, te.id, user, Some(tw.id))
        Ents.setNewResourceEntitlements(dummyRootOrgId, tw.id, user, Some(dummyRootOrgId))
        Ents.setNewResourceEntitlements(dummyRootOrgId, to.id, user, None)
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
      val Some(result) = route(app,request)
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
      val Some(result) = route(app,request)
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
      val Some(result) = route(app,request)
      status(result) must equalTo(CREATED)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome
      (json \ "name").asOpt[String] must beSome(testResourceName)
      (json \ "resource_type").asOpt[String] must beSome(resourceTypeName)
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "string_prop").asOpt[String] must beSome("some string")
      (json \ "properties" \ "bool_prop").asOpt[Boolean] must beSome(true)

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(mockProviderManager).getProvider(meq(testProvider), meq(s"${resourcePrefix}.create"), any)
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
    }//.pendingUntilFixed(", is broken until we can come to an agreement on create semantics")

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
      val Some(result) = route(app,request)
      status(result) must equalTo(CREATED)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome
      (json \ "name").asOpt[String] must beSome(testResourceName)
      (json \ "resource_type").asOpt[String] must beSome(resourceTypeName)
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "string_prop").asOpt[String] must beSome("some string")
      (json \ "properties" \ "bool_prop").asOpt[Boolean] must beSome(true)

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(mockProviderManager).getProvider(meq(testProvider), meq(s"${resourcePrefix}.create"), any)
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
    }//.pendingUntilFixed(", is broken until we can come to an agreement on create semantics")

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
      val Some(result) = route(app,request)
      status(result) must equalTo(CREATED)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome
      (json \ "name").asOpt[String] must beSome(testResourceName)
      (json \ "resource_type").asOpt[String] must beSome(resourceTypeName)
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "string_prop").asOpt[String] must beSome("some string")
      (json \ "properties" \ "bool_prop").asOpt[Boolean] must beSome(true)

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(mockProviderManager).getProvider(meq(testProvider), meq(s"${resourcePrefix}.create"), any)
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
    }//.pendingUntilFixed(", is broken until we can come to an agreement on create semantics")

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
      val Some(result) = route(app,request)
      status(result) must equalTo(CREATED)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome
      (json \ "name").asOpt[String] must beSome(testResourceName)
      (json \ "resource_type").asOpt[String] must beSome(resourceTypeName)
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "string_prop").asOpt[String] must beSome("some string")
      (json \ "properties" \ "bool_prop").asOpt[Boolean] must beSome(true)

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(mockProviderManager).getProvider(meq(testProvider), meq(s"${resourcePrefix}.${verb1}"), any)
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== s"${resourcePrefix}.${verb1}"
      invocation.context.org.id must_== testOrg.id
      invocation.context.workspace must beSome(testWork)
      invocation.context.environment must beSome(testEnv)
      invocation.queryParams must_== Map(
        "action" -> Seq(verb1),
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
    }//.pendingUntilFixed("we can come to an agreement on create semantics")

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
      val Some(result) = route(app,request)
      status(result) must equalTo(202)
      contentAsString(result) must_== "Hello, Chris"
      contentType(result) must beSome("text/plain")

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(mockProviderManager).getProvider(meq(testProvider), meq(s"${resourcePrefix}.${verb1}"), any)
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== s"${resourcePrefix}.${verb1}"
      invocation.context.org.id must_== testOrg.id
      invocation.context.workspace must beSome(testWork)
      invocation.context.environment must beSome(testEnv)
      invocation.queryParams must_==(Map(
        "action" -> Seq(verb1),
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
      val Some(result) = route(app,request)
      status(result) must equalTo(200)
      contentAsJson(result).toString must /("properties") /("string_prop" -> payload)
      contentAsJson(result).toString must /("properties") /("bool_prop" -> "true")

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(mockProviderManager).getProvider(meq(testProvider), meq(s"${resourcePrefix}.${verb2}"), any)
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== s"${resourcePrefix}.${verb2}"
      invocation.context.org.id must_== testOrg.id
      invocation.context.workspace must beSome(testWork)
      invocation.context.environment must beSome(testEnv)
      invocation.queryParams must_==(Map(
        "action" -> Seq(verb2),
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
      val Some(result) = route(app,request)
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
      val Some(result) = route(app,request)
      status(result) must equalTo(204)

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(mockProviderManager).getProvider(meq(testProvider), meq(s"${resourcePrefix}.delete"), any)
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== s"${resourcePrefix}.delete"
      invocation.context.org.id must_== testOrg.id
      invocation.context.workspace must beSome(testWork)
      invocation.context.environment must beSome(testEnv)
      invocation.queryParams must_==(Map(
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
      val Some(result) = route(app,request)
      status(result) must equalTo(204)

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(mockProviderManager).getProvider(meq(testProvider), meq(s"${resourcePrefix}.${verb2}"), any)
      there was one(mockActionProvider).invokeAction(invocationCaptor.capture())
      val invocation = invocationCaptor.getValue
      invocation.action must_== s"${resourcePrefix}.${verb2}"
      invocation.context.org.id must_== testOrg.id
      invocation.context.workspace must beSome(testWork)
      invocation.context.environment must beSome(testEnv)
      invocation.queryParams must_==(Map(
        "action" -> Seq(verb2),
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
      val Some(result) = route(app,request)
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
      val Some(result) = route(app,request)
      status(result) must equalTo(OK)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome(createdResource.id)
      (json \ "name").asOpt[String] must beSome(testResourceName)
      (json \ "resource_type").asOpt[String] must beSome(resourceTypeName)
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "string_prop").asOpt[String] must beSome("updated value")
      (json \ "properties" \ "bool_prop").asOpt[Boolean] must beSome(true)

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(mockProviderManager).getProvider(meq(testProvider), meq(s"${resourcePrefix}.update"), any)
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
      val Some(result) = route(app,request)
      status(result) must equalTo(OK)
      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome(createdResource.id)
      (json \ "name").asOpt[String] must beSome(testResourceName)
      (json \ "resource_type").asOpt[String] must beSome(resourceTypeName)
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "string_prop").asOpt[String] must beSome("updated value")
      (json \ "properties" \ "bool_prop").asOpt[Boolean] must beSome(true)

      val invocationCaptor = ArgumentCaptor.forClass(classOf[GenericActionInvocation])
      there was atLeastOne(mockProviderManager).getProvider(meq(testProvider), meq(s"${resourcePrefix}.${verb2}"), any)
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
      val Some(result) = route(app,request)
      status(result) must equalTo(BAD_REQUEST)

      ResourceFactory.findById(createdResource.id) must beSome
      val Some(updatedResource) = ResourceFactory.findById(createdResource.id)
      updatedResource.properties.get must havePairs(
        "provider" -> testProvider.id.toString,
        "string_prop" -> "original value"
      )
    }

  }

  "ResourceController rendering transforms" should {

    trait testVolume extends testAppMocks {
      val (testOrg,testWork,testEnv) = {
        val Success(to) = createOrg(name = uuid().toString)
        val Success((tw,te)) = createWorkEnv(org = to.id)
        Ents.setNewResourceEntitlements(dummyRootOrgId, te.id, user, Some(tw.id))
        Ents.setNewResourceEntitlements(dummyRootOrgId, tw.id, user, Some(dummyRootOrgId))
        Ents.setNewResourceEntitlements(dummyRootOrgId, to.id, user, None)
        (to,tw,te)
      }
      val Success(testProvider) = createKubernetesProvider(testEnv.id, "test-provider")
      val Success(volume) = createInstance(
        migrations.V13.VOLUME_TYPE_ID,
        name = "test-volume",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "type" -> "host_path",
          "size" -> "1000",
          "access_mode" -> "ReadWriteOnce",
          "provider" -> Json.obj(
            "id" -> testProvider.id
          ).toString,
          "config" -> Json.obj("host_path" -> "/some/path").toString
        ))
      )
      import ContainerSpec.existingVMSFmt
      val Success(volumeContainer) = createInstance(
        ResourceIds.Container,
        name = "test-container",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "image" -> "test-image",
          "container_type" -> "docker",
          "provider" -> Json.obj(
            "id" -> uuid()
          ).toString,
          "volumes" -> Json.toJson(Seq(ContainerSpec.ExistingVolumeMountSpec("/mount/path", volume.id))).toString
        ))
      )
    }

    trait testEndpoint extends testAppMocks {
      val (testOrg,testWork,testEnv) = {
        val Success(to) = createOrg(name = uuid().toString)
        val Success((tw,te)) = createWorkEnv(org = to.id)
        Ents.setNewResourceEntitlements(dummyRootOrgId, te.id, user, Some(tw.id))
        Ents.setNewResourceEntitlements(dummyRootOrgId, tw.id, user, Some(dummyRootOrgId))
        Ents.setNewResourceEntitlements(dummyRootOrgId, to.id, user, None)
        (to,tw,te)
      }
      val Success(kubeProvider) = createKubernetesProvider(parent = testEnv.id, config = Seq(
        "access_key" -> "test_key",
        "secret_key" -> "test_key"
      ))
      val Success(gtw) = createDummyGateway(testEnv.id)
      val Success(kongProviderWithAbsentVhost) = createDummyKong(testEnv.id, props = Map(
        "config" -> Json.obj(
          "env" -> Json.obj(
            "public" -> Json.obj(
            )
          )
        ).toString
      ))
      val Success(api) = createInstance(
        ResourceIds.Api,
        name = "test-api",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> Json.obj(
            "id" -> gtw.id,
            "locations" -> Seq(kongProviderWithAbsentVhost.id)
          ).toString
        ))
      )
      val Success(lambda) = createInstance(
        ResourceIds.Lambda,
        name = "test-lambda",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> Json.obj(
            "id" -> uuid()
          ).toString,
          "public" -> "true",
          "cpus" -> "1.0",
          "code_type" -> "inline",
          "timeout" -> "120",
          "handler" -> "whatever",
          "runtime" -> "whatever",
          "memory" -> "2048"
        ))
      )
      val Success(lambda2) = createInstance(
        ResourceIds.Lambda,
        name = "test-lambda-2",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> Json.obj(
            "id" -> uuid()
          ).toString,
          "public" -> "true",
          "cpus" -> "1.0",
          "code_type" -> "inline",
          "timeout" -> "120",
          "handler" -> "whatever",
          "runtime" -> "whatever",
          "memory" -> "2048"
        ))
      )
      val volumeMountPath = "/mnt/test-volume"
      val Success(containerVolume) = createInstance(
        migrations.V13.VOLUME_TYPE_ID,
        name = "container-volume",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> Json.obj(
            "id" -> kubeProvider.id
          ).toString,
          "type" -> "host_path",
          "config" -> Json.obj(
            "host_path" -> "/tmp"
          ).toString,
          "access_mode" -> "ReadOnlyMany",
          "size" -> "1000"
        ))
      )
      val Success(container) = createInstance(
        ResourceIds.Container,
        name = "test-container",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "image" -> "test-image",
          "container_type" -> "docker",
          "volumes" -> Json.toJson(
            Seq(Json.obj(
              "mount_path" -> volumeMountPath,
              "volume_id" -> containerVolume.id
            ))
          ).toString,
          "provider" -> Json.obj(
            "id" -> kubeProvider.id
          ).toString
        ))
      )
      val lambdaEndpointPath = "/endpoint/lambda"
      val Success(lambdaEndpointWithHostsAndPath) = createInstance(
        ResourceIds.ApiEndpoint,
        name = "lambda-endpoint-with-hosts",
        parent = Some(api.id),
        properties = Some(Map(
          "upstream_url" -> "http://original-upstream-url-is-irrelevant:1234/blah/blah/blah",
          "methods" -> Json.toJson(Seq("GET")).toString,
          "implementation_type" -> "lambda",
          "implementation_id" -> lambda2.id.toString,
          "resource" -> "/some/path",
          "hosts" -> Json.toJson(Seq("first.some-domain.com", "second.some-domain.com")).toString,
          "provider" -> Json.obj(
            "id" -> gtw.id,
            "locations" -> Seq(kongProviderWithAbsentVhost.id)
          ).toString
        ))
      )
      val Success(lambdaEndpointWithHosts) = createInstance(
        ResourceIds.ApiEndpoint,
        name = "lambda-endpoint-with-hosts",
        parent = Some(api.id),
        properties = Some(Map(
          "upstream_url" -> "http://original-upstream-url-is-irrelevant:1234/blah/blah/blah",
          "methods" -> Json.toJson(Seq("GET")).toString,
          "implementation_type" -> "lambda",
          "implementation_id" -> lambda2.id.toString,
          "hosts" -> Json.toJson(Seq("first.some-domain.com", "second.some-domain.com")).toString,
          "provider" -> Json.obj(
            "id" -> gtw.id,
            "locations" -> Seq(kongProviderWithAbsentVhost.id)
          ).toString
        ))
      )
      val Success(lambdaEndpoint) = createInstance(
        ResourceIds.ApiEndpoint,
        name = "lambda-endpoint",
        parent = Some(api.id),
        properties = Some(Map(
          "resource"     -> lambdaEndpointPath,
          "upstream_url" -> "http://original-upstream-url-is-irrelevant:1234/blah/blah/blah",
          "methods" -> Json.toJson(Seq("GET")).toString,
          "implementation_type" -> "lambda",
          "implementation_id" -> lambda.id.toString,
          "provider" -> Json.obj(
            "id" -> gtw.id,
            "locations" -> Seq(kongProviderWithAbsentVhost.id)
          ).toString
        ))
      )
      val containerEndpointPath = "/endpoint/container"
      val Success(containerEndpoint) = createInstance(
        ResourceIds.ApiEndpoint,
        name = "container-endpoint",
        parent = Some(api.id),
        properties = Some(Map(
          "resource"     -> containerEndpointPath,
          "upstream_url" -> "http://original-upstream-url-is-irrelevant:1234/blah/blah/blah",
          "methods" -> Json.toJson(Seq("GET")).toString,
          "implementation_type" -> "container",
          "implementation_id" -> container.id.toString,
          "provider" -> Json.obj(
            "id" -> gtw.id,
            "locations" -> Seq(kongProviderWithAbsentVhost.id)
          ).toString
        ))
      )
      val containerService = app.injector.instanceOf[ContainerService]
      containerService.getEnvironmentContainer(testOrg.name, testEnv.id, container.id) returns Future.successful(Some(container -> Seq.empty))
      containerService.listEnvironmentContainers(testOrg.name, testEnv.id) returns Future.successful(Seq(container -> Seq.empty))
    }

    
    "render apiendpoints with .properties.public_url if present in kong provider" in new testEndpoint {

      "protocol" | "vhost"                  | "expected_url"                                              |>
      "https"    ! "kong.mycompany.com"     ! s"https://kong.mycompany.com/${api.name}${lambdaEndpointPath}"    |
      "http"     ! "new-kong.mycompany.com" ! s"http://new-kong.mycompany.com/${api.name}${lambdaEndpointPath}" |
      {
        (testProto,testVhost,expectedUrl) =>
          val Success(_) = ResourceFactory.update(kongProviderWithAbsentVhost.copy(
            properties = Some(Map(
              "config" -> Json.obj(
                "env" -> Json.obj(
                  "public" -> Json.obj(
                    "PUBLIC_URL_VHOST_0" -> testVhost
                  )
                ),
                "external_protocol" -> testProto
              ).toString
            ))
          ), user.account.id)
          val Some(result) = route(app,fakeAuthRequest(GET,
            s"/${testOrg.name}/apiendpoints/${lambdaEndpoint.id}", testCreds
          ))
          status(result) must equalTo(OK)
          val json = contentAsJson(result)
          (json \ "properties" \ "public_url").asOpt[String] must beSome(expectedUrl)
      }

    }

    "render apiendpoints with .properties.public_url from .properties.hosts if present" in new testEndpoint {

      "protocol" | "vhost"           | "expected_url"                   |>
        "https"  ! "does-not-matter" ! s"https://first.some-domain.com" |
        "http"   ! "does-not-matter" ! s"http://first.some-domain.com"  |
        {
          (testProto,testVhost,expectedUrl) =>
            val Success(_) = ResourceFactory.update(kongProviderWithAbsentVhost.copy(
              properties = Some(Map(
                "config" -> Json.obj(
                  "env" -> Json.obj(
                    "public" -> Json.obj(
                      "PUBLIC_URL_VHOST_0" -> testVhost
                    )
                  ),
                  "external_protocol" -> testProto
                ).toString
              ))
            ), user.account.id)
            val Some(result) = route(app,fakeAuthRequest(GET,
              s"/${testOrg.name}/apiendpoints/${lambdaEndpointWithHosts.id}", testCreds
            ))
            status(result) must equalTo(OK)
            val json = contentAsJson(result)
            (json \ "properties" \ "public_url").asOpt[String] must beSome(expectedUrl)
        }
    }

    "render apiendpoints with .properties.public_url from .properties.hosts and .properties.resource if both present" in new testEndpoint {

      "protocol" | "vhost"           | "expected_url"                   |>
        "https"  ! "does-not-matter" ! s"https://first.some-domain.com/test-api/some/path" |
        "http"   ! "does-not-matter" ! s"http://first.some-domain.com/test-api/some/path"  |
        {
          (testProto,testVhost,expectedUrl) =>
            val Success(_) = ResourceFactory.update(kongProviderWithAbsentVhost.copy(
              properties = Some(Map(
                "config" -> Json.obj(
                  "env" -> Json.obj(
                    "public" -> Json.obj(
                      "PUBLIC_URL_VHOST_0" -> testVhost
                    )
                  ),
                  "external_protocol" -> testProto
                ).toString
              ))
            ), user.account.id)
            val Some(result) = route(app,fakeAuthRequest(GET,
              s"/${testOrg.name}/apiendpoints/${lambdaEndpointWithHostsAndPath.id}", testCreds
            ))
            status(result) must equalTo(OK)
            val json = contentAsJson(result)
            (json \ "properties" \ "public_url").asOpt[String] must beSome(expectedUrl)
        }
    }

    "not render apiendpoints with .properties.public_url if not present in kong provider" in new testEndpoint {
      val Some(result) = route(app,fakeAuthRequest(GET,
        s"/${testOrg.name}/apiendpoints/${lambdaEndpoint.id}", testCreds
      ))
      status(result) must equalTo(OK)
      val json = contentAsJson(result)
      (json \ "properties" \ "public_url") must beUndefined
    }

    "render single lambdas with .properties.apiendpoints if embed=apiendpoints" in new testEndpoint {
      val Success(_) = ResourceFactory.update(kongProviderWithAbsentVhost.copy(
        properties = Some(Map(
          "config" -> Json.obj(
            "env" -> Json.obj(
              "public" -> Json.obj(
                "PUBLIC_URL_VHOST_0" -> "kong.mycompany.com"
              )
            ),
            "external_protocol" -> "https"
          ).toString
        ))
      ), user.account.id)
      val Some(result) = route(app,fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/lambdas/${lambda.id}?embed=apiendpoints", testCreds
      ))
      status(result) must equalTo(OK)
      val json = contentAsJson(result)
      ((json \ "properties" \ "apiendpoints")(0) \ "id").asOpt[UUID] must beSome(lambdaEndpoint.id)
      ((json \ "properties" \ "apiendpoints")(0) \ "properties" \ "public_url").asOpt[String] must beSome(s"https://kong.mycompany.com/${api.name}${lambdaEndpointPath}")
    }

    "render listed lambdas with .properties.apiendpoints if embed=apiendpoints" in new testEndpoint {
      val Success(_) = ResourceFactory.update(kongProviderWithAbsentVhost.copy(
        properties = Some(Map(
          "config" -> Json.obj(
            "env" -> Json.obj(
              "public" -> Json.obj(
                "PUBLIC_URL_VHOST_0" -> "kong.mycompany.com"
              )
            ),
            "external_protocol" -> "https"
          ).toString
        ))
      ), user.account.id)
      val Some(result) = route(app,fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/lambdas?expand=true&embed=apiendpoints", testCreds
      ))
      status(result) must equalTo(OK)
      val json = contentAsJson(result)
      json.as[Seq[JsObject]] must contain(
        (json: JsObject) =>
          (((json \ "properties" \ "apiendpoints") (0) \ "id").asOpt[UUID] must beSome(lambdaEndpoint.id)) and
            (((json \ "properties" \ "apiendpoints") (0) \ "properties" \ "public_url").asOpt[String] must beSome(s"https://kong.mycompany.com/${api.name}${lambdaEndpointPath}"))
      ).exactly(1)
    }

    "not render lambdas with .properties.apiendpoints if missing embed=apiendpoints" in new testEndpoint {
      val Success(_) = ResourceFactory.update(kongProviderWithAbsentVhost.copy(
        properties = Some(Map(
          "config" -> Json.obj(
            "env" -> Json.obj(
              "public" -> Json.obj(
                "PUBLIC_URL_VHOST_0" -> "kong.mycompany.com"
              )
            ),
            "external_protocol" -> "https"
          ).toString
        ))
      ), user.account.id)
      val Some(result) = route(app,fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/lambdas/${lambda.id}", testCreds
      ))
      status(result) must equalTo(OK)
      val json = contentAsJson(result)
      (json \ "properties" \ "apiendpoints") must beUndefined
    }

    "render single containers with .properties.apiendpoints if embed=apiendpoints" in new testEndpoint {
      val Success(_) = ResourceFactory.update(kongProviderWithAbsentVhost.copy(
        properties = Some(Map(
          "config" -> Json.obj(
            "env" -> Json.obj(
              "public" -> Json.obj(
                "PUBLIC_URL_VHOST_0" -> "kong.mycompany.com"
              )
            ),
            "external_protocol" -> "https"
          ).toString
        ))
      ), user.account.id)
      val Some(result) = route(app,fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/containers/${container.id}?embed=apiendpoints", testCreds
      ))
      status(result) must equalTo(OK)
      val json = contentAsJson(result)
      ((json \ "properties" \ "apiendpoints")(0) \ "id").asOpt[UUID] must beSome(containerEndpoint.id)
      ((json \ "properties" \ "apiendpoints")(0) \ "properties" \ "public_url").asOpt[String] must beSome(s"https://kong.mycompany.com/${api.name}${containerEndpointPath}")
    }

    "render listed containers with .properties.apiendpoints if embed=apiendpoints" in new testEndpoint {
      val Success(_) = ResourceFactory.update(kongProviderWithAbsentVhost.copy(
        properties = Some(Map(
          "config" -> Json.obj(
            "env" -> Json.obj(
              "public" -> Json.obj(
                "PUBLIC_URL_VHOST_0" -> "kong.mycompany.com"
              )
            ),
            "external_protocol" -> "https"
          ).toString
        ))
      ), user.account.id)
      val Some(result) = route(app,fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/containers?expand=true&embed=apiendpoints", testCreds
      ))
      status(result) must equalTo(OK)
      val json = contentAsJson(result)
      ((json(0) \ "properties" \ "apiendpoints")(0) \ "id").asOpt[UUID] must beSome(containerEndpoint.id)
      ((json(0) \ "properties" \ "apiendpoints")(0) \ "properties" \ "public_url").asOpt[String] must beSome(s"https://kong.mycompany.com/${api.name}${containerEndpointPath}")
    }

    "not render containers with .properties.apiendpoints if missing embed=apiendpoints" in new testEndpoint {
      val Success(_) = ResourceFactory.update(kongProviderWithAbsentVhost.copy(
        properties = Some(Map(
          "config" -> Json.obj(
            "env" -> Json.obj(
              "public" -> Json.obj(
                "PUBLIC_URL_VHOST_0" -> "kong.mycompany.com"
              )
            ),
            "external_protocol" -> "https"
          ).toString
        ))
      ), user.account.id)
      val Some(result) = route(app,fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/containers/${container.id}", testCreds
      ))
      status(result) must equalTo(OK)
      val json = contentAsJson(result)
      (json \ "properties" \ "apiendpoints") must beUndefined
    }

    "render listed containers with .properties.volumes expanded if embed=volumes" in new testEndpoint {
      val Some(result) = route(app,fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/containers?expand=true&embed=volumes", testCreds
      ))
      status(result) must equalTo(OK)
      val json = contentAsJson(result)
      println(json)
      (json(0) \ "properties" \ "volumes").as[Seq[JsObject]].size must_== 1
      ((json(0) \ "properties" \ "volumes")(0) \ "mount_path").asOpt[String] must beSome(volumeMountPath)
      ((json(0) \ "properties" \ "volumes")(0) \ "volume_id").asOpt[UUID] must beSome(containerVolume.id)
      ((json(0) \ "properties" \ "volumes")(0) \ "volume_resource" \ "id").asOpt[UUID] must beSome(containerVolume.id)
      ((json(0) \ "properties" \ "volumes")(0) \ "volume_resource" \ "name").asOpt[String] must beSome(containerVolume.name)
      ((json(0) \ "properties" \ "volumes")(0) \ "volume_resource" \ "properties" \ "type").asOpt[String] must beSome("host_path")
      ((json(0) \ "properties" \ "volumes")(0) \ "volume_resource" \ "properties" \ "container" \ "id").asOpt[UUID] must beSome(container.id)
      ((json(0) \ "properties" \ "volumes")(0) \ "volume_resource" \ "properties" \ "mount_path").asOpt[String] must beSome(volumeMountPath)
    }

    "not render containers with .properties.apiendpoints if missing embed=apiendpoints" in new testEndpoint {
      val Some(result) = route(app,fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/containers?expand=true", testCreds
      ))
      status(result) must equalTo(OK)
      val json = contentAsJson(result)
      (json(0) \ "properties" \ "volumes").as[Seq[JsObject]].size must_== 1
      ((json(0) \ "properties" \ "volumes")(0) \ "mount_path").asOpt[String] must beSome(volumeMountPath)
      ((json(0) \ "properties" \ "volumes")(0) \ "volume_id").asOpt[UUID] must beSome(containerVolume.id)
      ((json(0) \ "properties" \ "volumes")(0) \ "volume_resource").isDefined must beFalse
    }

    "render single volume with .properties.container/.properties.mount_path if embed=container" in new testVolume {
      val Some(result) = route(app,fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/volumes/${volume.id}?embed=container", testCreds
      ))
      status(result) must equalTo(OK)
      val json = contentAsJson(result)
      (json \ "properties" \ "container" \ "id").asOpt[UUID] must beSome(volumeContainer.id)
      (json \ "properties" \ "container" \ "name").asOpt[String] must beSome(volumeContainer.name)
      (json \ "properties" \ "mount_path").asOpt[String] must beSome("/mount/path")
    }

    "render listed volumes with .properties.container/.properties.mount_path if embed=container" in new testVolume {
      val Some(result) = route(app,fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/volumes?expand=true&embed=container", testCreds
      ))
      status(result) must equalTo(OK)
      val json = contentAsJson(result).as[Seq[JsObject]].head
      (json \ "properties" \ "container" \ "id").asOpt[UUID] must beSome(volumeContainer.id)
      (json \ "properties" \ "container" \ "name").asOpt[String] must beSome(volumeContainer.name)
      (json \ "properties" \ "mount_path").asOpt[String] must beSome("/mount/path")
    }

    "not render volumes with .properties.container if missing embed=container" in new testVolume {
      val Some(result) = route(app,fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/volumes/${volume.id}", testCreds
      ))
      status(result) must equalTo(OK)
      val json = contentAsJson(result)
      (json \ "properties" \ "container") must beUndefined
      (json \ "properties" \ "mount_path") must beUndefined
    }

    "mask security credentials for provider" in new testEndpoint {
      val Some(result) = route(app,fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/providers/${kubeProvider.id}", testCreds
      ))
      status(result) must equalTo(OK)
      val json = contentAsJson(result)
      println(Json.stringify(json))
      (json \ "properties" \ "config" \ "access_key").asOpt[String] must beSome("*" * 8)
      (json \ "properties" \ "config" \ "secret_key").asOpt[String] must beSome("*" * 8)
    }

    "mask security credentials for embedded provider" in new testEndpoint {
      val Some(result) = route(app,fakeAuthRequest(GET,
        s"/${testOrg.name}/environments/${testEnv.id}/containers/${container.id}?embed=provider", testCreds
      ))
      status(result) must equalTo(OK)
      val json = contentAsJson(result)
      (json \ "properties" \ "provider" \ "properties" \ "config" \ "access_key").asOpt[String] must beSome("*" * 8)
      (json \ "properties" \ "provider" \ "properties" \ "config" \ "secret_key").asOpt[String] must beSome("*" * 8)
    }

  }

}