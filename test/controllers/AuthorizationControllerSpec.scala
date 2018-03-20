package controllers

import java.util.UUID

import org.specs2.specification.BeforeAll
import org.specs2.mock.Mockito
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.auth.Entitlement
import controllers.util.{DataStore, GatewayMethods, GestaltProviderMocking, ProviderMethods}
import play.api.libs.json.{JsObject, Json}
import play.api.test.{FakeRequest, PlaySpecification, WithApplication}
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp, PatchOps}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import org.mockito.Matchers
import org.specs2.matcher.JsonMatchers
import play.api.inject.bind

import scala.concurrent.Future
import scala.util.Success

class AuthorizationControllerSpec extends GestaltProviderMocking with BeforeAll with JsonMatchers {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

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

  abstract class FakeApplication extends WithDb(containerApp(
    additionalBindings = Seq(
      bind(classOf[ProviderMethods]).toInstance(mock[ProviderMethods]),
      bind(classOf[GatewayMethods]).toInstance(mock[GatewayMethods])
    )
  ))

  trait WithController extends FakeApplication {
    val controller = app.injector.instanceOf[AuthorizationController]
  }

  trait TestEndpointSupport extends FakeApplication with WithController {

    var Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    Entitlements.setNewResourceEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

    val mockGatewayMethods = app.injector.instanceOf[GatewayMethods]
    val mockProviderMethods = app.injector.instanceOf[ProviderMethods]

    val Success(testLambdaProvider) = createInstance(ResourceIds.LambdaProvider, "test-lambda-provider", properties = Some(Map(
      "config" ->
        """{
          |  "env": {
          |     "public": {
          |       "SERVICE_HOST": "laser.service",
          |       "SERVICE_PORT": "1111"
          |     }
          |  }
          |}""".stripMargin
    )))
    val Success(testGatewayProvider) = createInstance(ResourceIds.GatewayManager, "test-gateway-provider", properties = Some(Map(
      "config" ->
        """{
          |  "env": {
          |     "public": {
          |       "SERVICE_HOST": "gateway.service",
          |       "SERVICE_PORT": "6473"
          |     }
          |  }
          |}""".stripMargin
    )))
    val Success(testKongProvider) = createInstance(ResourceIds.KongGateway, "test-kong-provider", properties = None)
    val Success(testLambda) = createInstance(ResourceIds.Lambda, "test-lambda", properties = Some(Map(
      "public" -> "true",
      "cpus" -> "0.1",
      "memory" -> "128",
      "code_type" -> "inline",
      "timeout" -> "30",
      "handler" -> "blah;blah",
      "runtime" -> "custom",
      "provider" -> Json.obj(
        "name" -> testLambdaProvider.name,
        "id" -> testLambdaProvider.id.toString
      ).toString
    )))

    val Success(testApi) = controller.CreateWithEntitlements(
      dummyRootOrgId, user, Json.obj(
        "name" -> uuid(),
        "properties" -> Json.obj(
          "provider" -> Json.obj(
            "id" -> testGatewayProvider.id.toString,
            "locations" -> Json.arr(testKongProvider.id.toString).toString
          ).toString
        )
      ), ResourceIds.Api, Some(testEnv.id)
    )

    val Success(testEndpoint) = controller.CreateWithEntitlements(
      dummyRootOrgId, user, Json.obj(
        "name" -> uuid(),
        "properties" -> Json.obj(
          "resource"     -> "/original/path",
          "upstream_url" -> "http://original-upstream-url-is-irrelevant:1234/blah/blah/blah",
          "methods" -> Json.toJson(Seq("GET")).toString,
          "implementation_type" -> "lambda",
          "implementation_id" -> testLambda.id.toString,
          "provider" -> Json.obj(
            "id" -> testGatewayProvider.id.toString,
            "locations" -> Json.arr(testKongProvider.id.toString).toString
          ).toString,
          "plugins" -> Json.obj(
            "gestaltSecurity" -> Json.obj(
              "enabled" -> true,
              "users" -> Json.arr(),
              "groups" -> Json.arr()
            )
          ).toString
        )
      ), ResourceIds.ApiEndpoint, Some(testApi.id)
    )

    val Success(testUser) = createInstance(
      typeId = ResourceIds.User,
      name = uuid().toString,
      properties = Some(Map(
        "firstName" -> "Testy",
        "lastName" -> "McTestFace"
      ))
    )
    val Success(testGroup) = createInstance(
      typeId = ResourceIds.Group,
      name = uuid().toString
    )


  }

  "findOrFail" should {

    "succeed when the resource ID is valid" in new WithController {
      val resource = createInstance(ResourceIds.Org, uuid().toString)
      resource must beSuccessfulTry

      controller.findOrFail(ResourceIds.Org, resource.get.id) must beSuccessfulTry
    }

    "fail when the resource ID is invalid" in new WithController {
      controller.findOrFail(ResourceIds.Resource, uuid()) must beFailedTry.withThrowable[ResourceNotFoundException]
    }
  }

  "reconcile" should {

    "preserve a given list when there are no additions or deletions" in new WithController {
      val base = Seq("a", "b", "c", "d", "e")
      val adds = Seq.empty
      val deletes = Seq.empty

      controller.reconcile(base, adds, deletes) === base
    }

    "preserve a given list while reflecting additions" in new WithController {
      val base = Seq("a", "b", "c", "d", "e")
      val adds = Seq("f", "g", "h")
      val deletes = Seq.empty

      controller.reconcile(base, adds, deletes) === base ++ adds
    }

    "preserve a given list while reflecting deletions" in new WithController {
      val base = Seq("a", "b", "c", "d", "e")
      val adds = Seq.empty
      val deletes = Seq("b", "d")

      controller.reconcile(base, adds, deletes) === Seq("a", "c", "e")
    }

    "return empty if all entries are deleted" in new WithController {
      val base = Seq("a", "b", "c", "d", "e")
      val adds = Seq.empty
      val deletes = base

      controller.reconcile(base, adds, deletes) === Seq.empty
    }

    "return additions if empty and only additions are made" in new WithController {
      /*
       * start with an empty base, supply additions, should return == additions
       */
      val base = Seq.empty
      val adds = Seq("a", "b", "c", "d", "e")
      val deletes = Seq.empty

      controller.reconcile(base, adds, deletes) === adds
    }
  }

  import scala.util.{Try,Success,Failure}
  import com.galacticfog.gestalt.json.Js
  import com.galacticfog.gestalt.meta.auth.EntitlementProps

  "cascadeEntitlements" should {

    "cascade ADD identity when all descendant identities are empty" in new WithController {

      val acts = Seq("environment.view" -> Seq(dummyRootOrgId))
      val actsb = Seq("environment.view" -> Seq.empty)

      def setup(org: UUID) = {
        val (wrk, env1) = createWorkspaceEnvironment(org)
        setEntitlements2(wrk, actsb:_*)
        setEntitlements2(env1, actsb:_*)
      }

      val l1 = createOrg(uuid.toString, parent = Some(dummyRootOrgId))
      l1 must beSuccessfulTry
      val l2 = createOrg(uuid.toString, parent = Some(l1.get.id))
      l2 must beSuccessfulTry
      val l3 = createOrg(uuid.toString, parent = Some(l2.get.id))
      l3 must beSuccessfulTry
      val l4 = createOrg(uuid.toString, parent = Some(l3.get.id))
      l4 must beSuccessfulTry
      val l5 = createOrg(uuid.toString, parent = Some(l4.get.id))
      l5 must beSuccessfulTry

      // Create a Workspace and Environment in each org with an
      // environment.view entitlement (empty identities list).
      setup(l1.get.id)
      setup(l2.get.id)
      setup(l3.get.id)
      setup(l4.get.id)
      setup(l5.get.id)

      // Set entitlements on all the orgs
      setEntitlements2(l1.get.id, acts :_*) must beSuccessfulTry
      setEntitlements2(l2.get.id, actsb:_*) must beSuccessfulTry
      setEntitlements2(l3.get.id, actsb:_*) must beSuccessfulTry
      setEntitlements2(l4.get.id, actsb:_*) must beSuccessfulTry
      setEntitlements2(l5.get.id, actsb:_*) must beSuccessfulTry


      val es = ResourceFactory.findDescendantEntitlements(l1.get.id, "environment.view")
      es.size === 15
      (es exists { actionValue(_) != "environment.view" }) === false

      // Create new user to add to root ACL
      val bob = createNewUser(dummyRootOrgId)

      // Select the root entitlement - this is the one we perform the cascade against.
      val rootEntitlement = ResourceFactory.findChildrenOfType(
          dummyRootOrgId, l1.get.id, ResourceIds.Entitlement)(0)

      // Get new properties map with new ID added to identities
      val newprops: Map[String, String] = {
        val ps = EntitlementProps.make(rootEntitlement).addIdentities(bob.id)
        rootEntitlement.properties.get ++ Map(
            "identities" -> Json.stringify(Json.toJson(ps.identities)))
      }

      val current = ResourceFactory.findDescendantEntitlements(l1.get.id, "environment.view")

      // Copy identities back into properties map - cascade update to all descendants.
      val newEntitlement = rootEntitlement.copy(properties = Some(newprops))
      val updated = controller.cascadeEntitlementIdentities(
          l1.get.id, rootEntitlement, newEntitlement)

      // Ensure we have same number of entitlements in 'current' and 'updated'
      updated.size === current.size

      // Get any entitlements that DO NOT have the user 'bob' in identities.
      val missedSome = updated exists { ent =>
        val ids = EntitlementProps.make(ent).identities.get
        (!ids.contains(bob.id))
      }

      missedSome must beFalse


      val updatedroot = updated filter { _.id == rootEntitlement.id }
      val rps = EntitlementProps.make(updatedroot(0)).identities.get
      (rps.contains(dummyRootOrgId) && rps.contains(bob.id)) must beTrue

    }

    "reconcile ADDED identities when descendants have unique identity entries" in new WithController {

      // Create new user to add to root ACL
      val bob = createNewUser(dummyRootOrgId)
      val martha = createNewUser(dummyRootOrgId)

      val acts = Seq("environment.view" -> Seq(dummyRootOrgId))
      val actsb = Seq("environment.view" -> Seq(dummyRootOrgId, martha.id))

      def setup(org: UUID) = {
        val (wrk, env1) = createWorkspaceEnvironment(org)
        setEntitlements2(wrk, actsb:_*)
        setEntitlements2(env1, actsb:_*)
      }

      val l1 = createOrg(uuid.toString, parent = Some(dummyRootOrgId))
      l1 must beSuccessfulTry
      val l2 = createOrg(uuid.toString, parent = Some(l1.get.id))
      l2 must beSuccessfulTry
      val l3 = createOrg(uuid.toString, parent = Some(l2.get.id))
      l3 must beSuccessfulTry
      val l4 = createOrg(uuid.toString, parent = Some(l3.get.id))
      l4 must beSuccessfulTry
      val l5 = createOrg(uuid.toString, parent = Some(l4.get.id))
      l5 must beSuccessfulTry

      // Create a Workspace and Environment in each org with an
      // environment.view entitlement (empty identities list).
      setup(l1.get.id)
      setup(l2.get.id)
      setup(l3.get.id)
      setup(l4.get.id)
      setup(l5.get.id)

      // Set entitlements on all the orgs
      setEntitlements2(l1.get.id, acts :_*) must beSuccessfulTry
      setEntitlements2(l2.get.id, actsb:_*) must beSuccessfulTry
      setEntitlements2(l3.get.id, actsb:_*) must beSuccessfulTry
      setEntitlements2(l4.get.id, actsb:_*) must beSuccessfulTry
      setEntitlements2(l5.get.id, actsb:_*) must beSuccessfulTry


      val es = ResourceFactory.findDescendantEntitlements(l1.get.id, "environment.view")
      es.size === 15
      (es exists { actionValue(_) != "environment.view" }) === false

      // Select the root entitlement - this is the one we perform the cascade against.
      val rootEntitlement = ResourceFactory.findChildrenOfType(
          dummyRootOrgId, l1.get.id, ResourceIds.Entitlement)(0)

      // Get new properties map with new ID added to identities
      val newprops: Map[String, String] = {
        val ps = EntitlementProps.make(rootEntitlement).addIdentities(bob.id)
        rootEntitlement.properties.get ++ Map(
            "identities" -> Json.stringify(Json.toJson(ps.identities)))
      }

      val current = ResourceFactory.findDescendantEntitlements(l1.get.id, "environment.view")

      // Copy identities back into properties map - cascade update to all descendants.
      val newEntitlement = rootEntitlement.copy(properties = Some(newprops))
      val updated = controller.cascadeEntitlementIdentities(
          l1.get.id, rootEntitlement, newEntitlement)

      // Ensure we have same number of entitlements in 'current' and 'updated'
      updated.size === current.size

      // All entitlements should have 'dummyRoot' and 'bob' in ACL.
      val missedSome = updated exists { ent =>
        val ids = EntitlementProps.make(ent).identities.get
        (!ids.contains(dummyRootOrgId) && !ids.contains(bob.id))
      }

      missedSome must beFalse

      // The only entitlement missing 'martha' should be root
      val isroot = updated filter { ent =>
        val ids = EntitlementProps.make(ent).identities.get
        !ids.contains(martha.id)
      }

      isroot.size === 1
      isroot(0).id === rootEntitlement.id
    }

    "cascade REMOVE identities" in new WithController {

      // Create new user to add to root ACL
      val martha = createNewUser(dummyRootOrgId)

      val acts = Seq("environment.view" -> Seq(dummyRootOrgId, martha.id))
      val actsb = Seq("environment.view" -> Seq(dummyRootOrgId, martha.id))

      def setup(org: UUID) = {
        val (wrk, env1) = createWorkspaceEnvironment(org)
        setEntitlements2(wrk, actsb:_*)
        setEntitlements2(env1, actsb:_*)
      }

      val l1 = createOrg(uuid.toString, parent = Some(dummyRootOrgId))
      l1 must beSuccessfulTry
      val l2 = createOrg(uuid.toString, parent = Some(l1.get.id))
      l2 must beSuccessfulTry
      val l3 = createOrg(uuid.toString, parent = Some(l2.get.id))
      l3 must beSuccessfulTry
      val l4 = createOrg(uuid.toString, parent = Some(l3.get.id))
      l4 must beSuccessfulTry
      val l5 = createOrg(uuid.toString, parent = Some(l4.get.id))
      l5 must beSuccessfulTry

      // Create a Workspace and Environment in each org with an
      // environment.view entitlement (empty identities list).
      setup(l1.get.id)
      setup(l2.get.id)
      setup(l3.get.id)
      setup(l4.get.id)
      setup(l5.get.id)

      // Set entitlements on all the orgs
      setEntitlements2(l1.get.id, acts :_*) must beSuccessfulTry
      setEntitlements2(l2.get.id, actsb:_*) must beSuccessfulTry
      setEntitlements2(l3.get.id, actsb:_*) must beSuccessfulTry
      setEntitlements2(l4.get.id, actsb:_*) must beSuccessfulTry
      setEntitlements2(l5.get.id, actsb:_*) must beSuccessfulTry


      val es = ResourceFactory.findDescendantEntitlements(l1.get.id, "environment.view")
      es.size === 15
      (es exists { actionValue(_) != "environment.view" }) === false

      // Select the root entitlement - this is the one we perform the cascade against.
      val rootEntitlement = ResourceFactory.findChildrenOfType(
          dummyRootOrgId, l1.get.id, ResourceIds.Entitlement)(0)

      // Get new properties map user 'martha' removed from identities.
      val newprops: Map[String, String] = {
        val ps = EntitlementProps.make(rootEntitlement).removeIdentities(martha.id)
        rootEntitlement.properties.get ++ Map(
            "identities" -> Json.stringify(Json.toJson(ps.identities)))
      }

      val current = ResourceFactory.findDescendantEntitlements(l1.get.id, "environment.view")

      // All entitlements should have 'dummyRoot' and 'martha' in ACLs
      current exists { ent =>
        val ids = EntitlementProps.make(ent).identities.get
        (!ids.contains(dummyRootOrgId) || !ids.contains(martha.id))
      } must beFalse

      // Copy identities back into properties map - cascade update to all descendants.
      val newEntitlement = rootEntitlement.copy(properties = Some(newprops))
      val updated = controller.cascadeEntitlementIdentities(
          l1.get.id, rootEntitlement, newEntitlement)

      // Ensure we have same number of entitlements in 'current' and 'updated'
      updated.size === current.size

      // Zero entitlements should have 'martha' in the ACL.
      val missedSome = updated exists { ent =>
        val ids = EntitlementProps.make(ent).identities.get
        (ids.contains(martha.id))
      }

      missedSome must beFalse
    }
  }

  "apiendpoint.invoke entitlements" should {

    "result in update calls to the associated apiendpoints" in new TestEndpointSupport {

      val List(ent) = ResourceFactory.findDescendantEntitlements(testEndpoint.id, "apiendpoint.invoke")
      val updated = Entitlement.make(ent).withIdentities(
        ids = Seq(testUser.id, testGroup.id)
      )
      mockGatewayMethods.patchEndpointHandler(any, any, any, any) answers {
        (a: Any) =>
          val arr = a.asInstanceOf[Array[Object]]
          val r = arr(0).asInstanceOf[GestaltResourceInstance]
          // we don't really care how it answers, but we'd rather not log an error in this test
          Future.successful(r)
      }

      val body = Output.renderInstance(Entitlement.toGestalt(user.account.id, updated)).as[JsObject] - "resource_type"
      val resp = controller.putEntitlementFqon("root", ResourceIds.ApiEndpoint.toString, testEndpoint.id, ent.id)(fakeAuthRequest(
        PUT, s"/root/apiendpoints/${testEndpoint.id}/entitlements/${ent.id}", testCreds
      ).withBody(body))
      status(resp) must_== OK

      there was one(mockGatewayMethods).patchEndpointHandler(
        r = argThat( (r: GestaltResourceInstance) => r.id must_== testEndpoint.id),
        patch = Matchers.eq(PatchDocument(
          PatchOp.Replace("/properties/plugins/gestaltSecurity/users", Json.arr(testUser.id)),
          PatchOp.Replace("/properties/plugins/gestaltSecurity/groups", Json.arr(testGroup.id))
        )),
        user = any,
        request = any
      )

      val Some(updatedEndpoint) = ResourceFactory.findById(ResourceIds.ApiEndpoint, testEndpoint.id)
      val updatedPlugins = updatedEndpoint.properties.get("plugins")
      updatedPlugins must not /("gestaltSecurity") /("users")
      updatedPlugins must not /("gestaltSecurity") /("groups")
    }

  }
  
//  "postEntitlementCommon" should {
//    
//    "succeed when given valid data" in new TestApplication {
//      val org = createInstance(ResourceIds.Org, uuid.toString)
//      org must beSuccessfulTry
//      
//      //val controller = controllerInstance()
//      implicit val request = fakeAuthRequest(GET, s"/foo", testCreds)
//      
//      val input = Json.obj("name" -> "foo", "properties" -> Json.obj("action" -> "org.create"))
//      controller.postEntitlementCommon(
//          org.get.id, ResourceIds.Org, 
//          org.get.id, user, input, None) 
//    }
    
//    "fail when the parent resource can't be found" >> {
//      failure
//    }
    
//  }
  
//  "validateEntitlementPayload" should {
//    
//    "succeed when given valid data" >> {
//      failure
//    }
//    
//    "fail if caller DOES NOT have set-entitlements permission" >> {
//      failure
//    }
//    
//    "fail if the given action is invalid for the resource" >> {
//      failure
//    }
//    
//    "fail if there is already an entitlement for the specified action" >> {
//      failure
//    }
//    
//    "fail if any of the given identities DOES NOT exist" >> {
//      failure
//    }
//    
//  }
  
  
}