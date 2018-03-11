package controllers

import java.util.UUID

import org.specs2.specification.BeforeAll
import org.specs2.mock.Mockito
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.auth.Entitlement
import controllers.util.GestaltProviderMocking
import play.api.libs.json.Json
import play.api.test.{FakeRequest, PlaySpecification, WithApplication}
import controllers.util.DataStore
import com.galacticfog.gestalt.meta.test._
import play.api.libs.concurrent.Execution.Implicits.defaultContext

class AuthorizationControllerSpec extends GestaltProviderMocking with MetaRepositoryOps {

  sequential

  abstract class TestApplication extends WithDbController[AuthorizationController](containerApp())
  
  "findOrFail" should {
    
    "succeed when the resource ID is valid" in new TestApplication {
      val resource = createInstance(ResourceIds.Org, uuid().toString)
      resource must beSuccessfulTry

      controller.findOrFail(ResourceIds.Org, resource.get.id) must beSuccessfulTry
    }

    "fail when the resource ID is invalid" in new TestApplication {
      controller.findOrFail(ResourceIds.Resource, uuid()) must beFailedTry.withThrowable[ResourceNotFoundException]
    }
  }

  "reconcile" should {
    
    "preserve a given list when there are no additions or deletions" in new TestApplication { 
      val base = Seq("a", "b", "c", "d", "e")
      val adds = Seq.empty
      val deletes = Seq.empty
      
      controller.reconcile(base, adds, deletes) === base
    }
    
    "preserve a given list while reflecting additions" in new TestApplication { 
      val base = Seq("a", "b", "c", "d", "e")
      val adds = Seq("f", "g", "h")
      val deletes = Seq.empty
      
      controller.reconcile(base, adds, deletes) === base ++ adds
    }
    
    "preserve a given list while reflecting deletions" in new TestApplication { 
      val base = Seq("a", "b", "c", "d", "e")
      val adds = Seq.empty
      val deletes = Seq("b", "d")
      
      controller.reconcile(base, adds, deletes) === Seq("a", "c", "e") 
    }
    
    "return empty if all entries are deleted" in new TestApplication { 
      val base = Seq("a", "b", "c", "d", "e")
      val adds = Seq.empty
      val deletes = base
      
      controller.reconcile(base, adds, deletes) === Seq.empty
    }
    
    "return additions if empty and only additions are made" in new TestApplication { 
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
    
    "cascade ADD identity when all descendant identities are empty" in new TestApplication {
      
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
    
    "reconcile ADDED identities when descendants have unique identity entries" in new TestApplication {
      
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
    
    "cascade REMOVE identities" in new TestApplication {
      
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

    val Success(testUser) = createInstance(
      typeId = ResourceIds.User,
      name = uuid().toString
    )
    val Success(testGroup) = createInstance(
      typeId = ResourceIds.Group,
      name = uuid().toString
    )

    "result in update calls to the associated apiendpoints" in new TestApplication {

//      var Success((testWork, testEnv)) = createWorkEnv()
//      val Success(api) = controller.CreateWithEntitlements(
//        dummyRootOrgId, user, Json.obj(
//          "name" -> uuid()
//        ), ResourceIds.Api, Some(testEnv.id)
//      )
      val Success(endpoint) = controller.CreateWithEntitlements(
        dummyRootOrgId, user, Json.obj(
          "name" -> uuid(),
          "properties" -> Json.obj(
            "resource" -> "/test-endpoint"
          )
        ), ResourceIds.ApiEndpoint, None // Some(api.id)
      )
      val List(ent) = ResourceFactory.findDescendantEntitlements(endpoint.id, "apiendpoint.invoke")
      val updated = Entitlement.make(ent).withIdentities(
        ids = Seq(testUser.id, testGroup.id)
      )

      val _ = controller.putEntitlementFqon(dummyRootOrgId.toString, ResourceIds.ApiEndpoint.toString, endpoint.id, ent.id)(FakeRequest(
        "PUT", s"/root/apiendpoints/${endpoint.id}/entitlements/${ent.id}"
      ).withBody(
        controller.transformEntitlement(Entitlement.toGestalt(user.account.id, updated), dummyRootOrgId, None)
      ))

      there was one(mockGatewayMethods).patchEndpointHandler()

      ok("done")
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