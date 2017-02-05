package controllers

import java.util.UUID

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Success
import scala.util.Try

import org.joda.time.DateTimeZone
import org.mockito.Matchers.{ eq => meq }
import org.specs2.execute.AsResult
import org.specs2.execute.Result
import org.specs2.matcher.JsonMatchers
import org.specs2.specification.BeforeAll

import com.galacticfog.gestalt.data.Instance
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import controllers.util.GestaltSecurityMocking
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.PlaySpecification
import play.api.test.WithApplication

import java.util.UUID

import scala.concurrent.Future
import scala.util.Success
import scala.util.Try

import org.joda.time.DateTime
import org.mockito.Matchers.{ eq => meq }
import org.specs2.matcher.JsonMatchers
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification.BeforeAll

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.meta.api.errors._

import controllers.util.GestaltSecurityMocking
import javax.inject.Singleton
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.PlaySpecification
import play.api.test.WithApplication

import org.specs2.mock.Mockito

class AuthorizationControllerSpec 
  extends PlaySpecification 
    with GestaltSecurityMocking 
    with ResourceScope 
    with BeforeAll
    with Mockito {
  
  override def beforeAll(): Unit = pristineDatabase()

  sequential

  
  abstract class FakeSecurity extends WithApplication(containerApp()) {
  }
  
  import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
  
  object Ents extends AuthorizationMethods
  abstract class TestApplication extends WithApplication(containerApp()) {

    var testEnv: Instance = null
    var testWork: Instance = null
    var testProvider: Instance = null
    var mockMarathonClient: MarathonClient = null

    def testFQON: String = "root"
    def testWID: UUID = testWork.id
    def testEID: UUID = testEnv.id
    def testPID: UUID = testProvider.id

    override def around[T: AsResult](t: => T): Result = super.around {
      var Success((tW,tE)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
      testWork = tW
      testEnv = tE
      testProvider = createMarathonProvider(testEID, "test-provider").get
      mockMarathonClient = mock[MarathonClient]
      val cs = containerService
      cs.findWorkspaceEnvironment(testEID) returns Try((testWork,testEnv))
      cs.marathonProvider(testPID) returns testProvider
      cs.marathonClient(testProvider) returns mockMarathonClient
      t
    }

  }  
//  trait TestApplication extends FakeSecurity {
//    
//    val controller = app.injector.instanceOf[AuthorizationController]
//    
//    var Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
//
//    Ents.setNewEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))
//    
//    
//    val user2 = createMetaUser(account2)
//    
//    var testProvider = createMarathonProvider(testEnv.id, "test-provider").get
//    var mockMarathonClient = mock[MarathonClient]
//    
//    containerService.findWorkspaceEnvironment(testEnv.id) returns Try((testWork, testEnv))
//    containerService.marathonProvider(testProvider.id) returns testProvider
//    containerService.marathonClient(testProvider) returns mockMarathonClient 
//  }
  
//  abstract class TestApplication extends WithApplication(containerApp()) {
//  }  
  

  def controllerInstance()(implicit app: play.api.Application) = app.injector.instanceOf[AuthorizationController]
                                                             

  "findOrFail" should {
    
    "succeed when the resource ID is valid" in new WithApplication {
      val ac = controllerInstance()
      
      val resource = createInstance(ResourceIds.Org, uuid().toString)
      resource must beSuccessfulTry
      
      ac.findOrFail(ResourceIds.Org, resource.get.id) must beSuccessfulTry
    }
    
    "fail when the resource ID is invalid" in new WithApplication {
      val ac = controllerInstance() 
      
      ac.findOrFail(ResourceIds.Resource, uuid()) must beFailedTry.withThrowable[ResourceNotFoundException]
    }
  }

  "reconcile" should {
    
    "preserve a given list when there are no additions or deletions" in new WithApplication {
      val ac = controllerInstance()
      
      val base = Seq("a", "b", "c", "d", "e")
      val adds = Seq.empty
      val deletes = Seq.empty
      
      ac.reconcile(base, adds, deletes) === base
    }
    
    "preserve a given list while reflecting additions" in new WithApplication {
      val ac = controllerInstance()
      
      val base = Seq("a", "b", "c", "d", "e")
      val adds = Seq("f", "g", "h")
      val deletes = Seq.empty
      
      ac.reconcile(base, adds, deletes) === base ++ adds
    }
    
    "preserve a given list while reflecting deletions" in new WithApplication {
      val ac = controllerInstance()
      
      val base = Seq("a", "b", "c", "d", "e")
      val adds = Seq.empty
      val deletes = Seq("b", "d")
      
      ac.reconcile(base, adds, deletes) === Seq("a", "c", "e") 
    }
    
    "return empty if all entries are deleted" in new WithApplication {
      val ac = controllerInstance()
      
      val base = Seq("a", "b", "c", "d", "e")
      val adds = Seq.empty
      val deletes = base
      
      ac.reconcile(base, adds, deletes) === Seq.empty
    }
    
    "return additions if empty and only additions are made" in new WithApplication {
      /*
       * start with an empty base, supply additions, should return == additions
       */
      val ac = controllerInstance()
      
      val base = Seq.empty
      val adds = Seq("a", "b", "c", "d", "e")
      val deletes = Seq.empty
      
      ac.reconcile(base, adds, deletes) === adds
    }
  }
  
  import scala.util.{Try,Success,Failure}
  import com.galacticfog.gestalt.json.Js
  import com.galacticfog.gestalt.meta.auth.EntitlementProps
  
  "cascadeEntitlements" should {
    
    "cascade ADD identity when all descendant identities are empty" in new WithApplication {
      
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
      
      val ac = controllerInstance()
      
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
      val updated = ac.cascadeEntitlementIdentities(
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
    
    "reconcile ADDED identities when descendants have unique identity entries" in new WithApplication {
      
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
      
      val ac = controllerInstance()
      
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
      val updated = ac.cascadeEntitlementIdentities(
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
    
    "cascade REMOVE identities" in new WithApplication {
      
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
      
      val ac = controllerInstance()
      
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
      val updated = ac.cascadeEntitlementIdentities(
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
  
//  "postEntitlementCommon" should {
//    
//    "succeed when given valid data" in new TestApplication {
//      val org = createInstance(ResourceIds.Org, uuid.toString)
//      org must beSuccessfulTry
//      
//      val ac = controllerInstance()
//      implicit val request = fakeAuthRequest(GET, s"/foo", testCreds)
//      
//
//      
//
//      val input = Json.obj("name" -> "foo", "properties" -> Json.obj("action" -> "org.create"))
//      ac.postEntitlementCommon2(
//          org.get.id, ResourceIds.Org, 
//          org.get.id, user, input, None) 
//    }
//    
//    "fail when the parent resource can't be found" >> {
//      failure
//    }
//    
//  }
//  
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