package com.galacticfog.gestalt.meta.auth

import org.specs2.specification.BeforeAll
import org.specs2.specification.Scope

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.TypeFactory
import com.galacticfog.gestalt.data.bootstrap.ActionInfo
import com.galacticfog.gestalt.data.bootstrap.LineageInfo
import com.galacticfog.gestalt.data.bootstrap.SystemType
import com.galacticfog.gestalt.data.string2uuid
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.json.Js
import controllers.util.GestaltSecurityMocking
import java.util.UUID


trait ResourceTypeScope extends Scope {
  
}

class AuthorizationMethodsSpec extends GestaltSecurityMocking with ResourceScope with BeforeAll  {

  sequential
  
  override def beforeAll(): Unit = pristineDatabase
  
  case object Auth extends AuthorizationMethods

  def cat[A](a: A, b: String) = "%s.%s".format(a,b)
  

  "newEntitlement" should {
    
    "return a new Entitlement resource" >> {

       val ent = Auth.newEntitlement(dummyOwner.id, dummyRootOrgId, 
           resource = uuid(),
           action = "foo.bar", 
           identities = Option(Seq(dummyOwner.id)), 
           name = None, 
           value = None, 
           description = None)
           
       ent.properties.action === "foo.bar"
       ent.properties.identities.size === 1
       ent.properties.identities must beSome
       ent.properties.identities.get(0).toString === dummyOwner.id
    }
    
  }
  
  "entitlements" should {

    "generate a Seq of Entitlements corresponding to the input list of actions" in {
      
      val actions = Seq("foo.bar", "baz.qux", "garply.waldo")
      val ents = Auth.entitlements(dummyOwner.id, dummyRootOrgId, uuid(), actions)
      
      ents.size === 3
      
      ents foreach { x => println("\n" + x) }
      
      ents exists { _.properties.action == actions(0) } must beTrue
      ents exists { _.properties.action == actions(1) } must beTrue
      ents exists { _.properties.action == actions(2) } must beTrue
    }
  }
  
  "setEntitlements" should {
    
    "set the given entitlements on the given resource" >> {
      
      val (wrk, _) = createWorkspaceEnvironment()
      
      val beforeEnts = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, wrk)
      val actions = Seq("foo.bar", "baz.qux", "garply.waldo")
      val ents = Auth.entitlements(dummyOwner.id, dummyRootOrgId, uuid(), actions)      
      
      val user = this.dummyAuthAccountWithCreds()
      val workspace = {
        val t = ResourceFactory.findById(wrk)
        t must beSome
        t.get
      }
      Auth.setEntitlements(dummyRootOrgId, user, workspace, ents)
      val afterEnts = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, wrk)
      
      afterEnts.size === (beforeEnts.size + actions.size)
      
      afterEnts exists { _.properties.get("action") == actions(0) } must beTrue
      afterEnts exists { _.properties.get("action") == actions(1) } must beTrue
      afterEnts exists { _.properties.get("action") == actions(2) } must beTrue
      
    }
    
  def makeDummyEntitlement(action: String, identities: Seq[UUID]) = {
    Entitlement(uuid(), dummyRootOrgId, uuid.toString, 
        properties = EntitlementProps(action, identities = Option(identities)))
  }    
    
    "return an existing entitlement with merged identities if one exists" >> {
      val user1 = createNewUser()
      val user2 = createNewUser()      
      
      val target = {
        val r = createInstance(ResourceIds.Workspace, uuid.toString)
        r must beSuccessfulTry
        r.get
      }
      
      Auth.findEntitlementsByResource(target.id).isEmpty === true
      
      val action1 = uuid.toString
      val action2 = uuid.toString
      val action3 = uuid.toString
      
      val ents = Seq(
          makeDummyEntitlement(action1, Seq(user1.id)),
          makeDummyEntitlement(action2, Seq(user1.id)),
          makeDummyEntitlement(action3, Seq(user1.id)))
      
      val creator = dummyAuthAccountWithCreds(userInfo = Map("id" -> user1.id))
      val results = Auth.setEntitlements(dummyRootOrgId, creator, target, ents)      
      
      results.filter( _.isFailure ).isEmpty === true

      val ents2 = Auth.findEntitlementsByResource(target.id)
      ents2.size === 3
      
      /*
       *  Try adding the same entitlements with a different identity
       */
      val duplicateEnts = Seq(
          makeDummyEntitlement(action1, Seq(user2.id)),
          makeDummyEntitlement(action2, Seq(user2.id)),
          makeDummyEntitlement(action3, Seq(user2.id, user1.id)))
      
      val results2 = Auth.setEntitlements(dummyRootOrgId, creator, target, duplicateEnts)
      results2.filter(_.isFailure).isEmpty === true
      
      val ents3 = Auth.findEntitlementsByResource(target.id)
      ents3.size === 3
      
      val actionList: Seq[String] = ents3.map(_.properties.get("action"))
      val identityList: Seq[Seq[UUID]] = ents3.map { 
        Entitlement.make( _ ).properties.identities.get
      }
      
      actionList.contains(action1) === true
      actionList.contains(action2) === true
      actionList.contains(action3) === true

      /*
       * Ensure identities contains IDs of both users
       */
      val expectedIds = Seq(user1.id, user2.id).sorted
      identityList(0).sorted === expectedIds
      identityList(1).sorted === expectedIds
      identityList(2).sorted === expectedIds
    }    
    
  }
  
  "setNewResourceEntitlements" should {
    
    "create new entitlements corresponding to actions on the new resource" in {
      
      val child1 = uuid()
      val child2 = uuid()
      val parent = uuid()
      
      SystemType(dummyRootOrgId, dummyOwner,
        typeId      = child1, 
        typeName    = "child1."+child1.toString,
        extend      = Some(ResourceIds.Resource)
      ).withActionInfo(
        ActionInfo(
            prefix = "child1", 
            verbs = Seq("start", "stop"))    
      ).save()
      
      SystemType(dummyRootOrgId, dummyOwner,
        typeId      = child2, 
        typeName    = "child2." + child2.toString,
        extend      = Some(ResourceIds.Resource)
      ).withActionInfo(
        ActionInfo(
            prefix = "child2", 
            verbs = Seq("foo", "bar"))    
      ).save()      
      
      SystemType(dummyRootOrgId, dummyOwner,
        typeId      = parent, 
        typeName    = "parent1." + parent.toString,
        extend      = Some(ResourceIds.Resource)
      ).withActionInfo(
        ActionInfo(
            prefix = "parent", 
            verbs = Seq("create", "view", "update", "delete"))    
      ).withLineageInfo(
        LineageInfo(
          parent_types = Seq(ResourceIds.Org),
          child_types  = Option {
            Seq(child1, child2)})
      ).save()
      
      TypeFactory.findById(parent) must beSome
      TypeFactory.findById(child1) must beSome
      TypeFactory.findById(child2) must beSome
      
      val creator = dummyAuthAccountWithCreds(userInfo = Map("id" -> dummyOwner.id))
      
      val p  = createInstance(parent, name=uuid(), parent = Option(dummyRootOrgId))
      p must beSuccessfulTry
      val pe = Auth.setNewResourceEntitlements(dummyRootOrgId, p.get.id, creator, Option(dummyRootOrgId))
      pe.size === 16
      
      
      val c1 = createInstance(child1, name=uuid(), parent = Option(p.get.id))
      c1 must beSuccessfulTry
      val c1e = Auth.setNewResourceEntitlements(dummyRootOrgId, c1.get.id, creator, Option(p.get.id))
      c1e.size === 6
      
      
      val c2 = createInstance(child2, name=uuid(), parent = Option(p.get.id))
      c2 must beSuccessfulTry
      val c2e = Auth.setNewResourceEntitlements(dummyRootOrgId, c2.get.id, creator, Option(p.get.id))
      c2e.size === 6
      
      
//      Auth.setNewEntitlements(dummyRootOrgId, child1, resource, user, grants, parent)
//      val c1e = ResourceFactory.findChildrenOfType(dummyRootOrgId, child1, ResourceIds.Entitlement)
//      c1e.nonEmpty must beTrue
//      c1e.size === 4
//
//      val c2e = ResourceFactory.findChildrenOfType(dummyRootOrgId, child2, ResourceIds.Entitlement)
//      c2e.nonEmpty must beTrue
//      c2e.size === 4      
//      
//      val pe = ResourceFactory.findChildrenOfType(dummyRootOrgId, parent, ResourceIds.Entitlement)
//      pe.nonEmpty must beTrue
//      pe.size === 8

    }
//    
////    "create new entitlements corresponding to actions on child resources" in {
////      failure
////    }
//    
  }
  
  "withIdentities" should {
    
    "add new identities to an existing entitlement.identities sequence" >> {
      
      val oldIdentities = Seq(uuid(), uuid(), uuid())
      val newIdentities = Seq(uuid(), uuid())
      val action = "foo." + uuid.toString
      val (wrk, env) = this.createWorkspaceEnvironment()
      
      val e1 = createEntitlement(action, env, Some(oldIdentities))
      val e2 = Auth.withIdentities(e1, newIdentities)

      val ids = {
        val eids = Entitlement.make(e2).properties.identities
        eids must beSome
        eids.get
      }

      val a: Seq[UUID] = Seq(ids:_*)
      val b: Seq[UUID] = Seq((oldIdentities ++ newIdentities):_*)

      (a.sorted == b.sorted) must beTrue      
    }
    
    "accept an empty list of new identities" >> {
      
      val oldIdentities = Seq(uuid(), uuid(), uuid())
      val newIdentities = Seq.empty
      val action = "foo." + uuid.toString
      val (wrk, env) = this.createWorkspaceEnvironment()
      
      val e1 = createEntitlement(action, env, Some(oldIdentities))
      
      val e2 = Auth.withIdentities(e1, newIdentities)

      val ids = {
        val eids = Entitlement.make(e2).properties.identities
        eids must beSome
        eids.get
      }
      
      val a: Seq[UUID] = Seq(ids:_*)
      val b: Seq[UUID] = Seq(oldIdentities:_*)

      (a.sorted == b.sorted) must beTrue    
    }
    
    "silently ensure that each identity occurs only once" >> {
      
      val usera = uuid()
      val userb = uuid()
      
      val oldIdentities = Seq(usera, userb)
      val newIdentities = Seq(uuid(), uuid())
      
      // these are redundant IDs and should be filtered out.
      val newIds2 = Seq(usera, userb, usera, userb) 
      
      val action = "foo." + uuid.toString
      val (wrk, env) = this.createWorkspaceEnvironment()
      
      val e1 = createEntitlement(action, env, Some(oldIdentities))
      val e2 = Auth.withIdentities(e1, (newIdentities ++ newIds2))

      val ids = {
        val eids = Entitlement.make(e2).properties.identities
        eids must beSome
        eids.get
      }

      val a: Seq[UUID] = Seq(ids:_*)
      val b: Seq[UUID] = Seq((oldIdentities ++ newIdentities):_*)

      (a.sorted == b.sorted) must beTrue   
    }
  }
  
//  "All Entitlements" should {
//    
//    "be owned by the Root Org, with no exceptions" >> {
//      val ents = ResourceFactory.findAll(ResourceIds.Entitlement)
//      val rootid = com.galacticfog.gestalt.meta.api.Resource.findFqon("root").get.id
//      
//      println("///////////")
//      println("ENTS.SIZE : " + ents.size)
//      ents.foreach { en => 
//        val e = Entitlement.make(en)
//        println(s"type: ${en.owner.typeId}, id: ${en.owner.id}, p: ${e.name}") 
//      }
//      println("///////////")
//      println("DUMMY-ORG : " + dummyRootOrgId)
//      println("ROOT : " + rootid)
//      
//      val fails = ents.filter(ent => ent.owner.id != rootid.toString)
//      println("fails.size : " + fails.size)
//      fails.isEmpty must beTrue
//    }
//  }

}