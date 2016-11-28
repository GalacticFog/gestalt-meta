package com.galacticfog.gestalt.meta.auth


import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Step
import play.api.libs.json._

import java.util.UUID

import com.galacticfog.gestalt.meta.test._

//import org.scalatestplus.play.{OneAppPerSuite, PlaySpec}
import play.api.test._
import play.api.test.Helpers._
import com.galacticfog.gestalt.data.bootstrap.SystemType
import com.galacticfog.gestalt.data.bootstrap.ActionInfo
import com.galacticfog.gestalt.data.bootstrap.LineageInfo


trait ResourceTypeScope extends Scope {
  
}

class AuthorizationSpec extends PlaySpecification with ResourceScope with BeforeAll  {

  sequential
  
   override def beforeAll(): Unit = pristineDatabase
  
  case object Auth extends Authorization
  
  def cat[A](a: A, b: String) = "%s.%s".format(a,b)
  

  "getSelfActions" should {
    
    "return a list of fully qualified action names for the given resource" in new WithApplication {
      
      val acts = Auth.getSelfActions(ResourceIds.Resource)
      
      acts.size === 4
      acts.contains("resource.create")
      acts.contains("resource.view")
      acts.contains("resource.update")
      acts.contains("resource.delete")
    }
    
    
    "return an aggregate list of action names for the given type and any super types" in {
      val tid1 = uuid()
      val typename = uuid().toString
      
      SystemType(dummyRootOrgId, dummyOwner,
        typeId      = tid1, 
        typeName    = typename,
        extend      = Some(ResourceIds.Resource)
      ).withActionInfo(
        ActionInfo(
            prefix = typename, 
            verbs = Seq("start", "stop"))    
      ).save()
      
      val type1 = TypeFactory.findById(tid1)
      type1 must beSome
      
      val acts = Auth.getSelfActions(tid1)
      acts.nonEmpty must beTrue
      acts.size === 6
      
      /* Fully-Qualified Action-Name */
      def fqan(verb: String) = s"$typename.$verb"
      
      acts.contains(fqan("create")) must beTrue
      acts.contains(fqan("view")) must beTrue
      acts.contains(fqan("update")) must beTrue
      acts.contains(fqan("delete")) must beTrue
      acts.contains(fqan("start")) must beTrue
      acts.contains(fqan("stop")) must beTrue
      
      /*
       * Try a deeper inheritance hierarchy
       */
      val tid2 = uuid()
      val tid3 = uuid()
      
      val tname2 = uuid.toString()
      val tname3 = uuid.toString()
      
      SystemType(dummyRootOrgId, dummyOwner,
        typeId      = tid2, 
        typeName    = tname2,
        extend      = Some(ResourceIds.Resource)
        
      ).withActionInfo(
          ActionInfo(
              prefix = tname2, 
              verbs  = Seq("start", "stop", "restart"))
      ).save()
      
      SystemType(dummyRootOrgId, dummyOwner,
        typeId      = tid3, 
        typeName    = tname3,
        extend      = Some(tid2)
        
      ).withActionInfo (
          ActionInfo(
              prefix = tname3, 
              verbs  = Seq("foo", "bar", "baz", "qux"))          
      ).save()
      
      
      val type2 = TypeFactory.findById(tid2)
      type2 must beSome
      
      val type3 = TypeFactory.findById(tid2)
      type3 must beSome
      
      val acts2 = Auth.getSelfActions(tid3)
      acts2.nonEmpty must beTrue
      acts2.size === 11
      
      acts2.contains(s"$tname3.create") must beTrue
      acts2.contains(s"$tname3.view")   must beTrue
      acts2.contains(s"$tname3.update") must beTrue
      acts2.contains(s"$tname3.delete") must beTrue
      acts2.contains(s"$tname3.start")  must beTrue
      acts2.contains(s"$tname3.stop")   must beTrue
      acts2.contains(s"$tname3.restart") must beTrue
      acts2.contains(s"$tname3.foo") must beTrue
      acts2.contains(s"$tname3.bar") must beTrue
      acts2.contains(s"$tname3.baz") must beTrue
      acts2.contains(s"$tname3.qux") must beTrue
      
    }
    
    "get actions for supertypes only when resource has no unique actions" in {
      val tid1 = uuid()
      val typename = uuid().toString
      
      SystemType(dummyRootOrgId, dummyOwner,
        typeId      = tid1, 
        typeName    = typename,
        extend      = Some(ResourceIds.Resource)
      ).withActionInfo(
        ActionInfo(
            prefix = typename, 
            verbs = Seq.empty)    
      ).save()

      val tid2 = uuid()
      
      SystemType(dummyRootOrgId, dummyOwner,
        typeId      = tid2, 
        typeName    = tid2.toString,
        extend      = Some(tid1)
      ).withActionInfo(
        ActionInfo(
            prefix = "_not_used_", 
            verbs = Seq.empty)    
      ).save()      
      
      /* 
       * Resource should only have super-type actions - in this case, those defined on Gestalt::Resource
       * Here we have to strip off the prefixes that won't match - we only care that we have the same
       * action verbs.
       */
      val acts1 = Auth.getSelfActions(ResourceIds.Resource) map { s => s.drop(1 + s.lastIndexOf(""".""")) }
      val acts2 = Auth.getSelfActions(tid2) map { s => s.drop(1 + s.lastIndexOf(""".""")) }
      
      acts1 === acts2
    }
  }  
  
  "getChildActions" should {
    
    "return list of actions defined for all child-types of given type (include inherited)" in {
        val child1 = uuid()
        val parent = uuid()
        
        val c1prefix = "child1"
        val parentprefix = "parent"
        
        SystemType(dummyRootOrgId, dummyOwner,
          typeId      = child1, 
          typeName    = child1.toString,
          extend      = Some(ResourceIds.Resource)
        ).withActionInfo(
          ActionInfo(
              prefix = c1prefix, 
              verbs = Seq("foo", "bar"))    
        ).save()
        
        SystemType(dummyRootOrgId, dummyOwner,
          typeId      = parent, 
          typeName    = parent.toString,
          extend      = Some(ResourceIds.Resource)
        ).withActionInfo(
          ActionInfo(
              prefix = parentprefix, 
              verbs = Seq("create", "view", "update", "delete"))    
        ).withLineageInfo(
          LineageInfo(
            parent_types = Seq(ResourceIds.Org),
            child_types  = Option { 
              Seq(child1) 
            })
        ).save()
      
        val acts = Auth.getChildActions(parent)
        acts.nonEmpty must beTrue
        acts.size === 6
      
        acts.contains(cat(c1prefix, "create")) must beTrue
        acts.contains(cat(c1prefix, "view")) must beTrue
        acts.contains(cat(c1prefix, "update")) must beTrue
        acts.contains(cat(c1prefix, "delete")) must beTrue
        acts.contains(cat(c1prefix, "foo")) must beTrue
        acts.contains(cat(c1prefix, "bar")) must beTrue
    }
    
    "return an empty Seq if the type has no child types" in {
        val child1 = uuid()        
        val c1prefix = "child1"

        SystemType(dummyRootOrgId, dummyOwner,
          typeId      = child1, 
          typeName    = child1.toString,
          extend      = Some(ResourceIds.Resource)
        ).withActionInfo(
          ActionInfo(
              prefix = c1prefix, 
              verbs = Seq("foo", "bar"))    
        ).save()
        
        Auth.getChildActions(child1).isEmpty must beTrue
    }
  }
  
  "getNewResourceEntitlementSet" should {
    
    "contain actions names for the new ResourceType plus the actions of all child ResourceTypes" in {
      
      val child1 = uuid()
      val child2 = uuid()
      val parent = uuid()
      
      val c1prefix = "child1"
      val c2prefix = "child2"
      val parentprefix = "parent"
      
      SystemType(dummyRootOrgId, dummyOwner,
        typeId      = child1, 
        typeName    = child1.toString,
        extend      = Some(ResourceIds.Resource)
      ).withActionInfo(
        ActionInfo(
            prefix = c1prefix, 
            verbs = Seq("foo", "bar"))    
      ).save()
      
      SystemType(dummyRootOrgId, dummyOwner,
        typeId      = child2, 
        typeName    = child2.toString,
        extend      = Some(ResourceIds.Resource)
      ).withActionInfo(
        ActionInfo(
            prefix = c2prefix, 
            verbs = Seq("baz", "qux"))    
      ).save()
      
      SystemType(dummyRootOrgId, dummyOwner,
        typeId      = parent, 
        typeName    = parent.toString,
        extend      = Some(ResourceIds.Resource)
      ).withActionInfo(
        ActionInfo(
            prefix = parentprefix, 
            verbs = Seq("create", "view", "update", "delete"))    
      ).withLineageInfo(
        LineageInfo(
          parent_types = Seq(ResourceIds.Org),
          child_types  = Option {
            Seq( child1, child2)})
      ).save()      
      
      val acts = Auth.getNewResourceActionSet(parent)
      acts.nonEmpty must beTrue
      
      println("========CHILD2 ACTIONS=======")
      Auth.getNewResourceActionSet(child2) foreach println
      println("=============================")

      /*
       * There should be a total of 16 actions created:
       * child1 and child2 extend Resource which has CRUD = 8
       * 2 unique actions each for child 1 and 2 = 4
       * 4 CRUD actions for parent = 4
       */
      
      acts.size === 16
      
      
      acts.contains(cat(c1prefix, "foo")) must beTrue
      acts.contains(cat(c1prefix, "bar")) must beTrue
      acts.contains(cat(c1prefix, "create")) must beTrue
      acts.contains(cat(c1prefix, "view")) must beTrue
      acts.contains(cat(c1prefix, "update")) must beTrue
      acts.contains(cat(c1prefix, "delete")) must beTrue
      
      acts.contains(cat(c2prefix, "baz")) must beTrue
      acts.contains(cat(c2prefix, "qux")) must beTrue
      acts.contains(cat(c2prefix, "create")) must beTrue
      acts.contains(cat(c2prefix, "view")) must beTrue
      acts.contains(cat(c2prefix, "update")) must beTrue
      acts.contains(cat(c2prefix, "delete")) must beTrue
      
      acts.contains(cat(parentprefix, "create")) must beTrue
      acts.contains(cat(parentprefix, "view")) must beTrue
      acts.contains(cat(parentprefix, "update")) must beTrue
      acts.contains(cat(parentprefix, "delete")) must beTrue
      
    }
    
  }
  
  "newEntitlement" should {
    
    "return a new Entitlement resource" in {

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
  
  "setNewEntitlements" should {
    
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
      val pe = Auth.setNewEntitlements(dummyRootOrgId, p.get.id, creator, Option(dummyRootOrgId))
      pe.size === 16
      
      
      val c1 = createInstance(child1, name=uuid(), parent = Option(p.get.id))
      c1 must beSuccessfulTry
      val c1e = Auth.setNewEntitlements(dummyRootOrgId, c1.get.id, creator, Option(p.get.id))
      c1e.size === 6
      
      
      val c2 = createInstance(child2, name=uuid(), parent = Option(p.get.id))
      c2 must beSuccessfulTry
      val c2e = Auth.setNewEntitlements(dummyRootOrgId, c2.get.id, creator, Option(p.get.id))
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

}