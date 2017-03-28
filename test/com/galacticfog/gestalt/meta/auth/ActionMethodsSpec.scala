package com.galacticfog.gestalt.meta.auth

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data._
import org.specs2.specification._
import com.galacticfog.gestalt.meta.test._
import play.api.test._
import com.galacticfog.gestalt.data.bootstrap.SystemType
import com.galacticfog.gestalt.data.bootstrap.ActionInfo
import com.galacticfog.gestalt.data.bootstrap.LineageInfo

class ActionMethodsSpec extends PlaySpecification with ResourceScope with BeforeAll {

  sequential

  override def beforeAll(): Unit = pristineDatabase

  case object Auth extends AuthorizationMethods

  def cat[A](a: A, b: String) = "%s.%s".format(a,b)

  "getSelfActions" should {
    
    "return a list of fully qualified action names for the given resource" in {
      
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

}