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


class AuthorizationSpec extends PlaySpecification with ResourceScope {

  sequential
  
  case object Auth extends Authorization
  
  "resourceEntitlements" should {

    "return a sequence of Entitlements - one for each action in the input" in new WithApplication {
      
      val ents = Auth.resourceEntitlements(uuid(), uuid(), uuid(), 
          ResourceIds.Workspace, Auth.ACTIONS_CRUD)
      
      ents.size === 4
      ents.exists { _.properties.action == "workspace.create"} === true
      ents.exists { _.properties.action == "workspace.view"} === true
      ents.exists { _.properties.action == "workspace.update"} === true
      ents.exists { _.properties.action == "workspace.delete"} === true
    }
    
  }
  
  "generateEntitlements" should {
    
    "generate a list of entitlements - one for each action given for each resource-type" in new WithApplication {
      
      val oactions = Seq("create")
      val wactions = Seq("create", "view")
      val eactions = Seq("create", "view", "update")
      val cactions = Seq("create", "view", "update", "delete", "scale", "migrate")
      
      val grants = Map(
          ResourceIds.Org -> oactions,
          ResourceIds.Workspace -> wactions,
          ResourceIds.Environment -> eactions,
          ResourceIds.Container -> cactions)

      val totalEntitlementCount = Seq(oactions, wactions, eactions, cactions).flatten.size
      val ents = Auth.generateEntitlements(uuid(), uuid(), uuid(), grants)
      
      ents.size === totalEntitlementCount
      
      ents.exists { _.properties.action == Actions.Org.Create} === true
      ents.exists { _.properties.action == Actions.Org.View} === false
      
      ents.exists { _.properties.action == Actions.Workspace.Create} === true
      ents.exists { _.properties.action == Actions.Workspace.View} === true
      ents.exists { _.properties.action == Actions.Workspace.Update} === false
      
      ents.exists { _.properties.action == Actions.Environment.Create} === true
      ents.exists { _.properties.action == Actions.Environment.View} === true
      ents.exists { _.properties.action == Actions.Environment.Update} === true
      ents.exists { _.properties.action == Actions.Environment.Delete} === false
      
      ents.exists { _.properties.action == Actions.Container.Create} === true
      ents.exists { _.properties.action == Actions.Container.View} === true
      ents.exists { _.properties.action == Actions.Container.Update} === true
      ents.exists { _.properties.action == Actions.Container.Delete} === true
      ents.exists { _.properties.action == Actions.Container.Migrate} === true
      ents.exists { _.properties.action == Actions.Container.Scale} === true
    }
  }
  
  "buildActionList" should {
    
    "return the list of given entitlement actions from the given ResourceType" in new WithApplication {
      val wt = TypeFactory.findById(ResourceIds.Workspace)
      
      wt must beSome
      
      val acts = Auth.buildActionList(wt.get)
      
      acts must beSome
      acts.get.contains("workspace.create") must beTrue
      acts.get.contains("workspace.view")   must beTrue
      acts.get.contains("workspace.update") must beTrue
      acts.get.contains("workspace.delete") must beTrue
    }
    
    "return None if no actions are present" in new WithApplication {
      val dummytpe = createResourceType(name = uuid())
      
      Auth.buildActionList(dummytpe) must beNone
    }
  }
  
  "getEntitlementActions" should {
    
    "return a list of fully qualified action names for the given resource" in {
      
      val acts = Auth.getEntitlementActions(ResourceIds.Resource)
      
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
      
      val acts = Auth.getEntitlementActions(tid1)
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
      
      val acts2 = Auth.getEntitlementActions(tid3)
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
  }  

}