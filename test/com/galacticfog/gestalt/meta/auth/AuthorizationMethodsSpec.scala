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

class AuthorizationMethodsSpec extends PlaySpecification with ResourceScope with BeforeAll  {

  sequential
  
  override def beforeAll(): Unit = pristineDatabase
  
  case object Auth extends AuthorizationMethods
  
  def cat[A](a: A, b: String) = "%s.%s".format(a,b)
  

  
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