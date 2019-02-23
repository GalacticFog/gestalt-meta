package com.galacticfog.gestalt.meta.auth

import java.util.UUID
import org.specs2.mutable._
//import org.specs2.specification._
//import org.specs2.specification.BeforeAll
//
//import com.galacticfog.gestalt.data.string2uuid
//import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.data.ResourceFactory
import play.api.libs.json._


class EntitlementPropsSpec extends Specification with MetaRepositoryOps {

  /*
	  case class EntitlementProps(
      action: String, 
      value: Option[String] = None,
      identities: Option[Seq[UUID]] = None,
      parent: Option[JsValue] = None)
   */
  
  "make" should {
    
    "create an EntitlementProps from an Entitlement resource" >> {
      val (wrk, env) = createWorkspaceEnvironment()
      val user1 = createNewUser()
      val user2 = createNewUser()
      val user3 = createNewUser()

      val action = "environment.create"
      val identities = List(user1.id, user2.id, user3.id)
      val ents = setEntitlements2(env, (action -> identities))
      ents must beSuccessfulTry
      
      ents.get.size === 1
      val ent = ResourceFactory.findById(ents.get.head)

      ent must beSome
      val ep = EntitlementProps.make(ent.get)
      
      ep.action === action
      ep.identities must beSome
      ep.identities.get.toList === identities
      ep.parent must beSome
    }
  }
  
  "validate" should {
    
    "return Right[EntitlementProps] for a valid EntitlementProps" >> {
      val (wrk, env) = createWorkspaceEnvironment()
      val user1 = createNewUser()
      val user2 = createNewUser()
      val user3 = createNewUser()
      
      val action = "environment.create"
      val identities = List(user1.id, user2.id, user3.id)
      val ents = setEntitlements2(env, (action -> identities))
      ents must beSuccessfulTry
      
      ents.get.size === 1
      val ent = ResourceFactory.findById(ents.get.head)

      ent must beSome
      val ep = EntitlementProps.make(ent.get)
      
      ep.validate must beRight
    }
    
    "fail with Left[String] if any of the given identities do not exist" >> {
      val (wrk, env) = createWorkspaceEnvironment()
      val user1 = createNewUser()
      val user2 = createNewUser()
      
      val action = "environment.create"
      val identities = List(user1.id, user2.id, uuid())
      val ents = setEntitlements2(env, (action -> identities))
      ents must beSuccessfulTry
      
      ents.get.size === 1
      val ent = ResourceFactory.findById(ents.get.head)

      ent must beSome
      val ep = EntitlementProps.make(ent.get)
      
      ep.validate must beLeft
    }
    
  }
  
  "addIdentities" should {
    
    "add the given identities to the identities list" >> {
      val (wrk, env) = createWorkspaceEnvironment()
      val user1 = createNewUser()
      val user2 = createNewUser()
      val user3 = createNewUser()
      val user4 = createNewUser()
      
      val action = "environment.create"
      val identities = List(user1.id, user2.id, user3.id)
      val ents = setEntitlements2(env, (action -> identities))
      ents must beSuccessfulTry
      
      ents.get.size === 1
      val ent = ResourceFactory.findById(ents.get.head)

      ent must beSome
      val ep = EntitlementProps.make(ent.get)
      
      val oids = ep.identities.get
      oids.size === 3
      oids.contains(user1.id) === true
      oids.contains(user2.id) === true
      oids.contains(user3.id) === true
      oids.contains(user4.id) === false
      
      val nids = ep.addIdentities(user4.id).identities.get
      nids.size === 4
      nids.contains(user1.id) === true
      nids.contains(user2.id) === true
      nids.contains(user3.id) === true
      nids.contains(user4.id) === true      
    }
    
  }
  
  "removeIdentities" should {
    
    "remove the given identities from the identities list" >> {
      val (wrk, env) = createWorkspaceEnvironment()
      val user1 = createNewUser()
      val user2 = createNewUser()
      val user3 = createNewUser()
      val user4 = createNewUser()
      
      val action = "environment.create"
      val identities = List(user1.id, user2.id, user3.id, user4.id)
      val ents = setEntitlements2(env, (action -> identities))
      ents must beSuccessfulTry
      
      ents.get.size === 1
      val ent = ResourceFactory.findById(ents.get.head)

      ent must beSome
      val ep = EntitlementProps.make(ent.get)
      
      val oids = ep.identities.get
      oids.size === 4
      oids.contains(user1.id) === true
      oids.contains(user2.id) === true
      oids.contains(user3.id) === true
      oids.contains(user4.id) === true
      
      val nids = ep.removeIdentities(user4.id).identities.get
      nids.size === 3
      nids.contains(user1.id) === true
      nids.contains(user2.id) === true
      nids.contains(user3.id) === true
      nids.contains(user4.id) === false          
    }
    
  }
  
  "Entitlement" should {
    
    "addIdentitiesToResource" should {
      
      "add the given identities to the Entitlement resource identities property" >> {
        val (wrk, env) = createWorkspaceEnvironment()
        val user1 = createNewUser()
        val user2 = createNewUser()
        val user3 = createNewUser()
        val user4 = createNewUser()
        
        val action = "environment.create"
        val identities = List(user1.id, user2.id, user3.id)
        val ents = setEntitlements2(env, (action -> identities))
        ents must beSuccessfulTry
        
        ents.get.size === 1
        val ent = ResourceFactory.findById(ents.get.head)
  
        ent must beSome
        val ep = EntitlementProps.make(ent.get)
        
        val oids = ep.identities.get
        oids.size === 3
        oids.contains(user1.id) === true
        oids.contains(user2.id) === true
        oids.contains(user3.id) === true
        oids.contains(user4.id) === false
        
        val nids = {
          val e2 = Entitlement.addIdentitiesToResource(ent.get, Seq(user4.id))
          Json.parse(e2.properties.get("identities")).validate[List[UUID]].get
        }
        
        nids.size === 4
        nids.contains(user1.id) === true
        nids.contains(user2.id) === true
        nids.contains(user3.id) === true
        nids.contains(user4.id) === true      
      }
      
    }
    
  }
  
}
