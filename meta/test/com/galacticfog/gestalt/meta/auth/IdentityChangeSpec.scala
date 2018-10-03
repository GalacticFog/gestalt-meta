package com.galacticfog.gestalt.meta.auth


import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.BeforeAll
import org.specs2.specification.Scope

import com.galacticfog.gestalt.data.TypeFactory
import com.galacticfog.gestalt.data.bootstrap.ActionInfo
import com.galacticfog.gestalt.data.bootstrap.LineageInfo
import com.galacticfog.gestalt.data.bootstrap.SystemType
import com.galacticfog.gestalt.data.string2uuid
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope

import controllers.util.GestaltSecurityMocking

class IdentityChangeSpec extends Specification with ResourceScope with BeforeAll {
  
  sequential
  
  override def beforeAll(): Unit = pristineDatabase  
  
  "IdentityChange.getIdentities" should {
    
    "return the Entitlement.properties.identities list" >> {
      
      val org = newOrg()
      
      org must beSuccessfulTry
      
      val users = (for {
        i <- 1 to 5 
        u = createNewUser(org.get.id)
      } yield u).toSeq
      
      val ids = users map {_.id}
      val ent = createEntitlement("org.view", org.get.id, Some(ids))
      val parsed = IdentityChange.getIdentities(ent)
      
      parsed === ids
      
    }
    
    "return an empty list if Entitlement.properties.identities is null/empty" >> {
      val org = newOrg()
      
      org must beSuccessfulTry

      val ent = createEntitlement("org.view", org.get.id)
      IdentityChange.getIdentities(ent) must beEmpty
    }
  }
  
  "IdentityChange.deleted" should {
    
    
    "return a list of identities that have been deleted in an update" >> {
      val org = newOrg()
      org must beSuccessfulTry
      
      val id1 = uuid()
      val id2 = uuid()
      val id3 = uuid()
      val id4 = uuid()
      val id5 = uuid()
      
      val oldids = Seq(id1, id2, id3, id4, id5)
      val newids = Seq(oldids(0), oldids(2), oldids(4))
      
      val creator = org.get.owner.id
      val old = Entitlement.toGestalt(creator, newEntitlement("environment.view", org.get.id, Some(oldids)))
      val update = Entitlement.toGestalt(creator, newEntitlement("environment.view", org.get.id, Some(newids)))
      
      IdentityChange(old, update).deleted === Seq(id2, id4)
      
    }
    
    "return an empty list if no identities have been deleted" >> {
      val org = newOrg()
      org must beSuccessfulTry
      
      val id1 = uuid()
      val id2 = uuid()
      val id3 = uuid()
      val id4 = uuid()
      val id5 = uuid()
      
      val oldids = Seq(id1, id2, id3, id4, id5)
      val newids = oldids
      
      val creator = org.get.owner.id
      val old = Entitlement.toGestalt(creator, newEntitlement("environment.view", org.get.id, Some(oldids)))
      val update = Entitlement.toGestalt(creator, newEntitlement("environment.view", org.get.id, Some(newids)))
      
      IdentityChange(old, update).deleted === Seq.empty
    }
  }
  
  "IdentityChange.added" should {
    
    "return a list of identities that have been added in an update" >> {
      val org = newOrg()
      org must beSuccessfulTry
      
      val id1 = uuid()
      val id2 = uuid()
      val id3 = uuid()
      val id4 = uuid()
      val id5 = uuid()
      
      val oldids = Seq(id1, id2, id3)
      val newids = oldids ++ Seq(id4, id5)
      
      val creator = org.get.owner.id
      val old = Entitlement.toGestalt(creator, newEntitlement("environment.view", org.get.id, Some(oldids)))
      val update = Entitlement.toGestalt(creator, newEntitlement("environment.view", org.get.id, Some(newids)))
      
      IdentityChange(old, update).added === Seq(id4, id5)
    }
    
    "return an empty list if no identities have been added" >> {
      val org = newOrg()
      org must beSuccessfulTry
      
      val id1 = uuid()
      val id2 = uuid()
      val id3 = uuid()
      val id4 = uuid()
      val id5 = uuid()
      
      val oldids = Seq(id1, id2, id3, id4, id5)
      val newids = oldids
      
      val creator = org.get.owner.id
      val old = Entitlement.toGestalt(creator, newEntitlement("environment.view", org.get.id, Some(oldids)))
      val update = Entitlement.toGestalt(creator, newEntitlement("environment.view", org.get.id, Some(newids)))
      
      IdentityChange(old, update).deleted === Seq.empty
    }
    
  }
}