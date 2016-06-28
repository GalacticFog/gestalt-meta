package com.galacticfog.gestalt.meta.policy


import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import org.specs2.mutable._
import org.specs2.specification._

import org.joda.time.DateTime

import org.specs2.specification.Scope
import java.util.UUID

import scala.util.{Try,Success,Failure}


class PredicateSpec extends Specification with MetaResourceScope {

  
  val containerProps = Map(
    "cpu" -> "0.2",
    "memory" -> "1024",
    "numInstances" -> "2",
    "image" -> "nginx",
    "user" -> "gestalt-dev",
    "acceptedResourceRoles" -> "???",
    "labels" -> "???",
    "constraints" -> "???")
  
  val fakeContainer = newInstance(ResourceIds.Container, "fake-container")
  
  
//  "decide()" should {
//    
//    "be successful when making a valid comparison" in {
//      
//      val nameEquals = Predicate("name", "==", "fake-container")
//      
//      //val res = newInstance(ResourceIds.Environment, "TEST")
//      
//      val decision = decide(fakeContainer, nameEquals)
//      decision must beRight      
//    }
//    
//    "fail when making an invalid comparison" in {
//       
//      val res = newInstance(ResourceIds.Environment, "TEST")
//      val predicate = Predicate("name", "==", "NOT-THIS")
//      
//      val decision = decide(res, predicate)
//      decision must beLeft
//    }    
//    
//  }
  
}