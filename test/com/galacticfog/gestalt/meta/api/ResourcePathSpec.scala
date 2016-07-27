package com.galacticfog.gestalt.meta.api


import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Step
import play.api.libs.json._

import java.util.UUID

import com.galacticfog.gestalt.meta.test.ResourceScope

import com.galacticfog.gestalt.data.ResourceFactory.findById


class ResourcePathSpec extends Specification {
  
  "targetTypeId" should {
    
    "return ResourceIds.Org if the path contains one component" in {
      new ResourcePath("testorg").targetTypeId === ResourceIds.Org
    }

    "throw an exception if the target TYPE is not a valid system resource type" in {
      new ResourcePath(s"testorg/INVALID_TYPE/${uuid()}") must throwA[BadRequestException]
      new ResourcePath(s"testorg/workspaces/${uuid}/INVALID_TYPE/${uuid()}") must throwA[BadRequestException]
    }
    
    "throw an exception if the target ID is a malformed UUID" in {
      new ResourcePath("testorg/lambdas/INVALID_UUID") must throwA[BadRequestException]
      new ResourcePath(s"testorg/environments/${uuid}/containers/INVALID_UUID") must throwA[BadRequestException]
    }

    "throw an exception if the parent TYPE is not a valid system resource type" in {
      new ResourcePath(s"testorg/INVALID_TYPE/${uuid()}/environments/${uuid()}") must throwA[BadRequestException]
    }
    
    "throw an exception if the parent ID is a malformed UUID" in {
      new ResourcePath(s"testorg/workspaces/INVALID_UUID/environments/${uuid()}") must throwA[BadRequestException]
    }    
  }
  
  "isList" should {
    
    "return TRUE if the path refers to a resource list" in {
      new ResourcePath(s"/testorg/workspaces").isList must beTrue
      new ResourcePath(s"/testorg/workspaces/${uuid}/environments").isList must beTrue
    }
    
    "return FALSE if the path refers to a single resource" in {
      new ResourcePath(s"/testorg").isList must beFalse
      new ResourcePath(s"/testorg/workspaces/${uuid}").isList must beFalse
      new ResourcePath(s"/testorg/workspaces/${uuid}/environments/${uuid}").isList must beFalse
    }
  }
  
  "targetId" should {
    
    "contain the UUID of the target if the path refers to a single resource" in {
      val uuid1 = uuid.toString
      val uuid2 = uuid.toString
      new ResourcePath(s"/testorg/lambdas/$uuid1").targetId must beSome(uuid1)
      new ResourcePath(s"/testorg/environments/${uuid()}/containers/$uuid2").targetId must beSome(uuid2)
    }
    
    "be empty if the path refers to a resource list" in {
      new ResourcePath(s"/testorg/workspaces").targetId must beEmpty
      new ResourcePath(s"/testorg/workspaces/${uuid}/environments").targetId must beEmpty
    }
    
    "be empty if the path refers to an Org (FQON is the ID)" in {
      new ResourcePath("/testorg").targetId must beEmpty
    }
  }
  
  
}