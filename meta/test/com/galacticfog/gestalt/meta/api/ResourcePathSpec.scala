package com.galacticfog.gestalt.meta.api


import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._

import org.specs2.mutable._

import java.util.UUID

import com.galacticfog.gestalt.meta.test._



class ResourcePathSpec extends Specification with MetaRepositoryOps {
  
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
  
  "parentId" should {
    
    "contain the parent ID if the path refers to a second level resource or list" in {
      val parentId = uuid.toString
      new ResourcePath(s"/testorg/workspaces/$parentId/environments").parentId must beSome(UUID.fromString(parentId)) 
      new ResourcePath(s"/testorg/workspaces/$parentId/environments/${uuid}").parentId must beSome(UUID.fromString(parentId))
    }
    
    "be empty if the path refers to a first-level resource or list" in {
      new ResourcePath(s"/testorg").parentId must beNone
      new ResourcePath(s"/testorg/workspaces").parentId must beNone
      new ResourcePath(s"/testorg/workspaces/${uuid}").parentId must beNone
    }
  }
  
  "parentTypeId" should {
    
    "contain the parent Type ID if the path refers to a second level resource or list" in {
      println("PARENT-TYPE-ID: " + new ResourcePath(s"/testorg/workspaces/${uuid}/environments").parentTypeId)
      new ResourcePath(s"/testorg/workspaces/${uuid}/environments").parentTypeId must beSome(ResourceIds.Workspace) 
      new ResourcePath(s"/testorg/environments/${uuid}/lambdas/${uuid}").parentTypeId must beSome(ResourceIds.Environment)
    }
    
    "be empty if the path refers to a first-level resource or list" in {
      new ResourcePath(s"/testorg").parentId must beNone
      new ResourcePath(s"/testorg/workspaces").parentTypeId must beNone
      new ResourcePath(s"/testorg/workspaces/${uuid}").parentTypeId must beNone
    }
    
  }
  
  "isOrg" should {
    
    "be TRUE if the path contains only FQON" >> {
      new ResourcePath(s"/testorg").isOrg must beTrue
    }
  
    "be FALSE if the path points to a first-level resource or list" >> {
      new ResourcePath(s"/testorg/environments").isOrg must beFalse
      new ResourcePath(s"/testorg/environments/${uuid}").isOrg must beFalse
    }
    
    "be FALSE if the path points to a second-level resource of list" >> {
      new ResourcePath(s"/testorg/workspaces/${uuid}/environments").isOrg must beFalse
      new ResourcePath(s"/testorg/workspaces/${uuid}/environments/${uuid}").isOrg must beFalse
    }
    
  }
  
}