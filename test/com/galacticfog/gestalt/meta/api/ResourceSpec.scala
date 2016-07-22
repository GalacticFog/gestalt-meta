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

class ResourceSpec extends Specification {

  controllers.util.db.EnvConfig.getConnection()
  
  "typeOrElse" should {
    
    "return the ResourceType ID for the given resource API name" in {

      // TODO: This list of valid types is not comprehensive
      Resource.typeOrElse("orgs") must haveClass[UUID]
      Resource.typeOrElse("workspaces") must haveClass[UUID]
      Resource.typeOrElse("environments") must haveClass[UUID]
      Resource.typeOrElse("containers") must haveClass[UUID]
    }
    
    "throw an exception when the type name doesn't exist" in {
      Resource.typeOrElse("invalid_type") must throwA[BadRequestException]
    }
    
  }
  
  "mapPathData" should {
    
    "succeed when the path points to an FQON" in {
      val m = Resource.mapPathData("/alpha")
      
      m must beAnInstanceOf[Map[_,_]]
      m.size === 1
    }
    
    "succeed when the path points to a first-level resource" in {
      val m = Resource.mapPathData("/alpha/bravo/charlie")
      
      m must beAnInstanceOf[Map[_,_]]
      m.size === 3
      m(Resource.Fqon) === "alpha"
      m(Resource.TargetType) === "bravo"
      m(Resource.TargetId) === "charlie"
    }
    
    "succeed when the path points to a second-level resource" in {
      val m = Resource.mapPathData("/fqon/workspaces/123/environments/456")
      
      m must beAnInstanceOf[Map[_,_]]
      m.size === 5
      m(Resource.Fqon) === "fqon"
      m(Resource.ParentType) === "workspaces"
      m(Resource.ParentId) === "123"
      m(Resource.TargetType) === "environments"
      m(Resource.TargetId) === "456"
    }
    
    "fail with Exception if the path is empty" in {
      Resource.mapPathData("") must throwAn[IllegalArgumentException]
    }
    
    "fail with Exception if any path component is empty" in {
      Resource.mapPathData("/alpha//beta") must throwAn[IllegalArgumentException]
    }
    
    "fail with Exception if the path length is even" in {
      Resource.mapPathData("/alpha/beta") must throwAn[IllegalArgumentException]
      Resource.mapPathData("/alpha/beta/charlie/delta") must throwAn[IllegalArgumentException]
    }
    
    "fail with Exception if the path points to a resource deeper than the second-level" in {
      Resource.mapPathData("/fqon/workspaces/123/environments/456/lambdas/789") must throwAn[IllegalArgumentException]
    }
  }
  
  "findFqon" should {
    
    "find an Org when given a valid FQON" in {
      val org = Resource.findFqon(Map(Resource.Fqon -> "galacticfog"))
      org must beSome[GestaltResourceInstance]
      org.get.properties.get("fqon") === "galacticfog"
    }
    
    "return None when given an invalid FQON" in {
      Resource.findFqon(Map(Resource.Fqon -> "INVALID_FQON")) must beNone
    }
    
  }
  
  "isSubTypeOf" should {
    
    "return TRUE if targetType is a sub-type of baseType" in {
      Resource.isSubTypeOf(ResourceIds.Provider, ResourceIds.MarathonProvider) must beTrue
      Resource.isSubTypeOf(ResourceIds.Provider, ResourceIds.ApiGatewayProvider) must beTrue
      
      Resource.isSubTypeOf(ResourceIds.Rule, ResourceIds.RuleEvent) must beTrue
      Resource.isSubTypeOf(ResourceIds.Rule, ResourceIds.RuleLimit) must beTrue
    }
    
    
    "return FALSE if the targetType is NOT a sub-type of baseType" in {

      Resource.isSubTypeOf(ResourceIds.MarathonProvider, ResourceIds.Provider) must beFalse
      Resource.isSubTypeOf(ResourceIds.ApiGatewayProvider, ResourceIds.Provider) must beFalse
      Resource.isSubTypeOf(ResourceIds.RuleLimit, ResourceIds.Rule) must beFalse
      
      // random false
      Resource.isSubTypeOf(ResourceIds.Provider, ResourceIds.Workspace) must beFalse
      Resource.isSubTypeOf(ResourceIds.Rule, ResourceIds.Policy) must beFalse 
      Resource.isSubTypeOf(ResourceIds.Org, ResourceIds.Environment) must beFalse
      Resource.isSubTypeOf(ResourceIds.Provider, ResourceIds.Workspace) must beFalse
    }
  }
  
  "fromPath" should {
    
    "find a valid Org by FQON" in {
      val org = Resource.fromPath("/galacticfog")
      org must beSome
      org.get.name === "galacticfog"
    }
    
    "return None when the Org does not exist" in {
      Resource.fromPath("invalid_org") must beNone
    }
    
    "find a valid top-level resource (i.e. /{fqon}/environments/{id})" in new ResourceScope {
      val org = Resource.fromPath("galacticfog")
      org must beSome      
      
      val (wid,eid) = createWorkspaceEnvironment(org.get.id)
      
      val wrk = findById(ResourceIds.Workspace, wid) 
      wrk must beSome
      
      findById(ResourceIds.Environment, eid) must beSome
      
      val fqon = org.get.properties.get("fqon")
      
      val path1 = "/%s/workspaces/%s".format(fqon, wid)
      
      val path2 = "/%s/environments/%s".format(fqon, eid)
      
      // Find the Workspace by path
      val test1 = Resource.fromPath(path1)
      test1 must beSome
      test1.get.id === wid
      
      // Find the Environment by path
      val test2 = Resource.fromPath(path2)
      test2 must beSome
      test2.get.id === eid
    }
    
    "find a valid second-level resource (i.e. /{fqon}/workspaces/{id}/environments/{id})" in new ResourceScope {
      val org = Resource.fromPath("galacticfog")
      org must beSome      
      
      val (wid,eid) = createWorkspaceEnvironment(org.get.id)
      
      val wrk = findById(ResourceIds.Workspace, wid) 
      wrk must beSome
      
      findById(ResourceIds.Environment, eid) must beSome
      
      val fqon = org.get.properties.get("fqon")
      
      // Find the environment
      val path1 = "/%s/workspaces/%s/environments/%s".format(fqon, wid, eid)
      val test1 = Resource.fromPath(path1)
      
      test1 must beSome
      test1.get.id === eid
    }
    
    "throw an exception when an invalid resouce type name is given" in {
      Resource.fromPath(s"/galacticfog/invalid_resource/${uuid().toString}") must throwA[BadRequestException]
      Resource.fromPath(s"/galacticfog/workspaces/${uuid.toString}/invalid_resource/${uuid.toString}") must throwA[BadRequestException]
    }
    
    "throw an exception when the path contains an invalid UUID" in {
      Resource.fromPath("/galacticfog/invalid_resource/foo") must throwAn[IllegalArgumentException]
      Resource.fromPath(s"/galacticfog/workspaces/${uuid.toString}/environments/bar") must throwAn[IllegalArgumentException]
    }
    
    "CONDITION: typeName == 'providers" should {
      
      "find the resource if it is a valid subtype of Provider" in new ResourceScope {
        val org = Resource.fromPath("galacticfog")
        org must beSome
        
        val (wid,eid) = createWorkspaceEnvironment(org.get.id)
        val g1 = createDummyGateway(eid, org = org.get.id)
        
        findById(ResourceIds.ApiGatewayProvider, g1.get.id) must beSome
        
        Resource.lookupResource("providers", g1.get.id) must beSome
      }
      
      "return None if the resource is NOT a valid subtype of Provider" in new ResourceScope {
        val org = Resource.fromPath("galacticfog")
        org must beSome
        
        val (wid,eid) = createWorkspaceEnvironment(org.get.id)
        
        // UUID does not exist.
        Resource.lookupResource("providers", uuid()) must beNone
        
        // UUID names valid resource of wrong type.
        Resource.lookupResource("providers", eid) must beNone
      }
    }
    
    "CONDITION: typeName == 'rules" should {
      
      "find the resource if it is a valid subtype of Rule" in new ResourceScope {
        val org = Resource.fromPath("galacticfog")
        org must beSome
        
        val (wid,eid) = createWorkspaceEnvironment(org.get.id)
        val (pol,rul) = createPolicyRule(eid, ruleType = ResourceIds.RuleLimit, org = org.get.id)
        
        findById(ResourceIds.Policy, pol) must beSome
        findById(ResourceIds.RuleLimit, rul) must beSome
        
        Resource.lookupResource("rules", rul) must beSome
      }
      
      "return None if the resource is NOT a valid subtype of Rule]" in new ResourceScope {
        val org = Resource.fromPath("galacticfog")
        org must beSome
        
        val (wid,eid) = createWorkspaceEnvironment(org.get.id)
        
        Resource.lookupResource("rules", uuid()) // not exist
        Resource.lookupResource("rules", wid)    // wrong type
      }
    }    
  }
  
}