//package com.galacticfog.gestalt.meta.api
//
//
//import com.galacticfog.gestalt.meta.api.sdk._
//import com.galacticfog.gestalt.data._
//import com.galacticfog.gestalt.data.models._
//
//import org.specs2.mutable._
//import org.specs2.specification._
//import org.specs2.specification.Step
//import play.api.libs.json._
//
//import java.util.UUID
//
//
//class ResourceSpec extends Specification {
//
//  controllers.util.db.EnvConfig.getConnection()
//  
//  "typeOrElse" should {
//    
//    "return the ResourceType ID for the given resource API name" in {
//
//      // TODO: This list of valid types is not comprehensive
//      Resource.typeOrElse("orgs") must haveClass[UUID]
//      Resource.typeOrElse("workspaces") must haveClass[UUID]
//      Resource.typeOrElse("environments") must haveClass[UUID]
//      Resource.typeOrElse("containers") must haveClass[UUID]
//    }
//    
//    "throw an exception when the type name doesn't exist" in {
//      Resource.typeOrElse("invalid_type") must throwAn[IllegalArgumentException]
//    }
//  }
//  
//  "mapPathData" should {
//    
//    "succeed when the path points to an FQON" in {
//      val m = Resource.mapPathData("/alpha")
//      
//      m must beAnInstanceOf[Map[_,_]]
//      m.size === 1
//    }
//    
//    "succeed when the path points to a first-level resource" in {
//      val m = Resource.mapPathData("/alpha/bravo/charlie")
//      
//      m must beAnInstanceOf[Map[_,_]]
//      m.size === 3
//      m(Resource.Fqon) === "alpha"
//      m(Resource.TargetType) === "bravo"
//      m(Resource.TargetId) === "charlie"
//    }
//    
//    "succeed when the path points to a second-level resource" in {
//      val m = Resource.mapPathData("/fqon/workspaces/123/environments/456")
//      
//      m must beAnInstanceOf[Map[_,_]]
//      m.size === 5
//      m(Resource.Fqon) === "fqon"
//      m(Resource.ParentType) === "workspaces"
//      m(Resource.ParentId) === "123"
//      m(Resource.TargetType) === "environments"
//      m(Resource.TargetId) === "456"
//    }
//    
//    "fail with Exception if the path is empty" in {
//      Resource.mapPathData("") must throwAn[IllegalArgumentException]
//    }
//    
//    "fail with Exception if any path component is empty" in {
//      Resource.mapPathData("/alpha//beta") must throwAn[IllegalArgumentException]
//    }
//    
//    "fail with Exception if the path length is even" in {
//      Resource.mapPathData("/alpha/beta") must throwAn[IllegalArgumentException]
//      Resource.mapPathData("/alpha/beta/charlie/delta") must throwAn[IllegalArgumentException]
//    }
//    
//    "fail with Exception if the path points to a resource deeper than the second-level" in {
//      Resource.mapPathData("/fqon/workspaces/123/environments/456/lambdas/789") must throwAn[IllegalArgumentException]
//    }
//  }
//  
//  "findFqon" should {
//    
//    "find an Org when given a valid FQON" in {
//      val org = Resource.findFqon(Map(Resource.Fqon -> "galacticfog"))
//      org must beSome[GestaltResourceInstance]
//      org.get.properties.get("fqon") === "galacticfog"
//    }
//    
//    "return None when given an invalid FQON" in {
//      Resource.findFqon(Map(Resource.Fqon -> "INVALID_FQON")) must beNone
//    }
//    
//  }
//  
//}