//package com.galacticfog.gestalt.meta.auth
//
//import com.galacticfog.gestalt.meta.api.sdk._
//import com.galacticfog.gestalt.meta.api.errors._
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
//class ActionsSpec extends Specification {
//  
//  "actionName" should {
//    
//    "return the fully-qualified action name for the given resource-type and action" in {
//      Actions.actionName(ResourceIds.Workspace, "create") === "workspace.create"
//      Actions.actionName(ResourceIds.Environment, "view") === "environment.view"
//      Actions.actionName(ResourceIds.Lambda, "update") === "lambda.update"
//      Actions.actionName(ResourceIds.Container, "delete") === "container.delete"
//    }
//    
//    "throw a RuntimeException when given a resource-type ID that is not mapped" in {
//      Actions.actionName(uuid(), "create") must throwA[RuntimeException]
//    }
//    
//    "throw a BadRequestException when given an action name that does not exist on the resource-type" in {
//      Actions.actionName(ResourceIds.Org, "INVALID_ACTION") must throwA[BadRequestException]
//    }
//  }
//
//  "isValidAction" should {
//    
//    "return TRUE if the action is defined for the resource type" in {
//      Actions.Org.isValidAction("create") must beTrue
//      Actions.Org.isValidAction("view")   must beTrue
//      Actions.Org.isValidAction("update") must beTrue
//      Actions.Org.isValidAction("delete") must beTrue
//    }
//    
//    "return FALSE if the action is NOT defined for the resource type" in {
//      Actions.Org.isValidAction("INVALID_ACTION") must beFalse
//    }
//  }
//  
//  "Actions.Container" should {
//    
//    "have 'migrate' and 'scale' actions" in {
//      Actions.Container.isValidAction("migrate") must beTrue
//      Actions.Container.isValidAction("scale") must beTrue
//    }
//  }
//  
//  "Actions.Lambda" should {
//    "have an 'invoke' action" in {
//      Actions.Lambda.isValidAction("invoke") must beTrue
//    }
//  }
//  
//}
//
