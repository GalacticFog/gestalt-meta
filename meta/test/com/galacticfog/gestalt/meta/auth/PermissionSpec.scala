
/*
 * 
 * trait Entitlements is not yet used. 
 * 
 */

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
//import com.galacticfog.gestalt.meta.test._
//
//import play.api.test._
//import play.api.test.Helpers._
//
//import controllers.util._
//
//class PermissionSpec extends PlaySpecification with ResourceScope with GestaltSecurityMocking {
//
//  sequential
//  
//  //case object E extends Entitlements
//  
//  val E = injectController[Entitlements]()
//  
//  abstract class TestApplication extends WithApplication(containerApp()) {
//    val E = app.injector.instanceOf[ResourceController]  
//  }
//  
//  "resourceEntitlements" should {
//
//    "return a sequence of Entitlements - one for each action in the input" in new WithApplication {
//      
//      val ents = E.resourceEntitlements(uuid(), uuid(), uuid(), 
//          ResourceIds.Workspace, Seq("create", "view", "update", "delete"))
//      
//      ents.size === 4
//      ents.exists { _.properties.action == "workspace.create"} === true
//      ents.exists { _.properties.action == "workspace.view"} === true
//      ents.exists { _.properties.action == "workspace.update"} === true
//      ents.exists { _.properties.action == "workspace.delete"} === true
//    }
//    
//  }
//  
//  "generateEntitlements" should {
//    
//    "generate a list of entitlements - one for each action given for each resource-type" in new WithApplication {
//      
//      val oactions = Seq("create")
//      val wactions = Seq("create", "view")
//      val eactions = Seq("create", "view", "update")
//      val cactions = Seq("create", "view", "update", "delete", "scale", "migrate")
//      
//      val grants = Map(
//          ResourceIds.Org -> oactions,
//          ResourceIds.Workspace -> wactions,
//          ResourceIds.Environment -> eactions,
//          ResourceIds.Container -> cactions)
//
//      val totalEntitlementCount = Seq(oactions, wactions, eactions, cactions).flatten.size
//      val ents = E.generateEntitlements(uuid(), uuid(), uuid(), grants)
//      
//      ents.size === totalEntitlementCount
//      
//      ents.exists { _.properties.action == Actions.Org.Create} === true
//      ents.exists { _.properties.action == Actions.Org.View} === false
//      
//      ents.exists { _.properties.action == Actions.Workspace.Create} === true
//      ents.exists { _.properties.action == Actions.Workspace.View} === true
//      ents.exists { _.properties.action == Actions.Workspace.Update} === false
//      
//      ents.exists { _.properties.action == Actions.Environment.Create} === true
//      ents.exists { _.properties.action == Actions.Environment.View} === true
//      ents.exists { _.properties.action == Actions.Environment.Update} === true
//      ents.exists { _.properties.action == Actions.Environment.Delete} === false
//      
//      ents.exists { _.properties.action == Actions.Container.Create} === true
//      ents.exists { _.properties.action == Actions.Container.View} === true
//      ents.exists { _.properties.action == Actions.Container.Update} === true
//      ents.exists { _.properties.action == Actions.Container.Delete} === true
//      ents.exists { _.properties.action == Actions.Container.Migrate} === true
//      ents.exists { _.properties.action == Actions.Container.Scale} === true
//    }
//  }  
//
//}