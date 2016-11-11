package com.galacticfog.gestalt.meta.api.patch


import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Step
import play.api.libs.json._

import org.joda.time.DateTime

import com.galacticfog.gestalt.patch._
import controllers.util.JsonUtil
      
import com.galacticfog.gestalt.meta.test.ResourceScope
import play.api.libs.json._

class ResourcePatchSpec extends Specification with ResourceScope with PatchUtils with BeforeAll {

  sequential
  
  override def beforeAll(): Unit = pristineDatabase()
  
  "partitionOps" should {
    
    "partition patch ops into those targeting attributes, and those targeting properties" in {
      val aops = Seq(
          PatchOp.Add("/name", JsString("foo")),
          PatchOp.Remove("/description"))
          
      val pops = Seq(
          PatchOp.Remove("/properties/foo"),
          PatchOp.Remove("/properties/bar"),
          PatchOp.Remove("/properties/baz"),
          PatchOp.Remove("/properties/qux"))
      
      val (p1,a1) = ResourcePatch.partitionOps(aops ++ pops).get
      
      a1 == aops must beTrue
      p1 == pops must beTrue
    }
  }
  
  
  "applyPatch" should {
    
    "return a resource with updated attributes" in {
      
      val org = newOrg(id = dummyRootOrgId)
      org must beSuccessfulTry
      
      val updatedName = "UPDATED"
      val updatedDescription = "DESCRIPTION"
      
      val data = newDummyEnvironment(org = org.get.id)
      val w1 = ResourceFactory.findById(ResourceIds.Workspace, data("workspace"))
      
      val patch1 = PatchDocument(
          PatchOp.Replace("/name", JsString(updatedName)),
          PatchOp.Add("/description", JsString(updatedDescription)))
      
      val w2 = ResourcePatch.applyPatch(w1.get, patch1)
      
      w2 must beSuccessfulTry
      w2.get.name === updatedName
      w2.get.description must beSome
      w2.get.description.get === updatedDescription
    }
    
    
    "return a resource with updated properties" in {
      
      val org = newOrg(id = dummyRootOrgId)
      org must beSuccessfulTry
      
      val data = newDummyEnvironment(org = org.get.id)
      val w1 = ResourceFactory.findById(ResourceIds.Workspace, data("workspace"))      
      
      val public  = true
      val cpus    = 5
      val runtime = "javascript"
      val envSupport = "YES!!!"
      
      val patch1 = PatchDocument(
          PatchOp.Replace("/properties/public", JsBoolean(public)),
          PatchOp.Replace("/properties/cpus", JsNumber(cpus)),
          PatchOp.Replace("/properties/runtime", JsString(runtime)),
          PatchOp.Add("/properties/env/PATCH_SUPPORT", JsString(envSupport)))

      val l1 = ResourceFactory.findById(ResourceIds.Lambda, data("lambda"))
      l1 must beSome
      
      val l2 = ResourcePatch.applyPatch(l1.get, patch1)
      l2 must beSuccessfulTry
      
      // ----------------------------------------------------------------------
      // Properties before
      // ----------------------------------------------------------------------
      val publicProperty  = l1.get.properties.get.get("public")
      val cpusProperty    = l1.get.properties.get.get("cpus")
      val runtimeProperty = l1.get.properties.get.get("runtime")

      publicProperty  must beSome
      cpusProperty    must beSome
      runtimeProperty must beSome      
      l1.get.properties.get.get("env") must beNone
      
      // before props do not match new values
      publicProperty.get  !=== public.toString
      cpusProperty.get    !=== cpus.toString
      runtimeProperty.get !=== runtime
      
      
      // ----------------------------------------------------------------------
      // Properties after
      // ----------------------------------------------------------------------
      val publicProperty2  = l2.get.properties.get.get("public")
      val cpusProperty2    = l2.get.properties.get.get("cpus")
      val runtimeProperty2 = l2.get.properties.get.get("runtime")

      val envAfter = JsonUtil.safeParse[Map[String,String]](
          Json.toJson(PropertyPatch.unescape(l2.get.properties.get("env"))))
          
      publicProperty2  must beSome
      cpusProperty2    must beSome
      runtimeProperty2 must beSome      

      // after props match new values
      publicProperty2.get  === public.toString
      cpusProperty2.get    === cpus.toString
      runtimeProperty2.get === runtime
      
      envAfter.get("PATCH_SUPPORT") must beSome
      envAfter("PATCH_SUPPORT") === envSupport
    }    
    
    
    "return an updated resource that is persistable" in {
      
        val org = newOrg(id = dummyRootOrgId)
        org must beSuccessfulTry
        
        val data = newDummyEnvironment(org = org.get.id)
        val w1 = ResourceFactory.findById(ResourceIds.Workspace, data("workspace"))      
        
        val name        = "PATCHED"
        val description = "described"
        val public      = true
        val cpus        = 5
        val runtime     = "javascript"
        val envSupport  = "YES!!!"
        
        val patch1 = PatchDocument(
            PatchOp.Replace("/name", JsString(name)),
            PatchOp.Add("/description", JsString(description)),
            PatchOp.Replace("/properties/public", JsBoolean(public)),
            PatchOp.Replace("/properties/cpus", JsNumber(cpus)),
            PatchOp.Replace("/properties/runtime", JsString(runtime)),
            PatchOp.Add("/properties/env/PATCH_SUPPORT", JsString(envSupport)))

        val l1 = ResourceFactory.findById(ResourceIds.Lambda, data("lambda"))
        l1 must beSome
        
        
        // Properties before
        // ----------------------------------------------------------------------
        val nameBefore      = l1.get.name
        val descBefore      = l1.get.description
        val publicProperty  = l1.get.properties.get.get("public")
        val cpusProperty    = l1.get.properties.get.get("cpus")
        val runtimeProperty = l1.get.properties.get.get("runtime")
  
        publicProperty  must beSome
        cpusProperty    must beSome
        runtimeProperty must beSome      
        l1.get.properties.get.get("env") must beNone
        
        // before props do not match new values
        nameBefore          !=== name
        descBefore          must beNone
        publicProperty.get  !=== public.toString
        cpusProperty.get    !=== cpus.toString
        runtimeProperty.get !=== runtime
        
        // Apply the patch
        val l2 = ResourcePatch.applyPatch(l1.get, patch1)
        l2 must beSuccessfulTry
        
        val user = createNewUser(org.get.id)
        
        // Save updated representation
        val l3 = ResourceFactory.update(l2.get.asInstanceOf[GestaltResourceInstance], user.id)
        
        // Properties after
        // ----------------------------------------------------------------------   
        val nameAfter        = l3.get.name
        val descAfter        = l3.get.description
        val publicProperty2  = l3.get.properties.get.get("public")
        val cpusProperty2    = l3.get.properties.get.get("cpus")
        val runtimeProperty2 = l3.get.properties.get.get("runtime")
        
        val envAfter = 
          JsonUtil.safeParse[Map[String,String]](
            Json.toJson(PropertyPatch.unescape(l3.get.properties.get("env"))))
        
        descAfter            must beSome
        publicProperty2      must beSome
        cpusProperty2        must beSome
        runtimeProperty2     must beSome      
  
        // after props match new values
        nameAfter            === name
        descAfter.get        === description
        publicProperty2.get  === public.toString
        cpusProperty2.get    === cpus.toString
        runtimeProperty2.get === runtime
        
        envAfter.get("PATCH_SUPPORT") must beSome
        envAfter("PATCH_SUPPORT") === envSupport
    }
    
  }
}