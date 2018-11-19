package com.galacticfog.gestalt.meta.api.patch



import org.specs2.mutable._
import org.specs2.specification._
import play.api.libs.json._


import com.galacticfog.gestalt.patch._

import com.galacticfog.gestalt.meta.test.ResourceScope

class AttributePatchSpec extends Specification with ResourceScope with BeforeAll {

  sequential
  
  override def beforeAll(): Unit = pristineDatabase()
  
  def p(s: String) = JsPointer.toJsPath(s)
  def jstr(s: String) = JsString
  def jsonArray(ss: String*) = Json.toJson(Vector(ss:_*))  
  def jsonArrayInt(ss: Int*) = Json.toJson(Vector(ss:_*))  
  def jstrOpt(s: String) = Option(JsString(s))  
  
  "removeKeysFromMap" should {
    
    val m = Map("a" -> "alpha", "b" -> "bravo", "c" -> "charlie", "d" -> "delta", "e" -> "echo")
    
    "remove the given keys from the Map if they exist" in {
      val m2 = AttributePatch.removeKeysFromMap(m, Seq("a", "c", "e"))
      m2 must_== Map("b" -> "bravo", "d" -> "delta")
    }
    
    "throw an exception if any of the given keys does not exist" in {
      AttributePatch.removeKeysFromMap(m, Seq("a", "c", "invalid_key")) must throwAn[IllegalArgumentException]
    }
    
  }
  
  "validateOp" should {
    
    "always fail when target attribute(editable = false, _)" in {
      Attributes.attrs(Attributes.Id).editable must beFalse
      
      AttributePatch.validateOp(PatchOps.Add, PatchOp.Add(Attributes.Id, JsString("foo")), requireOptional = true) must beFailedTry
      AttributePatch.validateOp(PatchOps.Add, PatchOp.Add(Attributes.Id, JsString("foo")), requireOptional = false) must beFailedTry
      
      AttributePatch.validateOp(PatchOps.Remove, PatchOp.Remove(Attributes.Id), true) must beFailedTry
      AttributePatch.validateOp(PatchOps.Remove, PatchOp.Remove(Attributes.Id), false) must beFailedTry
      
      AttributePatch.validateOp(PatchOps.Replace, PatchOp.Replace(Attributes.Id, JsString("foo")), requireOptional = true) must beFailedTry
      AttributePatch.validateOp(PatchOps.Replace, PatchOp.Replace(Attributes.Id, JsString("foo")), requireOptional = false) must beFailedTry      
    }
    
    "fail for ADD when target attribute(editable = true, optional = false)" in {
      Attributes.attrs(Attributes.Name).editable must beTrue
      Attributes.attrs(Attributes.Name).optional must beFalse
      
      AttributePatch.validateOp(PatchOps.Add, PatchOp.Add(Attributes.Name, JsString("foo")), requireOptional = true) must beFailedTry
    }

    "fail for REMOVE when target attribute(editable = true, optional = false)" in {
      Attributes.attrs(Attributes.Name).editable must beTrue
      Attributes.attrs(Attributes.Name).optional must beFalse
      
      AttributePatch.validateOp(PatchOps.Remove, PatchOp.Remove(Attributes.Name), requireOptional = true) must beFailedTry
    }
    
    "succeed for ADD when target attribute(editable = true, optional = true)" in {
      Attributes.attrs(Attributes.Description).editable must beTrue
      Attributes.attrs(Attributes.Description).optional must beTrue
      
      AttributePatch.validateOp(PatchOps.Add, PatchOp.Add(Attributes.Description, JsString("foo")), requireOptional = true) must beSuccessfulTry
    }

    "succeed for REMOVE when target attribute(editable = true, optional = true)" in {
      Attributes.attrs(Attributes.Description).editable must beTrue
      Attributes.attrs(Attributes.Description).optional must beTrue
      
      AttributePatch.validateOp(PatchOps.Remove, PatchOp.Remove(Attributes.Description), requireOptional = true) must beSuccessfulTry
    }
  }

  "addAttribute" should {
    
//    val res = newInstance(uuid(), "new-resource")
    
    "add or replace an optional parameter" in {
      val res = newInstance(uuid(), "new-resource")
      res.description must beNone
      
      val mod1 = AttributePatch.addAttribute(res, PatchOp.Add("/description", JsString("foo")))
      mod1.description must beSome("foo")
      
      val mod2 = AttributePatch.addAttribute(res, PatchOp.Add("/description", JsString("bar")))
      mod2.description must beSome("bar")
    }    
    
    
    "add or replace an optional array" in {
      val res = newInstance(uuid(), "new-resource")
      res.tags must beNone
      
      val mod1 = AttributePatch.addAttribute(res, PatchOp.Add("/tags", jsonArray("a", "b", "c")))
      mod1.tags must beSome(List("a", "b", "c"))
      
      val mod2 = AttributePatch.addAttribute(mod1, PatchOp.Add("/tags", jsonArray("l", "m", "n")))
      mod2.tags must beSome(List("l", "m", "n"))
    }
    
    
    "add an element to an existing array at the given index" in {
      val res = newInstance(uuid(), "new-resource")
      val mod1 = AttributePatch.addAttribute(res, PatchOp.Add("/tags", jsonArray("a", "b", "c")))
      mod1.tags must beSome(List("a", "b", "c"))
      
      val mod2 = AttributePatch.addAttribute(mod1, PatchOp.Add("/tags/0", JsString("foo")))
      mod2.tags must beSome(List("foo", "a", "b", "c"))
      
      val mod3 = AttributePatch.addAttribute(mod1, PatchOp.Add("/tags/1", JsString("foo")))
      mod3.tags must beSome(List("a", "foo", "b", "c"))
      
      val mod4 = AttributePatch.addAttribute(mod1, PatchOp.Add("/tags/2", JsString("foo")))
      mod4.tags must beSome(List("a", "b", "foo", "c"))  
    }
    
    "throw an exception if the given index is out of bounds" in {
      val res = newInstance(uuid(), "new-resource")
      val mod1 = AttributePatch.addAttribute(res, PatchOp.Add("/tags", jsonArray("a", "b", "c")))
      mod1.tags must beSome(List("a", "b", "c"))
      
      AttributePatch.addAttribute(mod1, PatchOp.Add("/tags/-1", JsString("foo"))) must throwAn[IllegalArgumentException]
      AttributePatch.addAttribute(mod1, PatchOp.Add("/tags/3", JsString("foo"))) must throwAn[IllegalArgumentException]
    }
    
    "append an element to an existing array" in {
      val res = newInstance(uuid(), "new-resource")
      val mod1 = AttributePatch.addAttribute(res, PatchOp.Add("/tags", jsonArray("a", "b", "c")))
      mod1.tags must beSome(List("a", "b", "c"))      
      
      val mod2 = AttributePatch.addAttribute(mod1, PatchOp.Add("/tags/-", JsString("foo")))
      mod2.tags must beSome(List("a", "b", "c", "foo"))      
    }
    
    "throw an exception if the array does not exist (is null)" in {
      val res = newInstance(uuid(), "new-resource")
      res.tags must beNone
      AttributePatch.addAttribute(res, PatchOp.Add("/tags/-", JsString("foo"))) must throwAn[IllegalArgumentException]
    }
    
    "create a new Map and add the given value if the map does not exist" in {
      val res = newInstance(uuid(), "new-resource")
      val mod1 = AttributePatch.addAttribute(res, PatchOp.Add("/variables", Json.obj("key1" -> "value1")))
      mod1.variables must beSome(Map("key1" -> "value1"))  
    }
    
    "add the given object to an existing Map" in {
      val res = newInstance(uuid(), "new-resource")
      val mod1 = AttributePatch.addAttribute(res, PatchOp.Add("/variables", Json.obj("key1" -> "value1")))
      mod1.variables must beSome(Map("key1" -> "value1"))
      
      val mod2 = AttributePatch.addAttribute(mod1, PatchOp.Add("/variables", Json.obj("key2" -> "value2")))
      mod2.variables must beSome(Map("key1" -> "value1", "key2" -> "value2"))      
    }
    
    "replace the object in an existing Map if the object key already exists" in {
      val res = newInstance(uuid(), "new-resource")
      val mod1 = AttributePatch.addAttribute(res, PatchOp.Add("/variables", Json.obj("key1" -> "value1")))
      mod1.variables must beSome(Map("key1" -> "value1"))
      
      val mod2 = AttributePatch.addAttribute(mod1, PatchOp.Add("/variables", Json.obj("key1" -> "update")))
      mod2.variables must beSome(Map("key1" -> "update"))
    }
    
  }
  
  "removeAttribute" should {
    
    "remove an optional attribute" in {
      val res = newInstance(uuid(), "new-resource")
      Attributes.attrs(Attributes.Tags).optional must beTrue
      
      val mod1 = AttributePatch.removeAttribute(res, PatchOp.Remove("/tags"))
      mod1.tags must beEmpty
    }
    
    "remove the array element at the given index if the array exists" in {
      val res = newInstance(uuid(), "new-resource")
      val mod1 = AttributePatch.addAttribute(res, PatchOp.Add("/tags", jsonArray("a", "b", "c")))
      mod1.tags must beSome(List("a", "b", "c"))
      
      val mod2 = AttributePatch.removeAttribute(mod1, PatchOp.Remove("/tags/0"))
      mod2.tags must beSome(List("b", "c"))
      
      val mod3 = AttributePatch.removeAttribute(mod1, PatchOp.Remove("/tags/1"))
      mod3.tags must beSome(List("a", "c"))
      
      val mod4 = AttributePatch.removeAttribute(mod1, PatchOp.Remove("/tags/2"))
      mod4.tags must beSome(List("a", "b"))      
    }
    
    "throw an exception if the array index is out of bounds" in {
      val res = newInstance(uuid(), "new-resource")
      val mod1 = AttributePatch.addAttribute(res, PatchOp.Add("/tags", jsonArray("a", "b", "c")))
      mod1.tags must beSome(List("a", "b", "c"))
      
      AttributePatch.removeAttribute(mod1, PatchOp.Remove("/tags/-1")) must throwAn[IllegalArgumentException]
      AttributePatch.removeAttribute(mod1, PatchOp.Remove("/tags/3")) must throwAn[IllegalArgumentException]
    }

    "throw an exception if the 'append' operator (minus sign) terminates the path" in {
      val res = newInstance(uuid(), "new-resource")
      val mod1 = AttributePatch.addAttribute(res, PatchOp.Add("/tags", jsonArray("a", "b", "c")))
      mod1.tags must beSome(List("a", "b", "c"))
      
      AttributePatch.removeAttribute(mod1, PatchOp.Remove("/tags/-")) must throwAn[IllegalArgumentException]
    }
    
    "remove a Map attribute (and set it to None) if it exists" in {
      val res = newInstance(uuid(), "new-resource")
      val mod1 = AttributePatch.addAttribute(res, PatchOp.Add("/variables", Json.obj("key1" -> "value1")))
      mod1.variables must beSome(Map("key1" -> "value1"))
      
      val mod2 = AttributePatch.removeAttribute(mod1, PatchOp.Remove("/variables/key1"))
      mod2.variables must beEmpty
    }
    
    "remove the given key from the Map if it exists" in {
      val res = newInstance(uuid(), "new-resource")
      val mod1 = AttributePatch.addAttribute(res, PatchOp.Add("/variables", Json.obj("key1" -> "value1", "key2" -> "value2")))
      mod1.variables must beSome(Map("key1" -> "value1", "key2" -> "value2"))
      
      val mod2 = AttributePatch.removeAttribute(mod1, PatchOp.Remove("/variables/key1"))
      mod2.variables must beSome(Map("key2" -> "value2"))
      
    }
    
    
    "throw an exception if the Map doesn't exist" in {
      val res = newInstance(uuid(), "new-resource")
      AttributePatch.removeAttribute(res, PatchOp.Remove("/variables")) must throwAn[IllegalArgumentException]
    }
    
    
    "throw an exception if the given KEY in the Map does not exist" in {
      val res = newInstance(uuid(), "new-resource")
      val mod1 = AttributePatch.addAttribute(res, PatchOp.Add("/variables", Json.obj("key1" -> "value1", "key2" -> "value2")))
      mod1.variables must beSome(Map("key1" -> "value1", "key2" -> "value2"))
      
      AttributePatch.removeAttribute(res, PatchOp.Remove("/variables/INVALID_KEY")) must throwAn[IllegalArgumentException]
    }
    
  }
  
  
  "replaceAttribute" should {
    
    "replace an attribute" in {
      val res = newInstance(uuid(), "new-resource")
      res.name must_== "new-resource"
      
      val mod1 = AttributePatch.replaceAttribute(res, PatchOp.Replace("/name", JsString("updated-resource")))
      mod1.name must_== "updated-resource"
    }
    
    "replace the element at the given index in an array attribute" in {
      val res = newInstance(uuid(), "new-resource")
      val mod1 = AttributePatch.addAttribute(res, PatchOp.Add("/tags", jsonArray("a", "b", "c")))
      mod1.tags must beSome(List("a", "b", "c"))
      
      val mod2 = AttributePatch.replaceAttribute(mod1, PatchOp.Replace("/tags/0", JsString("foo")))
      mod2.tags must beSome(List("foo", "b", "c"))
      
      val mod3 = AttributePatch.replaceAttribute(mod1, PatchOp.Replace("/tags/1", JsString("foo")))
      mod3.tags must beSome(List("a", "foo", "c"))
      
      val mod4 = AttributePatch.replaceAttribute(mod1, PatchOp.Replace("/tags/2", JsString("foo")))
      mod4.tags must beSome(List("a", "b", "foo"))      
    }
    
    "replace the value at the given key in a Map attribute" in {
      val res = newInstance(uuid(), "new-resource")
      val mod1 = AttributePatch.addAttribute(res, PatchOp.Add("/variables", Json.obj("key1" -> "value1", "key2" -> "value2")))
      mod1.variables must beSome(Map("key1" -> "value1", "key2" -> "value2"))
      
      val mod2 = AttributePatch.replaceAttribute(mod1, PatchOp.Replace("/variables/key1", JsString("patched")))
      mod2.variables.get.get("key1") must beSome("patched")
    }
    
  }
  
  

  
}

