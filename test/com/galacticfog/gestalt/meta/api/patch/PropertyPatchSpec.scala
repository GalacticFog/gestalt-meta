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

import com.galacticfog.gestalt.meta.test.ResourceScope


class PropertyPatchSpec extends Specification with ResourceScope with BeforeAll {
  
  override def beforeAll(): Unit = pristineDatabase()
  
    case class Widget(id: Int, name: String)  
    implicit lazy val widgetFormat = Json.format[Widget]
    
  "unescape" should {
    
    "convert an (optionally) escaped String to the appropriate JsValue type" in {
      
      val esc1 = "[{\"name\":\"us-west-1\",\"enabled\":\"true\"}]"
      val esc2 = "false"
      val esc3 = "5.123"
      val esc4 = "This is a String"
      val esc5 = "This \"is a\" 'String'"
      val esc6 = "{\"typeId\":\"7\",\"id\":\"1\",\"name\":\"dev\",\"href\":\"http://example.com\"}"
      
      PropertyPatch.unescape(esc1) must haveClass[JsArray]
      PropertyPatch.unescape(esc2) must haveClass[JsBoolean]
      PropertyPatch.unescape(esc3) must haveClass[JsNumber]
      PropertyPatch.unescape(esc4) must haveClass[JsString]
      PropertyPatch.unescape(esc5) must haveClass[JsString]
      PropertyPatch.unescape(esc6) must haveClass[JsObject]
    }
    
  }
  
  "jsonProps" should {
    
    "convert a Map[String,String] with escaped values to a JSON object with unescaped values" in {
      
      val m1 = Map(
          "a" -> "[{\"name\":\"us-west-1\",\"enabled\":\"true\"}]",
          "b" -> "42",
          "c" -> "{\"typeId\": 7,\"id\":1,\"name\":\"dev\",\"href\":\"http://example.com\"}"
      )
      
      val m2 = PropertyPatch.jsonProps(m1)
      
      m2 must haveClass[JsObject]
      
      (m2 \ "a") must haveClass[JsArray]
      (m2 \ "b" ) === JsNumber(42)
      (m2 \ "c" \ "typeId") === JsNumber(7)
    }
  }
  
  
  "toStringMap" should {
    
    "convert a Map[String,JsValue] to a Map[String,String]" in {
      val m1 = Map(
          "a" -> JsString("foo"),
          "b" -> JsNumber(42),
          "c" -> JsBoolean(false),
          "d" -> Json.obj("one" -> 1, "two" -> 2))
      
      val m2 = PropertyPatch.toStringMap(m1)
      
      m2("a") must haveClass[String]
      m2("b") must haveClass[String]
      m2("c") must haveClass[String]
      m2("d") must haveClass[String]
    }
  }
  
  
  "fromJson[T]" should {

    "convert an appropriately structure JSON object to an instance of the given type" in {
      
      val j1 = Json.toJson(Widget(1, "foo"))
      val j2 = Json.toJson(Map("a" -> "alpha", "b" -> "beta"))
      
      val o1 = PropertyPatch.fromJson[Widget](j1)
      val o2 = PropertyPatch.fromJson[Map[String,String]](j2)
      
      o1 must beSuccessfulTry
      o1.get must haveClass[Widget]
      
      o2 must beSuccessfulTry
      o2.get.isInstanceOf[Map[_,_]]

    }
  }
  
  
  "toJson" should {
    
    "convert a GestaltResourceInstnce to a JsObject" in new ResourceScope {
      val i = newInstance(uuid(), "dummy-resource", properties = Some(Map("a" -> "foo", "b" -> "bar")))
      i must haveClass[GestaltResourceInstance]
      
      val j = PropertyPatch.toJson(i)
      
      (j \ "name") === JsString("dummy-resource")
      (j \ "properties" \ "a") === JsString("foo")
    }
  }
  
  
  "applyOps" should {
    
    "apply the given PATCH ops to the given resource" in new ResourceScope {

      val props = Map(
        "host" -> "example.com",
        "port" -> "8080",
        "protocol" -> "https"
      )

      val newHost = "foo.com"
      val newAuth = "BASIC"
      val res = newInstance(uuid(), "dummy", properties = Option(props))

      res.properties.get("host") !== newHost
      res.properties.get("protocol") === "https"
      res.properties.get.get("auth") must beNone
      
      val ops = Seq(
        PatchOp.Replace("/properties/host", JsString(newHost)),
        PatchOp.Remove("/properties/protocol"),
        PatchOp.Add("/properties/auth", JsString(newAuth)))
      
      val r2 = PropertyPatch.applyOps(res, ops)
      
      import PropertyPatch.{unescape => uesc}

      r2 must beSuccessfulTry
      r2.get.properties.get.get("protocol") must beNone          // Removed
      uesc(r2.get.properties.get("auth")).as[String] === newAuth // Added
      uesc(r2.get.properties.get("host")).as[String] === newHost // Replaced
      
    }
    
  }
  
}