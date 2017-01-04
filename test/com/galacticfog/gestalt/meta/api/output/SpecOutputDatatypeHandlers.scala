package com.galacticfog.gestalt.meta.api.output


import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Step
import play.api.libs.json._

import org.joda.time.DateTime

class SpecOutputDatatypeHandlers extends Specification {

  stopOnFail
  sequential
  
  val output = OutputDatatypeHandlers

  val dummyowner = ResourceOwnerLink(ResourceIds.User, uuid())
  val dummyprop = GestaltTypeProperty(name = "foo", orgId = uuid(), owner = dummyowner,
          appliesTo = uuid(), datatype = uuid(), typeId = uuid(), state = uuid(), 
          requirementType = uuid(), visibilityType = uuid())
  
  "stringList" should {
    
    "convert a comma-delimited string to a JsonArray[JsString]" in {
      val test = """[ "alpha", "beta", "charlie", "delta", "echo" ]"""
      val arr = output.renderStringList(dummyprop, test)
      arr.isInstanceOf[JsArray] === true
      arr(0).isInstanceOf[JsDefined] === true
      arr(0).get must haveClass[JsString]
    }
    
    "ignore square braces and convert a comma-delimited string to a JsonArray[JsString]" in {
      val test = """[ "alpha", "beta", "charlie", "delta", "echo" ]"""
      val arr = output.renderStringList(dummyprop, test)
      arr.isInstanceOf[JsArray] === true
      arr(0).isInstanceOf[JsDefined] === true
      arr(0).get must haveClass[JsString]
    }
    
    "convert a comma-delimited list of UUID strings to a JsArray[JsString]" in {
      val test = s"""[ "${uuid()}","${uuid()}", "${uuid()}" ]"""
      val arr = output.renderStringList(dummyprop, test)
      arr.isInstanceOf[JsArray] === true
      arr(0).isInstanceOf[JsDefined] === true      
      arr(0).get must haveClass[JsString]
    }
  }
  
  "intList" should {
    
    "convert comma-delimited string of integers to a JsArray[JsNumber]" in {
      val test = "[1,2,5436,6  ,7,8, 0]"
      val arr = output.renderIntList(dummyprop, test)
      println("INT-LIST : " + pretty(arr))
      arr.isInstanceOf[JsArray] === true
      arr(0).isInstanceOf[JsDefined] === true
      arr(0).get must haveClass[JsNumber]
    }
    
  }
  
  "booleanList" should {

    "convert a comma-delimited string of 'true', 'false' values to a JsArray[JsBoolean]" in {
      val test = "[ true, true, false, true, false, false,true,true]"
      val arr = output.renderBooleanList(dummyprop, test)
      println("BOOL-LIST : " + pretty(arr))
      arr.isInstanceOf[JsArray] === true
      arr(0).isInstanceOf[JsDefined] === true
      arr(0).get must haveClass[JsBoolean]
    }    
  }

  
  "dateTime" should {
    
    "convert a properly formatted ISO 8601 date-time string to a JsString" in {
      val dt = output.renderDateTime(dummyprop, DateTime.now.toString)
      dt.isInstanceOf[JsString] === true
    }
    
    "fail when the string is not properly formatted" in {
      output.renderDateTime(dummyprop, "foo") must throwA[Exception]
    }
  }
  
  "float" should {
    
    "convert a floating-point number to a JsNumber" in {
      output.renderFloat(dummyprop, "2.5").isInstanceOf[JsNumber] === true
    }
    
  }
  
  "float::list" should {
    
    "convert a list of floating-point numbers to a JsArray[JsNumber]" in {
      val test1 = "[1.1, 1.2, 1.3, 2.1, 456.543234677]"
      val test2 = "[1.1,   1.2,         1.3, 2.1, 456.543234677]"
      
      val arr = output.renderFloatList(dummyprop, test1)
      arr.isInstanceOf[JsArray] === true
      arr(0).isInstanceOf[JsDefined] === true
      arr(0).get.toString.toFloat === 1.1f
      
    }
    
  }  
  
  "json" should {
    
    "convert a well-formed JSON string to a JsObject" in {
      
      val test = """{ "name": "foo", "value": "bar" }"""
      val js = output.renderJson(dummyprop, test)
      
      println("JSON-OBJECT : " + pretty(js))
      
      js.isInstanceOf[JsObject] === true
    }
    
  }
  
  "json::list" should {
    
    "convert well-formed JSON string representing an array of JSON object into a JsArray[JsObject]" in {
      val json = """
        [
          {"op": "replace", "path": "/alpha", "value": "foo" },
          {"op": "replace", "path": "/beta", "value": "bar" },
          {"op": "replace", "path": "/charlie", "value": "baz" }
        ]
      """
      
      val arr = output.renderJsonList(dummyprop, json)
      println("***ARR-0 : " + arr(0))
      arr.isInstanceOf[JsArray] === true
    }
    
  }
  
  def pretty(json: JsValue) = Json.prettyPrint(json)
  
}


