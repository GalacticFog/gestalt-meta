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

  val output = OutputDatatypeHandlers

  val dummyowner = ResourceOwnerLink(ResourceIds.User, uuid())
  val dummyprop = GestaltTypeProperty(name = "foo", orgId = uuid(), owner = dummyowner,
          appliesTo = uuid(), datatype = uuid(), typeId = uuid(), state = uuid(), 
          requirementType = uuid(), visibilityType = uuid())
  
  "stringList" should {
    
    "convert a comma-delimited string to a JsonArray[JsString]" in {
      val test = """ "alpha", "beta", "charlie", "delta", "echo" """
      val arr = output.stringList(dummyprop, test)
      arr.isInstanceOf[JsArray] === true
      arr(0).isInstanceOf[JsString] === true
    }
    
    "ignore square braces and convert a comma-delimited string to a JsonArray[JsString]" in {
      val test = """[ "alpha", "beta", "charlie", "delta", "echo" ]"""
      val arr = output.stringList(dummyprop, test)
      arr.isInstanceOf[JsArray] === true
      arr(0).isInstanceOf[JsString] === true      
    }
    
    "convert a comma-delimited list of UUID strings to a JsArray[JsString]" in {
      val test = s""" "${uuid()}","${uuid()}", "${uuid()}" """
      val arr = output.stringList(dummyprop, test)
      arr.isInstanceOf[JsArray] === true
      arr(0).isInstanceOf[JsString] === true      
    }
  }
  
  "intList" should {
    
    "convert comma-delimited string of integers to a JsArray[JsNumber]" in {
      val test = "1,2,5436,6  ,7,8, 0"
      val arr = output.intList(dummyprop, test)
      println("INT-LIST : " + pretty(arr))
      arr.isInstanceOf[JsArray] === true
      arr(0).isInstanceOf[JsNumber] === true
    }
    
  }
  
  "booleanList" should {

    "convert a comma-delimited string of 'true', 'false' values to a JsArray[JsBoolean]" in {
      val test = " true, true, false, true, FALSE, false,true,True"
      val arr = output.booleanList(dummyprop, test)
      println("BOOL-LIST : " + pretty(arr))
      arr.isInstanceOf[JsArray] === true
      arr(0).isInstanceOf[JsBoolean] === true
    }    
  }

  
  "dateTime" should {
    
    "convert a properly formatted ISO 8601 date-time string to a JsString" in {
      val dt = output.dateTime(dummyprop, DateTime.now.toString)
      dt.isInstanceOf[JsString] === true
    }
    
    "fail when the string is not properly formatted" in {
      output.dateTime(dummyprop, "foo") must throwA[Exception]
    }
  }
  
  
  "json" should {
    
    "convert a well-formed JSON string to a JsObject" in {
      
      val test = """{ "name": "foo", "value": "bar" }"""
      val js = output.json(dummyprop, test)
      
      println("JSON-OBJECT : " + pretty(js))
      
      js.isInstanceOf[JsObject] === true
    }
    
  }
  
  def pretty(json: JsValue) = Json.prettyPrint(json)
  
}


