package controllers.util


import play.api.libs.json._

import scala.util.{Try,Success,Failure}


object JsonUtil {

  implicit def str2js(s: String) = JsString(s)
  
  /**
   * Replace the named property value in resource.properties 
   * with the given value
   */
  def replaceJsonPropValue(obj: JsObject, name: String, value: JsValue) = {
      (obj \ "properties").as[JsObject] ++ Json.obj(name -> value)
  }
  
  /**
   * Replace the entire resource.properties collection with the given object.
   */
  def replaceJsonProps(obj: JsObject, props: JsObject) = {
    obj ++ Json.obj("properties" -> props)
  }    
  
  def getJsonPropertyField(json: JsValue, field: String) = {
    json \ "properties" \ field match {
      case u: JsUndefined => None
      case v => Some(v)
    }
  }
  
  def getJsonField(json: JsValue, field: String) = {
    json \ field match {
      case u: JsUndefined => None
      case v => Some(v)
    }
  }  
  
  /**
   * Update or Insert an item into the properties collection.
   */
  def upsertProperty(obj: JsObject, name: String, value: JsValue) = Try {
    obj \ "properties" \ name match {
      case u : JsUndefined => {
        val ps  = replaceJsonPropValue(obj, name, value.as[String])
        replaceJsonProps(obj, ps)
      }
      case _ => obj
    }        
  }    
  
}