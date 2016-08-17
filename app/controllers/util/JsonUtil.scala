package controllers.util


import play.api.libs.json._

import scala.util.{Try,Success,Failure}
import play.api.{ Logger => log }
import scala.annotation.tailrec

import com.galacticfog.gestalt.meta.api.errors.BadRequestException


object JsonUtil {

  implicit def str2js(s: String) = JsString(s)
  
  /**
   * Replace a top-level JSON key name with a new name. If a new value is given it will be used
   * otherwise the new key will have the old value.
   */
  def replaceKey(obj: JsObject, oldName: String, newName: String, newValue: Option[JsValue] = None) = {
    obj \ oldName match {
      case u: JsUndefined => obj
      case v => {
        val value = {
          if (newValue.isDefined) newValue.get else v
        }
        (obj - oldName) ++ Json.obj(newName -> value)
      }
    }
  }
  
  /**
   * Replace the named property value in resource.properties 
   * with the given value. Returns *JUST* the properties JSON.
   */
  def replaceJsonPropValue(obj: JsObject, name: String, value: JsValue) = {
    val newprop = Json.obj(name -> value)
    (obj \ "properties") match {
      case u: JsUndefined => newprop
      case v => v.as[JsObject] ++ newprop
    }
  }
  
  /**
   * Replace the entire resource.properties collection with the given object.
   * Returns entire JSON object with updated properties.
   */
  def replaceJsonProps(obj: JsObject, props: JsObject) = {
    obj ++ Json.obj("properties" -> props)
  }

  def withJsonPropValue(obj: JsObject, prop: (String, JsValue)): JsObject = withJsonPropValue(obj, prop._1, prop._2)

  def withJsonPropValue(obj: JsObject, propName: String, propValue: JsValue): JsObject = {
    val newprops = replaceJsonPropValue(obj, propName, propValue)
    replaceJsonProps(obj, replaceJsonPropValue(obj, propName, propValue))
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
   * 
   * @param obj a JSON serialized GestaltResourceInstance
   * @param name name of the property to upsert
   * @param value value of the property as a JsValue
   */
  def upsertProperty(obj: JsObject, name: String, value: JsValue) = Try {
    val ps  = replaceJsonPropValue(obj, name, value)
    replaceJsonProps(obj, ps)    
  }  
  
  def upsertProperties(obj: JsObject, props: (String,JsValue)*): Try[JsObject] = {
    
    def go(ps: Seq[(String,JsValue)], o: JsObject): Try[JsObject] = {
      ps match {
        case Nil => Success(o)
        case h :: t => {
          upsertProperty(o, h._1, h._2) flatMap { x => go(t, x) }
        }
      }
    }
    go(props.toList, obj)
  }
  
  
  def find(obj: JsObject, path: String): Option[JsValue] = {  
    @tailrec 
    def go(cmps: List[String], path: String => JsValue): JsValue = {
      cmps match {
        case Nil    => JsNull
        case h :: t => if (t.size == 0) path(h) else go(t, (path(h) \_) )
      }
    }
    go(toPath(path), obj \ _) match {
      case u: JsUndefined => None
      case v => Option(v)
    }
  }
  
  private def toPath(path: String) = path.trim.stripPrefix("/").stripSuffix("/").split("/").toList
  
  def safeParse[A](js: String)(implicit reads: Reads[A]): A = {
    safeParse[A](Json.parse(js))
  }
 
  def safeParse[A](json: JsValue)(implicit reads: Reads[A]): A = {
    json.validate[A].map {
      case a => a
    }.recoverTotal { e => 
      throw new BadRequestException(
          s"Failed parsing JSON string: " + JsError.toFlatJson(e).toString) 
    }
  }
  
}

