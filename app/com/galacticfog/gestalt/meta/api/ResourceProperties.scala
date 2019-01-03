package com.galacticfog.gestalt.meta.api


import java.util.UUID
import scala.util.Try
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.patch._
import play.api.libs.json._


trait ResourceProperties {
  
  import PatchOp.{Add, Remove, Replace}
  
  val typeId: UUID
  
  private lazy val proptypes = Properties.getTypePropertyMap(typeId)
  
  
  def add(propJs: JsObject, path: String, value: JsValue): Try[JsObject] = {
    Js.transform(propJs, Add(path, value))
  }
  
  def replace(propJs: JsObject, path: String, value: JsValue): Try[JsObject] = {
    Js.transform(propJs, Replace(path, value))
  }
  
  def remove(propJs: JsObject, path: String): Try[JsObject] = {
    Js.transform(propJs, Remove(path))
  }
  
  /**
   * Apply multiple PATCH ops in one call.
   */
  def transform(propJs: JsObject, ops: PatchOp*): Try[JsObject] = {
    Js.transform(propJs, ops:_*)
  }
  
  /**
   * Copy properties JSON back to the properties Map
   */
  def saveProperties(res: GestaltResourceInstance, props: JsObject): GestaltResourceInstance = {
    val sprops = props.fieldSet.map { 
      case (k, v) => (k -> Json.stringify(v)) 
    }.toMap
    res.copy(properties = Some(sprops))
  }
  
  /**
   * Convert resource properties Map[String,String] to a JsObject
   */
  def toJsonObject(ps: Map[String, String]): JsObject = {
    ps.keys.foldLeft(Json.obj()) { (json, key) =>
      json ++ Json.obj(key -> (s2j(DataType.name(proptypes(key).datatype), ps(key))))
    }
  }
  
  /**
   * Convert resource Properties Map[String,String] to Map[String, JsValue]
   */
  def toJsonMap(ts: Map[String,GestaltTypeProperty], ps: Map[String, String]): Map[String,JsValue] = {
    ps.map { case (k, v) => (k, s2j(DataType.name(proptypes(k).datatype), v)) }
  }
  
  /**
   * Attempt to convert a String to an appropriately typed JsValue
   */
  private[api] def s2j(tpe: String, s: String): JsValue = {
    tpe match {
      case a if a.startsWith("resource::") => JsString(s)
      case b if b.endsWith("::list") => Json.parse(s).as[JsArray]
      case "string"|"uuid"|"datetime" => JsString(s)
      case "int"     => JsNumber(s.toInt)
      case "long"    => JsNumber(s.toLong)
      case "float"   => JsNumber(s.toFloat)
      case "boolean" => JsBoolean(s.toBoolean)
      case "json"    => Json.parse(s).as[JsObject]
      
      case _ => throw new RuntimeException("Unknown datatype")
    }
  }
}
