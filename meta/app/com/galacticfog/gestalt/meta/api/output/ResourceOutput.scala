package com.galacticfog.gestalt.meta.api.output

import com.galacticfog.gestalt.data._
import play.api.libs.json._
import java.util.UUID
import scala.language.implicitConversions

abstract class ResourceOutput {
  /**
   * Convert Option[String] to Option[JsString]
   */
  implicit def jsStringOpt(s: Option[String]): Option[JsString] = {
    if (s.isDefined) Some(JsString(s.get)) else None
  }
  
  /**
   * Convert a List[String] to a JsArray
   */
  implicit def jsonArray(ar: Option[List[String]]): Option[JsValue] = {
    if (ar.isEmpty) None else Some(Json.toJson(ar))
  }
  
  /**
   * Convert a Map[String,String] to a JsObject.
   */
  implicit def jsonHstore(hs: Option[Map[String,String]]): Option[JsValue] = {
    if (hs.isEmpty) None else Some(Json.toJson(hs))
  }
  
}