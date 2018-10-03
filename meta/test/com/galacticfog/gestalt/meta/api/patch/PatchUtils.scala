package com.galacticfog.gestalt.meta.api.patch


import com.galacticfog.gestalt.patch.JsPointer
import play.api.libs.json._

trait PatchUtils {

  def p(s: String) = JsPointer.toJsPath(s)
  def jstr(s: String) = JsString
  def jsonArray(ss: String*) = Json.toJson(Vector(ss:_*))  
  def jsonArrayInt(ss: Int*) = Json.toJson(Vector(ss:_*))  
  def jstrOpt(s: String) = Option(JsString(s))   
  
}