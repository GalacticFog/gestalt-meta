package com.galacticfog.gestalt.meta.api.patch

import play.api.libs.json._
import com.galacticfog.gestalt.data.models._
import scala.util.Try
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.patch._

/**
 * PatchDocument 'wrapper' for PATCH operations against GestaltResourceType objects.
 */
object PatchType {

  def applyPatch(r: GestaltResourceType, patch: PatchDocument): Try[JsValue] = {
    /**
     * Convert entire Type to json - apply patch to whole document
     */
    val props = r.properties.get
    val jprops = props map { case (k,v) => (k, Json.parse(v)) }
    println("***JPROPS : " + Json.prettyPrint(Json.toJson(jprops)))
    val json = {
      Json.parse(Json.stringify(Json.toJson(r))).as[JsObject] ++ Json.obj("properties" -> Json.toJson(jprops))
    }
    
    println("***JSON: " + Json.prettyPrint(json))
    patch.applyPatch(json)
  }
  
}