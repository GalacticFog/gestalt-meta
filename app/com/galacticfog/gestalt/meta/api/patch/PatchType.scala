package com.galacticfog.gestalt.meta.api.patch

import play.api.libs.json._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.util._
import com.galacticfog.gestalt.data.models._
import scala.util.{Try,Success,Failure}
import java.util.UUID
import com.galacticfog.gestalt.meta.api.output._
import controllers.util.trimquotes
import play.api.{ Logger => log }
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import com.galacticfog.gestalt.patch._


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