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

/**
 * PatchDocument 'wrapper' for PATCH operations against GestaltTypeProperty objects.
 */
object PatchProperty {
   def applyPatch(r: GestaltTypeProperty, patch: PatchDocument): Try[JsValue] = {
    /**
     * Convert Property to JSON - apply patch to whole document
     */
    val props = r.properties.getOrElse(Map())
    val jprops = props map { case (k,v) => (k, Json.parse(v)) }
    val json = {
      Json.parse(
          Json.stringify(Json.toJson(r))).as[JsObject] ++ 
          Json.obj("properties" -> Json.toJson(jprops))
    }
    patch.applyPatch(json)
  }
}