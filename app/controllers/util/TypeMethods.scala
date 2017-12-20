package controllers.util

import play.api.Logger
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import controllers.util._
import java.util.UUID

import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi

import scala.language.postfixOps
import com.galacticfog.gestalt.json.Js
import javax.inject.Singleton
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.data.ResourceSelector


object TypeMethods {

  private[this] val log = Logger(this.getClass)
  
  /**
   * Render a ResourceType to JSON.
   * Uses the provided querystring to determine whether or not to expand property definitions inline.
   * 
   * @param p the type to render
   * @param qs the querystring sent with the original request
   * @param metaUrl address of the hosting meta server (used in creating resource link hrefs)
   */
  def renderType(p: GestaltResourceType, qs: Map[String, Seq[String]], metaUrl: String): Try[JsValue] = Try {
    val typeJson = Output.renderResourceTypeOutput(p, Some(metaUrl))
    if (QueryString.singleBoolean(qs, "withprops")) {
      log.debug("Found 'withprops' query param - looking up type-properties for expansion...")
      TypeMethods.withPropertiesExpanded(p.id, typeJson, metaUrl)
    } else typeJson
  }  
  
  
  /**
   * Render a type with property_defs expanded inline. This method a JSON rendering of a GestaltResourceType
   * and injects fully rendered property_defs (as opposed to just property links)
   * 
   * @param pid UUID of the type being rendered
   * @param pjson JSON representing the type being rendered
   * @param metaUrl address of the hosting meta server
   */
  def withPropertiesExpanded(pid: UUID, pjson: JsValue, metaUrl: String) = {
    /*
     * This maps over all properties declared directly on the target type
     * (not inherited properties) and fully renders them to JSON (complete resource).
     */
    val jsonProperties = PropertyFactory.findAll(pid).map { p =>
      Output.renderTypePropertyOutput(p, Some(metaUrl))
    }
  
    /*
     * Replace property_defs with fully expanded properties.
     */
    pjson.as[JsObject] ++ Json.obj("property_defs" -> jsonProperties)
  }  
 
  def typeId(name: String): Option[UUID] = {
    TypeFactory.findByName(name) map { _.id }
  }  
}