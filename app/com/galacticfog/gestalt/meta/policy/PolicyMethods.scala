package com.galacticfog.gestalt.meta.policy


import java.util.UUID
import java.net.URL
import play.api.http.HttpVerbs
import play.api.libs.ws.WS
import play.api.Play.current

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.PropertyValidator
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.ResourceType
import com.galacticfog.gestalt.data.illegal
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.{ PatchOp, PatchDocument, PatchHandler }
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.{ GestaltResource => SecurityResource }
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController

import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import controllers.util.JsonUtil._
import controllers.util.db._

import play.api.Logger
import play.api.libs.json._
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._

import com.galacticfog.gestalt.meta.api._
import play.api.mvc.Result
import play.api.mvc.Action
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.security.api.json.JsonImports.linkFormat
import com.galacticfog.gestalt.laser.ApiResponse
import com.galacticfog.gestalt.meta.auth.Actions 
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
import com.galacticfog.gestalt.keymgr.GestaltLicense
import com.galacticfog.gestalt.keymgr.GestaltFeature




import org.joda.time.DateTime


case class EventArgs(
    rule: GestaltResourceInstance, 
    payload: Option[GestaltResourceInstance])

object EventArgs {
  implicit lazy val eventArgsFormat = Json.format[EventArgs]  
}

case class EventMessage(
    id: UUID,
    identity: UUID,
    timestamp: DateTime,
    /*resource: String,*/
    event: String,
    action: String,
    args: EventArgs) {
  
  def toJson() = {
    val msg = Json.toJson(this)(EventMessage.eventMessageFormat)
    val rul = Output.renderInstance(args.rule).as[JsObject]
    
    val ags = (msg \ "args").as[JsObject]
    val m2 = msg.as[JsObject] ++ Json.obj("args" -> (ags ++ Json.obj("rule" -> rul)))
    
    if (args.payload.isEmpty) 
      msg.as[JsObject] ++ Json.obj("args" -> (ags ++ Json.obj("rule" -> rul)))
    else {
      val pay = Output.renderInstance(args.payload.get).as[JsObject]
      msg.as[JsObject] ++ Json.obj(
          "args" -> (ags ++ Json.obj(
              "rule" -> rul,
              "payload" -> pay)))
    } 
  }
}

object EventMessage {
  
  implicit lazy val eventMessageFormat = Json.format[EventMessage]
  
  def make(
    id: UUID,
    identity: UUID,
    /*resource: String,*/
    event: String,
    action: String,
    rule: GestaltResourceInstance,
    payload: Option[GestaltResourceInstance]) = {
    
    EventMessage(
        id, identity, DateTime.now, /*resource,*/ event, action, 
        EventArgs(rule, payload))
  }
  
}




trait PolicyMethods {
  
  def effectiveEventRules(parentId: UUID, event: Option[String] = None): Option[GestaltResourceInstance] = {
    val rs = for {
      p <- ResourceFactory.findChildrenOfType(ResourceIds.Policy, parentId)
      r <- ResourceFactory.findChildrenOfType(ResourceIds.RuleEvent, p.id)
    } yield r
    
    val fs = if (event.isEmpty) rs else {
      rs filter { _.properties.get("actions").contains(event.get) }
    }
    // TODO: This is temporary. Need a strategy for multiple matching rules.
    if (fs.isEmpty) None else Some(fs(0))
  }
  
  
  def effectiveRules(parentId: UUID, ruleType: Option[UUID] = None, actions: Seq[String] = Seq()): Seq[GestaltResourceInstance] = {
    val rules = for {
      p <- ResourceFactory.findChildrenOfType(ResourceIds.Policy, parentId)
      r <- ResourceFactory.findChildrenOfSubType(ResourceIds.Rule, p.id)
    } yield r
    
    DebugLogRules(parentId, rules)
    
    def array(sa: String) = Json.parse(sa).validate[Seq[String]].get
    def matchAction(a: Seq[String], b: Seq[String]) = !(a intersect b).isEmpty
    def matchType(test: UUID) = ( test == (ruleType getOrElse test) )
    
    if (actions.isEmpty) rules else {
      rules filter { r =>
        matchType(r.typeId) &&
        matchAction(array(r.properties.get("actions")), actions)
      }
    }
  }    
}