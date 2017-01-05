package com.galacticfog.gestalt.meta.policy

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.output._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.sdk._
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
    val msg = Json.toJson(this)(EventMessage.eventMessageFormat).as[JsObject]
    val rul = Output.renderInstance(args.rule).as[JsObject]
    val pay = args.payload.map(Output.renderInstance(_).as[JsObject])

    val ags = (msg \ "args").as[JsObject]

    // set "args.rule" -> args.rule
    // and "args.payload.resource" -> args.payload
    val argsVal = pay.foldLeft[JsObject](
      ags ++ Json.obj("rule" -> rul)
    )(
      (l,p) => l ++ Json.obj("payload" -> Json.obj("resource" -> pay))
    )
    msg ++ Json.obj("args" -> argsVal)
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