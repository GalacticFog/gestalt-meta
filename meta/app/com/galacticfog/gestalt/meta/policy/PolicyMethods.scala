package com.galacticfog.gestalt.meta.policy

import java.util.UUID

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.output._
import play.api.libs.json._
import org.joda.time.DateTime
// import org.joda.time.format.DateTimeFormat


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

    // val formatter = DateTimeFormat.forPattern("yyyyMMddHHmmss")

    // msg ++ Json.obj("args" -> argsVal, "timestamp" -> timestamp.getMillis() / 1000)
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
