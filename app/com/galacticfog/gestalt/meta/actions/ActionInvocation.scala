package com.galacticfog.gestalt.meta.actions

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import play.api.libs.json.{Json, Writes, Reads}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try

case class ActionContext( org: GestaltResourceInstance,
                          workspace: Option[GestaltResourceInstance],
                          environment: Option[GestaltResourceInstance] )

case object ActionContext {
//  implicit val actionContextFmt: Writes[ActionContext] = new Writes[ActionContext] {
//    override def writes(o: ActionContext) = Json.obj(
//
//    )
//  }
}

case class ActionInvocation( action: String,
                             context: ActionContext,
                             provider: GestaltResourceInstance,
                             resource: Option[GestaltResourceInstance] = None,
                             payload: Option[String] = None )

case object ActionInvocation {
//  implicit val actionInvocationFmt = Json.format[ActionInvocation]
}

trait ActionProvider {
  def invokeAction(context: ActionInvocation): Future[GestaltResourceInstance]
}

trait ActionProviderManager {
  def getProvider(provider: GestaltResourceInstance): Try[ActionProvider]
}