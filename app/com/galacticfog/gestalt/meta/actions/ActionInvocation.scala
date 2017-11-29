package com.galacticfog.gestalt.meta.actions

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.InternalErrorException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import play.api.libs.json.{Json, Reads, Writes}
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

  def fromParent(org: GestaltResourceInstance, parent: GestaltResourceInstance): ActionContext = {
    ActionContext(
      org = org,
      workspace = parent.typeId match {
        case ResourceIds.Workspace => Some(parent)
        case ResourceIds.Environment => ResourceFactory.findParent(ResourceIds.Workspace, parent.id)
        case _ => None
      },
      environment = if (parent.typeId == ResourceIds.Environment) Some(parent) else None
    )
  }
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
  def invokeAction(context: ActionInvocation): Future[Either[GestaltResourceInstance,(Option[Int],Option[String],Option[String])]]
}

trait ActionProviderManager {
  def getProvider(provider: GestaltResourceInstance): Try[ActionProvider]
}

class DefaultActionProviderManager extends ActionProviderManager {
  override def getProvider(provider: GestaltResourceInstance) = Try{???}
}