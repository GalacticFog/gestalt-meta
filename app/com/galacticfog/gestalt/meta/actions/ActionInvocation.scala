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

  def fromParent(parent: GestaltResourceInstance): ActionContext = {
    ActionContext(
      org = ResourceFactory.findById(ResourceIds.Org, parent.orgId) getOrElse(throw new InternalErrorException(s"could not locate parent org with id '${parent.orgId}' for resource with id '${parent.id}'")),
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
  def invokeAction(context: ActionInvocation): Future[GestaltResourceInstance]
}

trait ActionProviderManager {
  def getProvider(provider: GestaltResourceInstance): Try[ActionProvider]
}