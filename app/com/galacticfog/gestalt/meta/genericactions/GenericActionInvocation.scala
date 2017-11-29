package com.galacticfog.gestalt.meta.genericactions

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import play.api.libs.json._

case class GenericActionContext(org: GestaltResourceInstance,
                                workspace: Option[GestaltResourceInstance],
                                environment: Option[GestaltResourceInstance] )

case object GenericActionContext {
//  implicit val actionContextFmt: Writes[ActionContext] = new Writes[ActionContext] {
//    override def writes(o: ActionContext) = Json.obj(
//
//    )
//  }

  def fromParent(org: GestaltResourceInstance, parent: GestaltResourceInstance): GenericActionContext = {
    GenericActionContext(
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

case class GenericActionInvocation(action: String,
                                   context: GenericActionContext,
                                   provider: GestaltResourceInstance,
                                   resource: Option[GestaltResourceInstance] = None,
                                   payload: Option[JsValue] = None )

case object GenericActionInvocation {
//  implicit val actionInvocationFmt = Json.format[ActionInvocation]
}

