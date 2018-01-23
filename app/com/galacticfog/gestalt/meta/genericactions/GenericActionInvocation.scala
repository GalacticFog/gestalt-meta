package com.galacticfog.gestalt.meta.genericactions

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import play.api.libs.json._
import play.api.mvc.RequestHeader

case class GenericActionContext(org: GestaltResourceInstance,
                                workspace: Option[GestaltResourceInstance],
                                environment: Option[GestaltResourceInstance],
                                queryParams: Map[String,Seq[String]]) {
  def toJson(): JsObject = Json.obj(
    "org" -> Output.renderInstance(org),
    "workspace" -> workspace.map(Output.renderInstance(_)),
    "environment" -> environment.map(Output.renderInstance(_)),
    "queryParams" -> Json.toJson(queryParams)
  )
}

case object GenericActionContext {

  /* TODO: make a structure for getting other query parameters into the invocation
   *
   * this is useful for query parameter on non-CRUD actions, like:
   *   POST .../containers/7?action=scale&numInstances=10
   */

  def fromParent(org: GestaltResourceInstance, parent: GestaltResourceInstance)(implicit request: RequestHeader): GenericActionContext = {
    GenericActionContext(
      org = org,
      workspace = parent.typeId match {
        case ResourceIds.Workspace => Some(parent)
        case ResourceIds.Environment => ResourceFactory.findParent(ResourceIds.Workspace, parent.id)
        case _ => None
      },
      environment = if (parent.typeId == ResourceIds.Environment) Some(parent) else None,
      queryParams = request.queryString - "action"
    )
  }
}

case class GenericActionInvocation(action: String,
                                   context: GenericActionContext,
                                   provider: GestaltResourceInstance,
                                   resource: Option[GestaltResourceInstance] = None,
                                   actionPayload: Option[JsValue] = None ) {
  def toJson(): JsObject = {

    Json.obj(
      "action" -> action,
      "context" -> context.toJson(),
      "provider" -> Output.renderInstance(provider),
      "resource" -> resource.map(Output.renderInstance(_)),
      "actionPayload" -> actionPayload
    )
  }
}


