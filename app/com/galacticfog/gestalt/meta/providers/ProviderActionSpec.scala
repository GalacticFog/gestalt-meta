package com.galacticfog.gestalt.meta.providers

import play.api.libs.json._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.json._

import scala.util.{Try,Success,Failure}
import java.util.UUID


case class ProviderActionSpec(name: String, endpoint_url: String, implementation: ActionImplSpec, ui_locations: Seq[UiLocation]) {

  def toResource(org: UUID, parent: UUID, owner: ResourceOwnerLink, implId: UUID, id: UUID = uuid()): GestaltResourceInstance = {

    val props = Map(
      "endpoint_url"   -> endpoint_url,
      "implementation" -> {
        // TODO: Here - implementation should be a resource link.
        //Json.stringify(Json.toJson(implementation))
        val impl = Json.toJson(implementation).as[JsObject] ++ Json.obj("id" -> implId.toString)
        Json.stringify(impl)
      },
      "parent" -> Json.stringify(Json.obj("id" -> parent.toString)),
      "ui_locations"   -> Json.stringify(Json.toJson(ui_locations)))

    GestaltResourceInstance(
      id = id,
      typeId = ResourceIds.ProviderAction,
      state = ResourceState.id(ResourceStates.Active),
      orgId = org,
      owner = owner,
      name = name,
      properties = Some(props))
  }
  
  /**
   * Convert an ActionSpec.implementation to an Action Instance implementation.
   */
  private[providers] def instanceImplementation(impljson: JsValue) = {
    
  }
}

object ProviderActionSpec {
  
  def fromResource(r: GestaltResourceInstance): ProviderActionSpec = {
    val props = r.properties getOrElse Map()

    val locations = props.get("ui_locations").fold(Seq[UiLocation]()) { locs =>
      Js.parse[Seq[UiLocation]](Json.parse(locs)) match {
        case Failure(e) => throw new IllegalArgumentException(s"Failed parsing 'ui_locations': ${e.getMessage}")
        case Success(l) => l
      }
    }
    
    val impl = props.get("implementation").fold {
      throw new RuntimeException("Could not find 'implementation' member of ActionImplSpec")
    }{ act =>
      
      Js.parse[ActionImplSpec](Json.parse(act)) match {
        case Failure(e) => throw new RuntimeException(s"Failed parsing 'implementation': ${e.getMessage}")
        case Success(a) => a
      }
    }

    val endpoint = props.get("endpoint_url") getOrElse {
      throw new RuntimeException("Could not find 'endpoint_url' in properties.")
    }
    ProviderActionSpec(r.name, endpoint, impl, locations)
  }
  
}

case class UiLocation(name: String, icon: Option[String])
case class ActionInput(kind: String, data: Option[String])
case class ActionImplSpec(kind: String, spec: Option[JsValue], id: Option[String], input: Option[ActionInput])

