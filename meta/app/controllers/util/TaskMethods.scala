package controllers.util

import java.util.UUID
import scala.util.Try
import play.api.libs.json.{Json, JsValue}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds


object TaskMethods extends MetaControllerUtils {
  
  /**
   * TODO: Format JSON for a Task resource. Currently this is used as a 'receipt' for API calls
   * where the meta function has been suppressed. This is just a stub - we'll want to get more 
   * information into the message.
   */
  def suppressionTaskJson(): JsValue = {
    Json.obj(
      "name" -> s"new-task-${UUID.randomUUID.toString}",
      "properties" -> Json.obj(
          "message" -> "This function has been suppressed."))    
  }
  
  /**
   * Create a Gestalt::Resource::Task in Meta
   */
  def createSuppressionTask(org: UUID, identity: AuthAccountWithCreds, payload: JsValue, parent: UUID): Try[GestaltResourceInstance] = {
    CreateWithEntitlements(org, identity, payload, migrations.V35.TASK_TYPE_ID, Some(parent))  
  }
  
}

import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.meta.api.sdk.ResourceStates

object State {
  def isPending(state: UUID): Boolean = state == ResourceState.id(ResourceStates.Pending)
  def isPending(state: Option[UUID]): Boolean = state.isDefined && isPending(state.get)
  
  def isActive(state: UUID): Boolean = state == ResourceState.id(ResourceStates.Active)
  def isActive(state: Option[UUID]): Boolean = state.isDefined && isActive(state.get)
  
  def isDeleted(state: UUID): Boolean = state == ResourceState.id(ResourceStates.Deleted)
  def isDeleted(state: Option[UUID]): Boolean = state.isDefined && isDeleted(state.get)
  
  def isDisabled(state: UUID): Boolean = state == ResourceState.id(ResourceStates.Disabled)
  def isDisabled(state: Option[UUID]): Boolean = state.isDefined && isDisabled(state.get)
  
  def isFailed(state: UUID): Boolean = state == ResourceState.id(ResourceStates.Failed)
  def isFailed(state: Option[UUID]): Boolean = state.isDefined && isFailed(state.get)
  
  def isCorrupt(state: UUID): Boolean = state == ResourceState.id(ResourceStates.Corrupt)
  def isCorrupt(state: Option[UUID]): Boolean = state.isDefined && isCorrupt(state.get)
}
