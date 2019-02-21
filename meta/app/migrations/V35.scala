package migrations


import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.sdk._
import play.api.libs.json._

import scala.util.Either
import scala.util.{Failure, Success, Try}


/**
 * Add Gestalt::Resource::Task type to Meta.
 */
class V35 extends MetaMigration() {

  import V35._

  implicit val acc = new MessageAccumulator()

  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {
    acc push "Looking up 'root' org"
    ResourceFactory.findRootOrg match {
      case Failure(e) => handleResultStatus(
          Failure(new RuntimeException(s"Could not locate root Org: ${e.getMessage}")), acc)
      case Success(org) =>
        addTypeToOrg(org.id, TASK_TYPE_ID, TASK_TYPE_NAME, identity, payload, acc) {
          createNewResourceType
        }
    }
  }

  def createNewResourceType(org: UUID, creator: GestaltResourceInstance): Try[GestaltResourceType] = {
    val descriptionText = "This type acts as a reciept for suppressed Meta functions in event calls."
    createResourceType(
      creator, TASK_TYPE_ID, TASK_TYPE_NAME,
      SystemType(org, ResourceOwnerLink(ResourceIds.User, creator.id),
        typeId = TASK_TYPE_ID,
        typeName = TASK_TYPE_NAME,
        desc = Some(descriptionText),
        extend = Some(ResourceIds.Resource)
      ).withTypeProperties(
        TypeProperty("message", "string", require = "optional"
      )).withActionInfo(ActionInfo(
        prefix = "task",
        verbs = Seq()
      )).withApiInfo (
          TypeApiInfo(rest_name = "tasks"
      )).withLineageInfo(LineageInfo(
        parent_types = Seq(ResourceIds.Org, ResourceIds.Workspace, ResourceIds.Environment)
      ))
    )
  }
}

object V35 {
  val TASK_TYPE_ID = UUID.fromString("57a02f53-abbe-4e85-9857-de34a9aee411")
  val TASK_TYPE_NAME = "Gestalt::Resource::Task"
}
