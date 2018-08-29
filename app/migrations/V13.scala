package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.meta.api.sdk.{ResourceOwnerLink, _}
import com.galacticfog.gestalt.meta.auth._
import play.api.libs.json._

import scala.util.{Either, Failure, Left, Right, Success}


class V13() extends MetaMigration with AuthorizationMethods {

  import V13._

  private implicit val acc = new MessageAccumulator()
  
  private val ENTITLEMENTS = Option(Seq(ResourceIds.Entitlement))

  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {

    val process = for {
      root <- {
        acc push "Looking up 'root' org"
        ResourceFactory.findRootOrg
      }
      volumeTpe <- createResourceType(
        root, VOLUME_TYPE_ID, VOLUME_TYPE_NAME,
        SystemType(root.id, ResourceOwnerLink(ResourceIds.Org, root.id),
          typeId = VOLUME_TYPE_ID,
          typeName = VOLUME_TYPE_NAME,
          desc = Some("Container volumes"),
          extend = Some(ResourceIds.Resource)
        ).withTypeProperties(
          TypeProperty("type", "string", require = "required"),
          TypeProperty("provider", "json", require = "required"),
          TypeProperty("config", "json", require = "required"),
          TypeProperty("reclamation_policy", "string", require = "optional"),
          TypeProperty("external_id", "string", require = "optional"),
          TypeProperty("mount_path", "string", require = "optional"),
          TypeProperty("size", "int", require = "required"),
          TypeProperty("access_mode", "string", require = "required"),
          TypeProperty("container_id", "resource::uuid", require = "optional", refersTo = Some(ResourceIds.Container), system = true)
        ).withActionInfo(ActionInfo(
          prefix = "volume",
          verbs = Seq("import")
        )).withLineageInfo(LineageInfo(
          parent_types = Seq(ResourceIds.Environment),
          child_types = ENTITLEMENTS
        )).withApiInfo(TypeApiInfo(
          rest_name = "volumes"
        ))
      )
    } yield volumeTpe

    process match {
      case Success(_) => {
        acc push "Meta update successful."
        Right(MigrationStatus(
            status = "SUCCESS",
            message = s"Upgrade successfully applied",
            succeeded = Some(acc.messages),
            None).toJson)
      }
      case Failure(e) => {
        Left(MigrationStatus(
            status = "FAILURE",
            message = s"There were errors performing this upgrade: ${e.getMessage}",
            succeeded = Some(acc.messages),
            errors = Some(Seq(e.getMessage))).toJson)
      }
    }

  }

}

object V13 {
  val VOLUME_TYPE_ID = UUID.fromString("24361db7-0fc0-4be5-9973-d88d5602956f")
  val VOLUME_TYPE_NAME = "Gestalt::Resource::Volume"
}
