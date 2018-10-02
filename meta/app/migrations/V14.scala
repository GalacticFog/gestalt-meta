package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.meta.api.sdk.{ResourceOwnerLink, _}
import com.galacticfog.gestalt.meta.auth._
import play.api.libs.json._

import scala.util.{Either, Failure, Left, Right, Success, Try}


class V14 extends MetaMigration with AuthorizationMethods {

  import V14._

  private implicit val acc = new MessageAccumulator()
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {

    val process = for {
      root <- {
        acc push "Looking up 'root' org"
        ResourceFactory.findRootOrg
      }
      creator <- Try{
        acc push s"Looking up creator '${identity}'"
        ResourceFactory.findById(ResourceIds.User, identity).getOrElse(throw new RuntimeException(s"Could not locate creator '${identity}'"))
      }
      ecsTpe <- createResourceType(
        creator, ECS_PROVIDER_TYPE_ID, ECS_PROVIDER_TYPE_NAME,
        SystemType(root.id, ResourceOwnerLink(ResourceIds.User, creator.id),
          typeId = ECS_PROVIDER_TYPE_ID,
          typeName = ECS_PROVIDER_TYPE_NAME,
          desc = Some("AWS ECS CaaS Provider"),
          extend = Some(ResourceIds.CaasProvider)
        ).withTypeProperties(
        ).withActionInfo(ActionInfo(
          prefix = "provider",
          verbs = Seq()
        )).withLineageInfo(LineageInfo(
          parent_types = Seq(ResourceIds.Org, ResourceIds.Workspace, ResourceIds.Environment),
          child_types = Option(Seq(ResourceIds.Environment, ResourceIds.Entitlement))
        ))
      )
    } yield ecsTpe

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

object V14 {
  val ECS_PROVIDER_TYPE_ID = UUID.fromString("8406d8e0-4b7e-49f1-acf7-04c519ca9dca")
  val ECS_PROVIDER_TYPE_NAME = "Gestalt::Configuration::Provider::CaaS::ECS"
}
