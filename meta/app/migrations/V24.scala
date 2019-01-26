package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.auth._
import play.api.libs.json._

import scala.util.{Either, Failure, Left, Right, Success, Try}


class V24() extends MetaMigration with AuthorizationMethods {

  import V24._

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
      _ <- createResourceType(
        creator, AWS_API_GATEWAY_PROVIDER_TYPE_ID, AWS_API_GATEWAY_PROVIDER_TYPE_NAME,
        SystemType(root.id, ResourceOwnerLink(ResourceIds.User, creator.id),
          typeId = AWS_API_GATEWAY_PROVIDER_TYPE_ID,
          typeName = AWS_API_GATEWAY_PROVIDER_TYPE_NAME,
          desc = Some("AWS API Gateway Provider"),
          extend = Some(ResourceIds.Provider)
        ).withTypeProperties(
        ).withActionInfo(ActionInfo(
          prefix = "provider",
          verbs = Seq()
        )).withLineageInfo(LineageInfo(
          parent_types = Seq(ResourceIds.Org, ResourceIds.Workspace, ResourceIds.Environment),
          child_types = Option(Seq(ResourceIds.Environment, ResourceIds.Entitlement))
        ))
      )
      apiEndpointType <- Try(TypeFactory.findById(ResourceIds.ApiEndpoint).getOrElse(throw new RuntimeException(s"Could not locate type ApiEndpoint")))
      _ <- if (PropertyFactory.findByType(ResourceIds.ApiEndpoint).exists(_.name == "whitelist")) {
        acc push s"Property `whitelist` already exists. Nothing to do."
        Success(())
        
      } else {
        val newProperty = GestaltTypeProperty(
            state = ResourceState.id(ResourceStates.Active),
            orgId = apiEndpointType.orgId,
            owner = apiEndpointType.owner,
            name = "whitelist",
            appliesTo = apiEndpointType.id,
            datatype = DataType.id("json::list"),
            requirementType = RequirementType.id("optional"))
        
        acc push s"Adding /properties/whitelist to ApiEndpoint Type."
        addPropertyTypeToResourceType(apiEndpointType, newProperty)(acc)         
      }
    } yield ()

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

object V24 {
  val AWS_API_GATEWAY_PROVIDER_TYPE_ID = UUID.fromString("4262ea0f-6f04-4ea9-b4b8-5ad2d75f8478")
  val AWS_API_GATEWAY_PROVIDER_TYPE_NAME = "Gestalt::Configuration::Provider::GatewayManager::AWS"
}
