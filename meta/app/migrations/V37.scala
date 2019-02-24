package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk.{ResourceOwnerLink,ResourceIds}
import com.galacticfog.gestalt.meta.auth._
import play.api.libs.json._
import scala.util.Either

/**
 * Add environment_type 'system' to list of acceptable environment_types.
 */
class V37()  extends MetaMigration with AuthorizationMethods {
  
  import V37._
  
  private implicit val acc = new MessageAccumulator()

  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {
    
    val process = for {
      rootorg <- {
        acc push "Looking up root Org for ownership"
        ResourceFactory.findRootOrg
      }
      _ = {
        acc push s"Testing for environment_type '${SYSTEM_ENV_TYPE_NAME}'"
        val envType = ReferenceFactory.findByName(ResourceIds.EnvironmentType, SYSTEM_ENV_TYPE_NAME)
        if (envType.nonEmpty) {
          acc push s"environment_type '${SYSTEM_ENV_TYPE_NAME}' already exists. Nothing to do."
        } else {
          
          acc push s"Persisting environment_type '${SYSTEM_ENV_TYPE_NAME}'"
          ReferenceFactory.createReferenceType(
              "environment_type", 
              Some(SYSTEM_ENV_TYPE_ID), 
              rootorg.id, 
              ResourceOwnerLink(ResourceIds.Org, rootorg.id.toString),
              Some("Indicates that the Environment belongs to Gestalt.")
            )(SYSTEM_ENV_TYPE_NAME)
        }
      }
    } yield rootorg    
    
    handleResultStatus(process)(acc)
  }
}

object V37 {
  val SYSTEM_ENV_TYPE_ID = UUID.fromString("d672854d-4de4-4374-8741-bff26de76fa5")
  val SYSTEM_ENV_TYPE_NAME = "system"  
}