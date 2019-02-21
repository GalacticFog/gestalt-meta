package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk.{ResourceOwnerLink,ResourceIds}
import com.galacticfog.gestalt.meta.auth._
import play.api.libs.json._
import scala.util.{Success,Failure,Either,Left,Right}

/**
 * Add Gestalt::Resource::State::Corrupt to ResourceState enumeration in Meta schema.
 */
class V16  extends MetaMigration with AuthorizationMethods {
  
  import V16._
  
  private implicit val acc = new MessageAccumulator()

  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {
    
    val process = for {
      rootorg <- {
        acc push "Looking up root Org for ownership"
        ResourceFactory.findRootOrg
      }
      _ = {
        acc push s"Testing for ResourceState '${STATE_CORRUPT_NAME}'"
        val state = ReferenceFactory.findByName(ResourceIds.ResourceState, STATE_CORRUPT_NAME)
        if (state.nonEmpty) {
          acc push s"ResourceState '${STATE_CORRUPT_NAME}' found with ID '${state.get.id}'."
          acc push "No changes made."
        
        } else {
          
          acc push s"Persisting ResourceState '${STATE_CORRUPT_NAME}'"
          ReferenceFactory.createReferenceType(
              "resource_state", //ResourceIds.ResourceState, 
              Some(STATE_CORRUPT_ID), 
              rootorg.id, 
              ResourceOwnerLink(ResourceIds.Org, rootorg.id.toString),
              Some("Indicates that the resource has one or more un-renderable properties.")
            )(STATE_CORRUPT_NAME)
        }
      }
    } yield rootorg
        
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
object V16 {
  val STATE_CORRUPT_ID = UUID.fromString("681099f4-da63-48ef-99d0-75c01166d202")
  val STATE_CORRUPT_NAME = "Gestalt::Resource::State::Corrupt"  
}