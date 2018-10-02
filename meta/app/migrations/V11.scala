package migrations

import java.util.UUID

import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data.{session, _}
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import play.api.libs.json._

import scala.util.{Either, Failure, Left, Right, Success, Try}
import com.galacticfog.gestalt.meta.api.Resource

class V11 extends MetaMigration() {

  private implicit val acc = new MessageAccumulator()

  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {
    acc push "Looking up root Org ID"
    val newOwner = {
      val id = Resource.findFqon("root").fold {
        throw new RuntimeException("Could not find root Org.")
      }{ r => r.id }
      ResourceOwnerLink(ResourceIds.Org, id)
    }
      
    val results = Seq(ResourceIds.User, ResourceIds.Group, ResourceIds.Entitlement).map { typeId =>
      val failures = changeOwnerAll(identity, typeId, newOwner).collect { case Failure(e) => e.getMessage }
      if (failures.nonEmpty) {
        Failure(new RuntimeException(failures.mkString("[", ",", "]")))
      } else {
        Success(())
      }
    }

    val failures = results.collect{ case Failure(e) => e.getMessage }
    if (failures.nonEmpty) {
      Left(MigrationStatus(
        status = "FAILURE",
        message = s"There were errors performing this upgrade",
        succeeded = Some(acc.messages),
        errors = Some(Seq(failures.mkString("[", ",", "]")))).toJson)      
    } else {
      acc push "Meta update successful."
      Right(MigrationStatus(
          status = "SUCCESS",
          message = s"Upgrade successfully applied",
          succeeded = Some(acc.messages),
          None).toJson)      
    }
  }
  
  private[migrations] def changeOwnerAll(
      identity: UUID, typeId: UUID, newOwner: ResourceOwnerLink): Seq[Try[GestaltResourceInstance]] = {
    /*
     *  Ensure new owner resource is valid
     */
    val validate = Try {
      if (!Seq(ResourceIds.Org, ResourceIds.User).contains(newOwner.typeId)) {
        throw new BadRequestException(s"New owner resource must be of type Org or User. found: ${newOwner.typeId}")
      }
      
      ResourceFactory.findById(newOwner.typeId, newOwner.id).getOrElse {
        throw new BadRequestException(s"Owner resource with ID '${newOwner.id}' does not exist.")
      }
    }
    
    validate match {
      case Failure(e) => Seq(validate)
      case Success(_) => {
        
        val label = ResourceLabel(typeId)
        
        acc push s"Searching all ${label} for update"
        
        val targets = ResourceFactory.findAll(typeId).collect { 
          case r if r.owner.id != newOwner.id =>
            r.copy(owner = newOwner)
        }
        acc push s"Updating owner of ${targets.size} ${label} to Root Org"
        targets.map(ResourceFactory.update(_, identity))
      }
    }
    
  }  
  
}