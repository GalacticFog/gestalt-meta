package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
// import com.galacticfog.gestalt.data.bootstrap._
// import com.galacticfog.gestalt.meta.api.sdk.{ResourceOwnerLink, _}
// import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.meta.auth._
import play.api.libs.json._
import controllers.util.TypeMethods

import scala.util.{Either, Failure, Left, Right, Success, Try}


class V28() extends MetaMigration with AuthorizationMethods {

  val manager = new HardDeleteInstanceManager[UUID](Map.empty[UUID,(Instance,UUID) => Try[Unit]])

  private implicit val acc = new MessageAccumulator()
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {
    // at the moment of writing these providers don't seem to have been used for any purpose other than testing this feature
    
    def hardDelete(tpe: models.GestaltResourceType): Unit = {
      val deleter = new HardDeleteResourceType[UUID]
      deleter.delete(tpe, identity, true, manager)

      val parentTypes = TypeMethods.getParentTypes(tpe)
      val unlinkErrors = TypeMethods.divorceParents(identity, tpe.id, parentTypes).filter {
        _.isFailure
      }
      log.debug("Testing for unlink errors...")
      if (unlinkErrors.nonEmpty) {
        throw new RuntimeException("There were errors unlinking parent-types.")
      }
      TypeMethods.destroyTypeEntitlements(tpe).get
    }
    
    val process = Try {
      acc push "Preparing to delete obsolete AWS Lambda Provider Resource Type"
      acc push "Looking up Gestalt::Configuration::Provider::AWSLambda"
      TypeFactory.findByName("Gestalt::Configuration::Provider::AWSLambda") match {
        case None => acc push "Resource type was not found, skipping"
        case Some(tpe) => {
          hardDelete(tpe)
          acc push "Deleted"
        }
      }
      acc push "Preparing to delete obsolete AWS API Gateway Provider Resource Type"
      acc push "Looking up Gestalt::Configuration::Provider::AWSAPIGateway"
      TypeFactory.findByName("Gestalt::Configuration::Provider::AWSAPIGateway") match {
        case None => acc push "Resource type was not found, skipping"
        case Some(tpe) => {
          hardDelete(tpe)
          acc push "Deleted"
        }
      }
    }

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