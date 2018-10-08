package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.sdk._
import play.api.libs.json._

import scala.util.{Either, Failure, Left, Right, Success, Try}


class V12() extends MetaMigration() {

  private implicit val acc = new MessageAccumulator()

  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {

    perform(identity) match {
      case Success(_) => {
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

  private[migrations] def perform(identity: UUID) = Try {
    for {
      lambda <- Try{TypeFactory.findById(ResourceIds.Lambda) getOrElse {
        throw new RuntimeException(s"Type ${Resources.Lambda} not found. Ensure database has been initialize.")
      }}
      t <- addPropertyTypeToResourceType(lambda, GestaltTypeProperty(
        typeId = ResourceIds.TypeProperty,
        state = ResourceState.id(ResourceStates.Active),
        owner = lambda.owner,
        orgId = lambda.orgId,
        name = "pre_warm",
        appliesTo = lambda.id,
        datatype = DataType.id("int"),
        isSealed = false,
        isSystem = true,
        requirementType = RequirementType.id("optional"),
        visibilityType = VisibilityType.id("plain")
      ))
      _ = acc push "Migration complete"
    } yield t
  }

}