package migrations

import java.util.UUID

import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data.{session, _}
import com.galacticfog.gestalt.meta.api.sdk._
import play.api.libs.json._

import scala.util.{Either, Failure, Left, Right, Success, Try}


class V10() extends MetaMigration() {

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
        name = "secrets",
        appliesTo = lambda.id,
        datatype = DataType.id("json::list"),
        isSealed = false,
        isSystem = true,
        requirementType = RequirementType.id("optional"),
        visibilityType = VisibilityType.id("plain")
      ))
      datafeed <- Try{TypeFactory.findById(V7.DATA_FEED_TYPE_ID) getOrElse {
        throw new RuntimeException(s"Type ${V7.DATA_FEED_TYPE_ID} not found. Ensure database has been initialize.")
      }}
      t <- addPropertyTypeToResourceType(datafeed, GestaltTypeProperty(
        typeId = ResourceIds.TypeProperty,
        state = ResourceState.id(ResourceStates.Active),
        owner = datafeed.owner,
        orgId = datafeed.orgId,
        name = "credentials",
        appliesTo = datafeed.id,
        datatype = DataType.id("json"),
        isSealed = false,
        isSystem = true,
        requirementType = RequirementType.id("optional"),
        visibilityType = VisibilityType.id("plain")
      ))
      _ = acc push "Migration complete"
    } yield t
  }

}
