package migrations

import java.util.UUID

import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data.{session, _}
import com.galacticfog.gestalt.meta.api.sdk._
import play.api.libs.json._

import scala.util.{Either, Failure, Left, Right, Success, Try}


class V6() extends MetaMigration() {

  private val acc = new MessageAccumulator()


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
    /*
     * Get all the resource-types upfront.
     */
    val apiendpoint = TypeFactory.findById(ResourceIds.ApiEndpoint) getOrElse {
      throw new RuntimeException(s"Type ${Resources.ApiEndpoint} not found. Ensure database has been initialize.")
    }

    PropertyFactory.findByName(apiendpoint.id, "hosts") match {
      case Some(_) =>
        acc push s"Type ${Resources.ApiEndpoint} already had 'hosts' property"
      case None =>
        PropertyFactory.create(apiendpoint.owner.id)(GestaltTypeProperty(
          typeId = ResourceIds.TypeProperty,
          state = ResourceState.id(ResourceStates.Active),
          owner = apiendpoint.owner,
          orgId = apiendpoint.orgId,
          name = "hosts",
          appliesTo = apiendpoint.id,
          datatype = DataType.id("string::list"),
          isSealed = false,
          isSystem = true,
          requirementType = RequirementType.id("optional"),
          visibilityType = VisibilityType.id("plain")
        )) match {
          case Success(_) =>
            acc push s"Added 'hosts' to Type ${Resources.ApiEndpoint}"
          case Failure(e) =>
            acc push s"ERROR adding 'hosts' to Type ${Resources.ApiEndpoint}: ${e.getMessage}"
            throw new RuntimeException(s"ERROR adding 'hosts' to Type ${Resources.ApiEndpoint}: ${e.getMessage}")
        }
    }

    PropertyFactory.findByName(apiendpoint.id, "resource") match {
      case None =>
        acc push s"ERROR: Type ${Resources.ApiEndpoint} did not have 'resource' property"
        throw new RuntimeException(s"ERROR: Type ${Resources.ApiEndpoint} did not have 'resource' property")
      case Some(tpe) =>
        PropertyFactory.update(
          tpe.copy(
            requirementType = RequirementType.id("optional")
          ),
          tpe.owner.id
        ) match {
          case Success(_) =>
            acc push s"Changed 'resource' to 'optional' on Type ${Resources.ApiEndpoint}"
          case Failure(e) =>
            acc push s"ERROR changing 'resource' to 'optional' on Type ${Resources.ApiEndpoint}"
            throw new RuntimeException(s"ERROR changing 'resource' to 'optional' on Type ${Resources.ApiEndpoint}")
        }
    }

    acc push "Migration complete"
  }

}
