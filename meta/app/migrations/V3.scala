package migrations


import java.util.UUID

import scala.util.{Either, Left, Right}
import scala.util.{Try, Success, Failure}

import com.galacticfog.gestalt.data.CoVariant
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.session
import play.api.libs.json._


class V3() extends MetaMigration() {

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
    val container = TypeFactory.findById(ResourceIds.Container) getOrElse {
      throw new RuntimeException(s"Type ${Resources.Container} not found. Ensure database has been initialize.")
    }
    val lambda = TypeFactory.findById(ResourceIds.Lambda) getOrElse {
      throw new RuntimeException(s"Type ${Resources.Lambda} not found. Ensure database has been initialize.")
    }

    PropertyFactory.findByName(apiendpoint.id, "public_url") match {
      case Some(_) =>
        acc push s"Type ${Resources.ApiEndpoint} already had 'public_url' property"
      case None =>
        PropertyFactory.create(apiendpoint.owner.id)(GestaltTypeProperty(
          typeId = ResourceIds.TypeProperty,
          state = ResourceState.id(ResourceStates.Active),
          owner = apiendpoint.owner,
          orgId = apiendpoint.orgId,
          name = "public_url",
          appliesTo = apiendpoint.id,
          datatype = DataType.id("string"),
          isSealed = false,
          isSystem = true,
          requirementType = RequirementType.id("optional"),
          visibilityType = VisibilityType.id("plain")
        )) match {
          case Success(_) =>
            acc push s"Added 'public_url' to Type ${Resources.ApiEndpoint}"
          case Failure(e) =>
            acc push s"ERROR adding 'public_url' to Type ${Resources.ApiEndpoint}: ${e.getMessage}"
        }
    }

    Seq(container, lambda) foreach {
      tpe => PropertyFactory.findByName(tpe.id, "apiendpoints") match {
          case Some(_) =>
            acc push s"Type ${tpe.id} already had 'apiendpoints' property"
          case None =>
            PropertyFactory.create(tpe.owner.id)(GestaltTypeProperty(
              typeId = ResourceIds.TypeProperty,
              state = ResourceState.id(ResourceStates.Active),
              owner = tpe.owner,
              orgId = tpe.orgId,
              name = "apiendpoints",
              appliesTo = tpe.id,
              datatype = DataType.id("json"),
              isSealed = false,
              isSystem = true,
              requirementType = RequirementType.id("optional"),
              visibilityType = VisibilityType.id("plain")
            )) match {
              case Success(_) =>
                acc push s"Added 'apiendpoints' to Type ${tpe.id}"
              case Failure(e) =>
                acc push s"ERROR adding 'apiendpoints' to Type ${tpe.id}: ${e.getMessage}"
            }
        }
    }

    acc push "Migration complete"
  }

}
