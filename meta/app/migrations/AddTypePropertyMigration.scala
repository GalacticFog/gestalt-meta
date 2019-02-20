package migrations

import java.util.UUID

import com.galacticfog.gestalt.data.models.GestaltTypeProperty
import com.galacticfog.gestalt.data.{PropertyFactory, ResourceState, TypeFactory}
import com.galacticfog.gestalt.meta.api.sdk.ResourceStates
import play.api.libs.json.JsValue

import scala.util.{Either, Failure, Left, Right, Success}

class AddTypePropertyMigration(targetType: UUID,
                               newPropertyName: String,
                               datatypeId: UUID,
                               requirementTypeId: UUID) extends MetaMigration() {

  protected val acc = new MessageAccumulator()

  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue, JsValue] = {

    /*
     * Get a reference to the Resource Type you want to enhance...
     */
    acc push "Looking up Container Resource Type..."
    val tpe = TypeFactory.findById(targetType).get

    /*
     * Check if the property already exists on the type...
     */
    val exists =
      PropertyFactory.findByType(targetType).exists(_.name == newPropertyName)

    val process = for {
      _ <- if (exists) {
        acc push s"Property $newPropertyName already exists. Nothing to do."
        Success(())
      } else {
        val newProperty = GestaltTypeProperty(
          state = ResourceState.id(ResourceStates.Active),
          orgId = tpe.orgId,
          owner = tpe.owner,
          name = newPropertyName,
          appliesTo = tpe.id,
          datatype = datatypeId,
          requirementType = requirementTypeId)

        acc push s"Adding /properties/$newPropertyName to $targetType Type."
        addPropertyTypeToResourceType(tpe, newProperty)(acc)
      }
    } yield ()

    process match {
      case Success(_) =>
        Right(MigrationStatus(
          status = "SUCCESS",
          message = "Upgrade successfully applied",
          succeeded = Some(acc.messages),
          None).toJson())
      case Failure(e) =>
        Left(MigrationStatus(
          status = "FAILURE",
          message = s"There were errors performing this upgrade: ${e.getMessage}",
          succeeded = Some(acc.messages),
          errors = Some(Seq(e.getMessage))).toJson())
    }
  }

}
