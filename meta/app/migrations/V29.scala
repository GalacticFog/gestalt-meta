package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.sdk._
import play.api.libs.json._
import scala.util.{Either, Failure, Left, Right, Success}

/*
 * 
 * Add `/properties/provider_mapping` to Environment
 * 
 */
class V29 extends MetaMigration() {

  implicit val acc = new MessageAccumulator()
  private val TARGET_TYPE = ResourceIds.Environment
  private val NEW_PROPERTY_NAME = "provider_mapping"
  
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {
  
    /*
     * Get a reference to the Resource Type you want to enhance...
     */
    acc push "Looking up Container Resource Type..."
    val tpe = TypeFactory.findById(TARGET_TYPE).get
    
    /*
     * Check if the property already exists on the type...
     */
    val exists = 
      PropertyFactory.findByType(TARGET_TYPE).exists(_.name == NEW_PROPERTY_NAME)

    val process = for {
      _ <- if (exists) {
        acc push s"Property ${NEW_PROPERTY_NAME} already exists. Nothing to do."
        Success(())
        
      } else {
        val newProperty = GestaltTypeProperty(
            state = ResourceState.id(ResourceStates.Active),
            orgId = tpe.orgId,
            owner = tpe.owner,
            name = NEW_PROPERTY_NAME,
            appliesTo = tpe.id,
            datatype = DataType.id("json"),
            requirementType = RequirementType.id("optional"))
        
        acc push s"Adding /properties/${NEW_PROPERTY_NAME} to ${TARGET_TYPE} Type."
        addPropertyTypeToResourceType(tpe, newProperty)(acc)         
      }
    } yield ()

    process match {
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
  
}
