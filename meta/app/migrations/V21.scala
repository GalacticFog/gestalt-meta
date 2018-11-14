package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.sdk._
import play.api.libs.json._

import scala.util.{Failure, Success, Try}


/*
 * 
 * Add `/properties/provider_subtype` to ::CaasProvider
 * 
 */
class V21 extends MetaMigration() {

  private val acc = new MessageAccumulator()
  private val TARGET_TYPE = ResourceIds.Lambda
  
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {
  
    /*
     * Get a reference to the Resource Type you want to enhance...
     */
    acc push "Looking up Lambda Resource Type..."
    val tpe = TypeFactory.findById(TARGET_TYPE).get

    def result1(): Try[Unit] = {
      
      if (PropertyFactory.findByType(TARGET_TYPE).exists(_.name == "aws_role_id")) {
        acc push s"Property aws_role_id already exists. Nothing to do."
        Success(())
        
      } else {
        val newProperty = GestaltTypeProperty(
            state = ResourceState.id(ResourceStates.Active),
            orgId = tpe.orgId,
            owner = tpe.owner,
            name = "aws_role_id",
            appliesTo = tpe.id,
            datatype = DataType.id("string"),
            requirementType = RequirementType.id("optional"))
        
        acc push s"Adding /properties/aws_role_id to ${TARGET_TYPE}."
        addPropertyTypeToResourceType(tpe, newProperty)(acc) map { _ => () }
      }
   }

    def result2(): Try[Unit] = {
      
      if (PropertyFactory.findByType(TARGET_TYPE).exists(_.name == "aws_function_id")) {
        acc push s"Property aws_function_id already exists. Nothing to do."
        Success(())
        
      } else {
        val newProperty = GestaltTypeProperty(
            state = ResourceState.id(ResourceStates.Active),
            orgId = tpe.orgId,
            owner = tpe.owner,
            name = "aws_function_id",
            appliesTo = tpe.id,
            datatype = DataType.id("string"),
            requirementType = RequirementType.id("optional"))
        
        acc push s"Adding /properties/aws_function_id to ${TARGET_TYPE}."
        addPropertyTypeToResourceType(tpe, newProperty)(acc) map { _ => () }
      }
   }

   val result = result1() flatMap { _ => result2() }

    result match {
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
 