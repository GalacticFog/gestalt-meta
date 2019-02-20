package migrations


import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.sdk._
//import com.galacticfog.gestalt.meta.api.errors._
import play.api.libs.json._

import scala.util.Either
import scala.util.{Failure, Success, Try}    


/**
 * Add suppress flag to `/properties/match_actions` to Event
 * Change datatype of `rules.properties.match_actions` from 'string::list' to 'json::list'
 * 
 * Depends-On: V1 (Change Rule 'actions' to 'match_actions')
 */
class V32 extends MetaMigration() {
  
  implicit val acc = new MessageAccumulator()
  
  private val TARGET_PROPERTY_NAME = "match_actions"  
  private val TARGET_PROPERTY_TYPE = "json::list"

  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue, JsValue] = {
    val result = for {
      property <- safeLookupPropertyByType(ResourceIds.Rule, TARGET_PROPERTY_NAME)
      updated  <- updatePropertyDatatype(property, TARGET_PROPERTY_TYPE, identity)
    } yield updated
    
    handleResultStatus(result, acc)
  }
  
  
  private[migrations] def updatePropertyDatatype(prop: GestaltTypeProperty, newType: String, identity: UUID)
                                                (implicit acc: MessageAccumulator): Try[GestaltTypeProperty] = Try {
    val newTypeId = DataType.id(TARGET_PROPERTY_TYPE)
    
    if (prop.datatype == newTypeId) {
      acc push s"Property '${TARGET_PROPERTY_NAME}' already set to '${TARGET_PROPERTY_TYPE}'. Nothing to do."
      prop 
    } else {
      acc push s"Updating property datatype from '${DataType.name(prop.datatype)}' to '${TARGET_PROPERTY_TYPE}'"
      val updatedProperty = prop.copy(datatype = newTypeId)
      
      acc push s"Writing updated property to Meta."
      PropertyFactory.update(updatedProperty, identity) match {
        case Success(property) => {
          acc push "Successfully wrote updated property to Meta."
          property
        }
        case Failure(e) => {
          acc push s"Failed writing updated property to Meta: ${e.getMessage}"
          throw e
        }        
      }
    }
  }

}
 