package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.sdk._
import play.api.libs.json._
import scala.util.{Try,Either}

/**
 * Add `parent` and `display_name` properties to base Resource type (inherited by ALL resources)
 */
class V29  extends MetaMigration {
  
  private implicit val acc = new MessageAccumulator()
  
  private val baseResource = TypeFactory.findById(ResourceIds.Resource).getOrElse {
    throw new RuntimeException(s"Type ${Resources.Resource} not found. Ensure database has been initialize.")
  }

  private val displayNameProperty = GestaltTypeProperty(
            state = ResourceState.id(ResourceStates.Active),
            orgId = baseResource.orgId,
            owner = baseResource.owner,
            name = "display_name",
            appliesTo = baseResource.id,
            datatype = DataType.id("string"),
            requirementType = RequirementType.id("optional"))
            
  private val parentProperty = GestaltTypeProperty(
            state = ResourceState.id(ResourceStates.Active),
            orgId = baseResource.orgId,
            owner = baseResource.owner,
            name = "parent",
            appliesTo = baseResource.id,
            datatype = DataType.id("json"),
            requirementType = RequirementType.id("optional"))       
  
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {
    val process = for {
      root <- {
        acc push "Looking up 'root' org"
        ResourceFactory.findRootOrg
      }
      creator <- Try{
        acc push s"Looking up creator '${identity}'"
        ResourceFactory.findById(ResourceIds.User, identity).getOrElse(throw new RuntimeException(s"Could not locate creator '${identity}'"))
      }
      _ <- addPropertyTypeToResourceType(baseResource, displayNameProperty)
      _ <- addPropertyTypeToResourceType(baseResource, parentProperty)
    } yield ()
    
    handleResultStatus(process, acc)
  }

}
