package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.meta.api.sdk._
import play.api.libs.json._
import scala.util.{Either, Success, Failure}

/**
 * Create the UserProfile ResourceType. This type is intended to store personal user preferences
 * and settings.
 */
class V28  extends MetaMigration {
  
  import V28._
  
  private implicit val acc = new MessageAccumulator()
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {  
    acc push "Looking up 'root' org"
    ResourceFactory.findRootOrg match {
      case Failure(e) => handleResultStatus(
          Failure(new RuntimeException(s"Could not locate root Org: ${e.getMessage}")), acc)
      case Success(org) => 
        addTypeToOrg(org.id, USERPROFILE_TYPE_ID, USERPROFILE_TYPE_NAME, identity, payload, acc) {
          createNewResourceType  
        }
    }
  }
  
  def createNewResourceType(org: UUID, creator: GestaltResourceInstance) = {
    createResourceType(
      creator, USERPROFILE_TYPE_ID, USERPROFILE_TYPE_NAME,    
      SystemType(org, ResourceOwnerLink(ResourceIds.User, creator.id),
        typeId = USERPROFILE_TYPE_ID,
        typeName = USERPROFILE_TYPE_NAME,
        desc = Some("Store personal user settings and preferences."),
        extend = Some(ResourceIds.Provider)
      ).withTypeProperties(
        TypeProperty("resource_favorites", "json::list", require = "optional")
      ).withActionInfo(ActionInfo(
        prefix = "userprofile",
        verbs = Seq()
      )).withApiInfo (
          TypeApiInfo(rest_name = "userprofiles"
      )).withLineageInfo(LineageInfo(
        parent_types = Seq(ResourceIds.User)
      ))
    )
  }
}

object V28 {
  val USERPROFILE_TYPE_ID = UUID.fromString("b9e3c356-b6d8-4561-a20a-398b5cd8d62d")
  val USERPROFILE_TYPE_NAME = "Gestalt::Resource::UserProfile"
}