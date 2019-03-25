package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.meta.api.sdk._
import play.api.libs.json._
import scala.util.{Either, Success, Failure, Try}

/**
 * Create the UserProfile ResourceType. This type is intended to store personal user preferences
 * and settings.
 */
class V30  extends MetaMigration {
  
  import V30._
  
  private implicit val acc = new MessageAccumulator()
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {  
    acc push "Looking up 'root' org"
    ResourceFactory.findRootOrg match {
      case Failure(e) => handleResultStatus(
          Failure(new RuntimeException(s"Could not locate root Org: ${e.getMessage}")))
      case Success(org) => 
//        addTypeToOrg(org.id, USERPROFILE_TYPE_ID, USERPROFILE_TYPE_NAME, identity, payload, acc) {
//          createNewResourceType  
//        }
//        
        val process: Try[_] = for {
          a <- addTypeToOrgTry(org.id, USERPROFILE_TYPE_ID, USERPROFILE_TYPE_NAME, identity, payload, acc) {
                createNewResourceType  
              }
          b <- {
            setUserPermissions(identity)
          }
          c <- {
            idempotentAddPropertyToType(USERPROFILE_TYPE_ID, "avatar", DataType.id("string"), RequirementType.id("optional"))            
          }
        } yield c
        
        handleResultStatus(process)(acc)
    }
  }
  
  import com.galacticfog.gestalt.meta.auth._
  import scala.util.Try
  
  def setUserPermissions(identity: UUID): Try[Unit] = {
    acc push "Looking up existing users for Entitlement setting..."
    
    val allUsers = ResourceFactory.findAll(ResourceIds.User)
    val updated: Seq[Try[GestaltResourceInstance]] = 
      if (allUsers.isEmpty) {
        acc push "No users found. Nothing to do."
        Seq.empty[Try[GestaltResourceInstance]]
      } else {
        acc push s"${allUsers.size} users found for update. Updating entitlements for 'userprofile.*'"      
        allUsers.flatMap { user =>
          ResourceFactory.findDescendantEntitlementsFuzzy(user.id, "%userprofile%").map { ent =>
            ResourceFactory.update(
                Entitlement.addIdentitiesToResource(ent, Seq(user.id)),
                identity)
          }
        }
      }
    val (success, failures) = updated.partition(_.isSuccess)
    
    if (failures.nonEmpty) {
      val message = failures.collect { case Failure(e) => e.getMessage }.mkString("[", ",", "]")
      acc push "There was an error setting Entitlements."
      Failure(new RuntimeException(s"Failed setting entitlements: $message"))  
    } else {
      acc push "Entitlements updated successfully"
      Success(())
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
        TypeProperty("avatar", "string", require = "optional"),  
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

object V30 {
  val USERPROFILE_TYPE_ID = UUID.fromString("b9e3c356-b6d8-4561-a20a-398b5cd8d62d")
  val USERPROFILE_TYPE_NAME = "Gestalt::Resource::UserProfile"
}




