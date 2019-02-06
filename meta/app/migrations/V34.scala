package migrations


import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.sdk._
import play.api.libs.json._

import scala.util.Either
import scala.util.{Failure, Success, Try}    


/**
 * Add Gestalt::Configuration::Provider::Git Provider type to Meta.
 */
class V34 extends MetaMigration() {
  
  import V34._
  
  implicit val acc = new MessageAccumulator()
    
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {  
    acc push "Looking up 'root' org"
    ResourceFactory.findRootOrg match {
      case Failure(e) => handleResultStatus(
          Failure(new RuntimeException(s"Could not locate root Org: ${e.getMessage}")), acc)
      case Success(org) => 
        addTypeToOrg(org.id, GIT_PROVIDER_TYPE_ID, GIT_PROVIDER_TYPE_NAME, identity, payload, acc) {
          createNewResourceType  
        }
    }
  }
  
  def createNewResourceType(org: UUID, creator: GestaltResourceInstance): Try[GestaltResourceType] = {
    val descriptionText = "This provider points at git repos for a workspace. It also has the ability to save/export gestalt configurations to git." 
    createResourceType(
      creator, GIT_PROVIDER_TYPE_ID, GIT_PROVIDER_TYPE_NAME,    
      SystemType(org, ResourceOwnerLink(ResourceIds.User, creator.id),
        typeId = GIT_PROVIDER_TYPE_ID,
        typeName = GIT_PROVIDER_TYPE_NAME,
        desc = Some(descriptionText),
        extend = Some(ResourceIds.Provider)
      ).withActionInfo(ActionInfo(
        prefix = "provider",
        verbs = Seq("export")
      )).withApiInfo (
          TypeApiInfo(rest_name = "providers"
      )).withLineageInfo(LineageInfo(
        parent_types = Seq(ResourceIds.Org, ResourceIds.Workspace, ResourceIds.Environment)
      ))
    )
  }
}

object V34 {
  val GIT_PROVIDER_TYPE_ID = UUID.fromString("8b3d347a-cd8f-4491-afe9-e8c7ed042317")
  val GIT_PROVIDER_TYPE_NAME = "Gestalt::Configuration::Provider::Git"
}
