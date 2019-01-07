package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.auth._
import play.api.libs.json._
import scala.util.{Either, Success, Failure}


class V25() extends MetaMigration with AuthorizationMethods {

  import V25._

  private implicit val acc = new MessageAccumulator()
  
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {
    acc push "Looking up 'root' org"
    ResourceFactory.findRootOrg match {
      case Failure(e) => handleResultStatus(
          Failure(new RuntimeException(s"Could not locate root Org: ${e.getMessage}")), acc)
      case Success(org) => 
        addTypeToOrg(org.id, APPDEPLOYMENT_TYPE_ID, APPDEPLOYMENT_TYPE_NAME, identity, payload, acc) {
          createAppDeployment 
        }
    }
  }
  
  def createAppDeployment(org: UUID, creator: GestaltResourceInstance) = {
    createResourceType(
      creator, APPDEPLOYMENT_TYPE_ID, APPDEPLOYMENT_TYPE_NAME,    
      SystemType(org, ResourceOwnerLink(ResourceIds.User, creator.id),
        typeId = APPDEPLOYMENT_TYPE_ID,
        typeName = APPDEPLOYMENT_TYPE_NAME,
        desc = Some("Gestalt Kubernetes Application Deployment"),
        extend = Some(ResourceIds.Configuration)
      ).withActionInfo(ActionInfo(
        prefix = "appdeployment",
        verbs = Seq()
      )).withApiInfo (
          TypeApiInfo(rest_name = "appdeployments"
      )).withLineageInfo(LineageInfo(
        parent_types = Seq(ResourceIds.Org, ResourceIds.Workspace, ResourceIds.Environment)
      ))
    )
  }
}

object V25 {
  val APPDEPLOYMENT_TYPE_ID = UUID.fromString("e97d8674-848a-487e-b640-823fdc39fa10")
  val APPDEPLOYMENT_TYPE_NAME = "Gestalt::Configuration::AppDeployment"
}