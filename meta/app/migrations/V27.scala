package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.meta.api.sdk._
import play.api.libs.json._
import scala.util.{Either, Success, Failure}

/**
 * Create the Provider::Catalog Resource Type in the root Org.
 */
class V27  extends MetaMigration {
  
  import V27._
  
  private implicit val acc = new MessageAccumulator()
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {  
    acc push "Looking up 'root' org"
    ResourceFactory.findRootOrg match {
      case Failure(e) => handleResultStatus(
          Failure(new RuntimeException(s"Could not locate root Org: ${e.getMessage}")), acc)
      case Success(org) => 
        addTypeToOrg(org.id, CATALOG_TYPE_ID, CATALOG_TYPE_NAME, identity, payload, acc) {
          createCatalogType  
        }
    }
  }
  
  def createCatalogType(org: UUID, creator: GestaltResourceInstance) = {
    createResourceType(
      creator, CATALOG_TYPE_ID, CATALOG_TYPE_NAME,    
      SystemType(org, ResourceOwnerLink(ResourceIds.User, creator.id),
        typeId = CATALOG_TYPE_ID,
        typeName = CATALOG_TYPE_NAME,
        desc = Some("Manage an AppDeployment Catalog."),
        extend = Some(ResourceIds.Provider)
      ).withActionInfo(ActionInfo(
        prefix = "provider",
        verbs = Seq()
      )).withApiInfo (
          TypeApiInfo(rest_name = "providers"
      )).withLineageInfo(LineageInfo(
        parent_types = Seq(ResourceIds.Org, ResourceIds.Workspace, ResourceIds.Environment)
      ))
    )
  }
}

object V27 {
  val CATALOG_TYPE_ID = UUID.fromString("13e40777-52f5-4768-bbc4-a08128fcae3b")
  val CATALOG_TYPE_NAME = "Gestalt::Configuration::Provider::Catalog"
}