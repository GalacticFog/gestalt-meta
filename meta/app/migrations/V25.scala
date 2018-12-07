package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.auth._
import play.api.libs.json._

import scala.util.{Either, Failure, Left, Right, Success, Try}


sealed trait ProcessState
object ProcessState {
  case object NothingToDo extends ProcessState
  case object Continue extends ProcessState
}

class V25() extends MetaMigration with AuthorizationMethods {

  import V25._

  private implicit val acc = new MessageAccumulator()
  
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {
    
    val process: Try[_] = 
      testExistingType(APPDEPLOYMENT_TYPE_ID, APPDEPLOYMENT_TYPE_NAME, acc).map { state =>
        state match {
          case ProcessState.NothingToDo => Success(())
          case ProcessState.Continue => for {
            root <- {
              acc push "Looking up 'root' org"
              ResourceFactory.findRootOrg
            }
            creator <- Try{
              acc push s"Looking up creator '${identity}'"
              ResourceFactory.findById(ResourceIds.User, identity).getOrElse {
                throw new RuntimeException(s"Could not locate creator '${identity}'")
              }
            }
            _ <- {
              acc push s"Creating Type ${APPDEPLOYMENT_TYPE_NAME}"
              createAppDeployment(root.id, creator)      
            }
          } yield ()
        }
      }
    
    handleResultStatus(process, acc)
  }
  
  def handleResultStatus[A](result: Try[A], acc: MessageAccumulator): Either[JsValue,JsValue] = {
    result match {
      case Success(_) => {
        acc push "Meta update successful."
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
  
  /**
   * Test if a ResourceType exists with the same name and ID as given.
   * 
   * @return ProcessState indicating how to proceed. If a type is found by either name or ID both values must match
   * the arguments given. If they do not match an error is thrown. If they match, return NothingToDo. If neither value
   * matches, return Continue.
   */
  def testExistingType(typeId: UUID, typeName: String, acc: MessageAccumulator): Try[ProcessState] = Try {
    val ad1 = TypeFactory.findByName(typeName)
    (if (ad1.isEmpty) ProcessState.Continue 
    else {
      // Type name exists - check that it has the expected ID
      if (ad1.get.id == typeId) {
        acc push s"Type ${typeName} with ID ${typeId} already exists. Nothing to do."
        ProcessState.NothingToDo
      } else {
        // Type exists but ID does not match expected - this is an error.
        throw new ConflictException(s"Type ${typeName} already exists, but its ID is unexpected. expected: ${typeId}, found: ${ad1.get.id}")
      }
    }) match {
      case ProcessState.NothingToDo => ProcessState.NothingToDo
      case ProcessState.Continue => {
        val ad2 = TypeFactory.findById(typeId)
        if (ad2.isEmpty) ProcessState.Continue
        else {
          // Type with ID already exists - check it has expected name
          if (ad2.get.name == typeName) {
            acc push s"Type ${typeName} with ID ${typeId} already exists. Nothing to do."
            ProcessState.NothingToDo
          } else {
            // Type with ID exists, buy name does not match expected - error.
            throw new ConflictException(s"Type with ID ${typeId} found, but name is unexpected. expected: ${typeName}. found: ${ad2.get.name}")
          }
        }
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