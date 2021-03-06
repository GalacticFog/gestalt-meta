package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.meta.api.sdk.{ResourceOwnerLink, _}
import com.galacticfog.gestalt.meta.auth._
import play.api.libs.json._

import scala.util.{Either, Failure, Left, Right, Success, Try}

/**
 * Add "Gestalt::Configuration::Provider::Lambda::AWS" Provider type to Meta.
 */
class V20() extends MetaMigration with AuthorizationMethods {

  import V20._

  private implicit val acc = new MessageAccumulator()
  
  private def schemaProps(schema: ConfigurationSchema) = {
    Map("config_schema" -> Json.stringify(Json.toJson(schema)))
  }
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {
    val awsLambdaProviderSchema = ConfigurationSchema(Seq(
      EnvEntry("AWS_REGION", public = true),
      EnvEntry("AWS_ACCESS_KEY", public = true),
      EnvEntry("AWS_SECRET_KEY", public = true)
    ))
    val process = for {
      root <- {
        acc push "Looking up 'root' org"
        ResourceFactory.findRootOrg
      }
      creator <- Try{
        acc push s"Looking up creator '${identity}'"
        ResourceFactory.findById(ResourceIds.User, identity).getOrElse(throw new RuntimeException(s"Could not locate creator '${identity}'"))
      }
      
      /*
       * TODO: Here we're deriving aws_lambda_provider from lambda_provider, which is technically 
       * incorrect, but should have no meaningful impact on the system in the short term, and solves
       * the issue doumented in https://gitlab.com/galacticfog/gestalt-meta/issues/642. That's the
       * immediate issue that needs to be solved. When we have the time and the testing manpower we
       * can solve this the correct way, which is to define an abstract base lambda_provider type, and 
       * derive concrete Laser and AWS types from that. That change will require changes in the UI, the
       * CLI, and the Installer simultaneously.
       */
      
      awsLambdaTpe <- createResourceType(
        creator, AWS_LAMBDA_PROVIDER_TYPE_ID, AWS_LAMBDA_PROVIDER_TYPE_NAME,
        SystemType(root.id, ResourceOwnerLink(ResourceIds.User, creator.id),
          typeId = AWS_LAMBDA_PROVIDER_TYPE_ID,
          typeName = AWS_LAMBDA_PROVIDER_TYPE_NAME,
          desc = Some("AWS Lambda Provider"),
          //extend = Some(ResourceIds.Provider),
          extend = Some(ResourceIds.LambdaProvider),
          selfProps = schemaProps(awsLambdaProviderSchema)
        ).withTypeProperties(
        ).withActionInfo(ActionInfo(
          prefix = "provider",
          verbs = Seq()
        )).withLineageInfo(LineageInfo(
          parent_types = Seq(ResourceIds.Org, ResourceIds.Workspace, ResourceIds.Environment),
          child_types = Option(Seq(ResourceIds.Environment, ResourceIds.Entitlement))
        ))
      )
    } yield awsLambdaTpe

    process match {
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

}

object V20 {
  val AWS_LAMBDA_PROVIDER_TYPE_ID = UUID.fromString("53261b96-2ab4-49d2-bada-0d4b55661b5c")
  val AWS_LAMBDA_PROVIDER_TYPE_NAME = "Gestalt::Configuration::Provider::Lambda::AWS"
}
