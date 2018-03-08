package migrations


import java.util.UUID

import scala.util.{Either, Left, Right}
import scala.util.{Try, Success, Failure}

import com.galacticfog.gestalt.data.CoVariant
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.json._
import com.galacticfog.gestalt.data.session
import play.api.libs.json._


class V2 extends MetaMigration() {

  private val acc = new MessageAccumulator()
  
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {

    addInvokeVerbToApiEndpointType(identity, payload)

  }
  
  private[migrations] def addInvokeVerbToApiEndpointType(identity: UUID, payload: Option[JsValue]) = {
    
    acc push s"Looking up Resource Type ApiEndpoint '${ResourceIds.ApiEndpoint}'"
    val endpoint = TypeFactory.findById(ResourceIds.ApiEndpoint).getOrElse {
      throw new RuntimeException("Could not find resource-type ApiEndpoint. This is a bug.")
    }

    val actions = endpoint.properties.get.get("actions").getOrElse {
      throw new RuntimeException("Could not find 'apiendpoint.properties.actions'. This is a bug.")
    }
    
    acc push "Parsing endpoint.properties.actions to ActionInfo object."
    val ai = Js.parse[ActionInfo](Json.parse(actions)) match {
      case Failure(e) => {
        println("Failed parsing 'apiendpoint.properties.actions'")
        throw e
      }
      case Success(a) => a
    }
    
    acc push "Adding new verb 'invoke' to verbs list."
    val newVerbs = {
      if (ai.verbs.contains("invoke")) ai.verbs
      else ("invoke" +: ai.verbs)
    }
    
    val jsonActions = Json.toJson(ai.copy(verbs = newVerbs))
    val newProps = endpoint.properties.get ++ Map("actions" -> jsonActions.toString)
    val updated = endpoint.copy(properties = Some(newProps))
    
    acc push "Performing update in Meta"
    TypeFactory.update(updated, identity) match {
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