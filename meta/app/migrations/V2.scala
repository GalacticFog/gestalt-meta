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
import com.galacticfog.gestalt.meta.auth._
import com.galacticfog.gestalt.meta.api.output._


class V2 extends MetaMigration() {

  private val acc = new MessageAccumulator()
  
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {

    val process = for {
      // This updates the ApiEndpoint Type with the invoke verb.
      _ <- addInvokeVerbToApiEndpointType(identity, payload)
      x <- {
        // This creates the 'apiendpoint.invoke' entitlement on all appropriate resources.
        val failures = addInvokeEntitlements(identity).collect { case Failure(e) => e.getMessage }
        if (failures.nonEmpty) {
          Failure(new RuntimeException(failures.mkString("[", ",", "]")))
        } else {
          Success(())
        }
      }
    } yield x

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
  
  
  private[migrations] def addInvokeEntitlements(identity: UUID): Seq[Try[Unit]] = {
    /*
     * Add 'apiendpoint.invoke' Entitlement to: 
     * - All existing ApiEndpoints
     * - All apiendpoint.parentType instances
     */
    
    val allEndpoints = ResourceFactory.findAll(ResourceIds.ApiEndpoint) 
    val tpe = TypeFactory.findById(ResourceIds.ApiEndpoint).map { t =>
      SystemType.fromResourceType(t)
    }.get
    
    val action = "apiendpoint.invoke"
    
    /*
     * Gather all existing resources that *can be* parent to an ApiEndpoint 
     */
    val parentTypes = tpe.lineage.get.parent_types
    val allParents = parentTypes.flatMap(typeId => ResourceFactory.findAll(typeId))
    
    /*
     * The full set of resources to update is the union all ApiEndpoints and all possible
     * ApiEndpoint parents.
     */
    val allForUpdate = (allEndpoints ++ allParents)
    
    /*
     * Here we gather all resources that *already* have the 'invoke' entitlement.
     */
    val entitlementExists = ResourceFactory.findEntitlementsByAction(action).map {
      ent => ResourceFactory.findParent(ent.id).get.id
    }
    
    def hasEntitlement(id: UUID): Boolean = {
      entitlementExists.contains(id)
    }
    
    acc push s"Adding 'apiendpoint.invoke' entitlement to existing instances. (${allForUpdate.size} resources total)"
    
    if (allForUpdate.isEmpty) {
      /*
       * There were no resources in the system requiring update - there's nothing to do.
       */
      acc push "There are no resource instances to update at this time."
      Seq(Success(()))
    } else {
    
      allForUpdate.map { p =>
        if (hasEntitlement(p.id)) {
          /*
           * Resource already has invoke entitlement - there's nothing to do.
           */
          acc push s"${ResourceLabel(p.typeId)} '${p.id}' already has the entitlement. Skipping..."
          Success(())
        } else {
          /*
           * Resource *does not* have the invoke entitlement. Create new entitlement and attach to resource.
           */
          val owner = p.owner.id
          val ent = Entitlement(
              id = UUID.randomUUID(),
              org = p.orgId,
              name = s"${p.id}.apiendpoint.invoke",
              properties = EntitlementProps(action, None, Some(Seq(identity, owner))))
          
          ResourceFactory.create(ResourceIds.User, identity)(
              Entitlement.toGestalt(identity, ent), parentId = Some(p.id)) match {
            case Failure(e) => throw e
            case Success(_) => {
              acc push s"Entitlement added to ${ResourceLabel(p.typeId)} '${p.id}'"
              Success(())
            }
          }
        }
      }
    }
  }
  
  
  private[migrations] def addInvokeVerbToApiEndpointType(identity: UUID, payload: Option[JsValue]) = Try {
    
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
    TypeFactory.update(updated, identity).get
  }
  
}