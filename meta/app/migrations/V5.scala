package migrations

import java.util.UUID

import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.data.{session, _}
import com.galacticfog.gestalt.json._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.auth._
import play.api.libs.json._

import scala.util.{Either, Failure, Left, Right, Success, Try}


class V5 extends MetaMigration() {

  private val acc = new MessageAccumulator()
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {

    val process = for {
      // This updates the Container Type with the import verb.
      _ <- addImportVerbToContainerType(identity, payload)
      x <- {
        // This creates the 'container.import' entitlement on all appropriate resources.
        val failures = addContainerImportEntitlements(identity).collect { case Failure(e) => e.getMessage }
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


  private[migrations] def addContainerImportEntitlements(identity: UUID): Seq[Try[Unit]] = {
    /*
     * Add 'container.import' Entitlement to:
     * - All existing Containers
     * - All container.parentType instances
     */

    val allContainers = ResourceFactory.findAll(ResourceIds.Container)
    val tpe = TypeFactory.findById(ResourceIds.Container).map { t =>
      SystemType.fromResourceType(t)
    }.get

    val action = "container.import"

    /*
     * Gather all existing resources that *can be* parent to an Container
     */
    val parentTypes = tpe.lineage.get.parent_types
    val allParents = parentTypes.flatMap(typeId => ResourceFactory.findAll(typeId))

    /*
     * The full set of resources to update is the union all Containers and all possible
     * Container parents.
     */
    val allForUpdate = (allContainers ++ allParents)

    /*
     * Here we gather all resources that *already* have the 'import' entitlement.
     */
    val entitlementExists = ResourceFactory.findEntitlementsByAction(action).map {
      ent => ResourceFactory.findParent(ent.id).get.id
    }

    def hasEntitlement(id: UUID): Boolean = {
      entitlementExists.contains(id)
    }

    acc push s"Adding 'container.import' entitlement to existing instances. (${allForUpdate.size} resources total)"

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
           * Resource already has import entitlement - there's nothing to do.
           */
          acc push s"${ResourceLabel(p.typeId)} '${p.id}' already has the entitlement. Skipping..."
          Success(())
        } else {
          /*
           * Resource *does not* have the import entitlement. Create new entitlement and attach to resource.
           */
          val owner = p.owner.id
          val ent = Entitlement(
              id = UUID.randomUUID(),
              org = p.orgId,
              name = s"${p.id}.container.import",
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


  private[migrations] def addImportVerbToContainerType(identity: UUID, payload: Option[JsValue]) = Try {
    
    acc push s"Looking up Resource Type Container '${ResourceIds.Container}'"
    val container = TypeFactory.findById(ResourceIds.Container).getOrElse {
      throw new RuntimeException("Could not find resource-type Container. This is a bug.")
    }

    val actions = container.properties.get.get("actions").getOrElse {
      throw new RuntimeException("Could not find 'container.properties.actions'. This is a bug.")
    }
    
    acc push "Parsing container.properties.actions to ActionInfo object."
    val ai = Js.parse[ActionInfo](Json.parse(actions)) match {
      case Failure(e) => {
        println("Failed parsing 'container.properties.actions'")
        throw e
      }
      case Success(a) => a
    }
    
    acc push "Adding new verb 'import' to verbs list."
    val newVerbs = (ai.verbs.toSet + "import").toSeq

    val jsonActions = Json.toJson(ai.copy(verbs = newVerbs))
    val newProps = container.properties.get ++ Map("actions" -> jsonActions.toString)
    val updated = container.copy(properties = Some(newProps))
    
    acc push "Performing update in Meta"
    TypeFactory.update(updated, identity).get
  }
  
}