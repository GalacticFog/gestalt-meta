package migrations


import java.util.UUID

import com.galacticfog.gestalt.data.bootstrap.{ActionInfo, SystemType, _}
import com.galacticfog.gestalt.data.{ResourceFactory, TypeFactory, session, _}
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceLabel}
import com.galacticfog.gestalt.meta.auth.{Entitlement, EntitlementProps}
import play.api.libs.json.{JsValue, Json}

import scala.util.{Either, Failure, Success, Try}


abstract class MetaMigration() {
  
  def migrate(identity: UUID, payload: Option[JsValue]): Either[JsValue,JsValue]
  
  def test(payload: Option[JsValue]): Option[JsValue] = {
    payload
  }

  private[migrations] def addEntitlementsToInstances(identity: UUID, action: String, typeId: UUID)
                                                    (implicit acc: MessageAccumulator): Seq[Try[Unit]] = {
    val allInstances = ResourceFactory.findAll(typeId)
    val tpe = TypeFactory.findById(typeId).map { t =>
      SystemType.fromResourceType(t)
    }.get

    val parentTypes = tpe.lineage.get.parent_types
    val allParents = parentTypes.flatMap(typeId => ResourceFactory.findAll(typeId))

    val allForUpdate = (allInstances ++ allParents)

    val entitlementExists = ResourceFactory.findEntitlementsByAction(action).map {
      ent => ResourceFactory.findParent(ent.id).get.id
    }

    def hasEntitlement(id: UUID): Boolean = {
      entitlementExists.contains(id)
    }

    acc push s"Adding '$action' entitlement to existing instances. (${allForUpdate.size} resources total)"

    if (allForUpdate.isEmpty) {
      acc push "There are no resource instances to update at this time."
      Seq(Success(()))
    } else {
      allForUpdate.map { p =>
        if (hasEntitlement(p.id)) {
          acc push s"${ResourceLabel(p.typeId)} '${p.id}' already has the entitlement. Skipping..."
          Success(())
        } else {
          val owner = p.owner.id
          val ent = Entitlement(
            id = UUID.randomUUID(),
            org = p.orgId,
            name = s"${p.id}.${action}",
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


  private[migrations] def addVerbToResourceType(identity: UUID, payload: Option[JsValue], verb: String, typeId: UUID)
                                               (implicit acc: MessageAccumulator) = Try {

    acc push s"Looking up Resource Type '$typeId'"
    val tpe = TypeFactory.findById(typeId).getOrElse {
      throw new RuntimeException(s"Could not find resource-type '$typeId'. This is a bug.")
    }

    val actions = tpe.properties.get.get("actions").getOrElse {
      throw new RuntimeException(s"Could not find '.properties.actions' on ${tpe.name}. This is a bug.")
    }

    acc push "Parsing .properties.actions to ActionInfo object."
    val ai = Js.parse[ActionInfo](Json.parse(actions)) match {
      case Failure(e) => {
        println(s"Failed parsing '.properties.actions' on ${tpe.name}")
        throw e
      }
      case Success(a) => a
    }

    acc push s"Adding new verb '$verb' to verbs list '${ai.verbs.mkString(",")}'"
    val newVerbs = (ai.verbs.toSet + verb).toSeq

    val jsonActions = Json.toJson(ai.copy(verbs = newVerbs))
    val newProps = tpe.properties.get ++ Map("actions" -> jsonActions.toString)
    val updated = tpe.copy(properties = Some(newProps))

    acc push "Performing update in Meta"
    TypeFactory.update(updated, identity).get
  }
}
