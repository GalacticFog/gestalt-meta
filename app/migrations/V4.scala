package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk._
import controllers.util.JsonInput
import play.api.libs.json._

import scala.util.{Either, Failure, Left, Right, Success, Try}


class V4() extends MetaMigration() with JsonInput {

  private val acc = new MessageAccumulator()

  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {
    perform(identity) match {
      case Success(_) => {
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

  private[migrations] def perform(identity: UUID) = for {
    root <- {
      acc push "Location 'root' org"
      ResourceFactory.findRootOrg
    }
    creator <- Try {
      acc push "Looking up creator"
      ResourceFactory.findById(ResourceIds.User, identity) getOrElse {throw new RuntimeException(s"Could not locate creator with id '${identity}'")}
    }
    _ = acc push "Looking for existing 'gestalt.upgrade' entitlement on org 'root'"
    r <- ResourceFactory.findAllByPropertyValue(ResourceIds.Entitlement, "action", "gestalt.upgrade").headOption.fold {
      acc push s"Did not find existing entitlement for 'gestalt.upgrade', will create under org 'root'"
      CreateNewResource(
        org = root.id,
        creator = creator,
        json = Json.obj(
          "name" -> s"${root.id}.gestalt.upgrade",
          "properties" -> Json.obj(
            "action" -> "gestalt.upgrade",
            "identities" -> Seq(identity),
            "parent" -> Json.obj(
              "id" -> root.id,
              "name" -> root.name
            )
          )
        ),
        typeId = Option(ResourceIds.Entitlement),
        parent = Option(root.id)
      )
    } { ent =>
      acc push s"Founding existing entitlement '${ent.name}'"
      Success(ent)
    }
  } yield acc push "Migration complete"

}
