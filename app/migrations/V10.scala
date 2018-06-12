package migrations

import java.util.UUID

import com.galacticfog.gestalt.meta.api.sdk._
import play.api.libs.json._

import scala.util.{Either, Failure, Left, Right, Success}


class V10 extends MetaMigration() {

  private implicit val acc = new MessageAccumulator()

  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {

    val process = for {
      _ <- addVerbToResourceType(identity, payload, verb = "viewmetrics", typeId = ResourceIds.LambdaProvider)
      x <- {
        val failures = addEntitlementsToInstances(identity, action = "provider.viewmetrics", typeId = ResourceIds.LambdaProvider).collect { case Failure(e) => e.getMessage }
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

}
