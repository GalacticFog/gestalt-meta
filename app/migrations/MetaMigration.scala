package migrations

import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import play.api.libs.json._


import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ListBuffer
import java.util.UUID
import scala.util.{Either,Left,Right}

case class StepMessage(step: Int, message: String)
object StepMessage { implicit lazy val stepMessageFormat = Json.format[StepMessage] }

class MessageAccumulator() {  
  val messages = new ListBuffer[StepMessage]()
  
  private val step = new AtomicInteger(0)
  private def next = step.incrementAndGet()
  
  def push(message: String) = {
    messages += StepMessage(next, message)
  }
  
  def pop(): StepMessage = {
    val last = messages.last
    messages -= last
    last
  }
  def toJson() = Json.toJson(messages.toSeq)
}
object MessageAccumulator {
  def apply() = new MessageAccumulator()
}

case class MigrationStatus(status: String, message: String, succeeded: Option[Seq[StepMessage]], errors: Option[Seq[String]]) {
  def toJson() = Json.toJson(this)
}


abstract class MetaMigration(version: String, identity: UUID) {

  def migrate(payload: Option[JsValue]): Either[JsValue,JsValue]
}