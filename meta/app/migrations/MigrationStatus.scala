package migrations


import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ListBuffer
import play.api.libs.json._


case class MigrationStatus(status: String, message: String, succeeded: Option[Seq[StepMessage]], errors: Option[Seq[String]]) {
  def toJson() = Json.toJson(this)
}

case class StepMessage(step: Int, message: String)

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