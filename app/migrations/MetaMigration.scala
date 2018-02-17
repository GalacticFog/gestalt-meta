package migrations


import java.util.UUID
import scala.util.Either
import play.api.libs.json.JsValue


abstract class MetaMigration() {
  
  def migrate(identity: UUID, payload: Option[JsValue]): Either[JsValue,JsValue]
  
}
