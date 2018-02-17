
import play.api.libs.json._

package object migrations {
  
  implicit lazy val stepMessageFormat = Json.format[StepMessage]
  implicit lazy val migrationStatusFormat = Json.format[MigrationStatus]
  
}