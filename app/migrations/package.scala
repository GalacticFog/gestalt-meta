
import play.api.libs.json._

package object migrations {
  implicit lazy val migrationStatusFormat = Json.format[MigrationStatus]   
}