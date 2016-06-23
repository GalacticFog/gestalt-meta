
import java.util.UUID
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds

package object controllers {
  def ActionPrefix(typeId: UUID) = typeId match {
    case ResourceIds.Org => "org"
    case ResourceIds.User => "user"
    case ResourceIds.Group => "group"
    case ResourceIds.Workspace => "workspace"
    case ResourceIds.Environment => "environment"
    case ResourceIds.Lambda => "lambda"
    case ResourceIds.Container => "container"
    case e => throw new IllegalArgumentException(s"Unknown action-prefix '$typeId'")
  }
}