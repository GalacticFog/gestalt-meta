package migrations

import java.util.UUID
import com.galacticfog.gestalt.data.{DataType, RequirementType}
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import play.api.libs.json._

class V36 extends MetaMigration() {

  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue, JsValue] = {

    implicit val acc = new MessageAccumulator()

    val process = for(
      _ <- idempotentAddPropertyToType(ResourceIds.Lambda, "gpu_support", DataType.id("json"), RequirementType.id("optional"));
      _ <- idempotentAddPropertyToType(ResourceIds.Container, "gpu_support", DataType.id("json"), RequirementType.id("optional"))
    ) yield ()

    handleResultStatus(process)
  }

}
