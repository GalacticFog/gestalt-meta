package migrations

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk._

/**
 * Add `/properties/provider_mapping` to Environment
 */
class V29 extends AddTypePropertyMigration(
  targetType = ResourceIds.Environment,
  newPropertyName = "provider_mapping",
  datatypeId = DataType.id("json"),
  requirementTypeId = RequirementType.id("optional")
)
