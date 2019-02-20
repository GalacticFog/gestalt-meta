package migrations

import com.galacticfog.gestalt.data.{DataType, RequirementType}
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds

/**
  * Add `/properties/gpu_support` to Lambda
  */
class V35 extends AddTypePropertyMigration(
  targetType = ResourceIds.Lambda,
  newPropertyName = "gpu_support",
  datatypeId = DataType.id("json"),
  requirementTypeId = RequirementType.id("optional")
)
