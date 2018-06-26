package migrations

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models.GestaltTypeProperty
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import play.api.test.PlaySpecification

class V10Spec extends PlaySpecification with MetaRepositoryOps {

  def haveName(name: String) = ((_:GestaltTypeProperty).name) ^^ be_==(name)
  def haveDatatype(dt: String) = ((_:GestaltTypeProperty).datatype) ^^ be_==(DataType.id(dt))
  def haveRequirement(req: String) = ((_:GestaltTypeProperty).requirementType) ^^ be_==(RequirementType.id(req))

  "V10" >> {

    "add 'secrets' to Lambda resource type" >> {
      val tpe = TypeFactory.findById(ResourceIds.Lambda)
      tpe must beSome
      PropertyFactory.findByName(tpe.get.id, "secrets") must beSome(
        haveName("secrets") and haveDatatype("json::list") and haveRequirement("optional")
      )
    }

  }

}