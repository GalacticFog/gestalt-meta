package migrations

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models.{GestaltResourceType, GestaltTypeProperty}
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import play.api.test.PlaySpecification

class V33Spec extends PlaySpecification with MetaRepositoryOps {
  def haveName(name: String) = ((_:GestaltTypeProperty).name) ^^ be_==(name)
  def haveDatatype(dt: String) = ((_:GestaltTypeProperty).datatype) ^^ be_==(DataType.id(dt))
  def haveRequirement(req: String) = ((_:GestaltTypeProperty).requirementType) ^^ be_==(RequirementType.id(req))

  "V33" >> {

    "create ::Job resource type" >> {
      TypeFactory.findById(V33.JOB_TYPE_ID) must beSome( ((_:GestaltResourceType).name) ^^ be_==(V33.JOB_TYPE_NAME) )
      TypeFactory.findByName(V33.JOB_TYPE_NAME) must beSome( ((_:GestaltResourceType).id) ^^ be_==(V33.JOB_TYPE_ID) )
    }
  }
}