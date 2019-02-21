package migrations

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models.{GestaltResourceType, GestaltTypeProperty}
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import play.api.test.PlaySpecification

class V35Spec extends PlaySpecification with MetaRepositoryOps {
  def haveName(name: String) = ((_:GestaltTypeProperty).name) ^^ be_==(name)
  def haveDatatype(dt: String) = ((_:GestaltTypeProperty).datatype) ^^ be_==(DataType.id(dt))
  def haveRequirement(req: String) = ((_:GestaltTypeProperty).requirementType) ^^ be_==(RequirementType.id(req))

  "V35" >> {

    "create Gestalt::Resource::Task resource type" >> {
      TypeFactory.findById(V35.TASK_TYPE_ID) must beSome( ((_:GestaltResourceType).name) ^^ be_==(V35.TASK_TYPE_NAME) )
      TypeFactory.findByName(V35.TASK_TYPE_NAME) must beSome( ((_:GestaltResourceType).id) ^^ be_==(V35.TASK_TYPE_ID) )
    }

    "run multiple times without error" >> {
      new V35().migrate(uuid(), None) must beRight
      new V35().migrate(uuid(), None) must beRight
    }

    "have properties.message" >> {
      PropertyFactory.findByName(V35.TASK_TYPE_ID, "message") must beSome(
       haveName("message") and haveDatatype("string") and haveRequirement("optional")
      )
    }
  }
}