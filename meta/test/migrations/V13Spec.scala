package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models.{GestaltResourceType, GestaltTypeProperty}
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import play.api.test.PlaySpecification

class V13Spec extends PlaySpecification with MetaRepositoryOps {

  def haveName(name: String) = ((_:GestaltTypeProperty).name) ^^ be_==(name)
  def haveDatatype(dt: String) = ((_:GestaltTypeProperty).datatype) ^^ be_==(DataType.id(dt))
  def haveRequirement(req: String) = ((_:GestaltTypeProperty).requirementType) ^^ be_==(RequirementType.id(req))

  "V13" >> {

    "create ::Volume resource type" >> {
      TypeFactory.findById(V13.VOLUME_TYPE_ID) must beSome( ((_:GestaltResourceType).name) ^^ be_==(V13.VOLUME_TYPE_NAME) )
      TypeFactory.findByName(V13.VOLUME_TYPE_NAME) must beSome( ((_:GestaltResourceType).id) ^^ be_==(V13.VOLUME_TYPE_ID) )
    }

    "be idempotent" >> {
      val v13 = new V13()
      v13.migrate(UUID.fromString(dummyOwner.id)) must beRight
    }


  }

}