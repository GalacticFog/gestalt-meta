package migrations

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models.GestaltTypeProperty
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceLabel}
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import org.specs2.specification.core.Fragments
import play.api.test.PlaySpecification

class V6Spec extends PlaySpecification with MetaRepositoryOps {

  def haveName(name: String) = ((_:GestaltTypeProperty).name) ^^ be_==(name)
  def haveDatatype(dt: String) = ((_:GestaltTypeProperty).datatype) ^^ be_==(DataType.id(dt))
  def haveRequirement(req: String) = ((_:GestaltTypeProperty).requirementType) ^^ be_==(RequirementType.id(req))

  "V6" >> {
    
    "add 'hosts' to ApiEndpoint resource type" >> {
      val Some(tpe) = TypeFactory.findById(ResourceIds.ApiEndpoint)
      PropertyFactory.findByName(tpe.id, "hosts") must beSome(
        haveName("hosts") and haveDatatype("string::list") and haveRequirement("optional")
      )
    }

    "change 'resource' on ApiEndpoint to optional" >> {
      val Some(tpe) = TypeFactory.findById(ResourceIds.ApiEndpoint)
      PropertyFactory.findByName(tpe.id, "resource") must beSome(
        haveName("resource") and haveDatatype("string") and haveRequirement("optional")
      )
    }

  }
  
}