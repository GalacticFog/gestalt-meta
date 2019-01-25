package migrations


import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models.GestaltTypeProperty
import com.galacticfog.gestalt.meta.api.sdk.ResourceLabel
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import org.specs2.specification.core.Fragments
import play.api.test.PlaySpecification

class V3Spec extends PlaySpecification with MetaRepositoryOps {

  def haveName(name: String) = ((_:GestaltTypeProperty).name) ^^ be_==(name)
  def haveDatatype(dt: String) = ((_:GestaltTypeProperty).datatype) ^^ be_==(DataType.id(dt))
  def haveRequirement(req: String) = ((_:GestaltTypeProperty).requirementType) ^^ be_==(RequirementType.id(req))

  "V3" >> {
    
    "add 'public_url' to ApiEndpoint resource type" >> {
      val tpe = TypeFactory.findById(ResourceIds.ApiEndpoint)
      tpe must beSome
      PropertyFactory.findByName(tpe.get.id, "public_url") must beSome(
        haveName("public_url") and haveDatatype("string") and haveRequirement("optional")
      )
    }

    Fragments.foreach(Seq(ResourceIds.Lambda, ResourceIds.Container)) { tpeId =>
      s"add 'apiendpoints' to ${ResourceLabel(tpeId)} resource type" ! {
        PropertyFactory.findByName(tpeId, "apiendpoints") must beSome(
          haveName("apiendpoints") and haveDatatype("json") and haveRequirement("optional")
        )
      } ^ br
    }

  }
  
}