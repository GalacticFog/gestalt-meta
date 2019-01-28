package migrations

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import play.api.test.PlaySpecification

class V29Spec extends PlaySpecification with MetaRepositoryOps {
  "V29" >> {

    "Add provider_mapping to Environment" >> {
      val tpe = TypeFactory.findById(ResourceIds.Environment)
      PropertyFactory.findByName(tpe.get.id, "provider_mapping") must beSome
    }
  }
}