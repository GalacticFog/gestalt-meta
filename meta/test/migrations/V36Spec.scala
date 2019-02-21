package migrations

import com.galacticfog.gestalt.data.{PropertyFactory, TypeFactory}
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import play.api.test.PlaySpecification

class V36Spec extends PlaySpecification with MetaRepositoryOps {
  "V36" >> {

    "Add gpu_support to Lambda" >> {
      val tpe = TypeFactory.findById(ResourceIds.Lambda)
      PropertyFactory.findByName(tpe.get.id, "gpu_support") must beSome
    }
  }
}
