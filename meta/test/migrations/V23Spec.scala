package migrations

import com.galacticfog.gestalt.data._
// import com.galacticfog.gestalt.data.models.GestaltTypeProperty
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import play.api.test.PlaySpecification

class V23Spec extends PlaySpecification with MetaRepositoryOps {
  "V23" >> {

    "Add is_http_aware to ApiEndpoint" >> {
      val tpe = TypeFactory.findById(ResourceIds.ApiEndpoint)
      PropertyFactory.findByName(tpe.get.id, "is_http_aware") must beSome
    }
  }

}