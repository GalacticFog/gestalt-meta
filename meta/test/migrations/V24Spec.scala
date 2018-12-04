package migrations

import com.galacticfog.gestalt.data._
// import com.galacticfog.gestalt.data.models.GestaltTypeProperty
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import play.api.test.PlaySpecification

class V24Spec extends PlaySpecification with MetaRepositoryOps {
  "V24" >> {

    "create AWS API Gateway Provider resource type" >> {
      val tpe = TypeFactory.findById(V24.AWS_API_GATEWAY_PROVIDER_TYPE_ID)
      tpe must beSome
    }

    "Add `whitelist` to ApiEndpoint" >> {
      val tpe = TypeFactory.findById(ResourceIds.ApiEndpoint)
      PropertyFactory.findByName(tpe.get.id, "whitelist") must beSome
    }
  }

}