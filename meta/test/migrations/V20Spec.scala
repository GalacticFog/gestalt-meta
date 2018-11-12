package migrations

import com.galacticfog.gestalt.data._
// import com.galacticfog.gestalt.data.models.GestaltTypeProperty
// import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import play.api.test.PlaySpecification

class V20Spec extends PlaySpecification with MetaRepositoryOps {
  "V20" >> {

    "create AWS Lambda resource type" >> {
      val tpe = TypeFactory.findById(V20.AWS_LAMBDA_PROVIDER_TYPE_ID)
      tpe must beSome
    }
  }

}