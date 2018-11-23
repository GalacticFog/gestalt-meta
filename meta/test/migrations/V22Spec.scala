package migrations

import com.galacticfog.gestalt.data._
// import com.galacticfog.gestalt.data.models.GestaltTypeProperty
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import play.api.test.PlaySpecification

class V22Spec extends PlaySpecification with MetaRepositoryOps {
  "V22" >> {

    "Add lambda.migrate to Lambda" >> {
      val Some(tpe) = TypeFactory.findById(ResourceIds.Lambda)
      tpe.properties.get.get("actions") must beSome("""{"prefix":"lambda","verbs":["invoke","migrate"]}""")
    }
  }

}