package migrations


import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import play.api.test.PlaySpecification


class V32Spec extends PlaySpecification with MetaRepositoryOps {
  
  "V32" should {

    "Change Rule 'match_actions' type to json::list" >> {
      val prop = PropertyFactory.findByName(ResourceIds.Rule, "match_actions") 
      prop must beSome
      prop.get.datatype === DataType.id("json::list")
    }


    "Run multiple times successfully" >> {
      new V32().migrate(uuid(), None) must beRight
      new V32().migrate(uuid(), None) must beRight
    }

  }
  
}
