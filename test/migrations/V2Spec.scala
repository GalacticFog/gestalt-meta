package migrations

import com.galacticfog.gestalt.data.PropertyFactory
import com.galacticfog.gestalt.data.TypeFactory
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.json.Js
import play.api.libs.json._

import play.api.test.PlaySpecification

/*
 * TODO: This test needs to be fleshed out. For now this is just to test that 'invoke' is
 * added to apiendpoint during unit-test bootstrap.
 */

class V2Spec extends PlaySpecification with MetaRepositoryOps {
  
  "V2" >> {
    
    "add 'invoke' to ApiEndpoint resource type" >> {
      val endpoint = TypeFactory.findById(ResourceIds.ApiEndpoint)
      endpoint must beSome
      
      val actionInfo = {
        val js = Json.parse(endpoint.get.properties.get("actions"))
        Js.parse[ActionInfo](js.as[JsObject])
      }
      actionInfo must beSuccessfulTry
      actionInfo.get.verbs.contains("invoke") === true
    }
    
  }
  
}