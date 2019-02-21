package com.galacticfog.gestalt.meta.policy

//import java.util.UUID
//import com.galacticfog.gestalt.data.ResourceFactory
//import com.galacticfog.gestalt.data.bootstrap._
//import com.galacticfog.gestalt.meta.api.errors._
//import com.galacticfog.gestalt.meta.api.sdk
//import com.galacticfog.gestalt.meta.api.sdk.{ GestaltTypePropertyInput, ResourceIds }

import com.galacticfog.gestalt.meta.test._
import play.api.test.PlaySpecification
import play.api.libs.json._

//import com.galacticfog.gestalt.events._

class RuleMatchActionSpec extends PlaySpecification with MetaRepositoryOps {

  "seqFromString" >> {
    "parse a Seq[RuleMatchAction] from a well-formed JSON string" >> {
      val acts = Seq(
        Json.obj("action" -> "foo"),
        Json.obj("action" -> "bar", "meta_function" -> "suppress"))
      val rs = RuleMatchAction.seqFromString(Json.toJson(acts).toString)
      rs must beSuccessfulTry
      rs.get.size === 2
    }
    
    "fail if the JSON is invalid" >> {
      val acts = Seq(
          Json.obj("action" -> "foo"),
          Json.obj("invalid" -> "property"))
      RuleMatchAction.seqFromString(Json.toJson(acts).toString) must beFailedTry          
    }
  }
}