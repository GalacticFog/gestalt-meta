package controllers.util

//import java.util.UUID
import com.galacticfog.gestalt.data.ResourceFactory
//import com.galacticfog.gestalt.data.bootstrap._
//import com.galacticfog.gestalt.meta.api.errors._
//import com.galacticfog.gestalt.meta.api.sdk
//import com.galacticfog.gestalt.meta.api.sdk.{ GestaltTypePropertyInput, ResourceIds }

import com.galacticfog.gestalt.meta.test._
import play.api.test.PlaySpecification
import play.api.libs.json._
import com.galacticfog.gestalt.events._


class SafeRequestSpec extends PlaySpecification with MetaRepositoryOps {
  
  val emt = new EventMethodsTrait {
    def eventsClient() = {
      mock[AmqpClient]
    }    
  }

  "findEffectiveRules" should {
    
    "return matching rules when they exist" >> {
      val (wrk, env) = createWorkspaceEnvironment()
      ResourceFactory.findById(wrk) must beSome
      ResourceFactory.findById(env) must beSome
      
      val lambda = newDummyLambda(env)
      lambda must beSuccessfulTry
      
      val matchEvent = "environment.create"
      val (policyId, ruleId) = createEventPolicy(
          wrk, lambda.get.id, Seq(Json.obj("action" -> matchEvent)))
      
      ResourceFactory.findById(policyId) must beSome
      ResourceFactory.findById(ruleId) must beSome      
      
      val rules = emt.findEffectiveEventRules(wrk, Some(matchEvent))
      rules.size === 1
    }
    
    
  }
}