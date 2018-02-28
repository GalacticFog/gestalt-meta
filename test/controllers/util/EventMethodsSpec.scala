package controllers.util


import java.util.UUID
import com.galacticfog.gestalt.data.{ResourceFactory, TypeFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.laser.{LaserEndpoint, LaserLambda}
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.{GestaltResourceInput, HostConfig, JsonClient, ResourceIds, ResourceStates}
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp, PatchOps}
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import controllers.SecurityResources
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.JsonMatchers
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification.{BeforeAll, Scope}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.{JsBoolean, JsValue, Json, JsObject}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.{FakeRequest, PlaySpecification}
import play.api.mvc._
import play.api.mvc.Results._
import play.api.http.HttpVerbs._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.ws.WSResponse
import com.galacticfog.gestalt.laser._
import play.api.http.HttpVerbs

import scala.concurrent.Future
import scala.util.Success
import com.galacticfog.gestalt.data.bootstrap._


class EventMethodsSpec extends PlaySpecification with MetaRepositoryOps {
  
  
  //def findEffectiveEventRules(parentId: UUID, event: Option[String] = None)
  
//  "findEffectiveEventRules" should {
//    
//    "find all event rules up the tree corresponding to the given event" >> {
//      val (wrk, env) = createWorkspaceEnvironment(dummyRootOrgId)
//      
//      val testLambda = newDummyLambda(env) 
//      testLambda must beSuccessfulTry
//      
//      val action = "resource.verb.post"
//      val (orgPolicy, orgRule) = createEventRule(dummyRootOrgId, testLambda.get.id, action)
//      val (wrkPolicy, wrkRule) = createEventRule(wrk, testLambda.get.id, action)
//      val (envPolicy, envRule) = createEventRule(env, testLambda.get.id, action)
//      
//      ResourceFactory.findById(ResourceIds.RuleEvent, orgRule) must beSome
//      ResourceFactory.findById(ResourceIds.RuleEvent, wrkRule) must beSome
//      ResourceFactory.findById(ResourceIds.RuleEvent, envRule) must beSome
//      
//      
//      val em = new EventMethods{}
//      
//      val effective = em.findEffectiveEventRules2(env, Option(action))
//      
//      println("****EFF.SIZE : " + effective.size)
//      failure
//    }
//    
//  }
  
}