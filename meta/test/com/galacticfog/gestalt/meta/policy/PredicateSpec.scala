package com.galacticfog.gestalt.meta.policy


import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import org.specs2.mutable._
//import org.specs2.specification._
//import org.joda.time.DateTime

//import org.specs2.specification.Scope
import java.util.UUID

import scala.util.Try
import controllers.util.RequestOptions
import com.galacticfog.gestalt.meta.test._
import play.api.libs.json._


class PredicateSpec extends Specification with MetaRepositoryOps {

  sequential
  
  val containerProps = Map(
    "cpus" -> "0.2",
    "memory" -> "1024",
    "num_instances" -> "2",
    "image" -> "nginx",
    "user" -> "gestalt-dev",
    "acceptedResourceRoles" -> """[ "production" ]""",
    "network" -> "BRIDGE",
    "container_type" -> "DOCKER",
    "provider" -> """{ "id" : "foo" }""",
    "boolean_property" -> "true")
    

  val dummyAccount = dummyAuthAccountWithCreds()
  def fakeContainer = newInstance(ResourceIds.Container, "fake-container", properties = Option(containerProps))
  val fakeUser = "fake-user"
  val dummyOpts = RequestOptions(dummyAccount, None, None, None)
  
  def getEvalMap(property: String, operator: String, value: JsValue) = {
    Map("eval_logic" -> 
      Json.stringify(Json.obj("property" -> property, "operator" -> operator, "value" -> value)))
  }
  
  def getRule(policyParent: UUID, predicate: Predicate[JsValue]): Try[GestaltResourceInstance] = Try {
    val (_, ruleId) = createPolicyRule(policyParent, 
        ruleProps = getEvalMap(predicate.property, predicate.operator, predicate.value))

    ResourceFactory.findById(ruleId).getOrElse {
      throw new RuntimeException(s"Could not find rule with ID '${ruleId}'")
    }
  }
  
  def getRuleQuick(predicate: Predicate[JsValue]): Try[GestaltResourceInstance] = {
    val (_,env) = this.createWorkspaceEnvironment()
    getRule(env, predicate)
  }
  
  
  "decide()" should {
    
    "be successful when making a valid comparison" >> {

      val cpuEquals  = Predicate("container.properties.cpus", "equals", JsNumber(0.2))
      val nameEquals = Predicate("container.name", "==", JsString("fake-container"))
      
      val (wrk, env) = createWorkspaceEnvironment()
      
      val rule1 = getRule(env, cpuEquals)
      rule1 must beSuccessfulTry
      
      val rule2 = getRule(env, nameEquals)
      rule2 must beSuccessfulTry

      decideJson(fakeUser, fakeContainer, rule1.get, None, dummyOpts) must beRight
      decideJson(fakeUser, fakeContainer, rule2.get, None, dummyOpts) must beRight
    }

    "fail when making an invalid comparison" in {
      
      val (wrk,env) = this.createWorkspaceEnvironment()      
      val environment = ResourceFactory.findById(env).get
      
      val rule1 = getRule(env, Predicate("environment.name", "==", JsString("NOT-THIS")))
      rule1 must beSuccessfulTry
      val rule2 = getRule(env, Predicate("environment.properties.environment_type", "equals", JsString("dev")))      
      rule2 must beSuccessfulTry
      
      decideJson(fakeUser, environment, rule1.get, None, dummyOpts) must beLeft
      decideJson(fakeUser, environment, rule2.get, None, dummyOpts) must beLeft
    }
    
    "be successful when operator 'is negative' and the property is missing" in { 
      val (wrk,env) = this.createWorkspaceEnvironment()
      val rule = getRule(env, Predicate("missing_property", "!=", JsString("foo")))
      rule must beSuccessfulTry
      
      decideJson(fakeUser, fakeContainer, rule.get, None, dummyOpts) must beRight 
    }
    
    /*
     * TODO: I think this is no longer true since we added 'strict'
     */
    "fail when operator 'is NOT negative' and the property is missing" in {
      val rule = getRuleQuick(Predicate("missing_property", "==", JsString("foo")))
      rule must beSuccessfulTry
      decideJson(fakeUser, fakeContainer, rule.get, None, dummyOpts) must beLeft    
    }.pendingUntilFixed()

    "match JsString -> JsArray" in {
      val rule1 = getRuleQuick(Predicate("container.properties.image", "inlist", JsString("nginx,haproxy")))
      val rule2 = getRuleQuick(Predicate("container.properties.image", "inlist", JsString("httpd,tomcat")))
      
      rule1 must beSuccessfulTry
      rule2 must beSuccessfulTry
      
      decideJson(fakeUser, fakeContainer, rule1.get, None, dummyOpts) must beRight
      decideJson(fakeUser, fakeContainer, rule2.get, None, dummyOpts) must beLeft
    }

    "match JsNumber -> JsArray" in {
      val rule1 = getRuleQuick(Predicate("container.properties.num_instances", "inlist", JsString("1,2,3")))
      val rule2 = getRuleQuick(Predicate("container.properties.num_instances", "inlist", JsString("4,5,6")))

      rule1 must beSuccessfulTry
      rule2 must beSuccessfulTry      
      
      decideJson(fakeUser, fakeContainer, rule1.get, None, dummyOpts) must beRight
      decideJson(fakeUser, fakeContainer, rule2.get, None, dummyOpts) must beLeft 
    }

    "match JsBoolean -> JsArray" in {
      
      import com.galacticfog.gestalt.data.bootstrap._
      
      val typeId = uuid()
      val typeName = uuid.toString
      val prefix = "newresource"
      
      SystemType(dummyRootOrgId, dummyOwner,
        typeId = typeId,
        typeName = typeName,
        extend = Some(ResourceIds.Resource)      
      ).withTypeProperties(
         TypeProperty("boolean_property", "boolean", require = "optional")
      ).withActionInfo(
        ActionInfo(prefix = prefix, verbs = Seq.empty)
      ).withApiInfo (
        TypeApiInfo(rest_name = "newresources")
      ).withLineageInfo(
        LineageInfo(parent_types = Seq(ResourceIds.Workspace))
      ).save()
      
      val resType = TypeFactory.findById(typeId)
      resType must beSome
      
      val res = createInstance(typeId, uuid.toString, properties = Some(Map("boolean_property" -> "true")))
      res must beSuccessfulTry
      
      val rule1 = getRuleQuick(Predicate(s"${prefix}.properties.boolean_property", "inlist", JsString("true,false")))
      val rule2 = getRuleQuick(Predicate(s"${prefix}.properties.boolean_property", "inlist", JsString("false,false")))

      rule1 must beSuccessfulTry
      rule2 must beSuccessfulTry                
      
      decideJson(fakeUser, res.get, rule1.get, None, dummyOpts) must beRight
      decideJson(fakeUser, res.get, rule2.get, None, dummyOpts) must beLeft
      
    }.pendingUntilFixed("Not handling bools properly.")
  }
  
}