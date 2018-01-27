package controllers.util


import java.util.UUID
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.laser.{LaserEndpoint, LaserLambda}
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
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
import play.api.libs.json._
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
import com.galacticfog.gestalt.json._


class PolicyMethodsSpec extends PlaySpecification with MetaRepositoryOps {

  val pm = new PolicyMethods {}
  
  "transformPolicy" should {
    
    val policy1 = Json.obj("name" -> uuid.toString)
    val policy2 = Json.obj("name" -> uuid.toString, "properties" -> Json.obj())
    
    "inject a `parent` link into the properties collection" >> {
      val (parentId, _) = createWorkspaceEnvironment()
      
      Js.find(policy1, "/properties/parent/id") must beNone
      
      val newpolicy = pm.transformPolicy(policy1, Map("parent" -> parentId.toString))
      
      newpolicy must beSuccessfulTry
      
      val test = Js.find(newpolicy.get.as[JsObject], "/properties/parent/id")
      
      test must beSome
      
      UUID.fromString(test.get.as[String]) === parentId
    }
    
    "succeed whether or not a properties key exists or not" >> {
    
      val (parentId, _) = createWorkspaceEnvironment()
      
      Js.find(policy2, "/properties/parent/id") must beNone
      
      val newpolicy = pm.transformPolicy(policy1, Map("parent" -> parentId.toString))
      
      newpolicy must beSuccessfulTry
      
      val test = Js.find(newpolicy.get.as[JsObject], "/properties/parent/id")
      
      test must beSome
      
      UUID.fromString(test.get.as[String]) === parentId
    }
    
    "replace an existing parent-link if one already exists" >> {
      
      val oldParentId = uuid()
      val (parentId, _) = createWorkspaceEnvironment()
      
      val policy3 = policy1 ++ Json.obj("properties" -> Json.obj("parent" -> Json.obj("id" -> oldParentId)))
      
      val old = Js.find(policy3, "/properties/parent/id")
      old must beSome
      UUID.fromString(old.get.as[String]) === oldParentId
      
      val newpolicy = pm.transformPolicy(policy3, Map("parent" -> parentId.toString))
      
      val test = Js.find(newpolicy.get.as[JsObject], "/properties/parent/id")
      
      test must beSome
      
      UUID.fromString(test.get.as[String]) === parentId
    }
    
    "fail if the data-map is missing the `parent` key" >> {
      val (parentId, _) = createWorkspaceEnvironment()
      
      Js.find(policy1, "/properties/parent/id") must beNone
      
      val newpolicy = pm.transformPolicy(policy1, Map.empty)
      
      newpolicy must beFailedTry
    }
    
    "fail if the given parent-id is invalid" >> {
      
      Js.find(policy1, "/properties/parent/id") must beNone
      
      val badParentId = uuid.toString
      val newpolicy = pm.transformPolicy(policy1, Map("parent" -> badParentId))
      
      newpolicy must beFailedTry
    }
  }
  
  "transformRule" should {

    val rule1 = Json.parse(s"""
      |{
      |  "name":"Env-Container-User-Limit",
      |  "resource_type":"limit",
      |  "properties":{
      |    "actions":["container.create", "container.update"],
      |    "eval_logic":{
      |      "property": "container.properties.user",
      |      "operator": "inList",
      |      "value": "alpha, bravo, charlie, NONE"
      |    }
      |  }
      |}          
      """.stripMargin).as[JsObject]
    
    "set /resource_type to typeId value from data-map" >> {
      val (wrk, _) = createWorkspaceEnvironment()
      val (policy, _) = createPolicyRule(wrk)
      
      val newrule = {
        val r = pm.transformRule(rule1, Map("typeId" -> ResourceIds.RuleLimit.toString, "parent" -> policy.toString))
        r must beSuccessfulTry
        r.get
      }
      
      val test = Js.find(newrule.as[JsObject], "/resource_type")
      
      test must beSome
      test.get.as[String] === ResourceIds.RuleLimit.toString
    }
        
    "set /properties/parent to the `parent` value from the data-map" >> {
      val (wrk, _) = createWorkspaceEnvironment()
      val (policy, _) = createPolicyRule(wrk)
      
      val newrule = {
        val r = pm.transformRule(rule1, Map("typeId" -> ResourceIds.RuleLimit.toString, "parent" -> policy.toString))
        r must beSuccessfulTry
        r.get
      }
      
      val test = Js.find(newrule.as[JsObject], "/properties/parent")
      test must beSome
      UUID.fromString(test.get.as[String]) === policy
    }
    
    "set /defined_at to `parent.properties.parent`" >> {
      // defined_at should be the id of the parent policy's parent.
      
      val (wrk, _) = createWorkspaceEnvironment()
      val (policy, _) = createPolicyRule(wrk)
      
      val newrule = {
        val r = pm.transformRule(rule1, Map("typeId" -> ResourceIds.RuleLimit.toString, "parent" -> policy.toString))
        r must beSuccessfulTry
        r.get
      }
      println(Json.prettyPrint(newrule))
      val test = Js.find(newrule.as[JsObject], "/properties/defined_at/parent/id")
      test must beSome
      UUID.fromString(test.get.as[String]) === wrk
    }

    "fail if parentId is missing from data-map" >> {
      pm.transformRule(rule1, Map("typeId" -> ResourceIds.RuleLimit.toString)) must beFailedTry
    }
    
    "fail if typeId is missing from data-map" >> {
      pm.transformRule(rule1, Map("parent" -> uuid.toString)) must beFailedTry
    }
        
    "fail if the Policy named by parentId does not exist" >> {
      val (wrk, _) = createWorkspaceEnvironment()
      val badParentPolicy = uuid.toString
      
      pm.transformRule(rule1, Map(
          "typeId" -> ResourceIds.RuleLimit.toString, 
          "parent" -> badParentPolicy)) must beFailedTry

    }

  }
  
}



