package controllers.util

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Step
import play.api.libs.json._

import java.util.UUID

import com.galacticfog.gestalt.meta.test._

import play.api.test._
import play.api.test.Helpers._
import controllers.PolicyController

class MetaControllerSpec  extends PlaySpecification with GestaltProviderMocking with ResourceScope with BeforeAll {
  
  sequential
  
  override def beforeAll(): Unit = pristineDatabase
  
  abstract class App extends WithDbController[PolicyController](containerApp()) {
    /*
     * Using PolicyController here is incidental - I just need something
     * that extends MetaController in order to access its members.
     */
    //val mc = app.injector.instanceOf[PolicyController]         
  }
  
  "resolveTypeFromPayload" should {
    
    "return a valid resource_type UUID when given a valid UUID" in new App { 
      val f = controller.resolveTypeFromPayload _
      
      f(GoodUUIDs.org)         must beSome(ResourceIds.Org)
      f(GoodUUIDs.workspace)   must beSome(ResourceIds.Workspace)
      f(GoodUUIDs.environment) must beSome(ResourceIds.Environment)
      f(GoodUUIDs.eventrule)   must beSome(ResourceIds.RuleEvent)
      f(GoodUUIDs.limitrule)   must beSome(ResourceIds.RuleLimit)
      f(GoodUUIDs.api)         must beSome(ResourceIds.Api)
      f(GoodUUIDs.apiendpoint) must beSome(ResourceIds.ApiEndpoint)
    }
    
    "return a valid resource_type UUID when given a valid resource type NAME" in new App { 
      val f = controller.resolveTypeFromPayload _
      
      f(GoodNames.org)         must beSome(ResourceIds.Org)
      f(GoodNames.workspace)   must beSome(ResourceIds.Workspace)
      f(GoodNames.environment) must beSome(ResourceIds.Environment)
      f(GoodNames.eventrule)   must beSome(ResourceIds.RuleEvent)
      f(GoodNames.limitrule)   must beSome(ResourceIds.RuleLimit)
      f(GoodNames.api)         must beSome(ResourceIds.Api)
      f(GoodNames.apiendpoint) must beSome(ResourceIds.ApiEndpoint)      
    }
    
    "fail when given invalid UUIDs" in new App { 
      controller.resolveTypeFromPayload(BadUUIDs.org) must throwAn[UnprocessableEntityException]
    }
    
    "fail when given invalid type names" in new App { 
      controller.resolveTypeFromPayload(BadUUIDs.org) must throwAn[UnprocessableEntityException]
    }
    
  }
}

object GoodUUIDs {
      val org = Json.parse(s"""{ "name": "org-uuid", "resource_type": "${ResourceIds.Org}"}""")
      val workspace = Json.parse(s"""{ "name": "workspace-uuid", "resource_type": "${ResourceIds.Workspace}"}""")
      val environment = Json.parse(s"""
        |{ 
        |  "name": "environment-uuid", 
        |  "resource_type": "${ResourceIds.Environment}",
        |  "properties": {
        |    "environment_type": "test"
        |  }
        |}
        """.trim.stripMargin)
        
      val eventrule = Json.parse(s"""
        |{
        |  "name":"event-rule-uuid",
        |  "resource_type":"${ResourceIds.RuleEvent}",
        |  "properties":{
        |    "actions":["container.postcreate"],
        |    "lambda":"{{lambda-1}}"
        |  }
        |}        
        """.trim.stripMargin)
        
      val limitrule = Json.parse(s"""
        |{
        |  "name":"limit-rule-uuid",
        |  "resource_type":"Gestalt::Resource::Rule::Limit",
        |  "properties":{
        |    "actions":["container.create", "container.update"],
        |    "eval_logic":{
        |      "property": "container.properties.user",
        |      "operator": "inList",
        |      "value": "alpha, bravo, charlie, NONE"
        |    }
        |  }
        |}  
        """.trim.stripMargin)
        
      val api = Json.parse(s"""
        |{
        |  "name": "api-3",
        |  "resource_type":"${ResourceIds.Api}",                
        |  "description": "My first API",
        |  "properties": {
        |  	"provider": {
        |  		"id": "{{gateway-manager}}",
        |  		"locations": ["{{kong-provider}}"]
        |  	}
        |  }
        |}        
        """.trim.stripMargin)
      
      val apiendpoint = Json.parse(s"""  
        |{
        |  "name":"endpoint-1",
        |  "resource_type":"${ResourceIds.ApiEndpoint}",        
        |  "properties":{
        |    "resource": "/path",
        |    "upstream_url": "http://example.com/{{lambda-1}}/invoke",
        |    "implementation_id": "{{lambda-1}}"
        |  }
        |}        
        """.trim.stripMargin)        
        
}

object GoodNames {
      val org = Json.parse(s"""{ "name": "org-uuid", "resource_type": "${Resources.Org}"}""")
      val workspace = Json.parse(s"""{ "name": "workspace-uuid", "resource_type": "${Resources.Workspace}"}""")
      val environment = Json.parse(s"""
        |{ 
        |  "name": "environment-uuid", 
        |  "resource_type": "${Resources.Environment}",
        |  "properties": {
        |    "environment_type": "test"
        |  }
        |}
        """.trim.stripMargin)
        
      val eventrule = Json.parse(s"""
        |{
        |  "name":"event-rule-uuid",
        |  "resource_type":"${Resources.RuleEvent}",
        |  "properties":{
        |    "actions":["container.postcreate"],
        |    "lambda":"{{lambda-1}}"
        |  }
        |}        
        """.trim.stripMargin)
        
      val limitrule = Json.parse(s"""
        |{
        |  "name":"limit-rule-uuid",
        |  "resource_type":"${Resources.RuleLimit}",
        |  "properties":{
        |    "actions":["container.create", "container.update"],
        |    "eval_logic":{
        |      "property": "container.properties.user",
        |      "operator": "inList",
        |      "value": "alpha, bravo, charlie, NONE"
        |    }
        |  }
        |}  
        """.trim.stripMargin)
        
      val api = Json.parse(s"""
        |{
        |  "name": "api-3",
        |  "resource_type":"${Resources.Api}",                
        |  "description": "My first API",
        |  "properties": {
        |  	"provider": {
        |  		"id": "{{gateway-manager}}",
        |  		"locations": ["{{kong-provider}}"]
        |  	}
        |  }
        |}        
        """.trim.stripMargin)
      
      val apiendpoint = Json.parse(s"""  
        |{
        |  "name":"endpoint-1",
        |  "resource_type":"${Resources.ApiEndpoint}",        
        |  "properties":{
        |    "resource": "/path",
        |    "upstream_url": "http://example.com/{{lambda-1}}/invoke",
        |    "implementation_id": "{{lambda-1}}"
        |  }
        |}        
        """.trim.stripMargin)
}

object BadUUIDs {
  val org = Json.parse(s"""{ "name": "org-uuid", "resource_type": "${uuid.toString}"}""")
}
object BadName {
  val org = Json.parse(s"""{ "name": "org-uuid", "resource_type": "Gestalt::Resource::Foo"}""")
}