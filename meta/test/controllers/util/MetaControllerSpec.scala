package controllers.util

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.data._
import play.api.libs.json._

import com.galacticfog.gestalt.meta.test._
import play.api.test._
import controllers.{Meta, SecurityResources}
import org.specs2.matcher.JsonMatchers


class MetaControllerSpec  extends PlaySpecification with MetaRepositoryOps with JsonMatchers {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  sequential
  
  override def beforeAll(): Unit = pristineDatabase

  abstract class App extends WithDbController[Meta](containerApp())

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
  
import com.galacticfog.gestalt.data.bootstrap.ActionInfo
import com.galacticfog.gestalt.data.bootstrap.LineageInfo
import com.galacticfog.gestalt.data.bootstrap.SystemType

  "CreateWithEntitlements(JSON)" should {
    
    "create a resource with appropriate entitlements when given good data" in new App {
      
      val parentTypeId = uuid()
      val childTypeId = uuid()

      val parentPrefix = "parent"
      val childPrefix = "child"
      
      SystemType(dummyRootOrgId, dummyOwner,
        typeId      = childTypeId, 
        typeName    = childTypeId.toString,
        extend      = Some(ResourceIds.Resource)
      ).withActionInfo(
        ActionInfo(
            prefix = childPrefix, 
            verbs = Seq("spin", "flip"))    
      ).save()      
      
      val childType = TypeFactory.findById(childTypeId)
      childType must beSome
      
      SystemType(dummyRootOrgId, dummyOwner,
        typeId      = parentTypeId, 
        typeName    = parentTypeId.toString,
        extend      = Some(ResourceIds.Resource)
      ).withActionInfo(
        ActionInfo(
            prefix = parentPrefix, 
            verbs = Seq.empty)
      ).withLineageInfo(
        LineageInfo(
          parent_types = Seq(ResourceIds.Org),
          child_types  = Option {
            Seq(childTypeId)})            
      ).save()

      val parentType = TypeFactory.findById(parentTypeId)
      parentType must beSome

      /*
       * Creating an instance of parent-type should set entitlements for itself plus child-types.
       */

      val mc = new MetaControllerUtils {}

      val user = dummyAuthAccountWithCreds()
      val parent = mc.CreateWithEntitlements(dummyRootOrgId, user, Json.obj("name" -> uuid.toString), parentTypeId, Some(dummyRootOrgId))
      parent must beSuccessfulTry
      
      /*
       * There should be 10 entitlements on parent: 
       * 4 CRUD parent
       * 4 CRUD child
       * 2 custom child actions (spin, flip)
       */
      
      val ents = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, parent.get.id)
      ents.sortBy(_.name) foreach { e => println(s"*** : ${e.properties.get("action")}") }
      
      ents.size === 10
      val actions = ents.map(_.properties.get("action"))
      
      actions.contains(parentPrefix + ".create") === true
      actions.contains(parentPrefix + ".view") === true
      actions.contains(parentPrefix + ".update") === true
      actions.contains(parentPrefix + ".delete") === true
      
      actions.contains(childPrefix + ".create") === true
      actions.contains(childPrefix + ".view") === true
      actions.contains(childPrefix + ".update") === true
      actions.contains(childPrefix + ".delete") === true
      actions.contains(childPrefix + ".spin") === true
      actions.contains(childPrefix + ".flip") === true

    }
    
    "fail when given a bad type-id" in new App {
      
      val mc = new MetaControllerUtils {}
      val user = dummyAuthAccountWithCreds()
      val badTypeId = uuid()
      
      mc.CreateWithEntitlements(dummyRootOrgId, user, 
          Json.obj("name" -> uuid.toString), 
          badTypeId, Some(dummyRootOrgId)) must beFailedTry      
    }
    
  }

//  "linked_provider creation" should {
//
//    "set `type` and `typeId` fields" in new App {
//      Ents.setNewEntitlements(dummyRootOrgId, dummyRootOrgId, user, None)
//      val Success(upstream) = createInstance(ResourceIds.LoggingProvider, "upstream-logging-provider",
//        parent = Option(dummyRootOrgId),
//        properties = Option(Map("parent" -> "{}"))
//      )
//
//      val fakeRequest = controller.SecuredRequest[JsValue](user, mock[DummyAuthenticator], FakeRequest(HttpVerbs.POST, "/root/providers").withBody[JsValue](Json.obj()))
//      val downstream = controller.postProviderCommon(dummyRootOrgId, ResourceIds.Org.toString, dummyRootOrgId, Json.obj(
//        "name" -> "downstream-caas-provider",
//        "description" -> "",
//        "resource_type" -> ResourceName(ResourceIds.KubeProvider),
//        "properties" -> Json.obj(
//          "config" -> Json.obj()
//        )
//      ))(fakeRequest)
//
//      contentAsString(downstream) must /("properties") /#(0) /("type" -> ResourceName(ResourceIds.KubeProvider))
//      contentAsString(downstream) must /("properties") /#(0) /("typeId" -> ResourceIds.KubeProvider.toString)
//    }
//
//  }

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