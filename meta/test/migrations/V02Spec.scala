package migrations


import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.json.Js
import play.api.libs.json._

import play.api.test.PlaySpecification
import java.util.UUID

/*
 * TODO: This test needs to be fleshed out. For now this is just to test that 'invoke' is
 * added to apiendpoint during unit-test bootstrap.
 */

class V2Spec extends PlaySpecification with MetaRepositoryOps {
  
  private val v2 = new V2()
  
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
    
    
    "addInvokeEntitlements" should {
      
      "add 'apiendpoint.invoke' to any existing ApiEndpoints" >> {
        
        val (wrk, env) = createWorkspaceEnvironment()
        val api = createApi(parent = Some(env))
        val ep1 = createApiEndpoint(api.get.id, properties = Some(Map("resource" -> "/foo")))
        val ep2 = createApiEndpoint(api.get.id, properties = Some(Map("resource" -> "/foo")))
        val ep3 = createApiEndpoint(api.get.id, properties = Some(Map("resource" -> "/foo")))
        
        ep1 must beSuccessfulTry
        ep2 must beSuccessfulTry
        ep3 must beSuccessfulTry
        
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, ep1.get.id).isEmpty === true
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, ep2.get.id).isEmpty === true
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, ep3.get.id).isEmpty === true
        
        v2.addInvokeEntitlements(UUID.fromString(dummyOwner.id))

        val x = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, ep1.get.id)
        val y = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, ep2.get.id)
        val z = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, ep3.get.id)        
        
        x.isEmpty === false
        x.head.properties.get("action") === "apiendpoint.invoke"
        y.isEmpty === false
        y.head.properties.get("action") === "apiendpoint.invoke"
        z.isEmpty === false
        z.head.properties.get("action") === "apiendpoint.invoke"        

      }
      
      "add 'apiendpoint.invoke' to any existing Apis" >> {
        val (wrk, env) = createWorkspaceEnvironment()
        val api1 = createApi(parent = Some(env))
        val api2 = createApi(parent = Some(env))
        val api3 = createApi(parent = Some(env))
        
        api1 must beSuccessfulTry
        api2 must beSuccessfulTry
        api3 must beSuccessfulTry
        
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, api1.get.id).isEmpty === true
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, api2.get.id).isEmpty === true
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, api3.get.id).isEmpty === true
        
        v2.addInvokeEntitlements(UUID.fromString(dummyOwner.id))

        val x = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, api1.get.id)
        val y = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, api2.get.id)
        val z = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, api3.get.id)        
        
        x.isEmpty === false
        x.head.properties.get("action") === "apiendpoint.invoke"
        y.isEmpty === false
        y.head.properties.get("action") === "apiendpoint.invoke"
        z.isEmpty === false
        z.head.properties.get("action") === "apiendpoint.invoke"        
      }
      
      
      "do nothing if an ApiEndpoint already has the entitlement" >> {
        val (wrk, env) = createWorkspaceEnvironment()
        val api = createApi(parent = Some(env))
        val ep1 = createApiEndpoint(api.get.id, properties = Some(Map("resource" -> "/foo")))
        val ep2 = createApiEndpoint(api.get.id, properties = Some(Map("resource" -> "/foo")))
        val ep3 = createApiEndpoint(api.get.id, properties = Some(Map("resource" -> "/foo")))
        
        ep1 must beSuccessfulTry
        ep2 must beSuccessfulTry
        ep3 must beSuccessfulTry
        
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, ep1.get.id).isEmpty === true
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, ep2.get.id).isEmpty === true
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, ep3.get.id).isEmpty === true
        
        setEntitlements(ep1.get.id, "action" -> "apiendpoint.invoke")
        setEntitlements(ep2.get.id, "action" -> "apiendpoint.invoke")
        setEntitlements(ep3.get.id, "action" -> "apiendpoint.invoke")

        val x = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, ep1.get.id)
        val y = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, ep2.get.id)
        val z = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, ep3.get.id)        
        
        x.size === 1
        x.head.properties.get("action") === "apiendpoint.invoke"
        y.size === 1
        y.head.properties.get("action") === "apiendpoint.invoke"
        z.size === 1
        z.head.properties.get("action") === "apiendpoint.invoke"
        
        v2.addInvokeEntitlements(UUID.fromString(dummyOwner.id))

        val x1 = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, ep1.get.id)
        val y1 = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, ep2.get.id)
        val z1 = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, ep3.get.id)        
        
        x1.size === 1
        x1.head.properties.get("action") === "apiendpoint.invoke"
        y1.size === 1
        y1.head.properties.get("action") === "apiendpoint.invoke"
        z1.size === 1
        z1.head.properties.get("action") === "apiendpoint.invoke"        
      }
      
      "do nothing if the Api already has the entitlement" >> {
        val (wrk, env) = createWorkspaceEnvironment()
        val api1 = createApi(parent = Some(env))
        val api2 = createApi(parent = Some(env))
        val api3 = createApi(parent = Some(env))

        api1 must beSuccessfulTry
        api2 must beSuccessfulTry
        api3 must beSuccessfulTry
        
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, api1.get.id).isEmpty === true
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, api2.get.id).isEmpty === true
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, api3.get.id).isEmpty === true
        
        setEntitlements(api1.get.id, "action" -> "apiendpoint.invoke")
        setEntitlements(api2.get.id, "action" -> "apiendpoint.invoke")
        setEntitlements(api3.get.id, "action" -> "apiendpoint.invoke")

        val x = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, api1.get.id)
        val y = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, api2.get.id)
        val z = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, api3.get.id)        
        
        x.size === 1
        x.head.properties.get("action") === "apiendpoint.invoke"
        y.size === 1
        y.head.properties.get("action") === "apiendpoint.invoke"
        z.size === 1
        z.head.properties.get("action") === "apiendpoint.invoke"
        
        v2.addInvokeEntitlements(UUID.fromString(dummyOwner.id))

        val x1 = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, api1.get.id)
        val y1 = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, api2.get.id)
        val z1 = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, api3.get.id)        
        
        x1.size === 1
        x1.head.properties.get("action") === "apiendpoint.invoke"
        y1.size === 1
        y1.head.properties.get("action") === "apiendpoint.invoke"
        z1.size === 1
        z1.head.properties.get("action") === "apiendpoint.invoke"     
      }
      
    }    
    
    
  }
  
}