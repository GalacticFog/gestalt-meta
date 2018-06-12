package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import play.api.libs.json._
import play.api.test.PlaySpecification

import scala.util.Success

class V9Spec extends PlaySpecification with MetaRepositoryOps {

  private val v9 = new V9()

  "V9" >> {

    "add 'viewmetrics' to Provider::Lambda resource type" >> {
      val Some(lambdaProvider) = TypeFactory.findById(ResourceIds.LambdaProvider)

      val Success(actionInfo) = {
        val js = Json.parse(lambdaProvider.properties.get("actions"))
        Js.parse[ActionInfo](js.as[JsObject])
      }
      actionInfo.verbs must contain("viewmetrics")
    }


    "update entitlements" should {

      "add 'provider.viewmetrics' to any existing Lambda providers" >> {
        val (_, env) = createWorkspaceEnvironment()
        val Success(p1) = createInstance(ResourceIds.LambdaProvider, "test-lambda-provider-1")
        val Success(p2) = createInstance(ResourceIds.LambdaProvider, "test-lambda-provider-2")
        val Success(p3) = createInstance(ResourceIds.LambdaProvider, "test-lambda-provider-3")

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, p1.id) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, p2.id) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, p3.id) must beEmpty

        v9.migrate(UUID.fromString(dummyOwner.id))

        val x = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, p1.id)
        val y = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, p2.id)
        val z = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, p3.id)

        x must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") )
        y must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") )
        z must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") )
      }

      "not add 'provider.viewmetrics' to any existing non-Lambda providers" >> {
        val (_, env) = createWorkspaceEnvironment()
        val Success(p) = createInstance(ResourceIds.CaasProvider, "test-caas-provider")

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, p.id) must beEmpty

        v9.migrate(UUID.fromString(dummyOwner.id))

        val x = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, p.id)

        x must not contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") )
      }

      "add 'provider.viewmetrics' to any existing Environment" >> {
        val (_, env1) = createWorkspaceEnvironment()
        val (_, env2) = createWorkspaceEnvironment()
        val (_, env3) = createWorkspaceEnvironment()

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env1) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env2) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env3) must beEmpty

        v9.migrate(UUID.fromString(dummyOwner.id))

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env1) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env2) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env3) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") ).exactly(1 times)
      }

      "be idempotent on Provider entitlements" >> {
        val (_, env) = createWorkspaceEnvironment()
        val Success(p1) = createInstance(ResourceIds.LambdaProvider, "test-lambda-provider-1")
        val Success(p2) = createInstance(ResourceIds.LambdaProvider, "test-lambda-provider-2")
        val Success(p3) = createInstance(ResourceIds.LambdaProvider, "test-lambda-provider-3")

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, p1.id) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, p2.id) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, p3.id) must beEmpty

        setEntitlements(p1.id, "action" -> "provider.viewmetrics")
        setEntitlements(p2.id, "action" -> "provider.viewmetrics")
        setEntitlements(p3.id, "action" -> "provider.viewmetrics")

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, p1.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, p2.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, p3.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") ).exactly(1 times)

        v9.migrate(UUID.fromString(dummyOwner.id))

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, p1.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, p2.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, p3.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") ).exactly(1 times)
      }

      "be idempotent on Environment entitlements" >> {
        val (_, env1) = createWorkspaceEnvironment()
        val (_, env2) = createWorkspaceEnvironment()
        val (_, env3) = createWorkspaceEnvironment()

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env1) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env2) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env3) must beEmpty

        setEntitlements(env1, "action" -> "provider.viewmetrics")
        setEntitlements(env2, "action" -> "provider.viewmetrics")
        setEntitlements(env3, "action" -> "provider.viewmetrics")

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env1) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env2) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env3) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") ).exactly(1 times)

        v9.migrate(UUID.fromString(dummyOwner.id))

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env1) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env2) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env3) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("provider.viewmetrics") ).exactly(1 times)
      }

    }


  }

}