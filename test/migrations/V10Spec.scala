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

class V10Spec extends PlaySpecification with MetaRepositoryOps {

  private val v10 = new V10()

  "V10" >> {

    "add 'viewstatus' action to StreamSpec resource type" >> {
      val Some(streamSpec) = TypeFactory.findById(V7.STREAM_SPEC_TYPE_ID)

      val Success(actionInfo) = {
        val js = Json.parse(streamSpec.properties.get("actions"))
        Js.parse[ActionInfo](js.as[JsObject])
      }
      actionInfo.verbs must contain("viewstatus")
    }.pendingUntilFixed("requires V7")


    "update entitlements" should {

      "add 'streamspec.viewstatus' to any existing StreamSpec" >> {
        val (_, env) = createWorkspaceEnvironment()
        val Success(i1) = createInstance(V7.STREAM_SPEC_TYPE_ID, "test-streamspec-1")
        val Success(i2) = createInstance(V7.STREAM_SPEC_TYPE_ID, "test-streamspec-2")
        val Success(i3) = createInstance(V7.STREAM_SPEC_TYPE_ID, "test-streamspec-3")

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, i1.id) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, i2.id) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, i3.id) must beEmpty

        v10.migrate(UUID.fromString(dummyOwner.id))

        val x = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, i1.id)
        val y = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, i2.id)
        val z = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, i3.id)

        x must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") )
        y must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") )
        z must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") )
      }.pendingUntilFixed("requires V7")

      "add 'streamspec.viewstatus' to any existing Environment" >> {
        val (_, env1) = createWorkspaceEnvironment()
        val (_, env2) = createWorkspaceEnvironment()
        val (_, env3) = createWorkspaceEnvironment()

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env1) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env2) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env3) must beEmpty

        v10.migrate(UUID.fromString(dummyOwner.id))

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env1) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env2) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env3) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") ).exactly(1 times)
      }.pendingUntilFixed("requires V7")

      "be idempotent on StreamSpec entitlements" >> {
        val (_, env) = createWorkspaceEnvironment()
        val Success(i1) = createInstance(ResourceIds.LambdaProvider, "test-streamspec-1")
        val Success(i2) = createInstance(ResourceIds.LambdaProvider, "test-streamspec-2")
        val Success(i3) = createInstance(ResourceIds.LambdaProvider, "test-streamspec-3")

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, i1.id) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, i2.id) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, i3.id) must beEmpty

        setEntitlements(i1.id, "action" -> "streamspec.viewstatus")
        setEntitlements(i2.id, "action" -> "streamspec.viewstatus")
        setEntitlements(i3.id, "action" -> "streamspec.viewstatus")

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, i1.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, i2.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, i3.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") ).exactly(1 times)

        v10.migrate(UUID.fromString(dummyOwner.id))

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, i1.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, i2.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, i3.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") ).exactly(1 times)
      }

      "be idempotent on Environment entitlements" >> {
        val (_, env1) = createWorkspaceEnvironment()
        val (_, env2) = createWorkspaceEnvironment()
        val (_, env3) = createWorkspaceEnvironment()

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env1) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env2) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env3) must beEmpty

        setEntitlements(env1, "action" -> "streamspec.viewstatus")
        setEntitlements(env2, "action" -> "streamspec.viewstatus")
        setEntitlements(env3, "action" -> "streamspec.viewstatus")

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env1) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env2) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env3) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") ).exactly(1 times)

        v10.migrate(UUID.fromString(dummyOwner.id))

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env1) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env2) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env3) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("streamspec.viewstatus") ).exactly(1 times)
      }

    }


  }

}