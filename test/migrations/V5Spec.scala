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

class V5Spec extends PlaySpecification with MetaRepositoryOps {

  private val v5 = new V5()

  "V2" >> {

    "add 'import' to Container resource type" >> {
      val container = TypeFactory.findById(ResourceIds.Container)
      container must beSome

      val actionInfo = {
        val js = Json.parse(container.get.properties.get("actions"))
        Js.parse[ActionInfo](js.as[JsObject])
      }
      actionInfo must beSuccessfulTry
      actionInfo.get.verbs.contains("import") === true
    }


    "addImportEntitlements" should {

      "add 'container.import' to any existing Containers" >> {
        val (_, env) = createWorkspaceEnvironment()
        val c1 = newDummyContainer(env)
        val c2 = newDummyContainer(env)
        val c3 = newDummyContainer(env)

        c1 must beSuccessfulTry
        c2 must beSuccessfulTry
        c3 must beSuccessfulTry

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, c1.get.id) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, c2.get.id) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, c3.get.id) must beEmpty

        v5.addContainerImportEntitlements(UUID.fromString(dummyOwner.id))

        val x = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, c1.get.id)
        val y = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, c2.get.id)
        val z = ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, c3.get.id)

        x must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") )
        y must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") )
        z must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") )
      }

      "add 'container.import' to any existing Environment" >> {
        val (_, env1) = createWorkspaceEnvironment()
        val (_, env2) = createWorkspaceEnvironment()
        val (_, env3) = createWorkspaceEnvironment()

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env1) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env2) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env3) must beEmpty

        v5.addContainerImportEntitlements(UUID.fromString(dummyOwner.id))

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env1) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env2) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env3) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") ).exactly(1 times)
      }

      "be idempotent on Container entitlements" >> {
        val (_, env) = createWorkspaceEnvironment()
        val Success(c1) = newDummyContainer(env)
        val Success(c2) = newDummyContainer(env)
        val Success(c3) = newDummyContainer(env)

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, c1.id) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, c2.id) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, c3.id) must beEmpty

        setEntitlements(c1.id, "action" -> "container.import")
        setEntitlements(c2.id, "action" -> "container.import")
        setEntitlements(c3.id, "action" -> "container.import")

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, c1.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, c2.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, c3.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") ).exactly(1 times)

        v5.addContainerImportEntitlements(UUID.fromString(dummyOwner.id))

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, c1.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, c2.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, c3.id) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") ).exactly(1 times)
      }

      "be idempotent on Environment entitlements" >> {
        val (_, env1) = createWorkspaceEnvironment()
        val (_, env2) = createWorkspaceEnvironment()
        val (_, env3) = createWorkspaceEnvironment()

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env1) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env2) must beEmpty
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env3) must beEmpty

        setEntitlements(env1, "action" -> "container.import")
        setEntitlements(env2, "action" -> "container.import")
        setEntitlements(env3, "action" -> "container.import")

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env1) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env2) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env3) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") ).exactly(1 times)

        v5.addContainerImportEntitlements(UUID.fromString(dummyOwner.id))

        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env1) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env2) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") ).exactly(1 times)
        ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, env3) must contain( ((_:GestaltResourceInstance).properties.get("action")) ^^ be_==("container.import") ).exactly(1 times)
      }

    }


  }

}