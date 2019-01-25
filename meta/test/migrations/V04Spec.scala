package migrations

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import play.api.libs.json.Json
import play.api.test.PlaySpecification

class V4Spec extends PlaySpecification with MetaRepositoryOps {

  private val v4 = new V4()

  "V4" >> {
    
    "add exactly one 'gestalt.upgrade' entitlement, to 'root' with entitled admin" in {
      val allUpgradeEnts = ResourceFactory.findAllByPropertyValue(ResourceIds.Entitlement, "action", "gestalt.upgrade")
      allUpgradeEnts must haveSize(1)
      val List(upgradeEnt) = allUpgradeEnts
      upgradeEnt.name must_== s"${dummyRootOrgId}.gestalt.upgrade"
      ResourceFactory.findParent(upgradeEnt.id) must beSome(
        (r: GestaltResourceInstance) => r.id must_== dummyRootOrgId
      )
      Json.parse(upgradeEnt.properties.get("identities")).as[Seq[String]] must containTheSameElementsAs(Seq(adminUserId.toString))
    }

    "be idempotent" in {
      ResourceFactory.findAllByPropertyValue(ResourceIds.Entitlement, "action", "gestalt.upgrade") must haveSize(1)
      v4.migrate(adminUserId) must beRight
      ResourceFactory.findAllByPropertyValue(ResourceIds.Entitlement, "action", "gestalt.upgrade") must haveSize(1)
    }

  }
  
}