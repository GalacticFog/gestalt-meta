package migrations

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.bootstrap.ActionInfo
// import com.galacticfog.gestalt.data.models.GestaltTypeProperty
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import com.galacticfog.gestalt.json.Js
import play.api.test.PlaySpecification
import play.api.libs.json._

class V26Spec extends PlaySpecification with MetaRepositoryOps {
  "V26" >> {

    "Add external_id to Secret" >> {
      val tpe = TypeFactory.findById(ResourceIds.Secret)
      PropertyFactory.findByName(tpe.get.id, "external_id") must beSome
    }

    "add 'import' to Secret resource type" >> {
      val tpe = TypeFactory.findById(ResourceIds.Secret)
      tpe must beSome

      val actionInfo = {
        val js = Json.parse(tpe.get.properties.get("actions"))
        Js.parse[ActionInfo](js.as[JsObject])
      }
      actionInfo must beSuccessfulTry
      actionInfo.get.verbs.contains("import") === true
    }

    "add 'import' to Volume resource type" >> {
      val tpe = TypeFactory.findById(migrations.V13.VOLUME_TYPE_ID)
      tpe must beSome

      val actionInfo = {
        val js = Json.parse(tpe.get.properties.get("actions"))
        Js.parse[ActionInfo](js.as[JsObject])
      }
      actionInfo must beSuccessfulTry
      actionInfo.get.verbs.contains("import") === true
    }
  }
}