package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models.{GestaltResourceType, GestaltTypeProperty}
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import controllers.SecurityResources
import play.api.test.PlaySpecification

import scala.util.Success

class V14Spec extends PlaySpecification with MetaRepositoryOps {

  def haveName(name: String) = ((_:GestaltTypeProperty).name) ^^ be_==(name)
  def haveDatatype(dt: String) = ((_:GestaltTypeProperty).datatype) ^^ be_==(DataType.id(dt))
  def haveRequirement(req: String) = ((_:GestaltTypeProperty).requirementType) ^^ be_==(RequirementType.id(req))

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    pristineDatabase()
    val Success(_) = Ents.createNewMetaUser(user, dummyRootOrgId, rootOwnerLink(), user.account,
      Some(Map(
        "firstName" -> user.account.firstName,
        "lastName" -> user.account.lastName,
        "email" -> user.account.email.getOrElse(""),
        "phoneNumber" -> user.account.phoneNumber.getOrElse("")
      )),
      user.account.description
    )
  }

  "V14" >> {

    "create ::ECS resource type" >> {
      TypeFactory.findById(V14.ECS_PROVIDER_TYPE_ID) must beSome( ((_:GestaltResourceType).name) ^^ be_==(V14.ECS_PROVIDER_TYPE_NAME) )
      TypeFactory.findByName(V14.ECS_PROVIDER_TYPE_NAME) must beSome( ((_:GestaltResourceType).id) ^^ be_==(V14.ECS_PROVIDER_TYPE_ID) )
    }

    "be idempotent" >> {
      val v14 = new V14()
      v14.migrate(UUID.fromString(user.account.id)) must beRight
    }


  }

}