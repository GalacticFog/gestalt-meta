package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import scala.language.implicitConversions

trait AccountLike {
  def id: UUID
  def name: String
  def orgId: UUID
}

object AccountLike {

  implicit def resource2AccountLike(r: GestaltResourceInstance): AccountLike = r.typeId match {
    case ResourceIds.User | ResourceIds.Org => new AccountLike {
      override def id: UUID = r.id
      override def name: String = r.name
      override def orgId: UUID = r.orgId
    }
    case _ => throw new RuntimeException(s"resource '${r.id}' of type '${sdk.ResourceLabel(r.typeId)}' could not be converted to AccountLike")
  }

  implicit def auth2AccountLike(auth: AuthAccountWithCreds): AccountLike = account2AccountLike(auth.account)

  implicit def account2AccountLike(account: GestaltAccount): AccountLike = new AccountLike {
    override def id: UUID = account.id
    override def name: String = account.name
    override def orgId: UUID = account.directory.orgId
  }

}
