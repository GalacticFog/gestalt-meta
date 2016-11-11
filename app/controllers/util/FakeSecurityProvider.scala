package controllers.util


import java.util.UUID

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.security.api.json.JsonImports._
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.GestaltResourceInput
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltAccountCreateWithRights
import com.galacticfog.gestalt.security.api.GestaltGroup
import com.galacticfog.gestalt.security.api.GestaltGroupCreateWithRights
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.GestaltOrgCreate
import com.galacticfog.gestalt.security.api.GestaltOrgSync
import com.galacticfog.gestalt.security.api.GestaltPasswordCredential
import com.galacticfog.gestalt.security.api.GestaltSecurityClient
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.api.{ResourceLink => SecurityLink}
import play.api.Logger
import play.api.libs.json._


class FakeSecurityProvider(client: Option[_] = None)
  extends SecurityProvider[Option[_],AuthAccountWithCreds](client) {
  
  //
  // Map[Group], Seq[Users]]
  //
  private[util] val groupUsers: Map[UUID,Seq[UUID]] = Map.empty
  
  /**
   * Echo GestaltResourceInput -> GestaltOrg
   */
  def createOrg(parent: UUID, auth: AuthAccountWithCreds, org: GestaltResourceInput): Try[GestaltOrg] = {
    Try(inputToOrg(org))
  }
  
  def addAccountsToGroup(group: UUID, accounts: Seq[UUID]): Try[Seq[SecurityLink]] = {
    ???
  }
  
  def removeAccountsFromGroup(group: UUID, accounts: Seq[UUID]): Try[Seq[SecurityLink]] = {
    ???
  }
  
  /**
   * Map users to groups - data: Map[Group-ID, Seq[User-ID]]
   */
  def withGroupMappedUsers(data: Map[UUID,Seq[UUID]]) = {
    ???
  }
  
//
//case class GestaltGroup(
//    id: UUID, 
//    name: String, 
//    description: Option[String], 
//    directory: GestaltDirectory, 
//    disabled: Boolean, 
//    accounts: Seq[ResourceLink])
//
//case class GestaltDirectory(
//  id: UUID, 
//  name: String, 
//  description: Option[String], 
//  orgId: UUID)
//
//case class GestaltAccount(
//  id: UUID,                           
//  username: String,                           
//  firstName: String,                           
//  lastName: String,                           
//  description: Option[String],                           
//  email: Option[String],                           
//  phoneNumber: Option[String],                           
//  directory: GestaltDirectory)
//
//
//case class GestaltOrg(
//    id: UUID, 
//    name: String, 
//    fqon: String, 
//    description: Option[String], 
//    parent: Option[ResourceLink], 
//    children: Seq[ResourceLink])
  
  
  def securityOrgFromJson(js: JsValue): Try[GestaltOrg] = {
    JsonUtil.tryParse[GestaltOrg](js)   
  }
  
  private def inputToOrg(in: GestaltResourceInput): GestaltOrg = {
    ???
  }
}