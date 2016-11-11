package controllers.util

import java.util.UUID

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Failure
import scala.util.Success
import scala.util.Try

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


/**
 * @tparam A type of securityClient
 * @tparam B type of Account
 */
abstract class SecurityProvider[A,B](client: A) {
  
//  def getOrgSyncTree(org: Option[UUID], user: B)
//  def getRootOrg(user: B)
//  def getRootAccount(user: B)
  
  //def getOrgs(user: B)
  
  def createOrg(parent: UUID, auth: B, org: GestaltResourceInput): Try[GestaltOrg]
  
//  def deleteOrg(org: UUID, auth: B)
//  
//  def getGroups(org: Option[UUID], auth: B)
//  def createGroup(org: UUID, auth: B, group: GestaltResourceInput)
//  def deleteGroup(group: UUID, auth: B)
//  def getGroupMembership(group: UUID, auth: B)

  def addAccountsToGroup(group: UUID, accounts: Seq[UUID]): Try[Seq[SecurityLink]]  
  def removeAccountsFromGroup(group: UUID, accounts: Seq[UUID]): Try[Seq[SecurityLink]]
  
//  def getAccounts(org: UUID, auth: B)
//  def createAccount
//  def deleteAccount
//  def getGroupAccounts


}