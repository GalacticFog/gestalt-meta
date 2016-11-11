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


class SecurityProviderImpl(client: GestaltSecurityClient)
  extends SecurityProvider[GestaltSecurityClient,AuthAccountWithCreds](client) {
  
  def createOrg(parent: UUID, auth: AuthAccountWithCreds, org: GestaltResourceInput): Try[GestaltOrg] = {
    
    Try{Await.result(GestaltOrg.createSubOrg(
      parentOrgId = parent,
      create = GestaltOrgCreate(
        name = org.name,
        createDefaultUserGroup = false,
        inheritParentMappings = Some(true),
        description = org.description
      ))(client.withCreds(auth.creds)), 5 seconds )}
  }
  
  def addAccountsToGroup(group: UUID, accounts: Seq[UUID]): Try[Seq[SecurityLink]] = {
    Try(Await.result(GestaltGroup.updateMembership(group, accounts, Seq())(client), 5 seconds))
  }
  
  def removeAccountsFromGroup(group: UUID, accounts: Seq[UUID]): Try[Seq[SecurityLink]] = {
    Try(Await.result(GestaltGroup.updateMembership(group, Seq(), remove = accounts)(client), 5 seconds))
  }
}

object SecurityProviderImpl {
  def apply(client: GestaltSecurityClient) = new SecurityProviderImpl(client)
}