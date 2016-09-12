package controllers

import com.galacticfog.gestalt.meta.auth.Authorization

import java.util.UUID
import scala.util.Try
import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.output.toOwnerLink
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.GestaltGroup
import com.galacticfog.gestalt.security.api.{GestaltResource => SecurityResource}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import com.galacticfog.gestalt.security.api.{GestaltResource => SecurityResource}

/**
 * Functions for dealing with resources that exist in both Security and in Meta (currently
 * Orgs and Users/Accounts). These functions are capable of transforming Security domain
 * objects to Meta domain objects and interacting with the Meta repository.
 */
trait SecurityResources {
  
  type SecurityResourceFunction = (UUID, AuthAccountWithCreds, GestaltResourceInput) => Try[SecurityResource]
  type MetaResourceFunction     = (UUID, UUID, SecurityResource,  Option[Hstore], Option[String]) => Try[GestaltResourceInstance]
  type SecurityDelete           = (UUID, AuthAccountWithCreds) => Try[Boolean]
  
  def createNewMetaOrg[T](creator: UUID, owningOrg: UUID, org: SecurityResource, properties: Option[Hstore], description: Option[String]) = { //(implicit securedRequest: SecuredRequest[T]) = {
    Try {
      val o = org.asInstanceOf[GestaltOrg]
      val parent = o.parent map { _.id }
      val props = Some(Map("fqon" -> o.fqon) ++ properties.getOrElse(Map()))
      ResourceFactory.create(ResourceIds.User, creator)(
          fromSecurityResource(org, ResourceIds.Org, owningOrg, creator, props, description), parent).get
    }
  }
  
  def createNewMetaGroup[T](creator: UUID, owningOrg: UUID, group: SecurityResource, properties: Option[Hstore], description: Option[String]) = {
    Try {
      val g = group.asInstanceOf[GestaltGroup]
      ResourceFactory.create(ResourceIds.Group, creator)(
        fromSecurityResource(g, ResourceIds.Group, owningOrg, creator, properties, description),
        Option(owningOrg)).get
    }
  }
  
  
  def createNewMetaUser[T](creator: UUID, owningOrg: UUID, account: SecurityResource, properties: Option[Hstore], description: Option[String]) = {//(implicit securedRequest: SecuredRequest[T]) = {
    Try {
      val a = account.asInstanceOf[GestaltAccount]
      ResourceFactory.create(ResourceIds.User, creator)(
          fromSecurityResource(a, ResourceIds.User, owningOrg, creator, properties, description),
          Option(owningOrg)).get
    }
  }
  
  
  /**
   * Permanently delete a Resource from both Meta and Security.
   */  
  def hardDeleteSynchronized(id: UUID, auth: AuthAccountWithCreds, fn: SecurityDelete) = {
    fn(id, auth) flatMap { x => ResourceFactory.hardDeleteResource(id) }
  }

  
  /**
   * Create a ResourceOwnerLink from AuthAccountWithCreds
   */
  def ownerFromAccount(account: AuthAccountWithCreds): ResourceOwnerLink = toOwnerLink(ResourceIds.User,
    account.account.id, name = Some(account.account.name), orgId = account.account.directory.orgId )  
  
  /**
   * Convert Security::GestaltOrg or Security::GestaltAccount to GestaltResourceInstance
   * (could be used for other object types as well)
   */
  def fromSecurityResource[T](sr: SecurityResource, typeId: UUID, org: UUID, creator: UUID, properties: Option[Hstore] = None, description: Option[String] = None) = { //(implicit request: SecuredRequest[T]) = {
    GestaltResourceInstance(
      id = sr.id,
      typeId = typeId,
      orgId = org, // this needs to be org from URI
      owner = ResourceOwnerLink(ResourceIds.User, creator /*request.identity.account.id*/),
      name = sr.name,
      description = description,
      properties = properties)
  }  
  
}