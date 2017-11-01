package controllers



import java.util.UUID
import scala.util.{Try, Success, Failure}
import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.output.toOwnerLink
import com.galacticfog.gestalt.security.api.{GestaltOrg, GestaltAccount, GestaltGroup}
import com.galacticfog.gestalt.security.api.{GestaltResource => SecurityResource}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import play.api.Logger
import play.api.libs.json._

import javax.inject._
import controllers.util.Security
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
import java.time.ZonedDateTime


@Singleton
class SecuritySync @Inject()(
    security: Security, 
    deleteController: DeleteController) 
      extends SecurityResources with AuthorizationMethods {
  
  private[this] val log = Logger(this.getClass)
  
  def synchronize(identity: AuthAccountWithCreds) = Try {
    
    log.info(s"Beginning sync with gestalt-security [${security.clientUrl}]...")
    val beginStamp = ZonedDateTime.now()
    val beginMillis = System.currentTimeMillis()
    
    val sd = security.getOrgSyncTree(None, identity).get
    
    val metaorgs = ResourceFactory.findAll(ResourceIds.Org)
    val metausers = ResourceFactory.findAll(ResourceIds.User)
    val metagroups = ResourceFactory.findAll(ResourceIds.Group)
    
    // Get Ids of everything in security
    val secOrgIds = sd.orgs map { _.id }
    val secAccIds = sd.accounts map { _.id }
    val secGroupIds = sd.groups map { _.id }
    
    // Get Ids of everything in meta
    val metaOrgIds = metaorgs map { _.id }
    val metaUserIds = metausers map { _.id }
    val metaGroupIds = metagroups map { _.id }
    
    val secOrgMap = (sd.orgs map { o => (o.id, o) }).toMap
    val secAccMap = (sd.accounts map { a => (a.id, a) }).toMap
    val secGroupMap = (sd.groups map { g => (g.id, g) }).toMap
    
    val (orgsCreate,orgsDelete,orgsUpdate)   = computeResourceDiffs(secOrgIds, metaOrgIds)
    val (usersCreate,usersDelete,usersUpdate) = computeResourceDiffs(secAccIds, metaUserIds)
    val (groupsCreate,groupsDelete,groupsUpdate) = computeResourceDiffs(secGroupIds, metaGroupIds)
    
    /*
     * TODO: Refactor this - as it is, errors are swallowed. 
     */
    val creator = identity.account.id
    
    log.debug(s"Deleting ${orgsDelete.size} Orgs.")
    deleteResources(identity, orgsDelete, "Org")
    
    log.debug(s"Deleting ${usersDelete.size} Users.")
    deleteResources(identity, usersDelete, "User")
    
    log.debug(s"Deleting ${groupsDelete.size} Groups.")
    deleteResources(identity, groupsDelete, "Group")
    
    createOrgs (ResourceIds.User, creator, (orgsCreate  map secOrgMap), identity)
    createUsers(ResourceIds.User, creator, (usersCreate map secAccMap), identity )
    createGroups(ResourceIds.User, creator, (groupsCreate map secGroupMap), identity)
    
    updateOrgs (creator, (orgsUpdate map secOrgMap), identity)
    updateUsers(creator, (usersUpdate map secAccMap), identity)
    updateGroups(creator, (groupsUpdate map secGroupMap), identity)
    
    val endMillis = System.currentTimeMillis()
    val totalMs = (endMillis - beginMillis)
    
    val stats = SyncStats(beginStamp, totalMs, 
      orgs   = Stat(orgsCreate, orgsUpdate, orgsDelete),
      groups = Stat(groupsCreate, groupsUpdate, groupsDelete),
      users  = Stat(usersCreate, usersUpdate, usersDelete))

    log.info("Synchronization complete. Statistics:")
    log.info(Json.prettyPrint(stats.toJson))
    stats
  }


  def createOrgs(creatorType: UUID, creator: UUID, rs: Iterable[GestaltOrg], account: AuthAccountWithCreds) = {
    for (org <- rs) {
      log.debug(s"Creating Org : ${org.name}")

      val parent = parentOrgId(org, account)
      createNewMetaOrg(account, parent, org, properties = None, None) match {
        case Failure(err) => throw err
        case Success(org) => {
          setNewEntitlements(org.id, org.id, account, parent = Option(parent))
        }
      }
    }
  }
  
  def updateOrgs(creator: UUID, rs: Iterable[GestaltOrg], account: AuthAccountWithCreds) = {
    for (org <- rs) {
      log.debug(s"Updating Org : ${org.name}")
      
      // TODO: ignore it if it doesn't exist, for now
      ResourceFactory.findById(org.id) foreach { o =>
        ResourceFactory.update(o.copy(name = org.name), creator, 
            updateTimestamp = false) match {
          case Failure(err) => throw err
          case Success(org) => {
            log.info(s"Successfully updated Org[${o.id}]")
          }
        }
      } 
    }
  }    
  
  def createGroups(creatorType: UUID, creator: UUID, rs: Iterable[GestaltGroup], account: AuthAccountWithCreds) = {
    for (group <- rs) {
      log.debug(s"Creating Group: ${group.name}")
      
      val org = group.directory.orgId
      createNewMetaGroup(account, org, group, 
          properties = None, group.description) match {
        case Failure(err) => throw err
        case Success(group) => setNewEntitlements(org, group.id, account, parent = Option(org))
      }
    }
  }
  
  def updateGroups(creator: UUID, rs: Iterable[GestaltGroup], account: AuthAccountWithCreds) = {
    for (group <- rs) {
      log.debug(s"Updating Group : ${group.name}")
      ResourceFactory.findById(group.id) foreach { g =>
        ResourceFactory.update(g.copy(name = group.name, description = group.description), creator,
            updateTimestamp = false)  
      }
    }
  }
  
  def createUsers(creatorType: UUID, creator: UUID, rs: Iterable[GestaltAccount], account: AuthAccountWithCreds) = {
    for (acc <- rs) {
      log.debug(s"Creating User : ${acc.name}")
      
      val org = acc.directory.orgId
      createNewMetaUser(account, org, acc,
          properties = Some(
            (userProps(acc) ++ Seq("gestalt_home" -> security.getRootOrg(account).get.fqon)).toMap),
          description = acc.description ) match {
        case Failure(err) => throw err
        case Success(usr) => {
          
          log.debug("Setting new user entitlements.")
          setNewEntitlements(org, usr.id, account, parent = Option(org))
          
          log.debug("Setting new user entitlements on root Org.")
          val rootorgid = security.getRootOrg(account).get.id
          
          def grantNewUserPermissions(caller: UUID, user: UUID, homeOrg: UUID) = {
            // Allow users to view their home-org
            grant(caller, user, homeOrg, "org.view")
            // Allow users to view themselves
            grant(caller, user, user, "user.view")
          }
          grantNewUserPermissions(creator, usr.id, rootorgid)
        }
      }
    }
  }
  
  def grantNewUserPermissions(caller: UUID, user: UUID, homeOrg: UUID) = {
    // Allow users to view their home-org
    grant(caller, user, homeOrg, "org.view")
    
    // Allow users to view themselves
    grant(caller, user, user, "user.view")
  }  
  
  def updateUsers(creator: UUID, rs: Iterable[GestaltAccount], account: AuthAccountWithCreds) = {
    for (acc <- rs) {
      log.debug(s"Updating User : ${acc.name}")
      
      // TODO: ignore it if it doesn't exist, for now
      ResourceFactory.findById(acc.id) foreach { a =>
        ResourceFactory.update(
          a.copy(
            name = acc.name, // name/username
            // update properties
            properties = a.properties map {
              _ ++ userProps(acc)
            } orElse {
              Some(userProps(acc).toMap)
            }
          ),
          creator,
          updateTimestamp = false
        )
      }
    }
  }
  
  private[controllers] def userProps(acc: GestaltAccount): Seq[(String,String)] = {
    Seq(
      "firstName" -> acc.firstName,
      "lastName" -> acc.lastName,
      "email" -> acc.email.getOrElse(""),
      "phoneNumber" -> acc.phoneNumber.getOrElse("")
    )
  }
  
  /**
   * Get parent of given org, root if org.parent is None 
   */
  private[controllers] def parentOrgId(o: GestaltOrg, identity: AuthAccountWithCreds): UUID = {
    o.parent.fold(security.getRootOrg(identity).get.id)(_.id)
  }

  private[controllers] def deleteResources(account: AuthAccountWithCreds, ids: Iterable[UUID], resType: String) = {
    for (id <- ids) {
      
      ResourceFactory.findById(id).fold {
        log.warn(s"Delete-Resource : $resType $id was not found in Meta. Nothing to do.")
      }{ res =>
        
        log.warn(s"SYNC: Deleting $resType id = $id, name = ${res.name}")
        /* 
         * NOTE: We skip the external deletes for Orgs, Groups, and Users because they're
         * already gone from security (that's why we're deleting them from Meta)
         */
        deleteController.manager.delete(res, account,
          force = true, 
          skipExternals = Seq(ResourceIds.Org, ResourceIds.Group, ResourceIds.User))  
      }
    }
  }
  
  private[controllers] def computeResourceDiffs(securityIds: Seq[UUID], metaIds: Seq[UUID]) = {
    val create = securityIds.diff(metaIds) // in security, not meta
    val delete = metaIds.diff(securityIds) // in meta, not security
    val update = securityIds.diff(create)  // in security, and in meta
    (create, delete, update)
  }  
}


/**
 * Functions for dealing with resources that exist in both Security and in Meta (currently
 * Orgs, Users, and Groups). These functions are capable of transforming Security domain
 * objects to Meta domain objects and interacting with the Meta repository.
 */
trait SecurityResources {
  
  type SecurityResourceFunction = (UUID, AuthAccountWithCreds, GestaltResourceInput) => Try[SecurityResource]
  type MetaResourceFunction     = (AuthAccountWithCreds, UUID, SecurityResource,  Option[Hstore], Option[String]) => Try[GestaltResourceInstance]
  type SecurityDelete           = (UUID, AuthAccountWithCreds) => Try[Boolean]
  
  def createNewMetaOrg[T](creator: AuthAccountWithCreds, owningOrg: UUID, org: SecurityResource, properties: Option[Hstore], description: Option[String]) = { //(implicit securedRequest: SecuredRequest[T]) = {
    Try {
      val o = org.asInstanceOf[GestaltOrg]
      val parent = o.parent map { _.id }
      val props = Some(Map("fqon" -> o.fqon) ++ properties.getOrElse(Map()))
      ResourceFactory.create(ResourceIds.User, creator.account.id)(
        fromSecurityResource(org, ResourceIds.Org, owningOrg, creator, props, description), parent).get
    }
  }
  
  def createNewMetaGroup[T](creator: AuthAccountWithCreds, owningOrg: UUID, group: SecurityResource, properties: Option[Hstore], description: Option[String]) = {
    Try {
      val g = group.asInstanceOf[GestaltGroup]
      ResourceFactory.create(ResourceIds.Group, creator.account.id)(
        fromSecurityResource(g, ResourceIds.Group, owningOrg, creator, properties, description),
        Option(owningOrg)).get
    }
  }
  
  def createNewMetaUser[T](creator: AuthAccountWithCreds, owningOrg: UUID, account: SecurityResource, properties: Option[Hstore], description: Option[String]) = {//(implicit securedRequest: SecuredRequest[T]) = {
    Try {
      val a = account.asInstanceOf[GestaltAccount]
      ResourceFactory.create(ResourceIds.User, creator.account.id)(
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
   * Convert Security::GestaltOrg or Security::GestaltAccount to GestaltResourceInstance
   * (could be used for other object types as well)
   */
  def fromSecurityResource[T](
      sr: SecurityResource, 
      typeId: UUID, 
      org: UUID, 
      creator: AuthAccountWithCreds, properties: Option[Hstore] = None, description: Option[String] = None) = {
    
    val ownerLink = SecurityResources.ownerFromAccount(creator)
    
    GestaltResourceInstance(
      id = sr.id,
      typeId = typeId,
      orgId = org, // this needs to be org from URI
      owner = SecurityResources.ownerFromAccount(creator),
      name = sr.name,
      description = description,
      state = ResourceState.id(ResourceStates.Active),
      properties = properties)
  }   
}

object SecurityResources {
  /**
   * Create a ResourceOwnerLink from AuthAccountWithCreds
   */
  def ownerFromAccount(account: AuthAccountWithCreds): ResourceOwnerLink = toOwnerLink(ResourceIds.User,
    account.account.id, name = Some(account.account.name), orgId = account.account.directory.orgId )
}


case class Stat(created: Seq[UUID], updated: Seq[UUID], deleted: Seq[UUID])
object Stat { implicit lazy val syncStatContainerFormat = Json.format[Stat] }

case class SyncStats(start: ZonedDateTime, totalMs: Long, orgs: Stat, users: Stat, groups: Stat) {
  def toJson() = Json.toJson(this)(SyncStats.syncStatsFormat)
}
object SyncStats { implicit lazy val syncStatsFormat = Json.format[SyncStats] }


