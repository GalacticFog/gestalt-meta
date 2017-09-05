package controllers


import java.util.UUID

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.DeleteManager
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.session
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltGroup
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import controllers.util.{GenericErrorResult, SecureController, Security}
import play.api.{Logger => log}
import play.api.libs.json.Json
import com.galacticfog.gestalt.meta.auth.Authorization
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi

import javax.inject.Singleton

@Singleton
class SyncController @Inject()( messagesApi: MessagesApi,
                                env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
                                security: Security,
                                deleteController: DeleteController )
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  private var adminId: UUID = null
  
  implicit lazy val syncStatContainerFormat = Json.format[Stat]
  implicit lazy val syncStatsFormat = Json.format[SyncStats]
  
  case class Stat(created: Seq[UUID], updated: Seq[UUID], deleted: Seq[UUID])
  case class SyncStats(orgs: Stat, users: Stat, groups: Stat) {
    def toJson() = Json.toJson(this)
  }
  
  /*
   * 
   * TODO: We currently update all Orgs, Groups, and Users on every sync. Until we devise some
   * method of performing diffs on the resource types, this can't be avoided.
   * 
   */
  def sync() = Audited() { implicit request =>
  
    Try {
      val sd = security.getOrgSyncTree(None, request.identity) match {
        case Success(data) => data
        case Failure(err)  => throw err
      }

      adminId = sd.admin map { a => UUID.fromString(a.id) } getOrElse {
        throw new ConflictException(
          "No 'admin' user found in gestalt-security. Cannot synchronize - no changes made.")
      }
      
      val rootId = getRootOrgId(request.identity)

      setNewEntitlements(rootId, rootId, request.identity, parent = None)
      
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
      val creator = request.identity.account.id
      
      log.debug(s"Deleting ${orgsDelete.size} Orgs.")
      deleteResources(request.identity, orgsDelete, "Org")
      
      log.debug(s"Deleting ${usersDelete.size} Users.")
      deleteResources(request.identity, usersDelete, "User")
      
      log.debug(s"Deleting ${groupsDelete.size} Groups.")
      deleteResources(request.identity, groupsDelete, "Group")
      
      createOrgs (ResourceIds.User, creator, (orgsCreate  map secOrgMap), request.identity)
      createUsers(ResourceIds.User, creator, (usersCreate map secAccMap), request.identity )
      createGroups(ResourceIds.User, creator, (groupsCreate map secGroupMap), request.identity)
      
      updateOrgs (creator, (orgsUpdate map secOrgMap), request.identity)
      updateUsers(creator, (usersUpdate map secAccMap), request.identity)
      updateGroups(creator, (groupsUpdate map secGroupMap), request.identity)
      
      val stats = SyncStats(
        orgs   = Stat(orgsCreate, orgsUpdate, orgsDelete),
        groups = Stat(groupsCreate, groupsUpdate, groupsDelete),
        users  = Stat(usersCreate, usersUpdate, usersDelete))

      log.info("Synchronization complete. Statistics:")
      log.info(Json.prettyPrint(stats.toJson))
      
    } match {
      case Success(_) => NoContent
      case Failure(e) => GenericErrorResult(500, e.getMessage)
    }

  }

  def deleteResources(account: AuthAccountWithCreds, ids: Iterable[UUID], resType: String) = {
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
        
        ResourceFactory.update(o.copy(name = org.name), creator) match {
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
        case Success(group) => {

          setNewEntitlements(org, group.id, account, parent = Option(org))
        }
      }
         
    }
  }
  
  def updateGroups(creator: UUID, rs: Iterable[GestaltGroup], account: AuthAccountWithCreds) = {
    for (group <- rs) {
      log.debug(s"Updating Group : ${group.name}")
      ResourceFactory.findById(group.id) foreach { g =>
        ResourceFactory.update(g.copy(name = group.name, description = group.description), creator)  
      }
    }
  }
  
  def createUsers(creatorType: UUID, creator: UUID, rs: Iterable[GestaltAccount], account: AuthAccountWithCreds) = {
  
    val admin = getAdminUserId(account)
    
    for (acc <- rs) {
      log.debug(s"Creating User : ${acc.name}")
      
      val org = acc.directory.orgId
      
      createNewMetaUser(account, org, acc,
          properties = Some(
            (userProps(acc) ++ Seq("gestalt_home" -> getRootOrgFqon(account))).toMap
          ),
          description = acc.description ) match {
        case Failure(err) => throw err
        case Success(usr) => {
          setNewEntitlements(org, usr.id, account, parent = Option(org))
        }
      }
    
    }

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
          creator
        )
      }
    }
  }    
  

  def getAdminUserId(account: AuthAccountWithCreds): AuthAccountWithCreds = {
    // TODO: Actually look up the 'admin' user - waiting on a change to the
    // security /sync payload to be able to identify this user.
    account  
  }
  
  def getRootOrgId(account: AuthAccountWithCreds): UUID = {
    security.getRootOrg(account) match {
      case Success(root) => root.id
      case Failure(err)  =>
        throw new RuntimeException(
            "Root Org not found in gestalt-security. Contact an administrator")
    }
  }
  
  def getRootOrgFqon(account: AuthAccountWithCreds): String = {
    security.getRootOrg(account).get.fqon
  }

  private def userProps(acc: GestaltAccount): Seq[(String,String)] = {
    Seq(
      "firstName" -> acc.firstName,
      "lastName" -> acc.lastName,
      "email" -> acc.email.getOrElse(""),
      "phoneNumber" -> acc.phoneNumber.getOrElse("")
    )
  }
  
  private def computeResourceDiffs(securityIds: Seq[UUID], metaIds: Seq[UUID]) = {
    val create = securityIds.diff(metaIds) // in security, not meta
    val delete = metaIds.diff(securityIds) // in meta, not security
    val update = securityIds.diff(create)  // in security, and in meta
    (create, delete, update)
  }
  
  /**
   * Get parent of given org, root if org.parent is None 
   */
  private def parentOrgId(o: GestaltOrg, identity: AuthAccountWithCreds) = {
    if (o.parent.isDefined) o.parent.get.id
    else security.getRootOrg(identity) match {
      case Success(org) => org.id
      case Failure(err) => throw err
    }
  }
  
  private def securitySyncTree(identity: AuthAccountWithCreds) = 
    security.getOrgSyncTree(None, identity) match {
      case Success(data) => data
      case Failure(err)  => throw err
    }  
  
}