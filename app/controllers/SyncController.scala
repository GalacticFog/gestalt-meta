package controllers

import java.util.UUID

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.data.ResourceFactory


import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.GestaltGroup

import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.tasks.play.io.NonLoggingTaskEvents

import controllers.util._
import play.api.{Logger => log}

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._


object SyncController extends MetaController with NonLoggingTaskEvents with SecurityResources {

  def sync() = Authenticate() { implicit request =>
    trace("sync")
    
    Try {
      
      val sd = Security.getOrgSyncTree(None, request.identity) match {
        case Success(data) => data
        case Failure(err)  => throw err
      }
  
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
      deleteResources(creator, orgsDelete)
      
      log.debug(s"Deleting ${usersDelete.size} Users.")
      deleteResources(creator, usersDelete)
      
      log.debug(s"Deleting ${groupsDelete.size} Groups.")
      deleteResources(creator, groupsDelete)
      
      createOrgs (ResourceIds.User, creator, (orgsCreate  map secOrgMap), request.identity)
      createUsers(ResourceIds.User, creator, (usersCreate map secAccMap), request.identity )
      createGroups(ResourceIds.User, creator, (groupsCreate map secGroupMap), request.identity)
      
      updateOrgs (creator, (orgsUpdate map secOrgMap) )
      updateUsers(creator, (usersUpdate map secAccMap) )
      updateGroups(creator, (groupsUpdate map secGroupMap))
      
    } match {
      case Success(_) => NoContent
      case Failure(e) => GenericErrorResult(500, e.getMessage)
    }
  }
  
  def deleteResources(identity: UUID, ids: Iterable[UUID]) = {
    for (id <- ids) ResourceFactory.hardDeleteResource(id).get
  }
  
  def createGroups(creatorType: UUID, creator: UUID, rs: Iterable[GestaltGroup], auth: AuthAccountWithCreds) = {
    for (group <- rs) {
      log.debug(s"Creating Group: ${group.name}")
      createNewMetaGroup(creator, group.directory.orgId, group, 
          properties = None, group.description)
    }
  }
  
  def updateGroups(creator: UUID, rs: Iterable[GestaltGroup]) = {
    for (group <- rs) {
      log.debug(s"Updating Group : ${group.name}")
      ResourceFactory.findById(group.id) foreach { g =>
        ResourceFactory.update(g.copy(name = group.name, description = group.description), creator)  
      }
    }
  }
  
  def createOrgs(creatorType: UUID, creator: UUID, rs: Iterable[GestaltOrg], auth: AuthAccountWithCreds) = {
    for (org <- rs) {
      log.debug(s"Creating Org : ${org.name}")
      val parent = parentOrgId(org, auth)
      createNewMetaOrg(creator, parent, org, properties = None, None).get
    }
  }

  
  def updateOrgs(creator: UUID, rs: Iterable[GestaltOrg]) = {
    for (org <- rs) {
      log.debug(s"Updating Org : ${org.name}")
      // TODO: ignore it if it doesn't exist, for now
      ResourceFactory.findById(org.id) foreach { o =>
        ResourceFactory.update(o.copy(name = org.name), creator)
      }
    }
  }

  def getRootOrgId(account: AuthAccountWithCreds): UUID = {
    val root = Security.getRootOrg(account)
    root.get.id
  }
  
  def getRootOrgFqon(account: AuthAccountWithCreds): String = {
    Security.getRootOrg(account).get.fqon
  }

  private def userProps(acc: GestaltAccount): Seq[(String,String)] = {
    Seq(
      "firstName" -> acc.firstName,
      "lastName" -> acc.lastName,
      "email" -> acc.email.getOrElse(""),
      "phoneNumber" -> acc.phoneNumber.getOrElse("")
    )
  }
  
  def createUsers(creatorType: UUID, creator: UUID, rs: Iterable[GestaltAccount], account: AuthAccountWithCreds) = {
    val root = getRootOrgId(account)
    for (acc <- rs) {
      log.debug(s"Creating User : ${acc.name}")
      createNewMetaUser(creator, acc.directory.orgId, acc,
        properties = Some(
          (userProps(acc) ++ Seq("gestalt_home" -> getRootOrgFqon(account))).toMap
        ),
        description = acc.description ).get
    }
  }
  
  def updateUsers(creator: UUID, rs: Iterable[GestaltAccount]) = {
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
  
  private def computeResourceDiffs(securityIds: Seq[UUID], metaIds: Seq[UUID]) = {
    val create = securityIds.diff(metaIds) // in security, not meta
    val delete = metaIds.diff(securityIds) // in meta, not security
    val update = securityIds.diff(create)  // in security, but not new
    (create, delete, update)
  }
  
  /**
   * Get parent of given org, root if org.parent is None 
   */
  private def parentOrgId(o: GestaltOrg, identity: AuthAccountWithCreds) = {
    if (o.parent.isDefined) o.parent.get.id
    else Security.getRootOrg(identity) match {
      case Success(org) => org.id
      case Failure(err) => throw err
    }
  }
  
  private def securitySyncTree(identity: AuthAccountWithCreds) = 
    Security.getOrgSyncTree(None, identity) match {
      case Success(data) => data
      case Failure(err)  => throw err
    }  
  
}