package controllers


import java.util.UUID

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.data._ //ResourceFactory
import com.galacticfog.gestalt.data.models._ //GestaltResourceInstance

import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.GestaltGroup

import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.tasks.play.io.NonLoggingTaskEvents

import controllers.util._
import play.api.{Logger => log}
import play.api.libs.json._

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
  
  def rootUser(auth: AuthAccountWithCreds) = {
    Security.getRootUser(auth)
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
  
  
  def createOrgs(creatorType: UUID, creator: UUID, rs: Iterable[GestaltOrg], account: AuthAccountWithCreds) = {
    
    for (org <- rs) {
      
      log.debug(s"Creating Org : ${org.name}")
      val parent = parentOrgId(org, account)
      val admin = getAdminUser(account)
      
      // Create new Org in Meta and add CRUD entitlements for admin user.
      createNewMetaOrg(admin.account.id, parent, org, properties = None, None) match {
        case Failure(err) => throw err
        case Success(org) => {

          // Successfully created Org - create CRUD entitlements for 'admin' user.
          val crud = resourceEntitlements(
              admin.account.id, org.id, org.id, ResourceIds.Org,
              Seq("create", "view", "update", "delete") )

          crud map { e => 
            CreateResource(
              ResourceIds.User, admin.account.id, org.id, Json.toJson(e), account, 
              Option(ResourceIds.Entitlement), Option(parent)).get

          }
          
        }    
      }
      
    }
    
  }
  
  
  def getActionName(typeId: UUID, action: String) = {
    s"${ActionPrefix(typeId)}.${action}"
  }
  
  
  def resourceEntitlements(
      creator: UUID, 
      org: UUID, 
      resource: UUID, 
      resourceType: UUID, 
      actions: Seq[String]): Seq[Entitlement] = {
    
    val ids = Option(Seq(creator))
    actions map { action =>
      newEntitlementResource(creator, org, resource, getActionName(resourceType, action), ids, None, None)
    }  
  }
  
  
  def newCreatorEntitlement(creator: UUID, org: UUID, resource: UUID, action: String) = {
    newEntitlementResource(creator, org, resource, action, Option(Seq(creator)), None, None)
  }
  
  
  def newEntitlementResource(
      creator: UUID,
      org: UUID, 
      resource: UUID, 
      action: String, 
      identities: Option[Seq[UUID]],
      name: Option[String] = None, 
      description: Option[String] = None): Entitlement = {
    
    log.error("newEntitlementResource(...)")
    
    val ent = Entitlement(
      id = UUID.randomUUID,
      org = org,
      name = (if (name.isDefined) name.get else s"${resource}.${action}"),
      description = description,
      properties = 
        EntitlementProps(
          action = action,
          value = None,
          identities = identities) )
          
    println("NEW-ENTITLEMENT:\n" + ent)
    println("AS-JSON:")
    println(Json.prettyPrint(Json.toJson(ent)))
    println
    
    ent

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
  
  def getAdminUser(account: AuthAccountWithCreds): AuthAccountWithCreds = {
    // TODO: Actually look up the 'admin' user - waiting on a change to the
    // security /sync payload to be able to identify this user.
    account  
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
    //val root = getRootOrgId(account)
    
    val admin = getAdminUser(account)
    
    for (acc <- rs) {
      log.debug(s"Creating User : ${acc.name}")
      
      val org = acc.directory.orgId
      
      createNewMetaUser(creator, org, acc,
          properties = Some(
            (userProps(acc) ++ Seq("gestalt_home" -> getRootOrgFqon(account))).toMap
          ),
          description = acc.description ) match {
        case Failure(err) => throw err
        case Success(usr) => {
          
          println("<////////////////////[ Creating User Entitlements ]////////////////////>")
          println("ORG: " + org)
          println("ADMIN: " + admin.account.id)
          println("RESOURCE: " + usr.id)
          
          val crud = resourceEntitlements(
              admin.account.id, 
              org,
              resource = usr.id,
              resourceType = ResourceIds.User,
              actions = Seq("create", "view", "update", "delete") )

          crud map { e =>
            CreateResource(
              ResourceIds.User, admin.account.id, org, Json.toJson(e), account, 
              Option(ResourceIds.Entitlement), Option(org)).get            
          }
          
        }
      }
    
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