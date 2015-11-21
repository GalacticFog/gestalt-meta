package controllers

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.PropertyValidator
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.ResourceIds
import com.galacticfog.gestalt.data.ResourceType
import com.galacticfog.gestalt.data.illegal
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.models.ResourceOwnerLink
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.{PatchOp, PatchDocument, PatchHandler}
import com.galacticfog.gestalt.meta.api.GestaltResourceInput
import com.galacticfog.gestalt.meta.api.output._

import com.galacticfog.gestalt.meta.api.rte
import com.galacticfog.gestalt.meta.api.ResourceNotFoundException
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.{ GestaltResource => SecurityResource }
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.galacticfog.gestalt.tasks.play.io.NonLoggingTaskEvents
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator

import controllers.util._
import play.api.{ Logger => log }
import play.api.libs.json.JsError
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.data.ResourceStates

import controllers.util.stringmap

object SyncController extends GestaltFrameworkSecuredController[DummyAuthenticator] with MetaController with NonLoggingTaskEvents with SecurityResources {

  def sync() = GestaltFrameworkAuthAction(nullOptString(None)) { implicit request =>
    trace("sync")
    
    Try {
      
      val sd = Security.getOrgSyncTree(None, request.identity) match {
        case Success(data) => data
        case Failure(err)  => throw err
      }
  
      val metaorgs = ResourceFactory.findAll(ResourceIds.Org)
      val metausers = ResourceFactory.findAll(ResourceIds.User)
  
      // Get Ids of everything in security
      val secOrgIds = sd.orgs map { _.id }
      val secAccIds = sd.accounts map { _.id }
      
      // Get Ids of everything in meta
      val metaOrgIds = metaorgs map { _.id }
      val metaUserIds = metausers map { _.id }
  
      val secOrgMap = (sd.orgs map { o => (o.id, o) }).toMap
      val secAccMap = (sd.accounts map { a => (a.id, a) }).toMap
  
      val (orgsCreate,orgsDelete)   = computeResourceDiffs(secOrgIds, metaOrgIds)
      val (usersCreate,usersDelete) = computeResourceDiffs(secAccIds, metaUserIds)
      
      /*
       * TODO: Refactor this - as it is, errors are swallowed. 
       */
      val creator = request.identity.account.id

      log.debug(s"Deleting ${orgsDelete.size} Orgs.")
      deleteResources(creator, orgsDelete)
      
      log.debug(s"Deleteing ${usersDelete.size} Users.")
      deleteResources(creator, usersDelete)
      
      createOrgs (creator, (orgsCreate  map secOrgMap), request.identity)
      createUsers(creator, (usersCreate map secAccMap) )
      
    } match {
      case Success(_) => NoContent
      case Failure(e) => InternalServerError(toError(500, e.getMessage))
    }
  }
  
  def deleteResources(identity: UUID, ids: Iterable[UUID]) = {
    for (id <- ids) ResourceFactory.hardDeleteResource(id).get
  }
  
  def createOrgs(creator: UUID, rs: Iterable[GestaltOrg], auth: AuthAccountWithCreds) = {
    for (org <- rs) {
      log.debug(s"Creating Org : ${org.name}")
      val parent = parentOrgId(org, auth)
      createNewMetaOrg(parent, org, creator, properties = None).get
    }
  }
  
  def createUsers(creator: UUID, rs: Iterable[GestaltAccount]) = {    
    for (acc <- rs) {
      log.debug(s"Creating User : ${acc.name}")
      createNewMetaUser(acc.directory.orgId, acc, creator, 
        properties = Some(Map(
          "email"       -> acc.email,
          "firstName"   -> acc.firstName,
          "lastName"    -> acc.lastName,
          "phoneNumber" -> acc.phoneNumber)) ).get
    }
  }
  
  
//  def sync() = GestaltFrameworkAuthAction(nullOptString(None)) { implicit request =>
//    trace("sync")
//
//    log.debug("Getting Orgs from Security.")
//    /* Get Security Org/User tree */
//    val sd = Security.getOrgSyncTree(None, request.identity) match {
//      case Success(data) => data
//      case Failure(err)  => throw err
//    }
//
//    /* Get parent of given org - get root Org if None */
//    def parentId(o: GestaltOrg) = {
//      if (o.parent.isDefined) o.parent.get.id
//      else Security.getRootOrg(request.identity) match {
//        case Success(org) => org.id
//        case Failure(err) => throw err
//      }
//    }
//
//    val metaorgs = ResourceFactory.findAll(ResourceIds.Org)
//    val metausers = ResourceFactory.findAll(ResourceIds.User)
//
//    // Get Ids of everything in security
//    val secOrgIds = sd.orgs map { _.id }
//    val secAccIds = sd.accounts map { _.id }
//    
//    // Get Ids of everything in meta
//    val metaOrgIds = metaorgs map { _.id }
//    val metaUserIds = metausers map { _.id }
//
//    val secOrgMap = (sd.orgs map { o => (o.id, o) }).toMap
//    val secAccMap = (sd.accounts map { a => (a.id, a) }).toMap
//
//    val orgsCreate = secOrgIds.diff(metaOrgIds) map { u => secOrgMap(u) }
//    val usersCreate = secAccIds.diff(metaUserIds) map { u => secAccMap(u) }
//
//    
//
//    log.debug(s"Orgs Create [${orgsCreate.size} orgs]:")
//    for (o <- orgsCreate) log.debug("%s, %s".format(o.id, o.name))
//
//    log.debug(s"Users Create [${usersCreate.size} users]:")
//    for (a <- usersCreate) log.debug("%s, %-10s : org: %s".format(a.id, a.name, a.directory.orgId))
//
//    log.debug("Creating Resources in Meta...")
//
//    
//    /*
//     * TODO: Refactor this - as it is any errors in the create methods are swallowed. 
//     */
//    
//    Try {
//      val creator = request.identity.account.id
//      
//      for (o <- orgsCreate) {
//        createNewMetaOrg(parentId(o), o, creator, properties = None)
//      }
//      
//      for (a <- usersCreate) {
//        val uprops = Some(Map(
//            "email"       -> a.email,
//            "firstName"   -> a.firstName,
//            "lastName"    -> a.lastName,
//            "phoneNumber" -> a.phoneNumber))
//        createNewMetaUser(a.directory.orgId, a, creator, properties = uprops)
//      }
//
//    } match {
//      case Success(_) => NoContent
//      case Failure(e) => InternalServerError
//    }
//    
//  }
  
  
  private def computeResourceDiffs(securityIds: Seq[UUID], metaIds: Seq[UUID]) = {
    val create = securityIds.diff(metaIds) // in security, not meta
    val delete = metaIds.diff(securityIds) // in meta, not security
    (create, delete)
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