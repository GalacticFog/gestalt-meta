package com.galacticfog.gestalt.meta.auth


import java.util.UUID

import scala.util.{Try,Success,Failure}

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import controllers.util._
import play.api.{Logger => log}
import play.api.libs.json._
import play.api.mvc.Result
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.annotation.tailrec


trait Authorization extends MetaController {

  
  /**
   * 
   * @param org Org the Entitlements belong to
   * @param resourceType UUID of the type to create the Entitlements for
   * @param resourceId UUID of the instance to create the Entitlements for
   * @param parent UUID of the parent of the resource the Entitlements are created for. If parent
   * is provided, the given Entitlements will be merged with corresponding Entitlements specified
   * on the resource's parent.
   */
  def Entitle(org: UUID, resourceType: UUID, resourceId: UUID, parent: Option[UUID])
      (entitlements: => Seq[Entitlement])(implicit request: SecuredRequest[_]): Seq[Try[GestaltResourceInstance]] = {
    Entitle(org, resourceType, resourceId, request.identity, parent)(entitlements)
  }
  
  def Entitle(org: UUID, resourceType: UUID, resourceId: UUID, user: AuthAccountWithCreds, parent: Option[UUID])(entitlements: => Seq[Entitlement])= {
    (if (parent.isEmpty) entitlements else
      mergeParentEntitlements(entitlements, resourceType, parent.get)) map { e =>
      CreateResource(
        ResourceIds.User, user.account.id, org, Json.toJson(e), user,
        Option(ResourceIds.Entitlement), Option(resourceId))
    }
  }  
  

  
  
//  def Authorize[T](target: UUID, actionName: String, caller: AuthAccountWithCreds)(block: => T): Try[T] = Try {
//    
//    ???
//  }
  
  def Authorize(target: UUID, actionName: String)(block: => play.api.mvc.Result)(implicit request: SecuredRequest[_]): play.api.mvc.Result = {
    Authorize(target, actionName, request.identity)(block)
  }
  
  def Authorize(target: UUID, actionName: String, caller: AuthAccountWithCreds)(block: => Result): Result = {
    isAuthorized(
        target, caller.account.id, actionName, caller) match {
      case Failure(err) => HandleExceptions(err)
      case Success(auth) => {
        if (auth) {
          log.info(s"{AUTHORIZED: user=${caller.account.id}, resource=${target}, action=${actionName}")
          block 
        } else ForbiddenResult(s"You do not have permission to perform this action. Failed: '$actionName'")
      }
    }
  }
  
  def AuthorizeList(action: String)(resources: => Seq[GestaltResourceInstance])(implicit request: SecuredRequest[_]) = {
    val caller = request.identity
    
    log.info(s"AUTHORIZE-LISTING : user=${caller.account.id}, action=${action}")
    
    val output = resources filter { r =>
      isAuthorized(r.id, caller.account.id, action, caller) getOrElse false
    }
    handleExpansion(output, request.queryString, META_URL)
  }
  
  def AuthorizeAsync(target: UUID, actionName: String, caller: AuthAccountWithCreds)(block: => Future[Result]): Future[Result] = {
    isAuthorized(
        target, caller.account.id, actionName, caller) match {
      case Failure(err) => Future(HandleExceptions(err))
      case Success(auth) => {
        if (auth) {
          log.info(s"{AUTHORIZED: user=${caller.account.id}, resource=${target}, action=${actionName}")
          block 
        } else Future(ForbiddenResult(s"You do not have permission to perform this action. Failed: '$actionName'"))
      }
    }
  }  
  
  def getResourceEntitlements(resource: UUID) = {
    ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, resource)
  }
  
  /*
   * TODO: This method should match entitlements with the same action-name, pulling identities from 'theirs' into ours.
   *  
   * If (ours contains theirs)
   *   keep ours
   *   get theirs.identities
   *   add identities they have that we don't
   * If (ours !contains theirs)
   *   Add theirs to ours (create the same entitlement on the current resource)
   * 
   */
  def mergeParentEntitlements(ours: Seq[Entitlement], ourType: UUID, parent: UUID) = {
    log.debug("mergeParentEntitlements(...)")
    
    val theirs = getResourceEntitlements(parent) map { Entitlement.make( _ ) }
    
    def go(pents: Seq[Entitlement], acc: Seq[Entitlement]): Seq[Entitlement] = {
      pents match {
        case Nil => acc
        case h :: t => {
          
          val currentAction = h.properties.action
          val dup = acc filter { e => e.properties.action == currentAction }
          
          if (dup.isEmpty) { 
            
            // We DO NOT have the entitlement, add it
            // -----------------------------------------------
            log.debug(s"Adding Entitlement action '$currentAction' to set.")
            go(t, (h.copy(id = UUID.randomUUID) +: acc))
            
          } else {
            
            log.debug(s"Found duplicate Entitlement in parent - merging identities.")
            
            // We already have the property, merge identities
            // -----------------------------------------------
            // 
            // Merge h->identities with dup->identities
            val duplicate = dup(0)
            val theirIds  = h.properties.identities getOrElse Seq()
            val ourIds    = duplicate.properties.identities getOrElse Seq()
            
            // Merge ids found in parent that we don't have.
            val allIds = ourIds ++ theirIds.diff(ourIds)
            
            // Copy properties (with new ids) into the duplicate entitlement.
            val newProperties  = duplicate.properties.copy(identities = Option(allIds))
            val newEntitlement = duplicate.copy(properties = newProperties)
            
            // Replace duplicate with newEntitlement in acc
            go(t, acc map { 
              case d if d.properties.action == currentAction => newEntitlement
              case x => x })
          }
          
        }
      }
    }
    
    go(theirs filter { e => e.properties.action.startsWith(ActionPrefix(ourType)) }, ours)
  }


  
  def getEntitlementsMerged(resourceId: UUID): Seq[GestaltResourceInstance] = {
    entitlementsMerged(ResourceFactory.findEffectiveEntitlements(resourceId))
  }

  def entitlementsMerged(es: Map[Int, Seq[GestaltResourceInstance]]): Seq[GestaltResourceInstance] = {

    @tailrec def go(
        exclude: Map[String, GestaltResourceInstance], 
        ez: List[(Int, Seq[GestaltResourceInstance])], 
        acc: Map[String, GestaltResourceInstance])  : Map[String, GestaltResourceInstance] = {
      ez match {
        case Nil => acc
        case h :: t => go(exclude, t, acc ++ accumulateEnts(acc, h._2))
      }
    }

    if (es.isEmpty) Seq() else {
      
      // Highest key corresponds with highest level in the hierarchy.
      val max = es.keys.max
      val topLevel = es(max)
      
      // Top-Down evaluation
      //val ents = (es - max).toSeq.sortWith((a,b) => a._1 > b._1)
      
      // Bottom-Up evaluation
      val ents = (es - max).toSeq.sortWith((a,b) => a._1 < b._1)
  
      // Add all top-level entitlements to map
      val output = topLevel map { e => (entitlementAction(e), e) } toMap    
      
      (output ++ go(output, ents.toList, Map())).values.toSeq
    }   
  }    
  
  /**
   * Get a Seq containing the User ID and the IDs of all Groups the User is a
   * member of.
   */
  def getUserMembership(user: UUID, account: AuthAccountWithCreds): Seq[UUID] = {
    //user +: (Security.getAccountGroups(user, account).get map { _.id })
    user +: account.groups.map( _.id )
  }
  
  private[auth] def isAuthorized(resource: UUID, identity: UUID, action: String, account: AuthAccountWithCreds) = Try {
    findMatchingEntitlement(resource, action) match {
      case None => false
      case Some(entitlement) => {
        println("*******************GOT ENTITLEMENTS**********************")
        val allowed = getAllowedIdentities(entitlement)
        val membership = getUserMembership(identity, account)
        
        println("***ALLOWED IDENTITIES:\n" + allowed)
        println("***MEMBERSHIP:\n" + membership)
        println("INTERSECT: " + (allowed intersect membership))
        
        (allowed intersect membership).isDefinedAt(0)
        
      }
    }
  }    
  
  
  def entitlementAction(e: GestaltResourceInstance) = {
    e.properties.get("action")
  }  
  
  private[auth] def entitlementsAll(es: Map[Int, Seq[GestaltResourceInstance]]): Seq[GestaltResourceInstance] = {
    es flatMap { case (_,v) => v } toSeq
  }  
  

  private[auth] def entitlementsFiltered(es: Map[Int, Seq[GestaltResourceInstance]], actions: Seq[String]): Seq[GestaltResourceInstance] = {
    es flatMap { case (_,v) => v } filter { e => actions.contains(entitlementAction(e)) } toSeq
  }  

  
  def accumulateEnts(exclude: Map[String,GestaltResourceInstance], ents: Seq[GestaltResourceInstance]) = {
    def go(es: Seq[GestaltResourceInstance], acc: Map[String, GestaltResourceInstance]): Map[String,GestaltResourceInstance] = {
      es match {
        case Nil => acc
        case h :: t => {
          val action = entitlementAction(h)
          val newacc = if (acc.contains(action)) acc else acc ++ Map(action -> h)
          
          go(t, newacc)
        }
      }
    }
    go(ents, exclude)
  }  

  

  
  object PermissionSet {
    val SELF = "self"
    val MERGED = "merged"
  }
  
  def entitlementsByAction(resource: UUID, action: String, setType: String = PermissionSet.SELF) = {
    val entitlements = setType match {
      case PermissionSet.SELF => ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, resource)
      case PermissionSet.MERGED => ResourceFactory.findEffectiveEntitlements(resource) flatMap { _._2 } toList
      case e => throw new IllegalArgumentException(s"Invalid PermissionSet type '$e'")
    } 
    entitlements filter { _.properties.get("action") == action }
  }  
  
  /**
   * Search the effective entitlements on the given resource for the given action name.
   * 
   * @param resource ID of Resource to search for the matching entitlement
   * @param action   exact name of the action to search for
   */
  private[auth] def findMatchingEntitlement(resource: UUID, action: String): Option[GestaltResourceInstance] = {
    val ents = getEntitlementsMerged(resource) filter { ent =>
      ent.properties.get("action") == action
    }
    println("**********Entitlements Found:\n" + ents)
    if (ents.isEmpty) None
    else if (ents.size > 1) {
      throw new RuntimeException(
          s"Multiple entitlements found for action '$action'. Data is corrupt.")
    } 
    else Option(ents(0))    
  }
  
  
  /**
   * Parse the array of identities defined in an entitlement Resource. 
   * (resource.properties.identities)
   * 
   */
  private[auth] def getAllowedIdentities(entitlement: GestaltResourceInstance): Seq[UUID] = {
    
    entitlement.properties.get.get("identities") match {
      case None => Seq()
      case Some(identities) => {
        
        Json.parse(identities).validate[Seq[String]].map {
          case sq: Seq[String] => sq map { UUID.fromString(_) }
        }.recoverTotal { e =>
          log.error("Failed parsing 'identities' to Seq[UUID].")
          throw new RuntimeException(
              s"Failed parsing Entitlement identities: " + JsError.toFlatJson(e).toString)
        }
        
      }
    }
  }  
  
  
  val ACTIONS_CRUD = Seq("create", "view", "update", "delete")

  
  /**
   * Generate a list of Entitlements on a list of Resource types.
   */
  def generateEntitlements(
    creator: UUID,
    org: UUID,
    resource: UUID,
    resourceTypes: Seq[UUID],
    actions: Seq[String]): Seq[Entitlement] = {

    for {
      t <- resourceTypes
      o <- resourceEntitlements(creator, org, resource, t, actions)
    } yield o
  }
  
  
  /**
   * Generate a list of Entitlements for a given Resource of a given type.
   */
  def resourceEntitlements(
      creator: UUID, 
      org: UUID, 
      resource: UUID, 
      resourceType: UUID, 
      actions: Seq[String]): Seq[Entitlement] = {
    
    val ids = Option(Seq(creator))
    actions map { action =>
      newEntitlementResource(creator, org, resource, mkActionName(resourceType, action), ids, None, None)
    }  
  }
  

  def newEntitlementResource(
      creator: UUID,
      org: UUID, 
      resource: UUID, 
      action: String, 
      identities: Option[Seq[UUID]],
      name: Option[String] = None, 
      description: Option[String] = None): Entitlement = {
    
    Entitlement(
      id = UUID.randomUUID,
      org = org,
      name = (if (name.isDefined) name.get else s"${resource}.${action}"),
      description = description,
      properties = 
        EntitlementProps(
          action = action,
          value = None,
          identities = identities) )
  }
  
  def mkActionName(typeId: UUID, action: String) = {
    s"${ActionPrefix(typeId)}.${action}"
  }
  
}

