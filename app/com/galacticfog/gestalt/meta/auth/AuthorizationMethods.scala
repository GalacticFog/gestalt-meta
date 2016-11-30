package com.galacticfog.gestalt.meta.auth

import java.util.UUID

import scala.util.{Try,Success,Failure}

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.models.GestaltResourceType
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import controllers.util._
import play.api.Logger
import play.api.libs.json._
import play.api.mvc.Result
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.annotation.tailrec

import com.galacticfog.gestalt.data.bootstrap.{ActionInfo,LineageInfo}

import scala.language.postfixOps

trait AuthorizationMethods extends ActionMethods with JsonInput {
  
  //private val log = Logger(this.getClass)
  
  /**
   * 
   * @param parent the parent of the new Resource
   */
  def setNewEntitlements(
      org: UUID,
      resource: UUID, 
      creator: AuthAccountWithCreds,
      parent: Option[UUID]) = {

    val res = ResourceFactory.findById(resource) getOrElse {
      throw new IllegalArgumentException(s"Resource with ID '$resource' not found.")
    }
    
    TypeFactory.findById(res.typeId).fold {
      throw new IllegalArgumentException(s"ResourceType with ID '${res.typeId}' not found.")
    }{ tpe =>

      // build list of actions for target and all child-type resources
      val actions = getNewResourceActionSet(res.typeId).toSeq
      
      if (log.isDebugEnabled) debugLogActions(res, actions)
      
      val ents = entitlements(creator.account.id, org, resource, actions)
      
      {
        if (parent.isEmpty) ents 
        else mergeParentEntitlements(ents, res.typeId, parent.get)
        
      } map { ent => CreateNewResource(org, creator, 
        json   = Json.toJson(ent), 
        typeId = Option(ResourceIds.Entitlement), 
        parent = /*parent*/ Option(resource))
      }

    }
  }
  
  private[this] def debugLogActions(res: GestaltResourceInstance, actions: Seq[String]) = {
    log.debug(s"Setting Entitlements for new : type=${ResourceLabel(res.typeId)}, name=${res.name}:")
    actions.sorted foreach { e => log.debug(e) }
  }
  
  def getResourceEntitlements(resource: UUID) = {
    ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, resource)
  }
  
  def mergeParentEntitlements(ours: Seq[Entitlement], ourType: UUID, parent: UUID) = {
    
    log.debug("mergeParentEntitlements(...)")
    
    /*
     * Get a list of Entitlements set on the parent.
     */
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
            
            log.debug(s"Found duplicate Entitlement in parent (${dup(0).name}- merging identities.")
            
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
    
    
    /*
     * TODO: It might be useful to add helper methods on GestaltResourceInstance to get 
     * the ActionInfo and LineageInfo objects from the type (???)
     */
    val tpe = getType(ourType)
    val prefix = JsonUtil.tryParse[ActionInfo](Json.parse(tpe.properties.get("actions"))).get.prefix
    
    /*
     * 
     * Here we filter parent entitlements for those that exist on the current ResourceType.
     * For example: If we're creating an environment, the parent is a workspace. Workspace will
     * have crud entitlements for environment. We take the entitlements we have in common and merge
     * the identities specified on the parent into the current resource.
     * 
     * This means that any identity that has permissions on the current resource type in the parent
     * will have them on this resource.
     * 
     * TODO: If this is the model we want to follow, we should do it not only for the current resource
     * type but for any resource types current has in common with the parent.
     * 
     */    
    go(theirs filter { e => e.properties.action.startsWith(prefix) }, ours)
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
  
  def isAuthorized(resource: UUID, identity: UUID, action: String, account: AuthAccountWithCreds) = Try {
    log.debug(s"Finding entitlements matching: $action($resource)")
    findMatchingEntitlement(resource, action) match {
      case None => false
      case Some(entitlement) => {
        val allowed = getAllowedIdentities(entitlement)
        val membership = getUserMembership(identity, account)

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
    
    if (ents.isEmpty) {
      log.info("No matching entitlements found.")
      None
    }
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

  /**
   * Generate a list of Entitlements corresponding to the given actions.
   */
  def entitlements(
      creator: UUID,
      org: UUID,
      resource: UUID,
      actions: Seq[String]): Seq[Entitlement] = {
    
    actions map { action =>
      newEntitlement(
          creator, org, 
          resource, 
          action, 
          Option(Seq(creator)), 
          name        = None, 
          value       = None, 
          description = None)
    }
  }  
  
  def newEntitlement(
      creator: UUID,
      org: UUID, 
      resource: UUID, 
      action: String,
      identities: Option[Seq[UUID]],
      name: Option[String] = None,
      value: Option[String] = None,
      description: Option[String] = None): Entitlement = {
    
    Entitlement(
      id = UUID.randomUUID,
      org = org,
      name = (if (name.isDefined) name.get else s"${resource}.${action}"),
      description = description,
      properties = EntitlementProps(action, value, identities) )
  }
  
  
  
  def isAuthorized(resource: UUID, action: String, account: AuthAccountWithCreds) = Try {
    
    log.debug(s"Finding entitlements matching: $action($resource)")
    
    findMatchingEntitlement(resource, action) match {
      case None => false
      case Some(entitlement) => {
        
        log.debug("Found matching entitlement.")
        
        val allowed = getAllowedIdentities(entitlement)
        val membership = getUserMembership(account.account.id, account)

        (allowed intersect membership).isDefinedAt(0)
        
      }
    }
  }
  
  
  
  
}