package controllers

import java.util.UUID

import scala.util.{Try,Success,Failure}

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import controllers.util._
import play.api.{Logger => log}
import play.api.libs.json._


object Actions {

  object Org {
    private val prefix = "org"
    val Create = s"$prefix.create"
    val View   = s"$prefix.view"
    val Update = s"$prefix.update"
    val Delete = s"$prefix.delete"
  }

  object Workspace {
    private val prefix = "workspace"
    val Create = s"$prefix.create"
    val View   = s"$prefix.view"
    val Update = s"$prefix.update"
    val Delete = s"$prefix.delete"
  }

  object Environment {
    private val prefix = "environment"
    val Create = s"$prefix.create"
    val View   = s"$prefix.view"
    val Update = s"$prefix.update"
    val Delete = s"$prefix.delete"
  }

  object Lambda {
    private val prefix = "lambda"
    val Create = s"$prefix.create"
    val View   = s"$prefix.view"
    val Update = s"$prefix.update"
    val Delete = s"$prefix.delete"
  }

  object Container {
    private val prefix = "container"
    val Create = s"$prefix.create"
    val View   = s"$prefix.view"
    val Update = s"$prefix.update"
    val Delete = s"$prefix.delete"
  }
}  

trait Authorization extends MetaController with SecurityResources {

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

  /**
   * 
   * @param org Org the Entitlements belong to
   * @param resourceType UUID of the type to create the Entitlements for
   * @param resourceId UUID of the instance to create the Entitlements for
   * @param parent UUID of the parent of the resource the Entitlements are created for. If parent
   * is provided, the given Entitlements will be merged with corresponding Entitlements specified
   * on the resource's parent.
   */
  def Entitle(org: UUID, resourceType: UUID, resourceId: UUID, parent: Option[UUID])(entitlements: => Seq[Entitlement])(implicit request: SecuredRequest[_]): Seq[Try[GestaltResourceInstance]] = {
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
  
  def AuthorizeList(action: String)(resources: => Seq[GestaltResourceInstance])(implicit request: SecuredRequest[_]) = {
    val caller = request.identity
    
    log.info(s"AUTHORIZE-LISTING : user=${caller.account.id}, action=${action}")
    
    val output = resources filter { r =>
      AuthorizationController.isAuthorized(r.id, caller.account.id, action, caller) getOrElse false
    }
    handleExpansion(output, request.queryString, META_URL)
  }
  
  
//  def Authorize[T](target: UUID, actionName: String, caller: AuthAccountWithCreds)(block: => T): Try[T] = Try {
//    
//    ???
//  }
  
  import org.joda.time.DateTime
  case class AuthorizationMessage(timestamp: DateTime = DateTime.now, status: String, user: UUID, resource: UUID, action: String, message: Option[String] = None)
  object AuthorizationMessage {
    implicit lazy val authorizationMessageFormat = Json.format[AuthorizationMessage]
  }
  
  
  
  //
  // TODO: Use AuthorizationMessage for trace logging.
  //
  
  def Authorize(target: UUID, actionName: String, caller: AuthAccountWithCreds)(block: => play.api.mvc.Result): play.api.mvc.Result = {
    AuthorizationController.isAuthorized(
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
  
  
  def Authorize(target: UUID, actionName: String)(block: => play.api.mvc.Result)(implicit request: SecuredRequest[_]): play.api.mvc.Result = {
    Authorize(target, actionName, request.identity)(block)
  }  
  

//  def isAuthorized(resource: UUID, identity: UUID, action: String, account: AuthAccountWithCreds) = Try {
//    findMatchingEntitlement(resource, action) match {
//      case None => false
//      case Some(entitlement) => {
//        val allowed = getAllowedIdentities(entitlement)
//        val membership = getUserMembership(identity, account)
//        (allowed intersect membership).isDefinedAt(0)
//      }
//    }
//  }  
  
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
    ent
  }  
  
  def mkActionName(typeId: UUID, action: String) = {
    s"${ActionPrefix(typeId)}.${action}"
  }
  
}

