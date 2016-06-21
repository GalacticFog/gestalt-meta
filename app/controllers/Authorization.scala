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
            // Merge merge h->identities with dup->identities
            val duplicate = dup(0)
            val theirIds = h.properties.identities getOrElse Seq()
            val ourIds = duplicate.properties.identities getOrElse Seq()
            
            // Merge ids found in parent that we don't have.
            val allIds = ourIds ++ theirIds.diff(ourIds)
            
            // Copy properties (with new ids) into the duplicate entitlement.
            val newProperties = duplicate.properties.copy(identities = Option(allIds))
            val newEntitlement = duplicate.copy(properties = newProperties)
            
            // Replace duplicate with newEntitlement in acc
            go(t, acc map { 
              case d if d.properties.action == currentAction => newEntitlement
              case x => x })
          }
          
        }
      }
    }
    
    val prefix = ActionPrefix(ourType)
    println("||||||||||||||Using prefix : " + prefix)
    go(theirs filter { e => e.properties.action.startsWith(prefix) }, ours)
    
  }
  
  
  private def seq2map(ents: Seq[Entitlement]): Map[String,Entitlement] = {
    ents map { e => (e.properties.action, e) } toMap
  }
  
  private def findByAction(ents: Seq[Entitlement], action: String): Option[Entitlement] = {
    val found = ents filter { e => e.properties.action == action }
    found size match {
      case 0 => None
      case 1 => Option(found(0))
      case _ => throw new RuntimeException(s"Multiple entitlements found for action '$action'")
    }
  }
  
  def mergeEntitlementsKeepLeft(ours: Seq[GestaltResourceInstance], theirs: Seq[GestaltResourceInstance]) = {
    
  }
  
  
  def eprops(r: GestaltResourceInstance): EntitlementProps = {
    val props = r.properties.get
    val action = props("action")
    val value = if (props.contains("value")) Some(props("value")) else None
    val identities = if (props.contains("identities")) Some(props("identities")) else None
    
    // TODO: Combine ops to regex for replaceAll
    def uuidsFromString(str: String) = str.
      stripPrefix("[").
      stripSuffix("]").
      replaceAll("\"", "").
      split(",").
      toSeq.
      map { UUID.fromString(_)
    }
    EntitlementProps(action, value, identities map { uuidsFromString(_) })
  }  
  
  /**
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
    val output = resources filter { r =>
      AuthorizationController.isAuthorized(r.id, caller.account.id, action, caller) getOrElse false
    }
    handleExpansion(output, request.queryString, META_URL)
  }
  
  def Authorize(target: UUID, actionName: String, caller: AuthAccountWithCreds)(block: => play.api.mvc.Result) = {//(implicit request: SecuredRequest[_]) = {
    AuthorizationController.isAuthorized(
        target, caller.account.id, actionName, caller) match {
      case Failure(err) => HandleExceptions(err)
      case Success(auth) => {
        if (auth) {
          log.info(s"{Authorized: user=${caller.account.id}, resource=${target}, action=${actionName}")
          block 
        } else ForbiddenResult(s"You do not have permission to perform this action. Failed: '$actionName'")
      }
    }
  }  
  
  def AuthorizeById(target: UUID, actionName: String)(block: => play.api.mvc.Result)(implicit request: SecuredRequest[_]) = {
    AuthorizationController.isAuthorized(
        target, request.identity.account.id, actionName, request.identity) match {
      case Failure(err) => HandleExceptions(err)
      case Success(auth) => {
        if (auth) {
          log.info(s"{Authorized: user=${request.identity.account.id}, resource=${target}, action=${actionName}")
          block 
        } else ForbiddenResult("You do not have permission to access this resource")
      }
    }
  }  
  
  
  val ACTIONS_CRUD = Seq("create", "view", "update", "delete")

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
  
  private def getActionName(typeId: UUID, action: String) = {
    s"${ActionPrefix(typeId)}.${action}"
  }
  
}

