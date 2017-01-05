package com.galacticfog.gestalt.meta.auth


import java.util.UUID

import scala.util.{Try,Success,Failure}

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.meta.api.errors.ForbiddenException

import controllers.util._
import play.api.{Logger => log}
import play.api.libs.json._
import play.api.mvc.Result
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.annotation.tailrec
import scala.language.postfixOps

import com.galacticfog.gestalt.data.models.GestaltResourceType
import com.galacticfog.gestalt.data.bootstrap.{ActionInfo,LineageInfo}
import com.galacticfog.gestalt.json.Js


trait Authorization extends MetaController with ActionMethods with AuthorizationMethods { self: SecureController =>

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
    val newEntitlements = {
      if (parent.isEmpty) entitlements else
      mergeParentEntitlements(entitlements, resourceType, parent.get)
    }
    newEntitlements map { e =>
      CreateResource(
        ResourceIds.User, user.account.id, org, Json.toJson(e), user,
        Option(ResourceIds.Entitlement), parentId = Option(resourceId))
    }
  }
  
  def Authorize(target: UUID, actionName: String)(block: => play.api.mvc.Result)(implicit request: SecuredRequest[_]): play.api.mvc.Result = {
    Authorize(target, actionName, request.identity)(block)
  }
  
  def Authorize(target: UUID, actionName: String, caller: AuthAccountWithCreds)(block: => Result): Result = {
    isAuthorized(target, caller.account.id, actionName, caller) match {
      case Failure(err) => HandleExceptions(err)
      case Success(auth) => if (auth) {
        log.info(s"{AUTHORIZED: user=${caller.account.id}, resource=${target}, action=${actionName}}")
        block 
      } else {
        log.warn(s"{UNAUTHORIZED: user=${caller.account.id}, resource=${target}, action=${actionName}}")
        ForbiddenResult(s"You do not have permission to perform this action. Failed: '$actionName'")
      }
    }
  }

  def AuthorizeAsync(target: UUID, actionName: String)(block: => play.api.mvc.Result)(implicit request: SecuredRequest[_]): Future[Result] = {
    AuthorizeAsync(target, actionName, request.identity)(block)
  }
  
  def AuthorizeAsync(target: UUID, actionName: String, caller: AuthAccountWithCreds)(block: => Result): Future[Result] = {
    Future( Authorize(target, actionName, caller)(block) )
  } 
   
  def AuthorizeList(action: String)(resources: => Seq[GestaltResourceInstance])(implicit request: SecuredRequest[_]) = {
    val caller = request.identity
    
    log.info(s"AUTHORIZE-LISTING : user=${caller.account.id}, action=${action}")
    
    val output = resources filter { r =>
      isAuthorized(r.id, caller.account.id, action, caller) getOrElse false
    }
    handleExpansion(output, request.queryString, META_URL)
  }
  
  def forbidden(action: String) = new ForbiddenException(
    s"You do not have permission to perform this action. Failed: '$action'"
  )
  
}

