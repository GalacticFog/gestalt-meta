package com.galacticfog.gestalt.meta.auth

import java.util.UUID

import scala.util.{Try,Success,Failure}

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import controllers.util._
import play.api.Logger
import play.api.libs.json._
import play.api.mvc.Result
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.annotation.tailrec


trait AuthorizationMethods extends EntitlementMethods {
  
  private val log = Logger(this.getClass)
  
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