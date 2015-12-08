package controllers

import java.util.UUID

import scala.util.Failure
import scala.util.Success

import com.galacticfog.gestalt.data.TypeFactory
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.security.api.errors.SecurityRESTException
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.galacticfog.gestalt.tasks.play.io.NonLoggingTaskEvents
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator

import controllers.util._
import play.api.{Logger => log}


object DeleteController extends GestaltFrameworkSecuredController[DummyAuthenticator] 
    with MetaController with NonLoggingTaskEvents with SecurityResources {

  /**
   * Permanently delete an Org from Security and Meta
   */
  def hardDeleteOrg(org: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    trace(s"hardeDeleteOrg($org)")
    hardDeleteSecure(org, request.identity, Security.deleteOrg)    
  }

  def hardDeleteOrgFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"hardDeleteOrgFqon($fqon)")
    orgFqon(fqon) match {
      case Some(org) => hardDeleteSecure(org.id, request.identity, Security.deleteOrg)
      case None      => OrgNotFound(fqon)
    }    
  }
  
  
  /**
   * Permanently delete a User/Account from Security and Meta
   */
  def hardDeleteUser(org: UUID, id: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    trace(s"hardDeleteUser(org = $org, user = $id")
    hardDeleteSecure(id, request.identity, Security.deleteAccount)
  }
  
  def hardDeleteUserFqon(fqon: String, id: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"hardDeleteUserFqon($fqon, user = $id")
    orgFqon(fqon) match {
      case Some(org) => hardDeleteSecure(id, request.identity, Security.deleteAccount)
      case None      => OrgNotFound(fqon)
    }
  }

  
  /**
   * Permanently delete a ResourceType along with any associated TypeProperties
   */
   def hardDeleteResourceTypeFqon(fqon: String, id: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
     trace(s"hardDeleteResourceTypeFqon($fqon, $id)")
     orgFqon(fqon) match {
       case Some(org) => hardDeleteResourceType(id)
       case None      => OrgNotFound(fqon)
     }
   }
  
   def hardDeleteResourceType(typeId: UUID) = {
     TypeFactory.hardDeleteType(typeId) match {
       case Success(_) => NoContent
       case Failure(e) => e match {
         case iae : IllegalArgumentException => BadRequestResult(iae.getMessage)
         case x => GenericErrorResult(500, x.getMessage)
       }
     }
   }

  /**
   * Permanently delete a Resource from Meta and Security
   */   
  def hardDeleteSecure(id: UUID, auth: AuthAccountWithCreds, fn: SecurityDelete) = {
    
    hardDeleteSynchronized(id, auth, fn) match {
      case Success(_) => NoContent
      case Failure(e) => {
        log.error(s"hardDeleteSecure: ERROR: " + e.getMessage)
        e match {
          case s: SecurityRESTException     => handleSecurityApiException(s)
          case r: ResourceNotFoundException => NotFoundResult(r.getMessage)
          case _ => GenericErrorResult(500, e.getMessage)
        }
      }
    }
  }

  
  // TODO: Simpler since resources aren't synced with security.
  private def hardDeleteMetaResource(id: UUID) = {
    ???
  }
   
}