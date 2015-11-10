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
import com.galacticfog.gestalt.meta.api.GestaltResourceInput
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.output.gestaltResourceInputFormat
import com.galacticfog.gestalt.meta.api.output.gestaltResourceInstanceFormat
import com.galacticfog.gestalt.meta.api.rte
import com.galacticfog.gestalt.meta.api.ResourceNotFoundException
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.{GestaltResource => SecurityResource}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.galacticfog.gestalt.tasks.play.io.NonLoggingTaskEvents
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator

import controllers.util.MetaController
import controllers.util.Security
import controllers.util.toError
import play.api.{Logger => log}
import play.api.libs.json.JsError
import play.api.libs.json.JsValue
import play.api.libs.json.Json


object DeleteController extends GestaltFrameworkSecuredController[DummyAuthenticator] 
    with MetaController with NonLoggingTaskEvents {
    
  
  type SecurityDelete = (UUID, AuthAccountWithCreds) => Try[Boolean]

  
  /**
   * Permanently delete an Org from Security and Meta
   */
  def hardDeleteOrg(org: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    trace(s"hardeDeleteOrg($org)")
    hardDelete(org, request.identity, Security.deleteOrg)    
  }

  def hardDeleteOrgFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"hardDeleteOrgFqon($fqon)")
    orgFqon(fqon) match {
      case Some(org) => hardDelete(org.id, request.identity, Security.deleteOrg)
      case None      => OrgNotFound(fqon)
    }    
  }
  
  
  /**
   * Permanently delete a User/Account from Security and Meta
   */
  def hardDeleteUser(org: UUID, id: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    trace(s"hardDeleteUser(org = $org, user = $id")
    hardDelete(id, request.identity, Security.deleteAccount)
  }
  
  def hardDeleteUserFqon(fqon: String, id: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"hardDeleteUserFqon($fqon, user = $id")
    orgFqon(fqon) match {
      case Some(org) => hardDelete(id, request.identity, Security.deleteAccount)
      case None      => OrgNotFound(fqon)
    }
  }


  /**
   * Permanently delete a Resource from Meta
   */
  def hardDelete(id: UUID, auth: AuthAccountWithCreds, fn: SecurityDelete) = {
    fn(id, auth) match {
      case Failure(e) => {
        log.error("hardDelete::Security Error : " + e.getMessage)
        handleSecurityApiException(e) //InternalServerError(toError(500, e.getMessage))
      }
      case Success(_) => ResourceFactory.hardDeleteResource(id) match {
        case Success(_) => NoContent
        case Failure(e) => e match {
          //
          // TODO: ResourceFactory needs to throw typed exception indicating NotFound
          //
          case rnf: ResourceNotFoundException => NotFound(rnf.getMessage)
          case _ => {
            log.error("hardDelete::Internal Delete Error")
            InternalServerError(toError(500, e.getMessage))
          }
        }
      }
    }
  }
  
  
  // TODO: Simpler since resources aren't synced with security.
  private def hardDeleteMetaResource(id: UUID) = {
    ???
  }
    
  
}