package controllers


import play.api.{ Logger => log }

import scala.util.Failure
import scala.util.Success

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.galacticfog.gestalt.tasks.play.io.NonLoggingTaskEvents
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator

import controllers.util.MetaController
import controllers.util.Security
import controllers.util.db._


object BootstrapController extends GestaltFrameworkSecuredController[DummyAuthenticator] 
    with MetaController with NonLoggingTaskEvents {
  
  def bootstrap() = GestaltFrameworkAuthAction(nullOptString(None)) { implicit request =>
    trace("bootstrap()")
    
    log.debug("Looking up root Org in gestalt-security...")
    val rootOrgId = Security.getRootOrg(request.identity) match {
      case Success(org) => org.id
      case Failure(err) => {
        log.error("Failed Looking up root user in gestalt-security: " + err.getMessage)
        throw err
      }
    }
    
    log.debug("Taking user from current request...")
    val adminUserId = request.identity.account.id
    val owner = ResourceOwnerLink(ResourceIds.User, adminUserId)
    
    log.debug("bootstrap : [root-org-id]  : " + rootOrgId.toString)
    log.debug("bootstrap : [root-user-id] : " + rootOrgId.toString)
    
    log.debug("Initializing bootstrapper...")
    val db = new Bootstrap(rootOrgId, owner, ConnectionManager.currentDataSource())
    
    log.debug("Beginning migration...")
    (for {
      a <- db.clean
      b <- db.migrate
      c <- db.loadReferenceData
      d <- db.loadSystemTypes
    } yield d) match {
      case Success(_) => {
        log.info("Successfully rebuilt Meta DB.")
        NoContent
      }
      case Failure(e) => {
        log.error("Could not rebuild Meta DB: " + e.getMessage)
        InternalServerError(e.getMessage)
      }
    }
    
  }
  
  
  
}