package controllers

import scala.util.Failure
import scala.util.Success

import com.galacticfog.gestalt.data.bootstrap.Bootstrap
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink

import controllers.util.Security
import controllers.util.db.ConnectionManager
import controllers.util.trace
import play.api.{Logger => log}
import com.galacticfog.gestalt.meta.auth.Authorization

object BootstrapController extends Authorization {
  
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
    val db = new Bootstrap(ResourceIds.Org, rootOrgId, rootOrgId, owner, ConnectionManager.currentDataSource())
    
    
    import scala.util.Try
    
    
    log.debug("Beginning migration...")
    
    (for {
      a <- db.clean
      b <- db.migrate
      c <- db.loadReferenceData
      d <- db.loadSystemTypes
      e <- db.initialize("root")
    } yield e) match {
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