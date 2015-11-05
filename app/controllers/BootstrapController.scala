package controllers


import play.api.{ Logger => log }

import scala.util.Failure
import scala.util.Success

import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.galacticfog.gestalt.tasks.play.io.NonLoggingTaskEvents
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator

import bootstrap.DbUtil
import controllers.util.MetaController
import controllers.util.Security



object BootstrapController extends GestaltFrameworkSecuredController[DummyAuthenticator] 
    with MetaController with NonLoggingTaskEvents {
  
  def bootstrap() = GestaltFrameworkAuthAction(nullOptString(None)) { implicit request =>
    trace("bootstrap()")
    
    val rootOrgId = Security.getRootOrg(request.identity) match {
      case Success(org) => org.id
      case Failure(err) => ???
    }
    val rootUserId = request.identity.account.id
    
    log.debug("bootstrap : [root-org-id]  : " + rootOrgId.toString)
    log.debug("bootstrap : [root-user-id] : " + rootOrgId.toString)
    
    val db = new DbUtil(rootOrgId, rootUserId)
    
    (for {
      a <- db.clean
      b <- db.migrate
      c <- db.createReferenceData
      d <- db.createBaseTypes
    } yield d) match {
      case Success(_) => NoContent
      case Failure(e) => InternalServerError(e.getMessage)
    }
    
  }
  
  
  
}