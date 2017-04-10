package controllers

import scala.util.Failure
import scala.util.Success
import com.galacticfog.gestalt.data.bootstrap.Bootstrap
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import controllers.util.{SecureController, Security, trace}
import controllers.util.db.ConnectionManager
import play.api.{Logger => log}
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi
import javax.inject.Singleton

import com.galacticfog.gestalt.meta.providers._
import com.galacticfog.gestalt.security.api.GestaltBasicCredentials
import modules.{GestaltLateInitSecurityEnvironment, SecurityKeyInit}

@Singleton
class BootstrapController @Inject()( messagesApi: MessagesApi,
                                     env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
                                     providerManager: ProviderManager,
                                     security: Security,
                                     securityInit: SecurityKeyInit,
                                     deleteController: DeleteController,
                                     connectionManager: ConnectionManager )
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  def initProviders() = Authenticate() { implicit request =>
    val results = providerManager.loadProviders()

    Ok("TESTING PROVIDER LOADING...")
  }
  
  def bootstrap() = Authenticate() { implicit request =>
    trace("bootstrap()")
    
    val clean = (request.queryString.contains("clean") && 
        request.queryString("clean")(0) == "true")
    
    if (clean) {
      log.info("Cleaning up containers...")
      val containers = ResourceFactory.findAll(ResourceIds.Container)
      if (containers.isEmpty) {
        log.info("No containers to clean - proceeding with bootstrap...")
      } else {
        containers foreach { c =>
          log.info(s"Deleting Container: ${c.id}, ${c.name}...")
          deleteController.manager.delete(c, request.identity, force = true)
        }
        log.info("Container deletes complete - proceeding with bootstrap...")
      }
    }

    log.debug("Looking up root Org in gestalt-security...")
    val rootOrgId = security.getRootOrg(request.identity) match {
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
    val db = new Bootstrap(ResourceIds.Org, rootOrgId, rootOrgId, owner, connectionManager.currentDataSource())
    
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
        request.identity.creds match {
          case apiCreds: GestaltBasicCredentials =>
            securityInit.init(apiCreds.username, apiCreds.password)
            log.info("Initializing gestalt-security-play with API credentials from /bootstrap")
          case _ =>
            log.warn("Bootstrap did not use API credentials; will not initialize security client")
        }
        NoContent
      }
      case Failure(e) => {
        log.error("Could not rebuild Meta DB: " + e.getMessage)
        InternalServerError(e.getMessage)
      }
    }
    
  }
  
  
  
}
