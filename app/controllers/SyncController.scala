package controllers

import java.util.UUID

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.DeleteManager
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.session
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltGroup
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import controllers.util.{GenericErrorResult, SecureController, Security}
import play.api.Logger
import play.api.libs.json.Json
import com.galacticfog.gestalt.meta.auth.Authorization
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi

import javax.inject.Singleton
import controllers.util.HandleExceptions

@Singleton
class SyncController @Inject()( 
    messagesApi: MessagesApi,
    env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
    securitySync: SecuritySync)
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  private[this] val log = Logger(this.getClass)

  /*
   * 
   * TODO: We currently update all Orgs, Groups, and Users on every sync. Until we devise some
   * method of performing diffs on the resource types, this can't be avoided.
   * 
   */
  import controllers.{Stat, SyncStats}
  
  def sync() = Audited() { implicit request =>  
    securitySync.synchronize(request.identity) match {
      case Success(s) => Ok(Json.toJson(s))
      case Failure(e) => {
        log.error("Failed to synchronize with gestalt-security.")
        HandleExceptions(e)
      }
    }
  }
  
}