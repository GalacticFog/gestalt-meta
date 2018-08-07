package controllers

import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecurity
import com.google.inject.Inject
import controllers.util.{HandleExceptions, SecureController}
import javax.inject.Singleton
import play.api.Logger
import play.api.i18n.MessagesApi
import play.api.libs.json.Json

import scala.util.{Failure, Success}

@Singleton
class SyncController @Inject()( 
    messagesApi: MessagesApi,
    sec: GestaltFrameworkSecurity,
    securitySync: SecuritySync)
  extends SecureController(messagesApi = messagesApi, sec = sec) with Authorization {
  
  private[this] val log = Logger(this.getClass)

  /*
   * 
   * TODO: We currently update all Orgs, Groups, and Users on every sync. Until we devise some
   * method of performing diffs on the resource types, this can't be avoided.
   * 
   */
  
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