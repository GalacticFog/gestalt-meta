package controllers

import com.galacticfog.gestalt.meta.api.BuildInfo
import controllers.util.{MetaHealth, SecureController}
import controllers.util.db.EnvConfig
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.mvc.Action
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi
import javax.inject.Singleton
import com.galacticfog.gestalt.meta.api.audit.Audit

@Singleton
class InfoController @Inject()( 
    messagesApi: MessagesApi,
    env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
    metaHealth: MetaHealth)
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  case class AboutMeta(status: String,
                       url: String,
                       time: String,
                       build_info: JsValue,
                       meta_repo_build_info: JsValue,
                       services: Map[String,ServiceInfo],
                       docker_image: Option[String] )
  case class ServiceInfo(url: String, status: String)

  implicit lazy val serviceInfoFormat = Json.format[ServiceInfo]
  implicit lazy val aboutMetaFormat = Json.format[AboutMeta]

  /*
   * TODO:
   */
  def about() = Audited() { implicit request =>
    val result = AboutMeta(
      status     = "OK",
      url        = META_URL,
      time       = org.joda.time.DateTime.now.toString,
      build_info = Json.parse(BuildInfo.toJson),
      meta_repo_build_info = Json.parse(com.galacticfog.gestalt.data.BuildInfo.toJson),
      services   = Map(
        "security"       -> ServiceInfo(url = EnvConfig.securityUrl, status = "OK"),
        "datastore"      -> ServiceInfo(url = EnvConfig.databaseUrl, status = "OK")
      ),
      docker_image = sys.env.get("MARATHON_APP_DOCKER_IMAGE")
    )
    Ok(Json.toJson(result))
  }


  /**
   * Unauthenticated health-check endpoint. Gives status as simple healthy/unhealthy.
   */
  def health() = Action { implicit request =>
    checkHealth(verbose = false)
  }
  
  
  /**
   * Authenticated health-check endpoint. Gives detailed information if status is 'unhealthy'
   * 
   * @param fqon Fully-Qualified Org Name
   */
  def healthAuthenticated(fqon: String) = Audited(fqon) { _ => checkHealth(verbose = true) }  
  
//  protected[controllers] def healthStatus() = {
//    MetaHealth.selfCheck(false) match {
//      case Left(e) => ???
//      case Right(s) => ???
//    }
//  }
  
  protected[controllers] def checkHealth(verbose: Boolean) = {
    metaHealth.selfCheck(verbose) match {
      case Left(err)      => InternalServerError(err)
      case Right(success) => Ok(success)
    }    
  }


  def options(path: String) = Action {Ok("")}
  
  
  import com.galacticfog.gestalt.meta.api.audit._
  
  def serviceCheck() = Audited() { implicit request =>
    request.queryString.get("feature").fold {
      throw new BadRequestException(s"Must supply value for `?feature` query param")
    }{ f =>
      f.headOption.fold {
        throw new BadRequestException(s"Must supply value for `?feature` query param")
      }{ f =>
        f.toLowerCase match {
          case "audit" => Ok(audit.check())
          case _ => throw new BadRequestException(s"Invalid feature query value. found: '$f'")
        }
      }
    }
  }

  
}
