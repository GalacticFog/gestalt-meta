package controllers

import com.galacticfog.gestalt.meta.api.BuildInfo

import controllers.util.MetaHealth
import controllers.util.db.EnvConfig
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.mvc.Action
import com.galacticfog.gestalt.meta.auth.Authorization

object InfoController extends Authorization {
  
  case class AboutMeta(status: String, url: String, time: String, build_info: JsValue, services: Map[String,ServiceInfo])
  case class ServiceInfo(url: String, status: String)
  
  private object HealthStatus {
    val OK       = "OK"
    val DEGRADED = "DEGRADED"
    val BROKEN   = "BROKEN"
  }
  
  implicit lazy val serviceInfoFormat = Json.format[ServiceInfo]
  implicit lazy val aboutMetaFormat = Json.format[AboutMeta]
  
  /*
   * TODO: 
   */
  def about() = Authenticate() { implicit request =>
    val result = AboutMeta( 
        status     = "OK",
        url        = META_URL.get,
        time       = org.joda.time.DateTime.now.toString,
        build_info = Json.parse(BuildInfo.toJson), 
        services   = Map(
            "security"       -> ServiceInfo(url = EnvConfig.securityUrl, status = "OK"),
            "gateway"        -> ServiceInfo(url = EnvConfig.gatewayUrl, status = "OK"),
            "gestalt-lambda" -> ServiceInfo(url = EnvConfig.lambdaUrl, status = "OK"),
            "datastore"      -> ServiceInfo(url = EnvConfig.databaseUrl, status = "OK")))
            
    Ok(Json.toJson(result))
  }  
  
  
  /**
   * Unauthenticated health-check endpoint. Gives status as simple healthy/unhealthy.
   */
  def health() = Action { checkHealth(verbose = false) }
  
  
  /**
   * Authenticated health-check endpoint. Gives detailed information if status is 'unhealthy'
   * 
   * @param fqon Fully-Qualified Org Name
   */
  def healthAuthenticated(fqon: String) = Authenticate(fqon) { checkHealth(verbose = true) }  
  
//  protected[controllers] def healthStatus() = {
//    MetaHealth.selfCheck(false) match {
//      case Left(e) => ???
//      case Right(s) => ???
//    }
//  }
  
  protected[controllers] def checkHealth(verbose: Boolean) = {
    MetaHealth.selfCheck(verbose) match {
      case Left(err)      => InternalServerError(err)
      case Right(success) => Ok(success)
    }    
  }
  
}