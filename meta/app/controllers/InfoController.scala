package controllers

import com.galacticfog.gestalt.meta.api.BuildInfo
import com.galacticfog.gestalt.meta.api.errors._

import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecurity
import com.google.inject.Inject
import controllers.util._
import controllers.util.db.EnvConfig
import controllers.util.{MetaHealth, SecureController}
import javax.inject.Singleton

import play.api.i18n.MessagesApi
import play.api.libs.json._
import play.api.mvc.Action
import play.api.cache._
import com.galacticfog.gestalt.data.ResourceFactory
import java.nio.file.{Files, Paths}
import com.galacticfog.gestalt.meta.api.errors._
import controllers.util.QueryString
import scala.io.Source
import actors._
import scala.util.{Try,Success,Failure}

@Singleton
class InfoController @Inject()( 
    messagesApi: MessagesApi,
    sec: GestaltFrameworkSecurity,
    metaHealth: MetaHealth,
    @NamedCache("upgrade-cache") cache: CacheApi)
  extends SecureController(messagesApi = messagesApi, sec = sec) with MetaController {

  case class AboutMeta(status: String,
                       url: String,
                       time: String,
                       build_info: JsValue,
                       meta_repo_build_info: JsValue,
                       services: Map[String,ServiceInfo],
                       docker_image: Option[String],
                       root_initialized: Option[Boolean])
  case class ServiceInfo(url: String, status: String)

  implicit lazy val serviceInfoFormat = Json.format[ServiceInfo]
  implicit lazy val aboutMetaFormat = Json.format[AboutMeta]

  /**
   * Determine if a Meta upgrade is available. This function simply checks the 'upgrade-cache' 
   * for data at the 'meta.upgrade' key. If data is found, it is formatted and returned with 
   * status TRUE. If there is nothing at that key it returns status FALSE.
   */
  def upgradeAvailable() = Audited() { implicit request =>

    Try(cache.get[JsObject](UpgradeVars.UPGRADE_CACHE_KEY)) match {
      case Failure(e) => HandleExceptions(e)
      case Success(json) => {
        json.fold {
          Ok(Json.toJson(UpgradeMessage(false)))
        }{ info =>
          // Ensure we can parse what we got from the cache.
          UpgradeMessage.fromUpgradeInfo(info) match {
            case Failure(e) => HandleExceptions(e)
            case Success(message) => Ok(Json.toJson(message))
          }
        }
      }
    }
  }
  
  /**
   * Get information about the status of automatic upgrade checking. Indicates
   * whether or not checks are enabled and displays the relevant environment variables.
   */
  def upgradeStatus() = Audited() { implicit request =>
    val url = UpgradeVars.baseUrl
    val interval = UpgradeVars.interval
    val enabled = UpgradeVars.checksEnabled
    
    val env = Json.obj(
      UpgradeVars.UrlName -> url,
      UpgradeVars.CheckIntervalName -> interval,
      UpgradeVars.EnableUpgradesName -> enabled
    )
    
    val message = if (enabled) 
      "Automatic upgrade checks are ENABLED"
    else 
      "Automatic upgrade checks are DISABLED"
    
    Ok(Json.obj( "message" -> message, "env" -> env))    
  }
  
  def about() = Audited() { implicit request =>
    import com.galacticfog.gestalt.meta.auth.DefaultMetaConfiguration
    val rootInitialized = new DefaultMetaConfiguration().getRoot().nonEmpty
    
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
      docker_image = sys.env.get("MARATHON_APP_DOCKER_IMAGE"),
      root_initialized = Some(rootInitialized)
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

  protected[controllers] def checkHealth(verbose: Boolean) = {
    metaHealth.selfCheck(verbose) match {
      case Left(err)      => InternalServerError(err)
      case Right(success) => Ok(success)
    }    
  }

  def options(path: String) = Action {Ok("")}
  
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
   
  def resourceStats() = Audited() { implicit request =>
    Ok(ResourceFactory.resourceStats("dsc"))  
  }
  
  def readAuditLogs() = Audited() { implicit request =>
    val fileVar = "META_AUDIT_LOG_FILE"
    val DEFAULT_MAX_LINES = 1000
    
    // Find META_AUDIT_LOG environment var
    val file: String = sys.env.get(fileVar).map(s => s) getOrElse {
      throw new BadRequestException(s"Env Var ${fileVar} not found.")
    }
    
    // Ensure named log-file exists
    val source = {
      if (Files.exists(Paths.get(file))) Source.fromFile(file)
      else throw new ResourceNotFoundException(s"Audit log file '${file}' not found")
    }
    
    try {
      val head = QueryString.single(request.queryString, "head").flatMap(parseInt(_))
      val tail = QueryString.single(request.queryString, "tail").flatMap(parseInt(_))
     
      val data: List[String] = {
        val dat = source.getLines
        if (head.nonEmpty) 
          dat.take(head.get).toList
        else if (tail.nonEmpty) {
          dat.toList.takeRight(tail.get)
        } else dat.take(DEFAULT_MAX_LINES).toList
      }
      
      Ok(data.mkString("\n")).withHeaders("Content-Type" -> "text/plain")
    
    } finally {
      source.close() 
    }
  }

  def parseInt(s: String): Option[Int] = try {
    Option(s.toInt)
  } catch {
    case e: Throwable => None
  }  
  
}
