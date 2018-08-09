package modules

import java.util.UUID

import actors.SystemConfigActor
import akka.actor.ActorRef
import com.galacticfog.gestalt.data.util.PostgresHealth
import com.galacticfog.gestalt.security.api.{GestaltSecurityClient, GestaltSecurityConfig, _}
import com.galacticfog.gestalt.security.play.silhouette.{AccountServiceImplWithCreds, AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.AbstractModule
import com.mohiva.play.silhouette.api.EventBus
import com.mohiva.play.silhouette.api.services.IdentityService
import javax.inject.{Inject, Named, Singleton}
import net.codingwell.scalaguice.ScalaModule
import play.api.Logger
import play.api.libs.ws.WSClient

import scala.concurrent.{Await, ExecutionContext}
import scala.util.{Failure, Success, Try}

class ProdSecurityModule extends AbstractModule with ScalaModule {

  override def configure(): Unit = {
    bind[SecurityClientProvider].to[GestaltLateInitSecurityEnvironment]
    bind[GestaltSecurityEnvironment[AuthAccountWithCreds]].to[GestaltLateInitSecurityEnvironment]
    bind[SecurityKeyInit].to[GestaltLateInitSecurityEnvironment]
    bind[EventBus].toInstance(EventBus())
    bind[IdentityService[AuthAccountWithCreds]].toInstance(new AccountServiceImplWithCreds())
  }

}

trait SecurityKeyInit {
  def init(org: UUID, caller: AuthAccountWithCreds): Unit
}

trait SecurityClientProvider {
  def client: GestaltSecurityClient
}


@Singleton
class GestaltLateInitSecurityEnvironment @Inject() ( wsclient: WSClient,
                                                     bus: EventBus,
                                                     identitySvc: IdentityService[AuthAccountWithCreds],
                                                     db: play.api.db.Database,
                                                     appconfig: play.api.Configuration,
                                                     @Named(SystemConfigActor.name) configActor: ActorRef )
                                                   ( implicit ec: ExecutionContext )
  extends GestaltSecurityEnvironment[AuthAccountWithCreds] with SecurityClientProvider with SecurityKeyInit {

  private val logger = Logger(this.getClass)

  private val KEY_NAME = "GESTALT_SECURITY_KEY"
  private val KEY_SECRET = "GESTALT_SECURITY_SECRET"  
  
  private var maybeEnvSecConfig: Option[GestaltSecurityConfig] = {
    logger.info("attempting to determine well-defined GestaltSecurityConfig for framework authentication mode from environment variables")
    discoverSecurityConfig
  }

  override def client: GestaltSecurityClient = {
    val c = config
    new GestaltSecurityClient(wsclient, c.protocol, c.hostname, c.port, GestaltBasicCredentials(c.apiKey, c.apiSecret))
  }

  override def config: GestaltSecurityConfig = {
    if (maybeEnvSecConfig.filter(_.isWellDefined).isEmpty) {
      logger.trace(maybeEnvSecConfig.toString)
      logger.warn("GestaltSecurityConfig is not fully initialized")
    }
    maybeEnvSecConfig getOrElse GestaltSecurityConfig(
      FRAMEWORK_SECURITY_MODE,
      HTTP,
      "localhost",
      9455,
      "",
      "",
      None,
      None
    )
  }

  override def init(org: UUID, caller: AuthAccountWithCreds) = {

    val creds = caller.creds.asInstanceOf[GestaltBasicCredentials]
    
    maybeEnvSecConfig = maybeEnvSecConfig map {
      _.copy(
        apiKey = creds.username,
        apiSecret = creds.password
      )
    }

    configActor ! SystemConfigActor.SetKeys(
      caller.account.id,
      Map(
        KEY_NAME -> Some(creds.username),
        KEY_SECRET -> Some(creds.password)
      )
    )
  }

  /**
   * Determine whether or not it is safe to lookup credentials in Meta repository.
   * Returns TRUE if meta repository has been initialized. FALSE otherwise.
   */
  private[modules] def proceedWithLookup(): Boolean = {
    appconfig.getString("meta.db.name").fold {
      logger.warn("Configuration item 'meta.db.name' not set.")
      false
    }{ nm =>
      logger.info("Checking Meta repository initialization...")
      PostgresHealth.dbInitialized(nm) match {
        case Failure(e) => {
          logger.error("Failed checking repository initialization.")
          throw e
        }
        case Success(initialized) => {
          if(!initialized) logger.info("Meta repository is NOT initialized.") 
          initialized
        }
      }      
    }    
  }
  
  def getCredsFromSystem: Option[(String, String)] = {

    if (proceedWithLookup()) {
      import akka.pattern.ask

      import scala.concurrent.duration._
      implicit val askTimeout: akka.util.Timeout = 15.seconds
      logger.info("Meta repository is initialized. Looking up credentials...")
      val fConfig = (configActor ? SystemConfigActor.GetAllConfig).mapTo[Map[String,String]]
      val fCreds = fConfig.map(
        config => config.get(KEY_NAME).zip(config.get(KEY_SECRET)).headOption
      )(ec)
      Await.result(fCreds, 15.seconds)
    } else {
      logger.info("Skipping credential lookup.")
      None
    }
  }

  
  private def discoverSecurityConfig: Option[GestaltSecurityConfig] = {
    logger.info("> checking environment for Gestalt security config")

    import GestaltSecurityConfig._

    def getEnv(name: String): Option[String] = scala.util.Properties.envOrNone(name)

    def defaultPort: Protocol => Int = _ match {
      case HTTP => 80
      case HTTPS => 443
    }
    
    def checkProtocol(proto: String): Protocol = proto match {
      case "HTTP" => HTTP
      case "http" => HTTP
      case "HTTPS" => HTTPS
      case "https" => HTTPS
      case _ => throw new RuntimeException("Invalid protocol for Gestalt security")
    }

    for {
      proto  <- getEnv(ePROTOCOL) orElse Some("http") map checkProtocol
      host   <- getEnv(eHOSTNAME)
      port   <- getEnv(ePORT) flatMap {s => Try{s.toInt}.toOption} orElse Some(defaultPort(proto))
      dbCreds = getCredsFromSystem
      key    = dbCreds.map(_._1) orElse getEnv(eKEY)    getOrElse {logger.warn("Could not identity API key for use with gestalt-security; authentication is not functional."); ""}
      secret = dbCreds.map(_._2) orElse getEnv(eSECRET) getOrElse {logger.warn("Could not identity API secret for use with gestalt-security; authentication is not functional."); ""}
      realm  = getEnv(eREALM)
    } yield GestaltSecurityConfig(
      mode=FRAMEWORK_SECURITY_MODE,
      protocol=proto,
      hostname=host,
      port=port,
      apiKey=key,
      apiSecret=secret,
      appId=None,
      realm = realm
    )
  }
}

