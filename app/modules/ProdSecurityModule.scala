package modules

import com.galacticfog.gestalt.security.api.{GestaltSecurityClient, GestaltSecurityConfig}
import com.galacticfog.gestalt.security.play.silhouette.{AccountServiceImplWithCreds, AuthAccountWithCreds, GestaltFrameworkAuthProvider, GestaltSecurityEnvironment}
import com.mohiva.play.silhouette.api.{EventBus, RequestProvider}
import com.mohiva.play.silhouette.api.services.{AuthenticatorService, IdentityService}
import com.mohiva.play.silhouette.impl.authenticators.{DummyAuthenticator, DummyAuthenticatorService}
import javax.inject.{Inject, Singleton}

import com.galacticfog.gestalt.security.api._
import com.google.inject.AbstractModule
import net.codingwell.scalaguice.ScalaModule
import play.api.Logger
import play.api.libs.ws.WSClient

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}
import java.util.UUID
import com.galacticfog.gestalt.data.util.PostgresHealth
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.meta.api.sdk._
import controllers.util.JsonInput
import play.api.libs.json._  

class ProdSecurityModule extends AbstractModule with ScalaModule {

  override def configure(): Unit = {
    bind[SecurityClientProvider].to[GestaltLateInitSecurityEnvironment]
    bind[GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator]].to[GestaltLateInitSecurityEnvironment]
    bind[SecurityKeyInit].to[GestaltLateInitSecurityEnvironment]
    bind[EventBus].toInstance(EventBus())
    bind[IdentityService[AuthAccountWithCreds]].toInstance(new AccountServiceImplWithCreds())
  }

}

trait SecurityKeyInit {
  def init(org: UUID, caller: AuthAccountWithCreds /*apiKey: String, apiSecret: String*/): Unit
}

trait SecurityClientProvider {
  def client: GestaltSecurityClient
}


@Singleton
class GestaltLateInitSecurityEnvironment @Inject() ( 
     wsclient: WSClient,
     bus: EventBus,
     identitySvc: IdentityService[AuthAccountWithCreds],
     db: play.api.db.Database,
     appconfig: play.api.Configuration)(implicit ec: ExecutionContext)
      extends GestaltSecurityEnvironment[AuthAccountWithCreds, DummyAuthenticator]
      with SecurityClientProvider with SecurityKeyInit {

  private val logger = Logger(this.getClass)

  private object jsonInput extends JsonInput {}
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

  override def identityService: IdentityService[AuthAccountWithCreds] = identitySvc

  override def authenticatorService: AuthenticatorService[DummyAuthenticator] = new DummyAuthenticatorService()(ec)

  override def requestProviders: Seq[RequestProvider] = {
    Try { client } match {
      case Success(c) => Seq(new GestaltFrameworkAuthProvider(client))
      case Failure(e) =>
        logger.warn("could not instantiate authentication provider",e)
        Seq.empty
    }
  }

  override def eventBus: EventBus = bus

  override implicit val executionContext: ExecutionContext = ec
  
  override def init(org: UUID, caller: AuthAccountWithCreds) = {
    
    val creds = caller.creds.asInstanceOf[GestaltBasicCredentials]
    
    maybeEnvSecConfig = maybeEnvSecConfig map {
      _.copy(
        apiKey = creds.username,
        apiSecret = creds.password
      )
    }
    
    ResourceFactory.findById(ResourceIds.SystemConfig).fold {
      logger.info("no system configuration found. creating...")
      val configJson = sysConfigJson(creds)
      newSysConfig(org, caller, configJson) recover {
        case e: Throwable => {
          logger.error("failed to create system configuration")
          throw e
        }
      }
      logger.info("new system configuration created.")
    }{ config =>
      val secprops = newSecurityProperties(creds)
      val newprops = (config.properties getOrElse Map.empty) ++ secprops
      val updated = config.copy(properties = Some(newprops))
      
      ResourceFactory.update(updated, caller.account.id)
    }
  }
  
  def newSysConfig(org: UUID, caller: AuthAccountWithCreds, configJson: JsValue) = {
    jsonInput.CreateNewResource(org, caller, configJson, 
      typeId = Some(ResourceIds.Configuration), 
      parent = Some(org))    
  }
  
  def sysConfigJson(creds: GestaltBasicCredentials) = {
    val props = newSecurityProperties(creds)
    Json.obj(
        "id" -> ResourceIds.SystemConfig.toString,
        "name" -> "gestalt-system-config",
        "properties" -> Json.obj("config" -> Json.toJson(props)))
  }
  
  def newSecurityProperties(creds: GestaltBasicCredentials) = 
    Map(KEY_NAME -> creds.username, KEY_SECRET -> creds.password)    
  
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
      logger.info("Meta repository is initialized. Looking up credentials...")

      ResourceFactory.findById(ResourceIds.SystemConfig).fold {
        logger.info("did not recover security credentials from system config")
        Option.empty[(String, String)]
        
      }{ config =>
        logger.info("recovered security credentials from system config")
        for {
          props <- config.properties
          config <- props.get("config")
          jsconfig <- Try(Json.parse(config)).toOption
          k <- (jsconfig \ KEY_NAME).asOpt[String]
          s <- (jsconfig \ KEY_SECRET).asOpt[String]
        } yield (k,s)
      }
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
      dbCreds = getCredsFromSystem //getCredsFromDB
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

