package modules

import com.galacticfog.gestalt.security.api.{GestaltSecurityClient, GestaltSecurityConfig}
import com.galacticfog.gestalt.security.play.silhouette.{AccountServiceImplWithCreds, AuthAccountWithCreds, GestaltFrameworkAuthProvider, GestaltSecurityEnvironment}
import com.mohiva.play.silhouette.api.{EventBus, RequestProvider}
import com.mohiva.play.silhouette.api.services.{AuthenticatorService, IdentityService}
import com.mohiva.play.silhouette.impl.authenticators.{DummyAuthenticator, DummyAuthenticatorService}
import javax.inject.{Inject, Singleton}

import com.galacticfog.gestalt.security.api._
import com.google.inject.AbstractModule
//import controllers.util.db.ConnectionManager
import net.codingwell.scalaguice.ScalaModule
import play.api.Logger
import play.api.libs.ws.WSClient

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}
import java.util.UUID


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
class GestaltLateInitSecurityEnvironment @Inject() ( wsclient: WSClient,
                                                     bus: EventBus,
                                                     identitySvc: IdentityService[AuthAccountWithCreds],
                                                     db: play.api.db.Database
                                                     /*,
                                                     connectionManager: ConnectionManager */)
                                                   ( implicit ec: ExecutionContext )
  extends GestaltSecurityEnvironment[AuthAccountWithCreds, DummyAuthenticator]
    with SecurityClientProvider with SecurityKeyInit {

  val logger = Logger(this.getClass)

  var maybeEnvSecConfig: Option[GestaltSecurityConfig] = {
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

  import com.galacticfog.gestalt.data.ResourceFactory
  import com.galacticfog.gestalt.data.models.GestaltResourceInstance
  import com.galacticfog.gestalt.meta.api.sdk._
  import controllers.util.JsonInput
  import play.api.libs.json._  
  
  override def eventBus: EventBus = bus

  override implicit val executionContext: ExecutionContext = ec

//  override def init(apiKey: String, apiSecret: String): Unit = {
//    import scalikejdbc._
//    implicit val autoSession = AutoSession
//    maybeEnvSecConfig = maybeEnvSecConfig map {
//      _.copy(
//        apiKey = apiKey,
//        apiSecret = apiSecret
//      )
//    }
//
//    sql"""
//      CREATE TABLE IF NOT EXISTS sec_init (
//        id INT PRIMARY KEY NOT NULL DEFAULT(0) CHECK (id = 0),
//        api_key TEXT NOT NULL,
//        api_secret TEXT NOT NULL
//      );
//    """.update.apply
//    
//    sql"""
//       INSERT INTO sec_init (id, api_key, api_secret)
//       VALUES (0,${apiKey},${apiSecret})
//    """.update.apply //    ON CONFLICT (id) DO UPDATE SET api_key = EXCLUDED.api_key, api_secret = EXCLUDED.api_secret;
//  }
  

  object jsonInput extends JsonInput {}
  val KEY_NAME = "GESTALT_SECURITY_KEY"
  val KEY_SECRET = "GESTALT_SECURITY_SECRET"
  
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
  
    
  def getCredsFromSystem: Option[(String, String)] = {
    ResourceFactory.findById(ResourceIds.SystemConfig).foldLeft {
      logger.info("did not recover security credentials from system config")
      Option.empty[(String, String)]
      
    }{ (_, config) =>
      logger.info("recovered security credentials from system config")
      config.properties.foldLeft(Option.empty[(String,String)]) { (_, ps) =>
        val k = ps.get(KEY_NAME) 
        val s = ps.get(KEY_SECRET)
        if (k.isEmpty || s.isEmpty) None else Some((k.get, s.get))
      }
    }
  }
  
//  def getCredsFromDB: Option[(String,String)] = {
//    import scalikejdbc._
//    implicit val autoSession = AutoSession
//    logger.info("attempting to recover security credentials from database")
//    Try{
//      sql"""
//        SELECT * FROM sec_init
//        WHERE id = 0
//      """
//      .map { rs => ( rs.string("api_key") , rs.string("api_secret") ) }
//      .single.apply
//    } match {
//      case Success(Some(o)) =>
//        logger.info("recovered security credentials from database")
//        Some(o)
//      case Success(None) =>
//        logger.info("did not recover security credentials from database")
//        None
//      case Failure(e) =>
//        logger.warn("failure recovering security credentials from database",e)
//        None
//    }
//  }

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
      key    = dbCreds.map(_._1) orElse getEnv(eKEY) getOrElse ""
      secret = dbCreds.map(_._2) orElse getEnv(eSECRET) getOrElse ""
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

