package modules

import com.galacticfog.gestalt.security.api.{GestaltSecurityClient, GestaltSecurityConfig}
import com.galacticfog.gestalt.security.play.silhouette.{AccountServiceImplWithCreds, AuthAccountWithCreds, GestaltFrameworkAuthProvider, GestaltSecurityEnvironment}
import com.mohiva.play.silhouette.api.{EventBus, RequestProvider}
import com.mohiva.play.silhouette.api.services.{AuthenticatorService, IdentityService}
import com.mohiva.play.silhouette.impl.authenticators.{DummyAuthenticator, DummyAuthenticatorService}
import javax.inject.{Inject, Singleton}

import com.galacticfog.gestalt.security.api._
import com.google.inject.AbstractModule
import controllers.util.db.ConnectionManager
import net.codingwell.scalaguice.ScalaModule
import play.api.Logger
import play.api.libs.ws.WSClient

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

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
  def init(apiKey: String, apiSecret: String): Unit
}

trait SecurityClientProvider {
  def client: GestaltSecurityClient
}

@Singleton
class GestaltLateInitSecurityEnvironment @Inject() ( wsclient: WSClient,
                                                     bus: EventBus,
                                                     identitySvc: IdentityService[AuthAccountWithCreds],
                                                     connectionManager: ConnectionManager )
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

  override def eventBus: EventBus = bus

  override implicit val executionContext: ExecutionContext = ec

  override def init(apiKey: String, apiSecret: String): Unit = {
    import scalikejdbc._
    implicit val autoSession = AutoSession
    maybeEnvSecConfig = maybeEnvSecConfig map {
      _.copy(
        apiKey = apiKey,
        apiSecret = apiSecret
      )
    }
    sql"""
      CREATE TABLE IF NOT EXISTS sec_init (
        id INT PRIMARY KEY NOT NULL DEFAULT(0) CHECK (id = 0),
        api_key TEXT NOT NULL,
        api_secret TEXT NOT NULL
      );
    """.update.apply
    sql"""
       INSERT INTO sec_init (id, api_key, api_secret)
       VALUES (0,${apiKey},${apiSecret})
    """.update.apply //    ON CONFLICT (id) DO UPDATE SET api_key = EXCLUDED.api_key, api_secret = EXCLUDED.api_secret;
  }

  def getCredsFromDB: Option[(String,String)] = {
    import scalikejdbc._
    implicit val autoSession = AutoSession
    logger.info("attempting to recover security credentials from database")
    Try{
      sql"""
        SELECT * FROM sec_init
        WHERE id = 0
      """
      .map { rs => ( rs.string("api_key") , rs.string("api_secret") ) }
      .single.apply
    } match {
      case Success(Some(o)) =>
        logger.info("recovered security credentials from database")
        Some(o)
      case Success(None) =>
        logger.info("did not recover security credentials from database")
        None
      case Failure(e) =>
        logger.warn("failure recovering security credentials from database",e)
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
      dbCreds = getCredsFromDB
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

