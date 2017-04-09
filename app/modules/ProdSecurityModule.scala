package modules

import com.galacticfog.gestalt.security.api.{GestaltSecurityClient, GestaltSecurityConfig}
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltFrameworkSecurityEnvironment, GestaltSecurityEnvironment}
import com.mohiva.play.silhouette.api.{EventBus, RequestProvider}
import com.mohiva.play.silhouette.api.services.{AuthenticatorService, IdentityService}
import com.mohiva.play.silhouette.impl.authenticators.{DummyAuthenticator, DummyAuthenticatorService}
import java.util.UUID
import javax.inject.Singleton

import com.galacticfog.gestalt.security.api._
import com.galacticfog.gestalt.security.play.silhouette.modules.GestaltFrameworkSecurityConfigModule
import com.google.inject.{AbstractModule, Provides}
import play.api.Logger

import scala.concurrent.ExecutionContext
import scala.util.Try

class ProdSecurityModule extends AbstractModule {

  override def configure(): Unit = {
    bind(classOf[SecurityClientProvider]).to(classOf[GestaltLateInitSecurityEnvironment])
  }

  @Singleton
  @Provides def providesEnvironment( securityConfig: GestaltSecurityConfig,
                                     securityClient: GestaltSecurityClient,
                                     eventBus: EventBus,
                                     identityService: IdentityService[AuthAccountWithCreds],
                                     authenticatorService: AuthenticatorService[DummyAuthenticator] )
                                   ( implicit ec: ExecutionContext ): GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator] = {
    new GestaltLateInitSecurityEnvironment( eventBus, identityService )
  }
}

object ProdSecurityModule {
  val FALLBACK_SECURITY_CONFIG: GestaltSecurityConfig = GestaltSecurityConfig(
    mode = FRAMEWORK_SECURITY_MODE,
    protocol = HTTP,
    hostname = "localhost",
    port = 9455,
    apiKey = UUID.randomUUID().toString,
    apiSecret = "00000noAPISecret00000000",
    appId = None
  )
}

trait SecurityClientProvider {
  def getClient: GestaltSecurityClient
}

class GestaltLateInitSecurityEnvironment( bus: EventBus,
                                          identitySvc: IdentityService[AuthAccountWithCreds] )
                                        ( implicit ec: ExecutionContext )

  extends GestaltSecurityEnvironment[AuthAccountWithCreds, DummyAuthenticator]
    with SecurityClientProvider {

  var maybeSecurityConfig = Try{
    Logger.info("attempting to determine GestaltSecurityConfig for framework authentication mode")
    GestaltSecurityConfig.getSecurityConfig
      .filter(_.isWellDefined)
      .getOrElse {
        Logger.warn("could not determine suitable GestaltSecurityConfig; relying on getFallbackSecurityConfig()")
        GestaltFrameworkSecurityConfigModule.FALLBACK_SECURITY_CONFIG
      }
  }.toOption

  override def client: GestaltSecurityClient = {
    ???
  }

  override def config: GestaltSecurityConfig = {
    ???
  }

  override def identityService: IdentityService[AuthAccountWithCreds] = identitySvc

  override def authenticatorService: AuthenticatorService[DummyAuthenticator] = new DummyAuthenticatorService()(ec)

  override def requestProviders: Seq[RequestProvider] = ???

  override def eventBus: EventBus = bus

  override implicit val executionContext: ExecutionContext = ec

  override def getClient: GestaltSecurityClient = ???
}

