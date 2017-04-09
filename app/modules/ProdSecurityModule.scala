package modules

import com.galacticfog.gestalt.security.api.{GestaltSecurityClient, GestaltSecurityConfig}
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltFrameworkSecurityEnvironment, GestaltSecurityEnvironment}
import com.mohiva.play.silhouette.api.{EventBus, RequestProvider}
import com.mohiva.play.silhouette.api.services.{AuthenticatorService, IdentityService}
import com.mohiva.play.silhouette.impl.authenticators.{DummyAuthenticator, DummyAuthenticatorService}
import java.util.UUID
import javax.inject.{Inject, Singleton}

import com.galacticfog.gestalt.security.api._
import com.google.inject.{AbstractModule, Provides}
import play.api.Logger

import scala.concurrent.ExecutionContext
import scala.util.Try

class ProdSecurityModule extends AbstractModule {

  override def configure(): Unit = {
    bind(classOf[SecurityClientProvider]).to(classOf[GestaltLateInitSecurityEnvironment])
    bind(classOf[GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator]]).to(classOf[GestaltLateInitSecurityEnvironment])
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

@Singleton
class GestaltLateInitSecurityEnvironment @Inject() ( bus: EventBus,
                                                     identitySvc: IdentityService[AuthAccountWithCreds] )
                                                   ( implicit ec: ExecutionContext )

  extends GestaltSecurityEnvironment[AuthAccountWithCreds, DummyAuthenticator]
    with SecurityClientProvider {

  var maybeSecurityConfig = {
    Logger.info("attempting to determine GestaltSecurityConfig for framework authentication mode")
    GestaltSecurityConfig.getSecurityConfig
      .filter(_.isWellDefined)
  }

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

