package modules

import com.galacticfog.gestalt.security.api.{GestaltSecurityClient, GestaltSecurityConfig}
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltFrameworkSecurityEnvironment, GestaltSecurityEnvironment}
import com.google.inject.{AbstractModule, Provides, TypeLiteral}
import com.mohiva.play.silhouette.api.EventBus
import com.mohiva.play.silhouette.api.services.{AuthenticatorService, IdentityService}
import com.mohiva.play.silhouette.impl.authenticators.{DummyAuthenticator, DummyAuthenticatorService}

import scala.concurrent.ExecutionContext

class ProdSecurityModule extends AbstractModule {

  override def configure(): Unit = {}

  @Provides
  def providesSilhouetteAuthSvc()( implicit ec: ExecutionContext ): AuthenticatorService[DummyAuthenticator] = {
    new DummyAuthenticatorService()
  }

  @Provides def providesEnvironment( securityConfig: GestaltSecurityConfig,
                                     securityClient: GestaltSecurityClient,
                                     eventBus: EventBus,
                                     identityService: IdentityService[AuthAccountWithCreds],
                                     authenticatorService: AuthenticatorService[DummyAuthenticator] )
                                   ( implicit ec: ExecutionContext ): GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator] = {
    new GestaltFrameworkSecurityEnvironment(
      securityConfig,
      securityClient,
      eventBus,
      identityService,
      authenticatorService
    )
  }
}

