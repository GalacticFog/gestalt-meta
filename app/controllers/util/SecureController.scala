package controllers.util


import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltFrameworkSecuredController, GestaltSecurityEnvironment}
import play.api.mvc.RequestHeader
import java.util.UUID

import play.api.i18n.MessagesApi


abstract class SecureController(messagesApi: MessagesApi,
                                env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
  extends GestaltFrameworkSecuredController[DummyAuthenticator](messagesApi, env) {

  def Authenticate() = new GestaltFrameworkAuthActionBuilderUUID(Some({rh: RequestHeader => None: Option[UUID]}))
  def Authenticate(fqon: String) = new GestaltFrameworkAuthActionBuilder(Some({rh: RequestHeader => Some(fqon)}))
  def Authenticate(org: UUID) = new GestaltFrameworkAuthActionBuilderUUID(Some({rh: RequestHeader => Some(org)}))
}


