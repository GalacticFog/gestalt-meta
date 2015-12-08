package controllers.util


import com.mohiva.play.silhouette.api.services.AuthenticatorService
import com.mohiva.play.silhouette.impl.authenticators.{DummyAuthenticatorService, DummyAuthenticator}

import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController

import play.api.mvc.RequestHeader
import java.util.UUID


trait SecureController extends GestaltFrameworkSecuredController[DummyAuthenticator] {  
  override def getAuthenticator: AuthenticatorService[DummyAuthenticator] = new DummyAuthenticatorService
  
  def Authenticate() = new GestaltFrameworkAuthActionBuilderUUID(Some({rh: RequestHeader => None: Option[UUID]}))
  def Authenticate(fqon: String) = new GestaltFrameworkAuthActionBuilder(Some({rh: RequestHeader => Some(fqon)}))
  def Authenticate(org: UUID) = new GestaltFrameworkAuthActionBuilderUUID(Some({rh: RequestHeader => Some(org)}))

}


