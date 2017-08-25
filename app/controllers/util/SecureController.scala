package controllers.util


import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltFrameworkSecuredController, GestaltSecurityEnvironment}
import play.api.mvc._
import java.util.UUID

import com.galacticfog.gestalt.security.api.GestaltSecurityClient
import play.api.i18n.MessagesApi
import scala.concurrent.Future
import play.api.Logger
 
import org.slf4j.LoggerFactory

abstract class SecureController(messagesApi: MessagesApi,
                                env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
  extends GestaltFrameworkSecuredController[DummyAuthenticator](messagesApi, env) {

  private val auditLog = LoggerFactory.getLogger("[AUDIT]")

  implicit def getSecurityClient: GestaltSecurityClient = env.client

  def Authenticate() = new GestaltFrameworkAuthActionBuilderUUID(Some({rh: RequestHeader => None: Option[UUID]}))
  def Authenticate(fqon: String) = new GestaltFrameworkAuthActionBuilder(Some({rh: RequestHeader => Some(fqon)}))
  def Authenticate(org: UUID) = new GestaltFrameworkAuthActionBuilderUUID(Some({rh: RequestHeader => Some(org)}))
  
  import org.slf4j.MarkerFactory
  
  val auditMarker = MarkerFactory.getMarker("AUDIT")  
  
  def Audited[A](fqon: String)(block: SecuredRequest[AnyContent] => Result) = Authenticate(fqon) { implicit request: SecuredRequest[AnyContent] =>
    auditLog.info(auditMarker, auditMessage(request))
    
    block(request)    
  }
  

  def auditMessage(sr: SecuredRequest[AnyContent]) = {
    val u = sr.identity.account
    "%s %s, [user: %s, %s, %s]".format(
        sr.method, 
        sr.uri, 
        u.id, 
        u.name, 
        (u.email getOrElse "N/A"))
  }
  /*
   * Audit Logger Name i.e. [AUDIT]
   * 
   */
  
  
//  def Audited[A](fqon: String)(block: SecuredRequest[A] => Future[Result]) = 
//      new GestaltFrameworkAuthActionBuilder(Some({rh: RequestHeader => Some(fqon)})) { request: SecuredRequest[A] =>
//
//  }

  

//  object AuditAction extends ActionBuilder[({ type R[B] = SecuredRequest[GestaltSecurityEnvironment, B] })#R] {
//    def invokeBlock[B](request: SecuredRequest[B], block: (SecuredRequest[B] => Future[Result])) = {
//      Logger.info("**AUDITED***")
//      block(request)
//    }
//  }  
//  
//  def Audited(fqon: String) = new GestaltFrameworkAuthActionBuilder(Some({rh: RequestHeader => Some(fqon)})) { 
//    AuditAction { r =>
//      ???
//    }
//  }
}


