package controllers.util


import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.galacticfog.gestalt.security.play.silhouette.GestaltSecurityEnvironment

import com.galacticfog.gestalt.security.api.GestaltSecurityClient
import com.galacticfog.gestalt.meta.api.errors.ApiException
import com.galacticfog.gestalt.meta.api.audit._

import scala.concurrent.Future
import org.slf4j.LoggerFactory
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.mvc._
import play.api.i18n.MessagesApi
import play.api.libs.json._

import scala.util.{Try, Success, Failure}

import com.galacticfog.gestalt.meta.api.audit.Audit
abstract class SecureController(
  messagesApi: MessagesApi,
  env: GestaltSecurityEnvironment[AuthAccountWithCreds, DummyAuthenticator])
    extends GestaltFrameworkSecuredController[DummyAuthenticator](messagesApi, env) {

  protected val audit = Audit
  
  implicit def getSecurityClient: GestaltSecurityClient = env.client

  private def Authenticate() = new GestaltFrameworkAuthActionBuilderUUID(Some({ rh: RequestHeader => None: Option[UUID] }))
  private def Authenticate(fqon: String) = new GestaltFrameworkAuthActionBuilder(Some({ rh: RequestHeader => Some(fqon) }))
  private def Authenticate(org: UUID) = new GestaltFrameworkAuthActionBuilderUUID(Some({ rh: RequestHeader => Some(org) }))
  
  def AsyncAudited()(block: SecuredRequest[JsValue] => Future[Result]) =
      Authenticate().async(parse.json) { implicit request: SecuredRequest[JsValue] =>
    auditedActionAsync(request, System.currentTimeMillis)(block)
  }
  
  def AsyncAudited(fqon: String)(block: SecuredRequest[JsValue] => Future[Result]) =
      Authenticate(fqon).async(parse.json) { implicit request: SecuredRequest[JsValue] =>
    auditedActionAsync(request, System.currentTimeMillis)(block)
  }

  def AsyncAuditedAny(fqon: String)(block: SecuredRequest[AnyContent] => Future[Result]) =
      Authenticate(fqon).async { implicit request: SecuredRequest[AnyContent] =>
    auditedActionAsync(request, System.currentTimeMillis)(block)
  }  
  
  def Audited(fqon: String)(block: SecuredRequest[AnyContent] => Result) = Authenticate(fqon){ 
      implicit request: SecuredRequest[AnyContent] =>  
    auditedAction(request, System.currentTimeMillis)(block)
  }

  def Audited()(block: SecuredRequest[AnyContent] => Result) = Authenticate() { 
      implicit request: SecuredRequest[AnyContent] =>
    auditedAction(request, System.currentTimeMillis)(block)
  }  
  
  private val FailureRange = Seq((400 to 599): _*)
  
  private def auditedActionAsync[A](request: SecuredRequest[A], startTime: Long)(block: SecuredRequest[A] => Future[Result]) = {
    Try {
      val result = block(request)
      result.map(r => logAction(request, r, startTime))
      result

    }.recover {
      case e: Throwable => Future(logUnknown(request, e, startTime))

    }.get    
  }
  
  private def auditedAction[A](request: SecuredRequest[A], startTime: Long)(block: SecuredRequest[A] => Result) = {
    Try {
      val result = block(request)
      logAction(request, result, startTime)
      result

    }.recover {
      case e: Throwable => logUnknown(request, e, startTime)

    }.get    
  }
  
  private def logAction[A](request: SecuredRequest[A], result: Result, startTime: Long) = {

    if (FailureRange.contains(result.header.status)) {
      audit.log(auditError(request, result.body.toString, result.header.status, startTime))
    } else {
      audit.log(auditPost(request, result, startTime))
    }
  }

  /**
   * Log an "uncaught" request thrown by the audited code block.
   */
  private def logUnknown[A](request: SecuredRequest[A], ex: Throwable, startTime: Long) = {
    audit.log(auditError(request, ex, startTime))
    HandleExceptions(ex)    
  }
  
  private def errorCode(e: Throwable): Int = {
    if (e.isInstanceOf[ApiException])
      e.asInstanceOf[ApiException].code else 500
  }

  private def auditError[A](sr: SecuredRequest[A], errorMessage: String, statusCode: Int, startTime: Long) = {
    val u = sr.identity.account
    val time = s"${(System.currentTimeMillis() - startTime).toString}ms"
    val email = u.email getOrElse "N/A"

    val template = " [ERROR] request.id=%s request.status=%s request.time=%s request.method=%s request.uri=%s user.id=%s user.name=%s user.email=%s error.message='%s'"
    template.format(sr.id, statusCode.toString, time, sr.method, sr.uri, u.id, u.name, email, errorMessage)
  }

  private def auditError[A](sr: SecuredRequest[A], ex: Throwable, startTime: Long): String = {
    auditError(sr, ex.getMessage, errorCode(ex), startTime)
  }  
  
  private def auditPost[A](sr: SecuredRequest[A], result: Result, startTime: Long) = {
    val u = sr.identity.account
    val time = s"${(System.currentTimeMillis() - startTime).toString}ms"
    val email = u.email getOrElse "N/A"

    val template = " request.id=%s request.status=%s request.time=%s request.method=%s request.uri=%s user.id=%s user.name=%s user.email=%s"
    template.format(
      sr.id,
      result.header.status,
      time,
      sr.method,
      sr.uri,
      u.id,
      u.name,
      email)
  }

  private def auditMessage[A](sr: SecuredRequest[A]) = {
    val u = sr.identity.account
    val template = " [ERROR] request.id=%s request.status=%s request.time=%s request.uri=%s user.id=%s, user.name=%s user.email=%s error.message='%s'"
    "(req=%d) %s %s, [user: %s, %s, %s]".format(
      sr.id,
      sr.method,
      sr.uri,
      u.id,
      u.name,
      (u.email getOrElse "N/A"))
  }
}


