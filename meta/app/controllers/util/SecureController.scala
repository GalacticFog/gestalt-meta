package controllers.util


import java.util.UUID

import com.galacticfog.gestalt.meta.api.audit.Audit
import com.galacticfog.gestalt.meta.api.errors.ApiException
import com.galacticfog.gestalt.security.api.GestaltSecurityClient
import com.galacticfog.gestalt.security.play.silhouette.{GestaltFrameworkSecurity, GestaltFrameworkSecurityEnvironment}
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import play.api.i18n.MessagesApi
import play.api.libs.json._
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try
abstract class SecureController( messagesApi: MessagesApi,
                                 sec: GestaltFrameworkSecurity) extends Controller {

  protected val audit = Audit
  
  implicit def getSecurityClient: GestaltSecurityClient = sec.securityClient

  private[controllers] def Authenticate(): ActionBuilder[({ type R[B] = SecuredRequest[GestaltFrameworkSecurityEnvironment, B] })#R] = sec.AuthAction()
  private[controllers] def Authenticate(fqon: String): ActionBuilder[({ type R[B] = SecuredRequest[GestaltFrameworkSecurityEnvironment, B] })#R] = sec.AuthAction({ rh: RequestHeader => Some(fqon) })
  private[controllers] def Authenticate(org: UUID): ActionBuilder[({ type R[B] = SecuredRequest[GestaltFrameworkSecurityEnvironment, B] })#R] = sec.AuthAction({ rh: RequestHeader => Some(org) })
  
  def AsyncAudited()(block: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue] => Future[Result]) =
      Authenticate().async(parse.json) { implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue] =>
    auditedActionAsync(request, System.currentTimeMillis)(block)
  }
  
  def AsyncAudited(fqon: String)(block: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue] => Future[Result]) =
      Authenticate(fqon).async(parse.json) { implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue] =>
    auditedActionAsync(request, System.currentTimeMillis)(block)
  }

  def AsyncAuditedAny()(block: SecuredRequest[GestaltFrameworkSecurityEnvironment,AnyContent] => Future[Result]) =
    Authenticate().async { implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,AnyContent] =>
      auditedActionAsync(request, System.currentTimeMillis)(block)
    }


  def AsyncAuditedAny(fqon: String)(block: SecuredRequest[GestaltFrameworkSecurityEnvironment,AnyContent] => Future[Result]) =
      Authenticate(fqon).async { implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,AnyContent] =>
    auditedActionAsync(request, System.currentTimeMillis)(block)
  }  
  
  def Audited(fqon: String)(block: SecuredRequest[GestaltFrameworkSecurityEnvironment,AnyContent] => Result) = Authenticate(fqon){
      implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,AnyContent] =>
    auditedAction(request, System.currentTimeMillis)(block)
  }

  def Audited()(block: SecuredRequest[GestaltFrameworkSecurityEnvironment,AnyContent] => Result) = Authenticate() {
      implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,AnyContent] =>
    auditedAction(request, System.currentTimeMillis)(block)
  }  

  private val FailureRange = Seq((400 to 599): _*)
  
  private def auditedActionAsync[A](request: SecuredRequest[GestaltFrameworkSecurityEnvironment,A], startTime: Long)(block: SecuredRequest[GestaltFrameworkSecurityEnvironment,A] => Future[Result]) = {
    Try {
      val result = block(request)
      result.map(r => logAction(request, r, startTime))
      result

    }.recover {
      case e: Throwable => Future(logUnknown(request, e, startTime))

    }.get    
  }
  
  private def auditedAction[A](request: SecuredRequest[GestaltFrameworkSecurityEnvironment,A], startTime: Long)(block: SecuredRequest[GestaltFrameworkSecurityEnvironment,A] => Result) = {
    Try {
      val result = block(request)
      logAction(request, result, startTime)
      result

    }.recover {
      case e: Throwable => logUnknown(request, e, startTime)

    }.get    
  }
  
  private def logAction[A](request: SecuredRequest[GestaltFrameworkSecurityEnvironment,A], result: Result, startTime: Long) = {
    if (FailureRange.contains(result.header.status)) {
      audit.log(auditError(request, result.body.toString, result.header.status, startTime))
    } else {
      audit.log(auditPost(request, result, startTime))
    }
  }

  /**
   * Log an "uncaught" request thrown by the audited code block.
   */
  private def logUnknown[A](request: SecuredRequest[GestaltFrameworkSecurityEnvironment,A], ex: Throwable, startTime: Long) = {
    audit.log(auditError(request, ex, startTime))
    HandleExceptions(ex)    
  }
  
  private def errorCode(e: Throwable): Int = e match {
    case api: ApiException => api.code
    case _ => 500
  }

  private def auditError[A](sr: SecuredRequest[GestaltFrameworkSecurityEnvironment,A], errorMessage: String, statusCode: Int, startTime: Long) = {
    val u = sr.identity.account
    val time = s"${(System.currentTimeMillis() - startTime).toString}ms"
    val email = u.email getOrElse "N/A"

    /*
     * You can use this to determine the actual handler engaged for the route.
     */
//    val route = if (sr.tags.contains(Tags.RouteController)) {
//      "%s.%s".format(
//        sr.tags(Tags.RouteController),
//        sr.tags(Tags.RouteActionMethod))
//    } else "ERROR_ROUTE_NOT_FOUND"
//    println("*****ROUTE : " + route)
      
      
    val template = " [ERROR] request.id=%s request.status=%s request.time=%s request.method=%s request.uri=%s user.id=%s user.name=%s user.email=%s error.message='%s'"
    template.format(sr.id, statusCode.toString, time, sr.method, sr.uri, u.id, u.name, email, errorMessage)
  }

  private def auditError[A](sr: SecuredRequest[GestaltFrameworkSecurityEnvironment,A], ex: Throwable, startTime: Long): String = {
    auditError(sr, ex.getMessage, errorCode(ex), startTime)
  }  
  
  
  
  private def auditPost[A](sr: SecuredRequest[GestaltFrameworkSecurityEnvironment,A], result: Result, startTime: Long) = {
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

  private def auditMessage[A](sr: SecuredRequest[GestaltFrameworkSecurityEnvironment,A]) = {
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


