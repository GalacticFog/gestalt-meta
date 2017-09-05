package controllers.util


import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltFrameworkSecuredController, GestaltSecurityEnvironment}
import play.api.mvc._
import com.galacticfog.gestalt.security.api.GestaltSecurityClient
import play.api.i18n.MessagesApi
import scala.concurrent.Future
import org.slf4j.LoggerFactory
import com.galacticfog.gestalt.meta.api.audit._
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json._


abstract class SecureController(
    messagesApi: MessagesApi,
    env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
      extends GestaltFrameworkSecuredController[DummyAuthenticator](messagesApi, env) {
  
  implicit def getSecurityClient: GestaltSecurityClient = env.client
  
  private def Authenticate() = new GestaltFrameworkAuthActionBuilderUUID(Some({rh: RequestHeader => None: Option[UUID]}))
  private def Authenticate(fqon: String) = new GestaltFrameworkAuthActionBuilder(Some({rh: RequestHeader => Some(fqon)}))
  private def Authenticate(org: UUID) = new GestaltFrameworkAuthActionBuilderUUID(Some({rh: RequestHeader => Some(org)}))
  
  
  
  def AsyncAudited()(
      block: SecuredRequest[JsValue] => Future[Result]) = 
        Authenticate().async(parse.json) { implicit request: SecuredRequest[JsValue] =>
          
    Audit.log(auditMessage(request))
    val (requestTime, result) = timefn(block(request))
    result.map(r => Audit.log(auditPost(request, r, Some(requestTime))))
    result
  }
  import play.api.mvc.BodyParser
  
  def AsyncAudited(fqon: String)(
      block: SecuredRequest[JsValue] => Future[Result]) = 
        Authenticate(fqon).async(parse.json) { implicit request: SecuredRequest[JsValue] =>

    Audit.log(auditMessage(request))
    val (requestTime, result) = timefn(block(request))
    result.map(r => Audit.log(auditPost(request, r, Some(requestTime))))
    result          
  }
  
  
  
  import com.galacticfog.gestalt.meta.api.errors.ApiException
  
  def Audited(fqon: String)(
      block: SecuredRequest[AnyContent] => Result) = 
        Authenticate(fqon) { implicit request: SecuredRequest[AnyContent] =>
          
    //Audit.log(auditMessage(request))
    try {
      val (requestTime, result) = timefn(block(request))
      Audit.log(auditPost(request, result, Some(requestTime)))
      result
    }
    catch {
      case e: Exception => {
        val statusCode = if (e.isInstanceOf[ApiException]) 
            e.asInstanceOf[ApiException].code else 500
        
  //          add status code to audit-error message...
        Audit.log(auditError(request, e))
        HandleExceptions(e)
      }
    }

  }
  
  def Audited()(
      block: SecuredRequest[AnyContent] => Result) = 
        Authenticate() { implicit request: SecuredRequest[AnyContent] =>
          
    Audit.log(auditMessage(request))
    val (requestTime, result) = timefn(block(request))
    Audit.log(auditPost(request, result, Some(requestTime)))
    result
  }
  
//  import scala.util.control.NonFatal
//  
//  def AuditedTest(fqon: String)(
//      block: SecuredRequest[AnyContent] => Result) = 
//        Authenticate(fqon) { implicit request: SecuredRequest[AnyContent] =>
//          
//    Audit.log(auditMessage(request))
//    try {
//      val (requestTime, result) = timefn(block(request))
//    } catch { 
//      case e: NonFatal => HandleExceptions(e)
//      case _ => throw e
//    } finally {
//      Audit.log(auditPost(request, result, Some(requestTime)))
//    }
//    result
//  }  
  
  def timefn[A](block: => A): (Long, A) = {
    val start  = System.currentTimeMillis()
    val result = block
    val total  = System.currentTimeMillis - start
    (total, result)    
  }
  
  def auditError[A](sr: SecuredRequest[A], ex: Exception) = {
    s"[req=${sr.id}] ERROR Request could not be completed: ${ex.getMessage}"
  }
  
  def auditPost[A](sr: SecuredRequest[A], result: Result, executionTimeMs: Option[Long] = None) = {
    val u = sr.identity.account

    val t1 = "(req=%%d) %%d %dms %%s %%s, [user: %%s, %%s, %%s]"
    val t2 = "(req=%d) (status: %d) %s %s, [user: %s, %s, %s]"
    val template = executionTimeMs.fold(t2)(t1.format(_))
    
    template.format(
        sr.id,
        result.header.status,
        sr.method, 
        sr.uri, 
        u.id, 
        u.name, 
        (u.email getOrElse "N/A"))    
  }
  
  def auditMessage[A](sr: SecuredRequest[A]) = {
    val u = sr.identity.account
    
    "(req=%d) %s %s, [user: %s, %s, %s]".format(
        sr.id,
        sr.method, 
        sr.uri, 
        u.id, 
        u.name, 
        (u.email getOrElse "N/A"))
  }

}


