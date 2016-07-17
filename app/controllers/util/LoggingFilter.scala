package controllers.util

import play.api.Logger
import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import play.api.libs.json._

protected[util] case class TraceLog(method: String, route: String, action: String, status: Int, execTimeMs: Option[Long])

protected[util] object LoggingFilter extends Filter {
  
  private[this] val log = Logger
  private[this] implicit lazy val traceLogFormat = Json.format[TraceLog]
  
  def apply(nextFilter: (RequestHeader) => Future[Result])(requestHeader: RequestHeader): Future[Result] = {
    
    val startTime = System.currentTimeMillis
    
    nextFilter(requestHeader).map { result =>
    
      val fqAction = if (requestHeader.tags.contains(Routes.ROUTE_CONTROLLER)) {
        "%s.%s".format(
          requestHeader.tags(Routes.ROUTE_CONTROLLER),
          requestHeader.tags(Routes.ROUTE_ACTION_METHOD))
      } else "ERROR_ROUTE_NOT_FOUND"
      
      val requestUrl = "%s://%s%s".format(
        if (requestHeader.secure) "https" else "http",
        requestHeader.host,
        requestHeader.uri)
      
      val requestTime = System.currentTimeMillis - startTime
      
      log.debug ( Json.prettyPrint ( Json.toJson (
        TraceLog(
          requestHeader.method,
          requestUrl, 
          fqAction,
          result.header.status,
          Some(requestTime))
      )))
      
      result.withHeaders("Request-Time" -> requestTime.toString)
    }
  }
}