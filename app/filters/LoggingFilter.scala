package controllers.util

import play.api.Logger
import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import play.api.libs.json._
import play.api.routing.Router.Tags

protected[util] case class TraceLog(method: String, route: String, action: String, status: Int, execTimeMs: Option[Long], requestId: Long)

protected[util] object LoggingFilter extends Filter {
  
  private[this] val log = Logger
  private[this] implicit lazy val traceLogFormat = Json.format[TraceLog]
  
  def apply(nextFilter: (RequestHeader) => Future[Result])(requestHeader: RequestHeader): Future[Result] = {
    log.debug("%s %s".format(requestHeader.method, requestHeader.uri))
    
    val startTime = System.currentTimeMillis
    
    nextFilter(requestHeader).map { result =>
    
      val fqAction = if (requestHeader.tags.contains(Tags.RouteController)) {
        "%s.%s".format(
          requestHeader.tags(Tags.RouteController),
          requestHeader.tags(Tags.RouteActionMethod))
      } else "ERROR_ROUTE_NOT_FOUND"
      
      val requestUrl = "%s://%s%s".format(
        if (requestHeader.secure) "https" else "http",
        requestHeader.host,
        requestHeader.uri)
      
      val requestTime = System.currentTimeMillis - startTime

      log.debug ( Json.prettyPrint ( Json.toJson ( TraceLog(
        requestHeader.method,
        requestUrl,
        fqAction,
        result.header.status,
        Some(requestTime),
        requestHeader.id
      ))))
      
      result.withHeaders("Request-Time" -> requestTime.toString)
    }
  }
}