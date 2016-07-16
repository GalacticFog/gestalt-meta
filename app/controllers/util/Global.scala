
package controllers.util

import play.api.{Logger => log}
import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import play.api.libs.json._


case class TraceLog(method: String, route: String, action: String, status: Int, execTimeMs: Option[Long])

object LoggingFilter extends Filter {
  
  implicit lazy val traceLogFormat = Json.format[TraceLog]
  
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


object Global extends WithFilters(LoggingFilter) with GlobalSettings  {
  
  override def onRouteRequest(request: RequestHeader): Option[Handler] = {
    super.onRouteRequest(request)  
  }
  
  override def onError(request: RequestHeader, ex: Throwable) = {
    Future.successful{ 
      HandleExceptions(ex)
    }
  }
  
  override def onBadRequest(request: RequestHeader, error: String) = {
    Future.successful(BadRequestResult(error))    
  }  
  
  override def onHandlerNotFound(request: RequestHeader) = {
    Future {
      if (request.path.endsWith("/"))
        MovedPermanently(request.path.dropRight(1))
      else NotFoundResult("ROUTE_NOT_FOUND: " + request.path)
    }
  }
  
}