package controllers.util

import play.api.{Logger, _}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.Results._
import play.api.mvc._

import scala.concurrent.Future

object Global extends GlobalSettings  {

  private[this] val log = Logger(this.getClass)

  override def onRouteRequest(request: RequestHeader): Option[Handler] = {
//    if (request.uri == "/statusupdate") setServiceStatus()
//    if (status == MetaHealth.Status.Unavailable)
//      Option(Action(ServiceUnavailable(health)))
//    else super.onRouteRequest(request)
    super.onRouteRequest(request)
  }

  override def onError(request: RequestHeader, ex: Throwable) = {
    log.warn("Caught exception at top-level.")
    ex.printStackTrace()

    Future.successful( HandleExceptions(ex) )
  }

  override def onBadRequest(request: RequestHeader, error: String) = {
    Future.successful( BadRequestResult(error) )    
  }  

  override def onHandlerNotFound(request: RequestHeader) = {
    Future {
      if (request.path.endsWith("/"))
        MovedPermanently(request.path.dropRight(1))
      else NotFoundResult("ROUTE_NOT_FOUND: " + request.path)
    }
  }
  
}
