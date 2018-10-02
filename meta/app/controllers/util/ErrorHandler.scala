package controllers.util

import play.api.Logger
import play.api.http.HttpErrorHandler
import play.api.http.Status._
import play.api.mvc.Results._
import play.api.mvc.{RequestHeader, Result}

import scala.concurrent.Future

class ErrorHandler extends HttpErrorHandler {

  private[this] val log = Logger(this.getClass)

  override def onClientError(request: RequestHeader, statusCode: Int, message: String): Future[Result] = {
    statusCode match {
      case BAD_REQUEST => Future.successful(BadRequestResult(message))
      case NOT_FOUND => Future.successful {
        if (request.path.endsWith("/")) MovedPermanently(request.path.dropRight(1))
        else NotFoundResult("ROUTE_NOT_FOUND: " + request.path)
      }
      case _ => Future.successful(GenericErrorResult(statusCode, message))
    }
  }

  override def onServerError(request: RequestHeader, exception: Throwable): Future[Result] = {
    Future.successful( HandleExceptions(exception) )
  }

}
