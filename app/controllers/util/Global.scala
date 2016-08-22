package controllers.util

import play.api.{Logger => log}
import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import play.api.libs.json._


object Global extends WithFilters(LoggingFilter) with GlobalSettings  {
  
  private[this] var status: String = null
  private[this] var health: JsValue = null
  
  def setServiceStatus() = {
    log.info("Checking Meta service health...")
    health = MetaHealth.selfCheck(verbose = true) match {
      case Left(error) => {
        log.warn("Service health is compromised.")
        error
      }
      case Right(info) => {
        log.info("Meta is healthy.")
        info
      }
    }
    log.info("Self-check results:\n" + Json.prettyPrint(health))
    status = (health \ "status").as[String]    
  }
  
  def unavailable() = Option(Action(ServiceUnavailable(health)))
  
  
  override def onStart(app: Application) {
    Logger.info("Starting Meta...")
    setServiceStatus()
  }
  
  override def onRouteRequest(request: RequestHeader): Option[Handler] = {
    
//    if (request.uri == "/statusupdate") setServiceStatus()
//    if (status == MetaHealth.Status.Unavailable) 
//      Option(Action(ServiceUnavailable(health)))
//    else super.onRouteRequest(request)
    
    super.onRouteRequest(request)
  }
  
  override def onError(request: RequestHeader, ex: Throwable) = {
    log.warn("Caught exception at top-level.")
    Future.successful( HandleExceptions(ex.getCause) )
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