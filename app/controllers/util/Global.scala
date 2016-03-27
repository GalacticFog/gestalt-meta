
package controllers.util

import play.api.{Logger => log}
import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import com.galacticfog.gestalt.tasks.io._
import com.galacticfog.gestalt.streaming.io._
import com.galacticfog.gestalt.streaming.io.EventMatchType._
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
    Future.successful(GenericErrorResult(500, ex.getMessage))
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



//  private var listener: GestaltStreamListener = null
//  
//  override def onStart(app: Application) {
//    val enabled = current.configuration.getBoolean("listener.enabled") getOrElse false
//    if (enabled) startTaskListener() else log.info("[meta]: TaskListener DISABLED.")
//  }  
//  
//  private def startTaskListener() {
//    val host = current.configuration.getString("listener.host") getOrElse {
//      rte("Required value 'listener.host' missing from application.conf")
//    }
//    val port = current.configuration.getInt("listener.port") getOrElse {
//      rte("Required value 'listener.port' missing from application.conf")
//    }
//    val channel = current.configuration.getString("listener.channel") getOrElse {
//      rte("Required value 'listener.channel' missing from application.conf")
//    }
//    
//    val listenConfig = ListenerConfig(host, port, channel)
//    
//    log.info("[meta]: Creating Task Listener with config:\n" + listenConfig.asJsString)
//    
//    listener = new GestaltStreamListener( listenConfig )
//    
//    //listener = new GestaltStreamListener()
//    listener.addFilters(filter("task.api", StartsWith, handler))
//    
//    log.info("[meta]: Task Listener started with config:\n" + listener.config.asJsString)
//    
//    log.info("[meta]: Starting Task Listener...")
//    listener.listen()
//
//    log.info("[meta]: Task Listener Started.")    
//  }  
//  
//  private def handler(json: JsValue) {
//    
//    println("[META]: Entered Task Handler-Function")
//    val event = unwrapJsResult[ApiTask]( json )
//    
//    /**
//     * Here's where we create the Task resource. To avoid changing the task
//     * structure, it might be possible to inject org/owner as args???
//     */
//    
//    //taskservice.createTask( event )
//    println("EVENT :\n" + event)
//  }  
//  
//  
//  import scala.reflect.runtime.universe.TypeTag
//  
//  def unwrapJsResult[T: TypeTag](json: JsValue)(implicit writes: Reads[T]) = {
//    println("JsonUtils::unwrapJsResult(...)")
//    json.validate[T] match {
//      case JsSuccess(event, _) => event
//      case err: JsError => {
//        println("JsonUtils::unwrapJsResult(...) => ERROR: " +
//            JsError.toFlatJson(err).toString)
//        throw new IllegalArgumentException {
//          JsError.toFlatJson(err).toString
//        }
//      }
//    }
//  }
  
}