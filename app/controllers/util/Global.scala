
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
//import controllers.util.toError

object Global extends GlobalSettings {

  override def onError(request: RequestHeader, ex: Throwable) = {
    log.debug("Global::onError(...)")
    Future.successful(InternalServerError(toError(500, ex.getMessage)))
  }
      
  override def onBadRequest(request: RequestHeader, error: String) = {
    log.debug("Global::onBadRequest(...)")
    Future.successful(BadRequest(toError(400, error)))    
  }  
  
  override def onHandlerNotFound(request: RequestHeader) = {
    log.debug("Global::onHandlerNotFound(...)")
    Future {
      if (request.path.endsWith("/"))
        MovedPermanently(request.path.dropRight(1))
      else NotFound(toError(404, request.path))
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