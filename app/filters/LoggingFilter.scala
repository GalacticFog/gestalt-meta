package filters

import play.api.Logger
import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.routing.Router.Tags
import scala.util.{Try,Success,Failure}

case class TraceLog(method: String, route: String, action: String, status: Int, execTimeMs: Option[Long], requestId: Long)

class LoggingFilter extends Filter {

  private[this] val log = Logger
  private[this] implicit lazy val traceLogFormat = Json.format[TraceLog]
  
  def apply(nextFilter: (RequestHeader) => Future[Result])
            (requestHeader: RequestHeader): Future[Result] = {

    Logger.info("%s %s".format(requestHeader.method, requestHeader.uri))

    val startTime = System.currentTimeMillis
    
    nextFilter(requestHeader).map { result =>
  
//      println("***Defining Action...")
      requestHeader.tags foreach { case (k,v) =>
        println("%-20s : %s".format(k, v))
      }
      val fqAction = if (requestHeader.tags.contains(Tags.RouteController)) {
        "%s.%s".format(
          requestHeader.tags(Tags.RouteController),
          requestHeader.tags(Tags.RouteActionMethod))
      } else "ERROR_ROUTE_NOT_FOUND"
      
//        println("***Action defined - building URL...")
        
      val requestUrl = "%s://%s%s".format(
        if (requestHeader.secure) "https" else "http",
        requestHeader.host,
        requestHeader.uri)
      
//        println("Setting request-time...")
        
      val requestTime = System.currentTimeMillis - startTime

//      Logger.info( Json.prettyPrint ( Json.toJson ( TraceLog(
//        requestHeader.method,
//        requestUrl,
//        fqAction,
//        result.header.status,
//        Some(requestTime),
//        requestHeader.id
//      ))))
  
      result.withHeaders("Request-Time" -> requestTime.toString)
    }

    
    
//    println("***TESTING RESULT...")
//    t match {
//      case Success(nf) => nf
//      case Failure(e) => {
//        println("***failed")
//        t.get
//      }
//    }
//    nf map { f => println("%%%%%%%%%%%% : " + f.header.status) }
//    
//    nf
  }

  /*
   * method: String, 
   * route: String, 
   * action: String, 
   * status: Int, 
   * execTimeMs: Option[Long], 
   * requestId: Long
   */
  
  private def msg(method: String, route: String, action: String, status: Int, execTimeMs: Option[Long], requestId: Long): String = {
    Json.prettyPrint { 
      Json.toJson ( TraceLog(
        method,
        route,
        action,
        status,
        execTimeMs,
        requestId
      ))
    }
  }
}