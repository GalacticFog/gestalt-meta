package controllers.util

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.sdk.JsonClient
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Await
import scala.concurrent.duration._
// import scala.util.Try
import java.net.URL

import akka.actor.ActorSystem
import akka.stream.Materializer
import org.joda.time.DateTime
import play.api.libs.json._
import controllers.util.db.EnvConfig

import scala.util.{Either, Left, Right}
import play.api.Logger

import scala.language.postfixOps
// import scala.util.control.NonFatal
import play.api.libs.ws.WSClient
import javax.inject.{Inject, Singleton}


case class ServiceUnavailableException(message: String) extends RuntimeException(message)

@Singleton
class MetaHealth @Inject()( dataStore: DataStore, ws: WSClient )(implicit actorSystem: ActorSystem, mat: Materializer) {
  
  val log = Logger(this.getClass)
  
  /*
   * Check connectivity with the following services:
   * - security
   * - postgres
   * - lambda
   * - apigateway
   * - rabbit - for policy
   */
  
  object Status {
    val Healthy = "healthy"
    val Degraded = "degraded"
    val Unavailable = "unavailable"
  }
  
  val DEFAULT_SERVICE_TIMEOUT_SECONDS = 5
  
  val rabbitWebUrl = "%s://%s:%s".format(
      EnvConfig.rabbitHttpProtocol, EnvConfig.rabbitHost, EnvConfig.rabbitHttpPort)
  
  // TODO: We need to store creds for all of these service in env.
  val rabbitConfig  = HostConfig.make(new URL(rabbitWebUrl), 
      creds = Option(BasicCredential("guest", "guest")))
  
  
  def selfCheck(verbose: Boolean): Either[JsValue,JsValue] = {
    
    dataStore.assertOnline(System.exit(1), "Check the PostgreSQL connection string.")
    
    val serviceMap = Map(
      rabbitConfig  -> "/api/aliveness-test/%2F"
    )
    
    val stats  = checkAll(serviceMap, DEFAULT_SERVICE_TIMEOUT_SECONDS)
    val errors = stats collect { case (k, Left(v)) => (k,v) }
    
    if (errors.isEmpty) Right(goodHealthMessage()) 
    else Left(badHealthMessage(Status.Unavailable, errors, verbose))
  }
  
  def goodHealthMessage() = {
    Json.obj(
        "timestamp" -> DateTime.now.toString,
        "status"    -> "healthy")
  }
  
  def badHealthMessage(status: String, badServices: Map[String,Throwable], verbose: Boolean = false): JsObject = {
    
    def message(status: String) = status match {
      case Status.Unavailable => "Meta is not operational"
      case Status.Degraded    => "Meta is operational but some features may be disabled."
    }
    
    val failed = badServices map { case (k, e) => 
      (k, e.getMessage)
    }
    
    Json.obj(
        "timestamp" -> DateTime.now.toString,
        "status"    -> status,
        "message"   -> message(status)) ++ {
          if (verbose) 
            Json.obj("failed_integrations" -> Json.toJson(failed)) else Json.obj()
        }
  }
  
  def checkAll(
      serviceMap: Map[HostConfig,String], 
      timeout: Int, 
      expected: Seq[Int] = Seq(200 to 299:_*)): Map[String,Either[Throwable,Unit]] = {
      
    serviceMap map { case (config, url) => 
      (mkurl(config), checkService(config, url, timeout, expected)) 
    }
  }
  
  def checkService(
      config: HostConfig, 
      resource: String, 
      timeout: Int,  
      expected: Seq[Int] = Seq(200 to 299:_*)): Either[Throwable,Unit] = {
    
    val client = new JsonClient(config, Some(ws))
    
    // try {
    val response = Await.result(client.get(resource), timeout seconds)
    response.status match {
      case s if expected.contains(s) => Right(()) 
      case e => Left(unexpectedStatus(mkurl(config), expected, e))
    }
    // } catch { 
    //     case NonFatal(e) => {
    //       log.error(s"Error checking service status: ${resource} : " + e.getMessage)
    //       throw e
    //     } 
    // }
  }
  
  def unexpectedStatus(url: String, expectedStatus: Seq[Int], receivedStatus: Int): Throwable = {
    receivedStatus match {
      case 503 => new ServiceUnavailableException("Service Unavailable")
      case _   => new RuntimeException(
          s"Unexpected HTTP status from '$url'. expected: ${expectedStatus.mkString(",")}, found: ${receivedStatus}.")
    }
  }
  
  // TODO: Add .url accessor to HostConfig
  def mkurl(config: HostConfig) = {
    "%s://%s%s".format(config.protocol, config.host, 
        (if (config.port.isDefined) s":${config.port.get}" else ""))
  }
  
}
