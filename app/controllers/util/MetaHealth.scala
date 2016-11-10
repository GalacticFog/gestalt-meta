package controllers.util

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.meta.api.sdk.{JsonWebClient => WebClient}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ ExecutionContext, Future, Promise, Await }
import scala.concurrent.duration._
import scala.util.{Try,Success,Failure}
import java.net.URL

import org.joda.time.DateTime

import play.api.libs.json._

import controllers.util.db.EnvConfig

import scala.util.{Either,Left,Right}
import play.api.Logger

case class ServiceUnavailableException(message: String) extends RuntimeException(message)

object MetaHealth {
  
  val log = Logger(this.getClass)
  
  /*
   * Check connectivity with the following services:
   * - security
   * - postgres
   * - lambda
   * - apigateway
   * - rabbit - for policy
   * 
   */
  object Status {
    val Healthy = "healthy"
    val Degraded = "degraded"
    val Unavailable = "unavailable"
  }

  val DEFAULT_SERVICE_TIMEOUT_SECONDS = 5

  val rabbitWebUrl = "%s://%s:%s".format(
      EnvConfig.rabbitHttpProtocol, EnvConfig.rabbitHost, EnvConfig.rabbitHttpPort)
  
  val gatewayConfig = HostConfig.make(new URL(EnvConfig.gatewayUrl))
  val lambdaConfig  = HostConfig.make(new URL(EnvConfig.lambdaUrl))
  
  // TODO: We need to store creds for all of these service in env.
  val rabbitConfig  = HostConfig.make(new URL(rabbitWebUrl), 
      creds = Option(BasicCredential("guest", "guest")))  
  
  
  def selfCheck(verbose: Boolean): Either[JsValue,JsValue] = {
    
    DataStore.assertOnline(System.exit(1), "Check the PostgreSQL connection string.")
    
    val serviceMap = Map(
        lambdaConfig  -> "/health",
        gatewayConfig -> "/health",
        rabbitConfig  -> "/api/aliveness-test/%2F")
    
    val stats  = checkAll(serviceMap, DEFAULT_SERVICE_TIMEOUT_SECONDS)
    val errors = stats collect { case (k,v) if v.isFailure => (k,v) }

    if (errors.isEmpty) Right(goodHealthMessage()) 
    else Left(badHealthMessage(Status.Unavailable, errors, verbose))
    
  }
  
  def goodHealthMessage() = {
    Json.obj(
        "timestamp" -> DateTime.now.toString,
        "status"    -> "healthy")
  }
  
  def badHealthMessage(status: String, badServices: Map[String,Try[Boolean]], verbose: Boolean = false): JsObject = {
    
    def message(status: String) = status match {
      case Status.Unavailable => "Meta is not operational"
      case Status.Degraded    => "Meta is operational but some features may be disabled."
    }
    
    val failed = badServices map { case (k,v) => 
      val message = v recover {
        case e: Throwable => e.getMessage
      }
      (k, message.get.toString)
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
      expected: Seq[Int] = WebClient.ALL_GOOD): Map[String,Try[Boolean]] = {
      
    serviceMap map { case (config, url) => 
      (mkurl(config), checkService(config, url, timeout, expected)) 
    }
  }
  
  import scala.util.control.NonFatal
  
  def checkService(
      config: HostConfig, 
      resource: String, 
      timeout: Int,  
      expected: Seq[Int] = WebClient.ALL_GOOD) = Try {
    
    val client = WebClient(config)
    try {
      val response = Await.result(client.request(resource).get, timeout seconds)
      response.status match {
        case s if expected.contains(s) => true 
        case e => unexpectedStatus(mkurl(config), expected, e)
      }
    } catch { 
        case NonFatal(e) => {
          log.error(s"Error checking service status: ${resource} : " + e.getMessage)
          throw e
        } 
    }
  }
  
  def unexpectedStatus(url: String, expectedStatus: Seq[Int], receivedStatus: Int) = {
    receivedStatus match {
      case 503 => throw new ServiceUnavailableException("Service Unavailable")
      case _   => throw new RuntimeException(
          s"Unexpected HTTP status from '$url'. expected: ${expectedStatus.mkString(",")}, found: ${receivedStatus}.")
    }
  }
  
  // TODO: Add .url accessor to HostConfig
  def mkurl(config: HostConfig) = {
    "%s://%s%s".format(config.protocol, config.host, 
        (if (config.port.isDefined) s":${config.port.get}" else ""))
  }

  
}