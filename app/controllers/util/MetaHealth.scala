package controllers.util

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.laser.{JsonWebClient => WebClient}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ ExecutionContext, Future, Promise, Await }
import scala.concurrent.duration._
import scala.util.{Try,Success,Failure}
import java.net.URL

import org.joda.time.DateTime

import play.api.libs.json._

import controllers.util.db.EnvConfig

import scala.util.{Either,Left,Right}

case class ServiceUnavailableException(message: String) extends RuntimeException(message)

object MetaHealth {

  /*
   * Check connectivity with the following services:
   * - security
   * - postgres
   * - lambda
   * - apigateway
   * - rabbit - for policy
   * 
   */
  
  val GESTALT_SERVICE_HEALTHY = "healthy"
  val GESTALT_SERVICE_DEGRADED = "degraded"
  val GESTALT_SERVICE_UNAVAILABLE = "unavailable"
  
  val DEFAULT_SERVICE_TIMEOUT_SECONDS = 5

  val rabbitWebUrl = "http://%s".format(EnvConfig.rabbitHost)
  
  val gatewayConfig = HostConfig.make(new URL(EnvConfig.gatewayUrl))
  val lambdaConfig  = HostConfig.make(new URL(EnvConfig.lambdaUrl))
  val rabbitConfig  = HostConfig.make(new URL(rabbitWebUrl), 
      creds = Option(BasicCredential("guest", "guest")))  
  

  def selfCheck(verbose: Boolean): Either[JsValue,JsValue] = {
    val serviceMap = Map(
        lambdaConfig  -> "/health",
        gatewayConfig -> "/health",
        rabbitConfig  -> "/api/aliveness-test/%2F")
    
    val stats  = checkAll(serviceMap, DEFAULT_SERVICE_TIMEOUT_SECONDS)
    val errors = stats collect { case (k,v) if v.isFailure => (k,v) }
    
    println("ERRORS : " + errors)
    println("IS-EMPTY: " + errors.isEmpty)
    
    if (errors.isEmpty) Right(goodHealthMessage()) 
    else Left(badHealthMessage(GESTALT_SERVICE_UNAVAILABLE, errors, verbose))
    
  }
  
  def goodHealthMessage() = {
    Json.obj(
        "timestamp" -> DateTime.now.toString,
        "status"    -> "healthy")
  }
  
  def badHealthMessage(status: String, badServices: Map[String,Try[Boolean]], verbose: Boolean = false): JsObject = {
    
    def message(status: String) = status match {
      case GESTALT_SERVICE_UNAVAILABLE => "Meta is not operational"
      case GESTALT_SERVICE_DEGRADED => "Meta is operational but some features may be disabled."
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
    } finally {
      client.close()
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
        (if (config.port.isDefined) s":${config.port}" else ""))
  }

  
}