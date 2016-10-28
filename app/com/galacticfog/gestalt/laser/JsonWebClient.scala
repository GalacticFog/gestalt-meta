package com.galacticfog.gestalt.laser

import java.nio.charset.StandardCharsets
import java.util.Base64

import play.api.Play.current
import play.api.libs.ws._
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import scala.concurrent.Future
import scala.util.{ Try, Success, Failure }

import play.api.libs.json._

import scala.concurrent.duration._
import scala.concurrent.Await

import com.ning.http.client.AsyncHttpClientConfig
import play.api.libs.ws.ning.NingWSClient
import java.net.URL

import com.galacticfog.gestalt.meta.api.sdk._


case class ApiResponse(status: Int, output: Option[JsValue], error: Option[String] = None)

object JsonWebClient {
  val ALL_GOOD = Seq((200 to 299):_*)

  def base64(s: String): String = Base64.getEncoder().encodeToString(s.getBytes(StandardCharsets.UTF_8))

  def apply(config: HostConfig) = {
    new JsonWebClient(config)
  }
}

class JsonWebClient(config: HostConfig) {
  
  implicit lazy val apiResponseFormat = Json.format[ApiResponse]
  
  val builder = new AsyncHttpClientConfig.Builder()
  val client  = new NingWSClient(builder.build())

  val portString = if (config.port.isDefined) s":${config.port.get.toString}" else ""
  val baseUrl = "%s://%s%s".format(config.protocol, config.host, portString)
  val timeout = config.timeout

  
  def close() = client.close()
  
  def get(resource: String, expected: Seq[Int] = JsonWebClient.ALL_GOOD, timeout: Int = timeout): Try[ApiResponse] = Try {
    val response = sync(resource, timeout)(request(_).get())
    unwrapResponse(response, expected)
  }

  def postEmpty(resource: String, expected: Seq[Int] = JsonWebClient.ALL_GOOD, timeout: Int = timeout): Try[ApiResponse] = Try {
    val response = Await.result(request(resource).post(Array.empty[Byte]), timeout seconds)
    unwrapResponse(response, expected)
  }

  def put(resource: String, payload: JsValue, expected: Seq[Int] = JsonWebClient.ALL_GOOD, timeout: Int = timeout): Try[ApiResponse] = Try {
    val response = Await.result(request(resource).put(payload), timeout seconds)
    unwrapResponse(response, expected)
  }
  
  def post(resource: String, payload: JsValue, expected: Seq[Int] = JsonWebClient.ALL_GOOD, timeout: Int = timeout): Try[ApiResponse] = Try {
    val response = Await.result(request(resource).post(payload), timeout seconds)
    unwrapResponse(response, expected)
  }

  def delete(resource: String, expected: Seq[Int] = JsonWebClient.ALL_GOOD, timeout: Int = timeout): Try[ApiResponse] = Try {
    val response = sync(resource, timeout)(request(_).delete())
    unwrapResponse(response, expected)
  }

  def req(resource: String, payload: String)(f: JsValue => Future[WSResponse]) = {

  }

  def sync(resource: String, timeout: Int = timeout)(f: String => Future[WSResponse]) = {
    Await.result(f(resource), timeout seconds)
  }

  def request(resource: String) = {
    val res = if (resource.trim.startsWith("/")) resource.trim else "/" + resource.trim
    val url = baseUrl + res
    val ws = config.creds match {
      case Some(c: APICredential) => c.addHeader(client.url(url))
      case None => client.url(url)
    }
    ws.withRequestTimeout(timeout * 1000)
  }

  import com.fasterxml.jackson.core.JsonParseException
  
  def unwrapResponse(response: WSResponse, expected: Seq[Int]) = {
    if (expected.contains(response.status)) {
      try {
        ApiResponse(response.status, output = Some(Json.parse(response.body)))
      } catch {
        case jpe: JsonParseException => throw new ApiResponseException("Response was not JSON: " + jpe.getMessage)
      }
    } else {
      println("BODY: " + response.body)
      println("UNDERLYING: " + response.underlying)
      println("ERROR: JsonWebClient.unwrapResponse => " + response.statusText)
      throw new ApiResponseException(
        Json.prettyPrint(Json.toJson(ApiResponse(response.status, None, error = Some(response.statusText))))
      )
    }
  }
  
  
  case class ApiResponseException(message: String) extends RuntimeException(message)
  
  
}


