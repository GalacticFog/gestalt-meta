package com.galacticfog.gestalt.laser

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

case class BasicCredential(username: String, password: String) {
  val scheme = WSAuthScheme.BASIC
  def asOpt = Option(this)
}

case class HostConfig(protocol: String, host: String, port: Option[Long], timeout: Int = 10, creds: Option[BasicCredential] = None)
case class ApiResponse(status: Int, output: Option[JsValue], error: Option[String] = None)

class JsonWebClient(config: HostConfig) {
  
  implicit lazy val apiResponseFormat = Json.format[ApiResponse]
  
  val builder = new AsyncHttpClientConfig.Builder()
  val client  = new NingWSClient(builder.build())

  val portString = if (config.port.isDefined) s":${config.port.get.toString}" else ""
  val baseUrl = "%s://%s%s".format(config.protocol, config.host, portString)
  val timeout = config.timeout

  def get(resource: String, expected: Seq[Int] = Seq(200), timeout: Int = timeout): Try[ApiResponse] = Try {
    val response = sync(resource, timeout)(request(_).get())
    unwrapResponse(response, expected)
  }

  def postEmpty(resource: String, expected: Seq[Int] = Seq(201), timeout: Int = timeout): Try[ApiResponse] = Try {
    val response = Await.result(request(resource).post(Array.empty[Byte]), timeout seconds)
    unwrapResponse(response, expected)
  }

  def post(resource: String, payload: JsValue, expected: Seq[Int] = Seq(201), timeout: Int = timeout): Try[ApiResponse] = Try {
    val response = Await.result(request(resource).post(payload), timeout seconds)
    unwrapResponse(response, expected)
  }

  def delete(resource: String, expected: Seq[Int] = Seq(200, 204), timeout: Int = timeout): Try[ApiResponse] = Try {
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
    val ws = if (config.creds.isDefined) {
      val c = config.creds.get
      client.url(url).withAuth(c.username, c.password, c.scheme)
    } else client.url(url)
    ws.withRequestTimeout(timeout * 1000)
  }

  def unwrapResponse(response: WSResponse, expected: Seq[Int]) = {
    if (expected.contains(response.status)) {
      ApiResponse(response.status, output = Some(Json.parse(response.body)))
    } else throw new ApiResponseException(
        Json.prettyPrint(Json.toJson(ApiResponse(response.status, None, error = Some(response.statusText)))))
  }
  
  
  case class ApiResponseException(message: String) extends RuntimeException(message)
  
  
}


