package com.galacticfog.gestalt.marathon

import java.util.UUID

import mockws.{MockWS, Route}
import org.specs2.matcher.{JsonMatchers, JsonType, Matcher}
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.specification.Scope
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.http.HttpVerbs._
import play.api.mvc._
import play.api.mvc.Results._
import play.api.test.{DefaultAwaitTimeout, FutureAwaits, PlaySpecification}
import play.api.mvc.BodyParsers.parse
import play.api.libs.concurrent.Execution.Implicits.defaultContext

class MarathonClientSpec extends PlaySpecification with Mockito with JsonMatchers with FutureAwaits with DefaultAwaitTimeout {

  val unauthorized = Unauthorized(Json.obj(
    "message" -> "TEST FAILURE: expected token was not present"
  ))
  val tmi = Unauthorized(Json.obj(
    "message" -> "TEST FAILURE: token was passed when not required"
  ))

  val expectedToken = UUID.randomUUID().toString()
  val marUrl = "https://marathon.mesos"
  def marathonWithAuth(response: JsValue = Json.obj()) = Route {
    case (_,url) if url.startsWith(marUrl) => Action{request => request.headers.get(AUTHORIZATION) match {
      case Some(auth) if auth.contains("token=" + expectedToken) => Ok(response)
      case _ => unauthorized
    }}
  }
  def marathonWithoutAuth(response: JsValue = Json.obj()) = Route {
    case (_,url) if url.startsWith(marUrl) => Action{request => request.headers.get(AUTHORIZATION) match {
      case Some(_) => tmi
      case None => Ok(response)
    }}
  }

  abstract class FakeWS(clientToken: Option[String], testRoute: MockWS.Routes) extends Scope {
    val routes = Route.apply(testRoute)
    val mockWS = MockWS.apply(routes)
    val marClient = MarathonClient(mockWS, marUrl, clientToken)
  }

  "MarathonClient" >> {

    "launchApp should use specified auth token" in new FakeWS(clientToken = Some(expectedToken), testRoute = marathonWithAuth()) {
      await(marClient.launchApp(Json.obj())) must_== Json.obj()
    }

    "launchApp should not use auth header if token not specified" in new FakeWS(clientToken = None, testRoute = marathonWithoutAuth()) {
      await(marClient.launchApp(Json.obj())) must_== Json.obj()
    }

    "getApplicationByAppId should use specified auth token" in new FakeWS(clientToken = Some(expectedToken), testRoute = marathonWithAuth(Json.obj("app" -> Json.obj()))) {
      await(marClient.getApplicationByAppId("/some/app/id")) must_== Json.obj()
    }

    "getApplicationByAppId should not use auth header if token not specified" in new FakeWS(clientToken = None, testRoute = marathonWithoutAuth(Json.obj("app" -> Json.obj()))) {
      await(marClient.getApplicationByAppId("/some/app/id")) must_== Json.obj()
    }

    "listApplicationsInEnvironment should use specified auth token" in new FakeWS(clientToken = Some(expectedToken), testRoute = marathonWithAuth(Json.obj("apps" -> Json.arr()))) {
      await(marClient.listApplicationsInEnvironment(None,"blah","blah","blah")) must_== Seq.empty
    }

    "listApplicationsInEnvironment should not use auth header if token not specified" in new FakeWS(clientToken = None, testRoute = marathonWithoutAuth(Json.obj("apps" -> Json.arr()))) {
      await(marClient.listApplicationsInEnvironment(None,"blah","blah","blah")) must_== Seq.empty
    }

    "updateApplication should use specified auth token" in new FakeWS(clientToken = Some(expectedToken), testRoute = marathonWithAuth(Json.obj())) {
      await(marClient.updateApplication("/some/app/id", Json.obj())) must_== Json.obj()
    }

    "updateApplication should not use auth header if token not specified" in new FakeWS(clientToken = None, testRoute = marathonWithoutAuth(Json.obj())) {
      await(marClient.updateApplication("/some/app/id", Json.obj())) must_== Json.obj()
    }

    "deleteApplication should use specified auth token" in new FakeWS(clientToken = Some(expectedToken), testRoute = marathonWithAuth(Json.obj())) {
      await(marClient.deleteApplication("/some/app/id")) must_== Json.obj()
    }

    "deleteApplication should not use auth header if token not specified" in new FakeWS(clientToken = None, testRoute = marathonWithoutAuth(Json.obj())) {
      await(marClient.deleteApplication("/some/app/id")) must_== Json.obj()
    }

    "scaleApplication should use specified auth token" in new FakeWS(clientToken = Some(expectedToken), testRoute = marathonWithAuth(Json.obj())) {
      await(marClient.scaleApplication("/some/app/id", 1)) must_== Json.obj()
    }

    "scaleApplication should not use auth header if token not specified" in new FakeWS(clientToken = None, testRoute = marathonWithoutAuth(Json.obj())) {
      await(marClient.scaleApplication("/some/app/id", 1)) must_== Json.obj()
    }

    "getInfo should use specified auth token" in new FakeWS(clientToken = Some(expectedToken), testRoute = marathonWithAuth(Json.obj())) {
      await(marClient.getInfo()) must_== Json.obj()
    }

    "getInfo should not use auth header if token not specified" in new FakeWS(clientToken = None, testRoute = marathonWithoutAuth(Json.obj())) {
      await(marClient.getInfo()) must_== Json.obj()
    }

    "listDeploymentsAffectingEnvironment should use specified auth token" in new FakeWS(clientToken = Some(expectedToken), testRoute = marathonWithAuth(Json.obj())) {
      await(marClient.listDeploymentsAffectingEnvironment("blah","blah","blah")) must_== Json.arr(Json.arr())
    }

    "listDeploymentsAffectingEnvironment should not use auth header if token not specified" in new FakeWS(clientToken = None, testRoute = marathonWithoutAuth(Json.obj())) {
      await(marClient.listDeploymentsAffectingEnvironment("blah","blah","blah")) must_== Json.arr(Json.arr())
    }

  }

}


