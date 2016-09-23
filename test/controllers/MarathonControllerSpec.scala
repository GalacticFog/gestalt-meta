package controllers

import java.util.UUID

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.api.GestaltSecurityClient
import com.galacticfog.gestalt.security.play.silhouette.test.FakeGestaltSecurityEnvironment
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util.{ContainerService, GestaltSecurityMocking}
import org.specs2.specification._
import play.api.libs.json.{JsArray, Json}
import play.api.test._
import play.api.{Application, GlobalSettings, Play}
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.{ExecutionContext, Future}


class MarathonControllerSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll {

  override def beforeAll(): Unit = pristineDatabase()

  lazy val creds = dummyCreds()
  lazy val authResponse = dummyAuthAccount()
  lazy val mockSecurityClient = mock[GestaltSecurityClient]
  lazy val fakeSecurity = FakeGestaltSecurityEnvironment[DummyAuthenticator](Seq(
    creds -> authResponse
  ), mockSecurityClient)

  def testGlobal() = new GlobalSettings {
    lazy val cs = mock[ContainerService]
    override def getControllerInstance[A](controllerClass: Class[A]): A = {
      if (classOf[ContainerService] == controllerClass) cs.asInstanceOf[A]
      else if (classOf[MarathonAPIController] == controllerClass) {
        new MarathonAPIController(cs) {
          override val env = fakeSecurity
          override val securityClient = mockSecurityClient
        }
      }.asInstanceOf[A]
      else super.getControllerInstance(controllerClass)
    }
  }

  lazy val (testWID,testEID) = createWorkspaceEnvironment(wrkName = "test-workspace", envName = "test-environment")
  lazy val testPID = createMarathonProvider(testEID, "test-provider").get.id

  def fakeAuthRequest(method: String, path: String) = FakeRequest(method, path).withHeaders(AUTHORIZATION -> creds.headerValue)

  def containerService(implicit app: Application) = Play.global(app).getControllerInstance(classOf[ContainerService])

  "MarathonAPIController" should {
    
    "support Marathon GET /v2/info" in new WithApplication(FakeApplication(withGlobal = Some(testGlobal))) {
      val cs = containerService
      val pr = mock[GestaltResourceInstance]
      val mc = mock[MarathonClient]
      cs.marathonProvider(testPID) returns pr
      cs.marathonClient(pr) returns mc
      val js = Json.obj()
      mc.getInfo()(any[ExecutionContext]) returns Future.successful(js)

      val request = fakeAuthRequest(GET, s"/root/environments/${testEID}/providers/${testPID}/v2/info")
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      contentAsJson(result) must equalTo(js)
    }

    "support Marathon GET /v2/deployments" in new WithApplication(FakeApplication(withGlobal = Some(testGlobal))) {
      val cs = containerService
      val pr = mock[GestaltResourceInstance]
      val mc = mock[MarathonClient]
      cs.marathonProvider(testPID) returns pr
      cs.marathonClient(pr) returns mc
      val js = Json.arr()
      mc.listDeploymentsAffectingEnvironment_marathon_v2(fqon = "root", wrkName = "test-workspace", envName = "test-environment") returns Future.successful(js)
      mc.getInfo()(any[ExecutionContext]) returns Future.successful(js)

      val request = fakeAuthRequest(GET, s"/root/environments/${testEID}/providers/${testPID}/v2/deployments")
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      contentAsJson(result) must equalTo(js)
    }

  }
}