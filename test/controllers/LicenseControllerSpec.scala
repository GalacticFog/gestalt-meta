package controllers

import java.util.UUID

import com.galacticfog.gestalt.data.Instance
import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.marathon
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.api.GestaltSecurityClient
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.test.FakeGestaltSecurityEnvironment
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util.{ContainerService, GestaltSecurityMocking}
import org.joda.time.{DateTimeZone, DateTime}
import org.specs2.execute.{Result, AsResult}
import org.specs2.matcher.JsonMatchers
import org.specs2.specification._
import play.api.libs.json.{JsArray, Json}
import play.api.test._
import play.api.{Application, GlobalSettings, Play}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import org.mockito.Matchers.{eq => meq}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Try, Success}


class LicenseControllerSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll with JsonMatchers {
  
  override def beforeAll(): Unit = pristineDatabase()

  sequential

  lazy val creds = dummyCreds()
  lazy val authResponse = dummyAuthAccount()
  lazy val mockSecurityClient = mock[GestaltSecurityClient]
  lazy val fakeSecurity = FakeGestaltSecurityEnvironment[DummyAuthenticator](Seq(
    creds -> authResponse
  ), mockSecurityClient)  
  
  def fakeAuthRequest(method: String, path: String) = { 
    FakeRequest(method, path).withHeaders(AUTHORIZATION -> creds.headerValue)
  }
  
//  import play.api.mvc.Controller
//  
//  def MetaGlobalSettings[A](typeMap: Map[Class[_], _ <: Controller]) = new GlobalSettings {
//    override def getControllerInstance[A](controllerClass: Class[A]): A = {
//      typeMap.get(controllerClass).fold {
//        super.getControllerInstance(controllerClass) 
//      }{ _.asInstanceOf[A] }
//    }    
//  }
//  
//  abstract class DynamicApplication(typeMap: Map[Class[_], _ <: Controller]) extends WithApplication(
//      FakeApplication(withGlobal = Option(MetaGlobalSettings(typeMap)))) {
//  }

  def testGlobal() = new GlobalSettings {
    override def getControllerInstance[A](controllerClass: Class[A]): A = {
      if (classOf[LicenseController] == controllerClass) {
        new LicenseController {
          override val env = fakeSecurity
          override val securityClient = mockSecurityClient
        }
      }.asInstanceOf[A]
      else super.getControllerInstance(controllerClass)
    }
  }  
  
  abstract class TestApplication extends WithApplication(FakeApplication(withGlobal = Some(testGlobal))) 
  
  "LicenseController" should {
    
    "support GET /licenses" in new TestApplication {
      val js = Json.arr()
      val request = fakeAuthRequest(GET, s"/root/licenses")
      val Some(result) = route(request)
      status(result) must equalTo(OK)
      contentAsJson(result) must equalTo(js)      
    }.pendingUntilFixed("Need to make GestaltLicense into interface")
    
  }
  
  
}



