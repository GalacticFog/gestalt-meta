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


class LicenseControllerSpec extends GestaltSecurityMocking with ResourceScope with BeforeAll with JsonMatchers {
  
  override def beforeAll(): Unit = pristineDatabase()

  sequential
  
  abstract class TestApplication extends WithApplication(containerApp()) {
    
  }
  
  "LicenseController" should {
    
    "support GET /licenses" in new TestApplication {
      
      val js = Json.arr()
      val request = fakeAuthRequest(GET, s"root/licenses", testCreds)
      val Some(result) = route(request)
      
      status(result) must equalTo(OK)
      
      contentAsJson(result) must equalTo(js)      
    }.pendingUntilFixed("Need to make GestaltLicense into interface")
    
  }
  
  
}



