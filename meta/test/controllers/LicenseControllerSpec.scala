package controllers


import com.galacticfog.gestalt.meta.test._
import org.specs2.matcher.JsonMatchers
import play.api.libs.json.Json
import org.mockito.Matchers.{eq => _}



class LicenseControllerSpec extends MetaRepositoryOps with JsonMatchers {
  
  //override def beforeAll(): Unit = pristineDatabase()
override def beforeAll(): Unit = ()
  sequential
  
  abstract class TestApplication extends WithDb(containerApp()) {
    
  }
  
  "LicenseController" should {
    
    "support GET /licenses" in new TestApplication {
      
      val js = Json.arr()
      val request = fakeAuthRequest(GET, s"root/licenses", testCreds)
      val Some(result) = route(app,request)
      
      status(result) must equalTo(OK)
      
      contentAsJson(result) must equalTo(js)      
    }.pendingUntilFixed("Need to make GestaltLicense into interface")
    
  }
  
  
}



