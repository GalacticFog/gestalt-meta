package controllers.util


import controllers.ResourceController
import play.api._
import play.api.test._
import play.api.test.Helpers._




// https://goo.gl/ic5eBu
import org.apache.commons.codec.binary.{Base64 => ApacheBase64}
object Base64 {
  def decode(encoded: String) = new String(ApacheBase64.decodeBase64(encoded.getBytes))
  def encode(decoded: String) = new String(ApacheBase64.encodeBase64(decoded.getBytes))
}

import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Step

trait BeforeAfterAll extends Specification {
  override def map(fragments: =>Fragments) = {
    beforeAll()
    fragments ^ Step(afterAll)
  }

  def beforeAll()
  def afterAll()
}

trait GestaltPlaySpecification extends BeforeAfterAll {
  lazy val app: FakeApplication = FakeApplication()
  
  def beforeAll() { Play.start(app) }
  def afterAll() { Play.stop() }  
  
  def basicAuthHeader(user: String, pass: String): (String,String) = {
    ("Authorization", "Basic " + Base64.encode(s"$user:$pass"))
  }
}


import controllers._

class SpecGlobal extends PlaySpecification  with GestaltPlaySpecification {
  
  sequential
  
  val user     = "admin"
  val password = "M8keitw0rk"  
  val auth     = basicAuthHeader(user, password)
  
  def get(uri: String) = FakeRequest(GET, uri).withHeaders(auth)
  
  
  "Global" should {
    
//    "handle invalid routes with a 404" in new WithApplication {
//      val request = get("/orgs/foo") //FakeRequest(GET, "/orgs/foo").withHeaders(basicAuthHeader(user, password))
//
//      // Call a controller method directly
//      val result = ResourceController.getAllOrgs.apply(request)
//
//      // Call via router (this doesn't work for 'framework' errors???)
//      route(request)
//
//      status(result) must_== OK
//
//      //status(result) === OK
//    }
    
  }
   
}