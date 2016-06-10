package controllers.util


import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Scope
import play.api.libs.json._

trait JsonTestScope extends Scope {


val json1 = """
{    
  "name": "url",
  "properties": {
    "protocol": "http",
    "host": "localhost",
    "port": 1234,
    "resource": {
      "uri": "/foo",
      "content_type": "application/json",
      "accept": "application/json",
      "timeout": 5000
    }
  }
}
"""

  val jsonUrl = Json.parse(json1).as[JsObject]
  
  def getJsonPropValue(obj: JsObject, keyName: String): JsValue = {
    obj \ "properties" \ keyName match {
      case u: JsUndefined => throw new IllegalArgumentException(s"Key '$keyName' not found")
      case v => v
    }
  }
  
}


class JsonUtilSpec extends Specification {

  "find" should {
    
    "find a top-level JSON key that exists" in new JsonTestScope {
      val name = JsonUtil.find(jsonUrl, "name") 
      name must beSome
      name.get.as[String] must_== "url"
    }
    
    "find a nested JSON key that exists" in new JsonTestScope {
      val timeout = JsonUtil.find(jsonUrl, "/properties/resource/timeout")
      timeout must beSome
      timeout.get.as[Int] must_== 5000
    }

    "return None when given a JSON path that does not exist" in new JsonTestScope {
      JsonUtil.find(jsonUrl, "/path/does/not/exist")
    }
    
  }
  
  "replaceKey" should {
    
    "replace an existing JSON key name, leaving the original value when newValue == None" in new JsonTestScope {
      failure
    }
    
    "replace an existing JSON key name using a new value when one is given" in new JsonTestScope {
      failure
    }
    
    
    
  }
  
  "upsertProperty" should {
    
    "replace an existing JSON key in the properties collection if it exists" in new JsonTestScope {
      
      val p1 = getJsonPropValue(jsonUrl, "protocol")
      p1.as[String] must_== "http"
      
      val url2 = JsonUtil.upsertProperty(jsonUrl, "protocol", JsString("https"))
      url2 must beSuccessfulTry

      getJsonPropValue(url2.get, "protocol").as[String] must_== "https"
    }
    
    "insert a JSON key into properties collection if is does not exist" in new JsonTestScope {
      getJsonPropValue(jsonUrl, "creds") must throwA[IllegalArgumentException]
      
      val url2 = JsonUtil.upsertProperty(jsonUrl, "creds", JsString("username:password"))
      url2 must beSuccessfulTry
      
      getJsonPropValue(url2.get, "creds").as[String] must_== "username:password"
    }
    
  }
  
}