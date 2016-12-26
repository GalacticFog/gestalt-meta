package controllers.util


import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Scope
import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.json.Js

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
    Js.find(obj, s"/properties/$keyName").get
  }
  
}


class JsonUtilSpec extends Specification {

  
  "replaceKey" should {
    
    "replace an existing JSON key name, leaving the original value when newValue == None" in new JsonTestScope {
      failure
    }.pendingUntilFixed
    
    "replace an existing JSON key name using a new value when one is given" in new JsonTestScope {
      failure
    }.pendingUntilFixed
    
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
  
  
  case class Widget(id: Int, name: String)
  implicit lazy val wf = Json.format[Widget]
        

  
  "safeParse[A](String)" should {

    "parse a JSON string to the given type" in {
      val w = JsonUtil.safeParse[Widget]("""{ "id": 123, "name": "foo" }""")
      w must haveClass[Widget]
      
      val a1 = """[ "one", "two", "three"  ]"""
      val a2 = """[ 1, 2, 3 ]"""
      val a3 = """[ true, false, true ]"""
      
      val w2 = JsonUtil.safeParse[Seq[String]](a1)
      w2 must beAnInstanceOf[Seq[String]]
      w2.size === 3
      
      val w3 = JsonUtil.safeParse[Seq[Int]](a2)
      w3 must beAnInstanceOf[Seq[Int]]
      
      val w4 = JsonUtil.safeParse[Seq[Boolean]](a3)
      w4 must beAnInstanceOf[Seq[Boolean]]
    }
    
    "throw an Exception if the string does not represent the type" in {
      JsonUtil.safeParse[Widget]("""{ "id": 123 }""") must throwA[BadRequestException]
      JsonUtil.safeParse[Seq[Int]]("""{ "id": 123 }""") must throwA[BadRequestException]
      JsonUtil.safeParse[Seq[Int]]("""[ "one", "two", "three" ]""") must throwA[BadRequestException]
    }
    
  }  
  
  "safeParse[A](JsValue)" should {

    "parse a JsValue to the given type" in {
      
      val w = JsonUtil.safeParse[Widget](Json.toJson(Widget(123, "foo")))
      w must haveClass[Widget]
      
      val a1 = Json.parse("""[ "one", "two", "three"  ]""")
      val a2 = Json.parse("""[ 1, 2, 3 ]""")
      val a3 = Json.parse("""[ true, false, true ]""")
      
      val w2 = JsonUtil.safeParse[Seq[String]](a1)
      w2 must beAnInstanceOf[Seq[String]]
      w2.size === 3
      
      val w3 = JsonUtil.safeParse[Seq[Int]](a2)
      w3 must beAnInstanceOf[Seq[Int]]
      
      val w4 = JsonUtil.safeParse[Seq[Boolean]](a3)
      w4 must beAnInstanceOf[Seq[Boolean]]
    }
    
    "throw an Exception if the JsValue does not represent the type" in {
      JsonUtil.safeParse[Seq[Int]](Json.parse("""{ "id": 123 }""")) must throwA[BadRequestException]
      JsonUtil.safeParse[Seq[Int]](Json.parse("""[ "one", "two", "three" ]""")) must throwA[BadRequestException]
    }
    
  }    
  
}