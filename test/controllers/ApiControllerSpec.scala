package controllers

import org.specs2.specification.BeforeAll
import org.specs2.mock.Mockito
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.meta.api.errors._
import controllers.util.GestaltProviderMocking
import play.api.libs.json.Json
import play.api.test.PlaySpecification
import play.api.test.WithApplication
import java.util.UUID

import com.galacticfog.gestalt.json.Js
import play.api.libs.json._

class ApiControllerSpec extends PlaySpecification 
    with GestaltProviderMocking
    with ResourceScope 
    with BeforeAll {
  
  override def beforeAll(): Unit = pristineDatabase()

  sequential
  
  abstract class TestApplication extends WithApplication(containerApp()) {
    val ac = app.injector.instanceOf[ApiController]
  }
  
  
  def endpointJson(lambdaId: UUID = uuid()) = Json.parse {
    s"""
      |{
      |"name":"endpoint-1",
      |"properties":{
      |  "resource": "/path",
      |  "upstream_url": "http://example.com/$lambdaId/invoke",
      |  "implementation_id": "$lambdaId"
      |}
    }""".trim.stripMargin
}
  
  
  "validateNewEndpoint" should {
    
    "inject api.providers.locations[0] into endpoint payload" in new TestApplication {
      val location = uuid()
      val provider = Json.obj(
          "id" -> uuid.toString,
          "locations" -> Json.arr(location))
          
      val props = Map("provider" -> Json.stringify(provider)) 
      val api = createApi(properties = Some(props))
      api must beSuccessfulTry
      
      
      println("ENDPOINT-BEFORE:")
      println(Json.prettyPrint(endpointJson()))
      val endpoint = ac.validateNewEndpoint(endpointJson(), api.get)
      endpoint must beSuccessfulTry
      println("\nENDPOINT-AFTER:")
      println(Json.prettyPrint(endpoint.get))
      
      val locid = Js.find(endpoint.get.as[JsObject], "/properties/location_id")
      locid must beSome
      
      UUID.fromString(locid.get.as[String]) === location
    }
    
    "fail if api.properties is none" in new TestApplication { 
      val api = createApi(properties = None)
      api must beSuccessfulTry
      
      ac.validateNewEndpoint(endpointJson(), api.get) must beFailedTry.withThrowable[UnprocessableEntityException]
    }
    
    "fail if api.properties.locations is missing" in new TestApplication {
      val location = uuid()
      val provider = Json.obj("id" -> uuid.toString)
          
      val props = Map("provider" -> Json.stringify(provider)) 
      val api = createApi(properties = Some(props))
      api must beSuccessfulTry

      ac.validateNewEndpoint(endpointJson(), api.get) must beFailedTry.withThrowable[UnprocessableEntityException]
    }
    
    "fail if api.properties.locations is empty" in new TestApplication {
      val location = uuid()
      val provider = Json.obj(
          "id" -> uuid.toString,
          "locations" -> Json.arr())
          
      val props = Map("provider" -> Json.stringify(provider)) 
      val api = createApi(properties = Some(props))
      api must beSuccessfulTry

      ac.validateNewEndpoint(endpointJson(), api.get) must beFailedTry.withThrowable[UnprocessableEntityException]
    }
  }
}