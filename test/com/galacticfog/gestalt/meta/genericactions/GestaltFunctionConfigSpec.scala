package com.galacticfog.gestalt.meta.genericactions

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Step
import play.api.libs.json._

import java.util.UUID

import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.json.Js

import play.api.test._
import play.api.test.Helpers._
import com.galacticfog.gestalt.data.EnvironmentType
import scala.util.{Try, Success, Failure}

class GestaltFunctionConfigSpec extends Specification {

  val rawConfig = """
  {
    "config": {
      "gitAddress": "http://...",
      "endpoints": [
        {
          "kind": "http",
          "url": "http://example.com",
          "actions": [
            {
              "name": "foo.bar",
              "description": "Bars a foo (like nobody's business).",
              "post": {
                "responses": [
                  {
                    "code": 201,
                    "content_type": "application/json"
                  }
                ]
              }
            }
          ]
        },
        {
          "kind": "http",
          "url": "http://localhost:8080",
          "actions": [
            {
              "name": "streamspec.create",
              "description": "Create a new StreamSpec.",
              "post": {
                "body": {
                  "content_type": "application/json"
                },
                "responses": [
                  {
                    "code": 200,
                    "content_type": "application/json"
                  }
                ]
              }
            },
            {
              "name": "streamspec.update",
              "description": "Update an existing StreamSpec",
              "post": {
                "body": {
                  "content_type": "application/json"
                },
                "responses": [
                  {
                    "code": 200,
                    "content_type": "application/json"
                  }
                ]
              }
            },
            {
              "name": "streamspec.delete",
              "description": "Delete a StreamSpec. Destroys all running instances",
              "post": {
                "body": {
                  "content_type": "application/json"
                },
                "responses": [
                  {
                    "code": 201,
                    "content_type": "application/json"
                  }
                ]
              }
            },
            {
              "name": "streamspec.start",
              "description": "Start a new instance of a StreamSpec.",
              "post": {
                "body": {
                  "content_type": "application/json"
                },
                "responses": [
                  {
                    "code": 200,
                    "content_type": "application/json"
                  },
                  {
                    "code": 409,
                    "content_type": "application/json",
                    "description": "Conflict"
                  }
                ]
              }
            },
            {
              "name": "streamspec.stop",
              "description": "Stop a Stream Instance",
              "post": {
                "body": {
                  "content_type": "application/json"
                },
                "responses": [
                  {
                    "code": 200,
                    "content_type": "application/json"
                  }
                ]
              }
            },
            {
              "name": "streamspec.restart",
              "display_name": "Restart Stream",
              "description": "Stop then restart a running Stream",
              "post": {
                "body": {
                  "content_type": "application/json"
                },
                "responses": [
                  {
                    "code": 200,
                    "content_type": "text/html",
                    "gestalt_ui": {
                      "render": "inline",
                      "locations": [
                        "environment.list",
                        "environment.detail"
                      ],
                      "icon": {
                        "svg": "...optional embedded svg..."
                      }
                    }
                  }
                ]
              }
            }
          ]
        }
      ]
    }
  }
  """
  
  "GestaltFunction" should {
    
    "fail construction if zero FunctionVerbs are given" >> {
      GestaltFunction("foo") must throwA[RuntimeException]
    }
    
    "fail construction if more than one FunctionVerb is given" >> {
      val get = Some(
          GetVerb(Some("get a foo"), 
            responses = Seq(FunctionResponse(200, Some("application/json"), None)))
      )
      val post = Some(
        PostVerb(Some("create a foo"), 
          responses = Seq(FunctionResponse(201, Some("application/json"), None)))
      ) 
      GestaltFunction("foo", get = get, post = post) must throwA[RuntimeException]
    }
    
    "succeed with a single verb" >> {
      val get = Some(
        GetVerb(Some("get a foo"), 
          responses = Seq(FunctionResponse(200, Some("application/json"), None)))
      )     
      Try(GestaltFunction("foo", get = get)) must beSuccessfulTry      
    }   
  }

  "GestaltEndpoint" should {
    
    "throw an exception if there are zero actions specified" >> {
      GestaltEndpoint("http", "http://example.com", actions = Seq.empty) must throwA[RuntimeException]
    }
    
    "succeed if there is at least one action present" >> {
      val get = Some(
        GetVerb(Some("get a foo"), 
          responses = Seq(FunctionResponse(200, Some("application/json"), None)))
      )     
      val func = GestaltFunction("foo", get = get)
      Try(GestaltEndpoint("http", "http://example.com", actions = Seq(func))) must beSuccessfulTry      
    }
  }

  "GestaltFunctionConfig" should {
    
    "parse a properly formatted array of provider endpoints" >> {
      val js = (Json.parse(rawConfig) \ "config").as[JsValue]
      
      Js.parse[GestaltFunctionConfig](js)(
        formatGestaltFunctionConfig) must beSuccessfulTry
    }
  }
    
}