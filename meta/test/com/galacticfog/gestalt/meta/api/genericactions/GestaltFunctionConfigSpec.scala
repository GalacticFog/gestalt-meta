package com.galacticfog.gestalt.meta.genericactions

import org.specs2.mutable._
import play.api.libs.json._

import com.galacticfog.gestalt.json.Js
import scala.util.Try

class GestaltFunctionConfigSpec extends Specification {

  val rawConfig = """
  {
    "config": {
      "gitAddress": "http://...",
      "endpoints": [
        {
          "kind": "http",
          "url": "http://example.com",
          "authentication": "Basic Zm9vYmFyMTp3aG9jYW5pdGJlbm93",
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
                "query_parameters": [
                  {
                    "name": "resource_id",
                    "value": "${RESOURCE_ID}"
                  }
                ],    
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
  
  val rawAction = s"""
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
    }    
  """
  
  val rawUiAction = s"""
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
    """
  
  
  "GestaltFunctionConfig" should {
    
    "parse a properly formatted array of provider endpoints" >> {
      val js = (Json.parse(rawConfig) \ "config").as[JsValue]
      
      Js.parse[GestaltFunctionConfig](js)(
        formatGestaltFunctionConfig) must beSuccessfulTry
    }
  }  
  
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
    
    "hasUi()" should {
      
      "return TRUE when there is at least one response with a gestalt_ui object" >> {
        val uiAction = Js.parse[GestaltFunction](Json.parse(rawUiAction).as[JsObject])(formatGestaltFunction)
        uiAction must beSuccessfulTry
        uiAction.get.hasUi() === true
      }
      
      "return FALSE if there are no responses with gestalt_ui objects" >> {
        val nonUiAction = Js.parse[GestaltFunction](Json.parse(rawAction).as[JsObject])(formatGestaltFunction)
        nonUiAction must beSuccessfulTry
        nonUiAction.get.hasUi() === false
      }
      
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
    
    "throw an exception if there are actions with duplicate names" >> {
      val get = Some(
        GetVerb(Some("get a foo"), 
          responses = Seq(FunctionResponse(200, Some("application/json"), None)))
      )
      val fn1 = GestaltFunction("duplicate.name", get = get)
      val fn2 = GestaltFunction("duplicate.name", get = get)
      Try(GestaltEndpoint("http", "http://example.com", actions = Seq(fn1, fn2))) must beFailedTry         
    }    
  }
  
  
  
//  "getFunctionConfig" should {
//    
//    "extract a GestaltFunctionConfig from a Provider instance if present" >> {
//      
//      failure
//    }
//    
//    "return nothing if there is no function config present" >> {
//      
//      failure
//    }
//  }
  
  
  
}