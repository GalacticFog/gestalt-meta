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

import play.api.test._
import play.api.test.Helpers._
import com.galacticfog.gestalt.data.EnvironmentType


class GestaltFunctionConfigSpec extends Specification {
  /*
{
  "endpoints": [
    {
      "kind": "http",
      "url": "https://lambdas.galacticfg.com/myapi/invoke",
      "actions": [
        {
          "name": "widget.view",
          "get": {
            "query_parameters": [
              {
                "name": "accessToken",
                "description": "The access token provided by the authentication application",
                "example": "abcde12345",
                "required": true,
                "data_type": "string"
              }
            ],
            "responses": [
              {
                "code": 200,
                "content-type": "text/json"
              }
            ]
          }
        },
        {
          "name": "widget.login",
          "description": "Provides access to your widgets!",
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
                    "svg": "PHN2ZyB3aWR0aD0yMDAgaGVpZ2h0PTIwMCAKICAgICB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIAogICAgIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIj4gICAgICAgCiAgPGltYWdlIHhsaW5rOmhyZWY9Imh0dHBzOi8vbWRuLm1vemlsbGFkZW1vcy5vcmcvZmlsZXMvNjQ1Ny9tZG5fbG9nb19vbmx5X2NvbG9yLnBuZyIgaGVpZ2h0PSIyMDAiIHdpZHRoPSIyMDAiLz4gICAgCjwvc3ZnPg=="
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
   */
  
  
  "GestaltFunction" should {
    
    "fail construction if zero FunctionVerbs are given" >> {
      failure
    }
    
    "fail construction if more than one FunctionVerb is given" >> {
      failure
    }
    
  }
  
  
  
}