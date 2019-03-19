package com.galacticfog.gestalt.meta.genericactions

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.bootstrap._
//import com.galacticfog.gestalt.data.models.GestaltResourceInstance
//import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
//import com.galacticfog.gestalt.meta.genericactions.GenericProvider.RawInvocationResponse
//import com.galacticfog.gestalt.meta.genericactions._
import com.galacticfog.gestalt.meta.test.{MetaRepositoryOps, DbShutdown}
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig

//import controllers.SecurityResources
//import org.specs2.matcher.JsonMatchers
//import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification._
//import play.api.http.HeaderNames
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
//import play.api.libs.json.Json.toJsFieldJsValueWrapper
//import play.api.libs.json.Reads._
import play.api.libs.json._
//import play.api.libs.ws.WSClient
//import play.api.mvc.Action
//import play.api.mvc.BodyParsers.parse
//import play.api.mvc.Results.{Ok, Status, Unauthorized}
import play.api.test._//{DefaultAwaitTimeout, FutureAwaits, PlaySpecification}
//import mockws.{MockWS, Route}
import controllers.util._
import scala.util.{Success, Try}
import scala.concurrent.Future


class GenericProviderManagerSpec extends PlaySpecification 
    with GestaltSecurityMocking with MetaRepositoryOps with DbShutdown {
  
  abstract class TestScope extends Scope {
    val mockProviderMethods = mock[ProviderMethods]
    val mockGestaltSecurityConfig = mock[GestaltSecurityConfig]
    val appBuilder =
      new GuiceApplicationBuilder()
        .disable[modules.ProdSecurityModule]
        .disable[modules.MetaDefaultSkuber]
        .disable[modules.MetaDefaultServices]
        .disable[modules.HealthModule]
        .bindings(
          bind(classOf[GestaltSecurityConfig]).toInstance(mockGestaltSecurityConfig),
          bind(classOf[ProviderMethods]).toInstance(mockProviderMethods)
        )
    val injector = appBuilder.injector()
    setInjector(injector)

    val Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    Entitlements.setNewResourceEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

    val testUrl = "http://some-laser.some-domain/lambdas/b2d51c3d-aaaf-4575-b29d-4f0cb52d53fc/invokeSync"
    val Success(providerWithDefaultEndpoint) = createInstance(ResourceIds.Provider, "test-provider", properties = Some(Map(
      "config" -> Json.obj(
        "env" -> Json.obj(
          "public" -> Json.obj(
            "SERVICE_HOST" -> "some-laser.some-domain",
            "SERVICE_PORT" -> "9000"
          )
        ),
        "services" -> Seq(
          Json.obj(
            "container_spec" -> Json.obj(
              "name" -> "some-service",
              "properties" -> Json.obj(
                "image" -> "some:image"
              )
            )
          )
        ),

      "endpoints" -> Json.arr(
        Json.obj(
        "kind" -> "http",
        "url" -> testUrl,
        "actions" -> Json.arr(
            Json.obj(
                "name" -> "some-verb",
                "description" -> "Bars foos",
                "post" -> Json.obj(
                  "body" -> Json.obj(
                    "content_type" -> "application/json"
                  ),
                  "responses" -> Json.arr(
                    Json.obj(
                      "code" -> 201,
                      "content_type" -> "application/json"
                    )
                   )
                 )
               )
             )
           )
         ),
        "providerSpecificConfig" -> Json.obj(
          "password" -> "monkey",
          "url" -> "whatever"
        )
      ).toString
    )))
    
    val Some(rootOrg) = ResourceFactory.findById(ResourceIds.Org, dummyRootOrgId)
    
    val providerStringId = uuid()
    val providerStringName = uuid.toString
      
    SystemType(
        dummyRootOrgId, dummyOwner,
        typeId   = providerStringId,
        typeName = providerStringName,
        extend = Some(ResourceIds.Provider)
    ).withTypeProperties (
        TypeProperty("provider", "string", require = "optional")
    ).save()
      
    
    val providerJsonId = uuid()
    val providerJsonName = uuid.toString
      
    SystemType(
        dummyRootOrgId, dummyOwner,
        typeId   = providerJsonId,
        typeName = providerJsonName,
        extend = Some(ResourceIds.Provider)
    ).withTypeProperties (
        TypeProperty("provider", "string", require = "optional")
    ).save()          
    
    
    val gp = new GenericProvider {
      override def invokeAction(invocation: GenericActionInvocation): Future[GenericProvider.InvocationResponse] = ???
    }
    
    val testEndpointUrl = "http://example.com"
    def fakeBuildLambdaUrl(endpoint: GestaltEndpoint)(invocation: GenericActionInvocation): Try[String] = {
      Success(testUrl)
    }
  }
  
  sequential

  "GenericProvider" >> {

    "getProviderFromResource" should {
      
      "parse provider ID when properties.provider.data_type == String" in new TestScope {
        val pid = uuid.toString()
        val provider = createInstance(providerStringId, providerStringName,
          properties = Some(Map("provider" -> pid)))
        provider must beSuccessfulTry
        
        val result = gp.getProviderFromResource(provider.get)
        result must beSuccessfulTry
        
        result.get === UUID.fromString(pid)
      }
      
      "parse provider ID when properties.provider.data_type == JSON" in new TestScope {
        val pid = uuid.toString()
        val provider = createInstance(providerJsonId, providerJsonName,
          properties = Some(Map("provider" -> Json.stringify(Json.obj("id" -> pid)))))
        provider must beSuccessfulTry
        
        val result = gp.getProviderFromResource(provider.get)
        result must beSuccessfulTry
        
        result.get === UUID.fromString(pid)        
      }
      
      "fail when the given String is not a valid UUID" in new TestScope {
        val provider = createInstance(providerStringId, providerStringName,
          properties = Some(Map("provider" -> "_not_a_uuid_")))
        provider must beSuccessfulTry
        
        gp.getProviderFromResource(provider.get) must beFailedTry
      }
      
      "fail when the given JSON id is not a valid UUID" in new TestScope {
        val pid = uuid.toString()
        val provider = createInstance(providerJsonId, providerJsonName,
          properties = Some(Map("provider" -> Json.stringify(Json.obj("id" -> "-not-a-uuid-")))))
        provider must beSuccessfulTry
        
        gp.getProviderFromResource(provider.get) must beFailedTry
      }
    }
    
    "isUrlTemplate" should {
      "return TRUE when the given string is a valid URL template" in new TestScope {
        gp.isUrlTemplate("<lambda.address>") === true
      }
      
      "return FALSE when the given string is not a valid URL template" in new TestScope {
        gp.isUrlTemplate("http://example.com") === false
        gp.isUrlTemplate("<lambda.foo>") === false
      }
    }
     
    "findEndpointUrl" should {
      
      "use `endpoint.url` when it is a valid URL" in new TestScope {
        val raw = Json.parse(rawConfig)
        
        val typeId = uuid
        val typeName = uuid.toString
        
        SystemType(
            dummyRootOrgId, dummyOwner,
            typeId   = typeId,
            typeName = typeName,
            extend = Some(ResourceIds.Provider)
        ).save()      
        
        val config = Json.stringify((raw \ "config").get)
        val test = createInstance(typeId, uuid.toString, 
            properties = Some(Map("config" -> config)))
        
        test must beSuccessfulTry
        test.get.typeId === typeId
        
        val fc = getFunctionConfig(test.get)
        fc must beSuccessfulTry
        
        val invocation = newActionInvocation(action1)
        
        val result = gp.findEndpointUrl(fc.get, invocation)
        result must beSuccessfulTry.withValue(action1Url)
      }
      
      "call the `build` function when `endpoint.url` is a template and `implementation` is present" in new TestScope {
        val raw = Json.parse(rawConfig)
        
        val typeId = uuid
        val typeName = uuid.toString
        
        SystemType(
            dummyRootOrgId, dummyOwner,
            typeId   = typeId,
            typeName = typeName,
            extend = Some(ResourceIds.Provider)
        ).save()      
        
        val config = Json.stringify((raw \ "config").get)
        val test = createInstance(typeId, uuid.toString, 
            properties = Some(Map("config" -> config)))
        
        test must beSuccessfulTry
        test.get.typeId === typeId
        
        val fc = getFunctionConfig(test.get)
        fc must beSuccessfulTry
        
        val invocation = newActionInvocation(action1)
        
        val result = gp.findEndpointUrl(fc.get, invocation)
        result must beSuccessfulTry.withValue(testEndpointUrl)
      }
      
      "fail if `endpoint.url` is invalid and is NOT a template" in new TestScope {
        val raw = Json.parse(rawConfig)
        
        val typeId = uuid
        val typeName = uuid.toString
        
        SystemType(
            dummyRootOrgId, dummyOwner,
            typeId   = typeId,
            typeName = typeName,
            extend = Some(ResourceIds.Provider)
        ).save()      
        
        val config = Json.stringify((raw \ "config").get)
        val test = createInstance(typeId, uuid.toString, 
            properties = Some(Map("config" -> config)))
        
        test must beSuccessfulTry
        test.get.typeId === typeId
        
        val fc = getFunctionConfig(test.get)
        fc must beSuccessfulTry
        val invocation = newActionInvocation(invalidAction2)
        
        gp.findEndpointUrl(fc.get, invocation) must beFailedTry
      }
      
      "fail if `endpoint.url` is invaid, IS a template, but implementation is missing" in new TestScope { 
        val raw = Json.parse(rawConfig)
        
        val typeId = uuid
        val typeName = uuid.toString
        
        SystemType(
            dummyRootOrgId, dummyOwner,
            typeId   = typeId,
            typeName = typeName,
            extend = Some(ResourceIds.Provider)
        ).save()      
        
        val config = Json.stringify((raw \ "config").get)
        val test = createInstance(typeId, uuid.toString, 
            properties = Some(Map("config" -> config)))
        
        test must beSuccessfulTry
        test.get.typeId === typeId
        
        val fc = getFunctionConfig(test.get)
        fc must beSuccessfulTry
        
        val invocation = newActionInvocation(validAction3)
        
        gp.findEndpointUrl(fc.get, invocation) must beFailedTry
      }
    }
    
    "addressFromVariables" should {
      
      "parse an address when SERVICE_HOST and SERVICE_PORT are defined" in new TestScope {
        val host = "http://foo.example.com"
        val port = "8181"
        val propJson = s"""
          {
            "env": {
              "public": {
                "SERVICE_HOST" : "${host}",
                "SERVICE_PORT" : "${port}"
              }
            }
          }
          """        
        val props = Some(Map("config" -> Json.stringify(Json.parse(propJson))))
        val res = createInstance(ResourceIds.LambdaProvider, uuid.toString, properties = props)
        res must beSuccessfulTry
        
        val address = gp.addressFromVariables(res.get)
        address must beSuccessfulTry.withValue(s"http://${host}:${port}")
      }

      "parse an address when SERVICE_HOST_OVERRIDE and SERVICE_PORT_OVERRIDE are defined" in new TestScope {
        val host = "http://foo.example.com"
        val port = "8181"
        val propJson = s"""
          {
            "env": {
              "public": {
                "SERVICE_HOST_OVERRIDE" : "${host}",
                "SERVICE_PORT_OVERRIDE" : "${port}"
              }
            }
          }
          """
        val props = Some(Map("config" -> Json.stringify(Json.parse(propJson))))
        val res = createInstance(ResourceIds.LambdaProvider, uuid.toString, properties = props)
        res must beSuccessfulTry
        
        val address = gp.addressFromVariables(res.get)
        address must beSuccessfulTry.withValue(s"http://${host}:${port}")
      }
      
      "prefer values in *_OVERRIDE when present" in new TestScope {
        val host = "http://foo.example.com"
        val port = "8181"
        val hostOver = "https://bar.example.com"
        val propJson = s"""
          {
            "env": {
              "public": {
                "SERVICE_HOST" : "${host}",
                "SERVICE_PORT" : "${port}",
                "SERVICE_HOST_OVERRIDE" : "${hostOver}"
              }
            }
          }
          """
        val props = Some(Map("config" -> Json.stringify(Json.parse(propJson))))
        val res = createInstance(ResourceIds.LambdaProvider, uuid.toString, properties = props)
        res must beSuccessfulTry

        val address = gp.addressFromVariables(res.get)
        address must beSuccessfulTry.withValue(s"http://${hostOver}:${port}")
      }
      
      "fail if it can't find the variables it needs" in new TestScope {
        /*
         * There MUST be at least one of each SERVICE_HOST* and SERVICE_PORT* variables.
         */
        val propJson = s"""
          {
            "env": {
              "public": {
                "SERVICE_ALPHA" : "alpha",
                "SERVICE_BETA"  : "beta",
                "SERVICE_GAMMA" : "gamma"
              }
            }
          }
          """
        val props = Some(Map("config" -> Json.stringify(Json.parse(propJson))))
        val res = createInstance(ResourceIds.LambdaProvider, uuid.toString, properties = props)
        res must beSuccessfulTry
        
        gp.addressFromVariables(res.get) must beFailedTry        
      }
    }
    
    "buildLambdaUrl" should {
      
      def createLambdaProvider(parentId: UUID, protocol: String, host: String, port: Int): Try[GestaltResourceInstance] = {
        val propJson = s"""
          {
            "env": {
              "public": {
                "SERVICE_HOST" : "${host}",
                "SERVICE_PORT_OVERRIDE" : "${port}",
                "SERVICE_PROTOCOL_OVERRIDE" : "${protocol}"
              }
            }
          }
          """
        val props = Some(Map("config" -> Json.stringify(Json.parse(propJson))))
        createInstance(ResourceIds.LambdaProvider, uuid.toString, 
            properties = props,
            parent = Some(parentId))        
      }
      
      "build a lambda URL when lambda and provider exist" in new TestScope {
        val (_, eid) = createWorkspaceEnvironment()
        val protocol = "https"
        val host = "foo.example.com"
        val port = "8686"

        // Create lambda provider
        val provider = createLambdaProvider(eid, protocol, host, port.toInt)
        provider must beSuccessfulTry
        
        // Create lambda
        val lambda = newDummyLambda(eid, provider.get.id)
        lambda must beSuccessfulTry
        
        
        // Create fake provider that uses lambda
        val epId = "<lambda.address>/invokeSync"
        val impId = lambda.get.id
        val action = "foo.bar"
        val configJson = Json.parse(fakeImplementationConfig(epId, impId, action))

        val testProps = Some(Map(
            "config" -> Json.stringify((configJson \ "config").get)
        ))
        val test = createInstance(providerStringId, uuid.toString,
            properties = testProps,
            parent = Some(eid))
        test must beSuccessfulTry
        
        // Get Implementation from Config
        val fc = getFunctionConfig(test.get)
        fc must beSuccessfulTry
        
        val ep = fc.get.getEndpointByActionName(action)
        ep must beSome
                
        val invocation = newActionInvocation(action)
        
        val url = gp.buildLambdaUrl(ep.get)(invocation)
        url must beSuccessfulTry.withValue(s"${protocol}://${host}:${port}")        
      }
      
      "fail if the implementing lambda does not exist" in new TestScope {
        val (_, eid) = createWorkspaceEnvironment()
        val protocol = "https"
        val host = "foo.example.com"
        val port = "8686"

        // Create lambda provider
        val provider = createLambdaProvider(eid, protocol, host, port.toInt)
        provider must beSuccessfulTry

        // Create fake provider that uses lambda
        val epId = "<lambda.address>/invokeSync"
        val impId = uuid() //lambda.get.id
        val action = "foo.bar"
        val configJson = Json.parse(fakeImplementationConfig(epId, impId, action))

        val testProps = Some(Map(
            "config" -> Json.stringify((configJson \ "config").get)
        ))
        val test = createInstance(providerStringId, uuid.toString,
            properties = testProps,
            parent = Some(eid))
        test must beSuccessfulTry
        
        // Get Implementation from Config
        val fc = getFunctionConfig(test.get)
        fc must beSuccessfulTry
        
        val ep = fc.get.getEndpointByActionName(action)
        ep must beSome
        
        val invocation = newActionInvocation(action)

        // Test URL
        gp.buildLambdaUrl(ep.get)(invocation) must beFailedTry
      }
      
      "fail if the lambda provider does not have necessary environment vars" in new TestScope {
        failure
      }.pendingUntilFixed("Implement this")
      
    }
    
    "buildProviderUrl" should {
      
def createLambdaProvider(parentId: UUID, protocol: String, host: String, port: Int): Try[GestaltResourceInstance] = {
        val propJson = s"""
          {
            "env": {
              "public": {
                "SERVICE_HOST" : "${host}",
                "SERVICE_PORT_OVERRIDE" : "${port}",
                "SERVICE_PROTOCOL_OVERRIDE" : "${protocol}"
              }
            }
          }
          """
        val props = Some(Map("config" -> Json.stringify(Json.parse(propJson))))
        createInstance(ResourceIds.LambdaProvider, uuid.toString, 
            properties = props,
            parent = Some(parentId))        
      }
      
      "build a lambda URL when lambda and provider exist" in new TestScope {
        val (_, eid) = createWorkspaceEnvironment()
        val protocol = "https"
        val host = "foo.example.com"
        val port = "8686"

        // Create lambda provider
        val provider = createLambdaProvider(eid, protocol, host, port.toInt)
        provider must beSuccessfulTry
        
        // Create fake provider that uses lambda
        val epId = "<provider.address>/invokeSync"
        val action = "foo.bar"
        val configJson = Json.parse(fakeNonImplConfig(epId, action))

        val testProps = Some(Map(
            "config" -> Json.stringify((configJson \ "config").get)
        ))
        val testProvider = createInstance(providerStringId, uuid.toString,
            properties = testProps,
            parent = Some(eid))
        testProvider must beSuccessfulTry
        
        // Get Implementation from Config
        val fc = getFunctionConfig(testProvider.get)
        fc must beSuccessfulTry
        
        val ep = fc.get.getEndpointByActionName(action)
        ep must beSome
                
        val invocation = newActionInvocation(action, provider = provider.get)
        
        val url = gp.buildProviderUrl(ep.get)(invocation)
        url must beSuccessfulTry.withValue(s"${protocol}://${host}:${port}")        
      }
    }
    
    
    "makeEndpointUrl" should {
      
      "return a URL with template token replaced with lambda-address" in new TestScope {

        val lambdaAddress = "http://lambda.example.com"
        val tail1 = "/invokeSync"
        val tail2 = "/functions/create"
        val turl1 = "<lambda.address>" + tail1
        val turl2 = "<lambda.address>" + tail2
        
        gp.makeEndpointUrl(turl1, lambdaAddress) must beSuccessfulTry.withValue(lambdaAddress + tail1)
        gp.makeEndpointUrl(turl2, lambdaAddress) must beSuccessfulTry.withValue(lambdaAddress + tail2)
      }
      
    }
//    "appropriately instantiate HttpGenericProvider classes for configured provider" in new TestScope {
//      val providerManager = new DefaultGenericProviderManager(mock[WSClient])
//      providerManager.getProvider(
//          providerWithDefaultEndpoint, 
//          "some-verb", 
//          callerAuth = "fakeCreds") must beASuccessfulTry(beSome(
//        beAnInstanceOf[HttpGenericProvider]
//          and (((_:GenericProvider).asInstanceOf[HttpGenericProvider].url) ^^ be_==(testUrl))
//      ))
//    }

  }
  
  
  lazy val action1 = "foo.bar"
  lazy val action1Url = "http://example.com"
  lazy val implementation1 = uuid.toString
  
  lazy val invalidAction2 = "bar.baz"
  lazy val action2Url = "_not_url_/invokeSync"
  lazy val implementation2 = uuid.toString
  
  lazy val validAction3 = "baz.qux"
  lazy val action3Url = "<lambda.address>/invokeSync"
  lazy val implementation3 = uuid.toString
  
  lazy val validAction4 = "qux.waldo"
  lazy val action4Url = "<lambda.address>/invokeSync"
  lazy val implementation4 = uuid.toString
  
  
  def fakeNonImplConfig(endpointUrl: String, actionName: String) = s"""
   {
    "config": {
      "endpoints": [
        {
          "kind": "http",
          "url": "${endpointUrl}",
          "actions": [
            {
              "name": "${actionName}",
              "description": "The '${actionName}' action!",
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
        }
      ]
    }
  }    
    """
  def fakeImplementationConfig(
                                endpointUrl: String, 
                                implementationId: UUID,
                                actionName: String) = s"""
   {
    "config": {
      "endpoints": [
        {
          "kind": "http",
          "url": "${endpointUrl}",
          "implementation": {
            "kind": "lambda",
            "id": "${implementationId}"
          },
          "actions": [
            {
              "name": "${actionName}",
              "description": "The '${actionName}' action!",
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
        }
      ]
    }
  }
  """
  
  
  lazy val rawConfig = s"""
  {
    "config": {
      "gitAddress": "http://...",
      "endpoints": [
        {
          "kind": "http",
          "url": "${action1Url}",
          "authentication": "Basic Zm9vYmFyMTp3aG9jYW5pdGJlbm93",
          "implementation": {
            "kind": "lambda",
            "id": "${implementation1}"
          },
          "actions": [
            {
              "name": "${action1}",
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
          "url": "${action2Url}",
          "actions": [
            {
              "name": "${invalidAction2}",
              "description": "Baz's a bar!.",
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
          "url": "${action3Url}",
          "actions": [
            {
              "name": "${validAction3}",
              "description": "Baz's a bar!.",
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
          "url": "${action4Url}",
          "implementation": {
            "kind": "lambda",
            "id": "${implementation4}"
          },          
          "actions": [
            {
              "name": "${validAction4}",
              "description": "Baz's a bar!.",
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
                    "value": ""
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
  
}