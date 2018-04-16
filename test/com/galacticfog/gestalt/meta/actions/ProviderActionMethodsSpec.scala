package com.galacticfog.gestalt.meta.actions

import java.util.UUID

import com.galacticfog.gestalt.data.{ResourceFactory, ResourceState}

import scala.concurrent.Future
import scala.util.Success
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.{JsonMatchers, Matcher}
import org.specs2.matcher.ValueCheck.typedValueCheck
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.meta.api.{ContainerSpec, SecretSpec, sdk}
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceStates}
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import controllers.util.ContainerService
import org.specs2.execute.{AsResult, Result}
import play.api.inject.guice.GuiceableModule
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.{JsObject, JsString, Json}
import play.api.test.{PlaySpecification, WithApplication}
import services.{CaasService, MarathonClientFactory, ProviderContext, SkuberFactory}
import play.api.inject.bind
import controllers.util.LambdaMethods
import com.galacticfog.gestalt.meta.providers._
import com.galacticfog.gestalt.meta.api.errors.BadRequestException

class ProviderActionMethodsSpec extends PlaySpecification with MetaRepositoryOps {
  
  sequential
  
  abstract class FakeSecurity extends WithDb(containerApp(
    additionalBindings = Seq(
      bind(classOf[LambdaMethods]).toInstance(mockLambdaMethods)
    )
  ))
  
  val methods = injector.instanceOf[ProviderActionMethods]
  
  
  "resolveActionImplementation" should {

    "with implementation.id" should {
      
      "fail if .kind is not a valid type-name" in new FakeSecurity {
        val impl = ActionImplSpec(
            kind = "INVALID::TYPE", 
            spec = Some(Json.obj()), 
            id = None, 
            input = None)
            
        val actionSpec = ProviderActionSpec(
            name = uuid.toString, 
            endpoint_url = Some("http://example.com/example"), 
            implementation = impl, 
            ui_locations = Seq.empty)
        val (_, eid) = createWorkspaceEnvironment()
        
        val env = ResourceFactory.findById(ResourceIds.Environment, eid)
        env must beSome
        
        val account = dummyAuthAccountWithCreds()
        
        methods.resolveActionImplementation(dummyRootOrgId, actionSpec, env.get, account) must throwA[BadRequestException].like {
          case bre: BadRequestException => bre.getMessage must contain("Invalid implementation.kind") 
        }
      }
      
      "fail if NEITHER .id nor .spec are given" in new FakeSecurity {
        val impl = ActionImplSpec(
            kind = "Gestalt::Resource::Workspace",
            spec = None,
            id = None,
            input = None)
            
        val actionSpec = ProviderActionSpec(
            name = uuid.toString,
            endpoint_url = Some("http://example.com/example"),
            implementation = impl,
            ui_locations = Seq.empty)
            
        val (_, eid) = createWorkspaceEnvironment()        
        val env = ResourceFactory.findById(ResourceIds.Environment, eid)
        env must beSome
        
        val account = dummyAuthAccountWithCreds()
        
        methods.resolveActionImplementation(dummyRootOrgId, actionSpec, env.get, account) must throwA[BadRequestException].like {
          case bre: BadRequestException => bre.getMessage must contain("You must provide either") 
        }        
      }
      
      "fail if BOTH .id and .spec are given" in new FakeSecurity {
        val impl = ActionImplSpec(
            kind = "Gestalt::Resource::Workspace",
            spec = Some(Json.obj()),
            id = Some(uuid.toString),
            input = None)
            
        val actionSpec = ProviderActionSpec(
            name = uuid.toString,
            endpoint_url = Some("http://example.com/example"),
            implementation = impl,
            ui_locations = Seq.empty)
            
        val (_, eid) = createWorkspaceEnvironment()        
        val env = ResourceFactory.findById(ResourceIds.Environment, eid)
        env must beSome
        
        val account = dummyAuthAccountWithCreds()
        
        methods.resolveActionImplementation(dummyRootOrgId, actionSpec, env.get, account) must throwA[BadRequestException].like {
          case bre: BadRequestException => bre.getMessage must contain("You must provide only one of") 
        }        
      }
      
      "fail if implementation.id is not a valid UUID" in new FakeSecurity {
        val impl = ActionImplSpec(
            kind = "Gestalt::Resource::Workspace",
            spec = None,
            id = Some("INVALID-UUID"),
            input = None)
            
        val actionSpec = ProviderActionSpec(
            name = uuid.toString,
            endpoint_url = Some("http://example.com/example"),
            implementation = impl,
            ui_locations = Seq.empty)
            
        val (_, eid) = createWorkspaceEnvironment()        
        val env = ResourceFactory.findById(ResourceIds.Environment, eid)
        env must beSome
        
        val account = dummyAuthAccountWithCreds()
        
        methods.resolveActionImplementation(dummyRootOrgId, actionSpec, env.get, account) must throwA[BadRequestException].like {
          case bre: BadRequestException => bre.getMessage must contain("Not a valid UUID") 
        }
      }
      
      "fail if the given implementation.id resource does not exist" in new FakeSecurity {
        val impl = ActionImplSpec(
            kind = "Gestalt::Resource::Workspace",
            spec = None,
            id = Some(uuid.toString),
            input = None)
            
        val actionSpec = ProviderActionSpec(
            name = uuid.toString,
            endpoint_url = Some("http://example.com/example"),
            implementation = impl,
            ui_locations = Seq.empty)
            
        val (_, eid) = createWorkspaceEnvironment()        
        val env = ResourceFactory.findById(ResourceIds.Environment, eid)
        env must beSome
        
        val account = dummyAuthAccountWithCreds()
        
        methods.resolveActionImplementation(dummyRootOrgId, actionSpec, env.get, account) must throwA[BadRequestException].like {
          case bre: BadRequestException => bre.getMessage must contain("not found.") 
        }
      }
    }
    
//    "with implementation.spec" should {
//      "succeed when given a valid lambda spec" in new FakeSecurity {
//        failure
//      }
//    }
  }
}