package com.galacticfog.gestalt.meta.actions


import com.galacticfog.gestalt.data.ResourceFactory

import org.mockito.Matchers.{eq => _}
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test._
import play.api.libs.json.Json
import play.api.test.PlaySpecification
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
  )) {
    lazy val methods = app.injector.instanceOf[ProviderActionMethods]
  }

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