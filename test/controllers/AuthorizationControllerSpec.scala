//package controllers
//
//import java.util.UUID
//
//import scala.concurrent.Future
//import scala.util.Success
//import scala.util.Try
//
//import org.joda.time.DateTime
//import org.mockito.Matchers.{ eq => meq }
//import org.specs2.matcher.JsonMatchers
//import org.specs2.matcher.ValueCheck.typedValueCheck
//import org.specs2.specification.BeforeAll
//
//import com.galacticfog.gestalt.data.ResourceFactory
//import com.galacticfog.gestalt.data.models.GestaltResourceInstance
//import com.galacticfog.gestalt.marathon.MarathonClient
//import com.galacticfog.gestalt.meta.api.ContainerSpec
//import com.galacticfog.gestalt.meta.api.output.Output
//import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
//import com.galacticfog.gestalt.meta.test.ResourceScope
//import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
//
//import controllers.util.GestaltSecurityMocking
//import javax.inject.Singleton
//import play.api.libs.concurrent.Execution.Implicits.defaultContext
//import play.api.libs.json.JsValue.jsValueToJsLookup
//import play.api.libs.json.Json
//import play.api.libs.json.Json.toJsFieldJsValueWrapper
//import play.api.test.PlaySpecification
//import play.api.test.WithApplication
//
//class AuthorizationControllerSpec extends GestaltSecurityMocking with ResourceScope with BeforeAll {
//  
//  override def beforeAll(): Unit = pristineDatabase()
//
//  sequential
//
//  abstract class FakeSecurity extends WithApplication(containerApp()) {
//  }
//  
//  import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
//  
//  object Ents extends AuthorizationMethods
//  
//  trait TestApplication extends FakeSecurity {
//    
//    val controller = app.injector.instanceOf[AuthorizationController]
//    
//    var Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
//
//    Ents.setNewEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))
//    
//    
//    val user2 = createMetaUser(account2)
//    
//    var testProvider = createMarathonProvider(testEnv.id, "test-provider").get
//    var mockMarathonClient = mock[MarathonClient]
//    
//    containerService.findWorkspaceEnvironment(testEnv.id) returns Try((testWork, testEnv))
//    containerService.marathonProvider(testProvider.id) returns testProvider
//    containerService.marathonClient(testProvider) returns mockMarathonClient 
//  }
//  
//  
//  "validateEntitlementPayload" should {
//    
//    "succeed when given valid data" >> {
//      failure
//    }
//    
//    "fail if caller DOES NOT have set-entitlements permission" >> {
//      failure
//    }
//    
//    "fail if the given action is invalid for the resource" >> {
//      failure
//    }
//    
//    "fail if there is already an entitlement for the specified action" >> {
//      failure
//    }
//    
//    "fail if any of the given identities DOES NOT exist" >> {
//      failure
//    }
//    
//  }
//  
//  
//}