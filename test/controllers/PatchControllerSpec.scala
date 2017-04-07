package controllers


import controllers.util.GestaltProviderMocking
import org.specs2.mock.Mockito
import org.specs2.specification._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.data._
import play.api.test._
import org.specs2.matcher.JsonMatchers
import com.galacticfog.gestalt.patch._
import com.galacticfog.gestalt.meta.api.ResourcePath

class PatchControllerSpec extends PlaySpecification with GestaltProviderMocking with JsonMatchers with ResourceScope with BeforeAll with Mockito {

  sequential
  
  override def beforeAll(): Unit = pristineDatabase
  
  
  abstract class TestApplication extends WithApplication(containerApp()) {
  }
  
  "Patch" should {

    "Environment Variables" should {

      "add a new env object when it does not exist" in new TestApplication {
        
        val orgName = "patchtest1"
        val org = newOrg(id = dummyRootOrgId, name = orgName)
        org must beSuccessfulTry
        

        // Create new environment and ensure /properties/env is None
        //
        val data = newDummyEnvironment(org = org.get.id)
        val envRes = ResourceFactory.findById(data("environment")).get
        envRes.properties.get.get("env") must beNone

        // Create PATCH JSON
        //
        val var1 = "DATABASE" -> "gestalt-test"
        val var2 = "USERNAME" -> "user1"
        val var3 = "PASSWORD" -> "secret"

        val patchJs = Json.toJson(Seq(
            PatchOp.Add("/properties/env", Json.obj(
                var1._1 -> var1._2, 
                var2._1 -> var2._2, 
                var3._1 -> var3._2 )))
        )
        
        // Create dummy user account
        //
        val account = dummyAuthAccountWithCreds(userInfo = Map("id" -> adminUserId))

        val path = new ResourcePath(s"/${orgName}/environments/${envRes.id}")

        val rc = app.injector.instanceOf[ResourceController]
        val pc = app.injector.instanceOf[PatchController]
        
        val updated = pc.Patch(envRes, patchJs, account)
        updated must beSuccessfulTry
        
        val envJs = Output.renderInstance(updated.get)

        envJs.toString must /("properties") /("env") /(var1._1 -> var1._2)
        
      }

      
//      "add a new key to the map when env already exists" in {
//
//        failure
//      }
//
//      "replace the entire env object with a new value" in {
//        failure
//      }
//
//      "replace the value of a single key" in {
//        failure
//      }
//
//      "remove a single key from the env object" in {
//        failure
//      }
//
//      "remove the entire env object" in {
//        failure
//      }
    }
  }
  

  
//  "toPatch" should {
//    
//    "return a PatchDocument from valid patch JSON" in {
//      failure
//    }
//    
//    "fail with a BadRequestException if the JSON is invalid" in {
//      failure
//    }
//    
//  }
  
}

