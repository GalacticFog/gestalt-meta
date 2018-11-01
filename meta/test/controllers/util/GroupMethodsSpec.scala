package controllers.util

import scala.Left

import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAll

import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.patch.PatchOp
import com.galacticfog.gestalt.patch.PatchOps
import com.galacticfog.gestalt.security.api.GestaltSecurityClient
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import com.galacticfog.gestalt.security.play.silhouette.fakes.FakeGestaltFrameworkSecurityEnvironment

import play.api.libs.json.JsString

class GroupMethodsSpec extends Specification with ResourceScope with GestaltSecurityMocking with BeforeAll {

  override def beforeAll(): Unit = pristineDatabase

  lazy val creds = dummyCreds()   
  lazy val authResponse = dummyAuthResponse()   
  lazy val mockSecurityClient = mock[GestaltSecurityClient]   
  

  lazy val fakeSecurity = FakeGestaltFrameworkSecurityEnvironment(
      identities = Seq(testCreds -> testAuthResponse),
      securityConfig = mock[GestaltSecurityConfig],
      securityClient = mock[GestaltSecurityClient]
  )

  val gm = new GroupMethods(mock[Security])
  
  "opsToMap" should {
    
    "convert a Seq[PatchOp] to a Map[String,Seq[PatchOp]] with ops keyed by op-name" in {
      
      val ops = Seq(
          PatchOp.Add("/foo", JsString("")),
          PatchOp.Add("/foo2", JsString("")),
          PatchOp.Add("/foo3", JsString("")),
          PatchOp.Replace("/bar", JsString("")),
          PatchOp.Replace("/bar2", JsString("")),
          PatchOp.Remove("/baz") )
      
      val map = gm.opsToMap(ops)
      
      map(PatchOps.Add).size === 3
      map(PatchOps.Replace).size === 2
      map(PatchOps.Remove).size === 1
    }
    
    "succeed if an allowed-list is given and the input seq contains only those ops" in {
      val ops = Seq(
          PatchOp.Add("/foo", JsString("")),
          PatchOp.Remove("/bar") )
      
      val map = gm.opsToMap(ops, allowed = Seq(PatchOps.Add,PatchOps.Remove))
      
      map(PatchOps.Add).size === 1
      map(PatchOps.Remove).size === 1
    }
    
    "fail if an allowed-list is given and the input seq contains an outlier" in {
      val ops = Seq(
          PatchOp.Add("/foo", JsString("")),
          PatchOp.Replace("/qux", JsString("")),
          PatchOp.Remove("/bar") )
          
      val allowed = Seq(PatchOps.Add,PatchOps.Remove)
      gm.opsToMap(ops, allowed) must throwA[BadRequestException]
    }
  }
  
  
  
  "getValidatedUsers" should {
    
    "return a list of valid user resources" in {

      val org = newOrg(id = dummyRootOrgId)
      org must beSuccessfulTry      
      
      val user1 = createNewUser(org.get.id).id
      val user2 = createNewUser(org.get.id).id
      val user3 = createNewUser(org.get.id).id
      
      val validated = gm.getValidatedUsers(Seq(user1, user2, user3))
      
      validated must beRight
      validated.fold( l => false, r => 
        r.map(_.id).diff(Seq(user1, user2, user3)).isEmpty ) must beTrue
    }
    
    
    "fail with a list of user ids that were not found" in {
      val org = newOrg()
      org must beSuccessfulTry      
      
      val invaliduser = uuid() 
      
      val users = Seq(
        createNewUser(org.get.id).id,
        createNewUser(org.get.id).id,
        invaliduser)
      
      val validated = gm.getValidatedUsers(users)
      validated must beLeft
      
      validated === Left(Seq(invaliduser))
    }
    
  }  
  
}