package controllers.util

import java.util.UUID
import com.galacticfog.gestalt.data.bootstrap.Bootstrap
import controllers.util.db.ConnectionManager
import org.specs2.mutable._
import org.specs2.specification._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import play.api.test._
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import org.specs2.matcher.JsonMatchers

import com.galacticfog.gestalt.patch._
import com.galacticfog.gestalt.meta.api.ResourcePath
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.api.{GestaltSecurityClient,GestaltSecurityConfig}

import com.galacticfog.gestalt.security.play.silhouette.fakes.FakeGestaltFrameworkSecurityEnvironment

import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator

import org.specs2.specification._
import play.api.libs.json.{JsArray, Json}
import play.api.test._
import play.api.{Application, GlobalSettings, Play}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.{ExecutionContext, Future}


  import com.galacticfog.gestalt.security.play.silhouette._
  import com.galacticfog.gestalt.security.api._
import com.galacticfog.gestalt.security.api.{ResourceLink => SecurityLink}  

class GroupMethodsSpec extends Specification with ResourceScope with GestaltSecurityMocking with BeforeAll {

  override def beforeAll(): Unit = pristineDatabase

  lazy val creds = dummyCreds()   
  lazy val authResponse = dummyAuthResponse()   
  lazy val mockSecurityClient = mock[GestaltSecurityClient]   
  

  lazy val fakeSecurity = FakeGestaltFrameworkSecurityEnvironment[DummyAuthenticator](
      identities = Seq(testCreds -> testAuthResponse),
      config = mock[GestaltSecurityConfig],
      client = mock[GestaltSecurityClient])

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