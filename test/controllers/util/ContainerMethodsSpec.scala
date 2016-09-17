package controllers.util


import java.util.UUID
import com.galacticfog.gestalt.data.bootstrap.Bootstrap
import controllers.util.db.ConnectionManager
import org.specs2.mutable._
import org.specs2.specification._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import play.api.test._
import scala.util.{Try,Success,Failure}

class ContainerMethodsSpec extends PlaySpecification with ResourceScope with BeforeAll {

  // providerQueryParam(qs: Map[String,Seq[String]])
  
  sequential
  
  override def beforeAll() = pristineDatabase()
  
  "providerQueryParam" should {
    
    
    def setupenv(org: UUID) = {
      val (wrk,env) = createWorkspaceEnvironment(org)
      val provider = createMarathonProvider(env)
    
      (wrk,env,provider.get.id)
    }
    
    "SUCCEED if there is a single 'provider' param that is a valid UUID to a valid Marathon Provider" in new WithApplication {

      val org = newOrg(id = dummyRootOrgId).get
      val (wrk,env,prv) = setupenv(org.id)
      val good = Map("provider" -> Seq(prv.toString))
      
      val param = ContainerMethods.providerQueryParam(good)
      param must beSuccessfulTry
      param.get === prv
    }
    
    "FAIL if provider parameter is missing" in {
      val org = newOrg(id = dummyRootOrgId).get
      val (wrk,env,prv) = setupenv(org.id)
      val bad: Map[String,Seq[String]] = Map.empty
      
      ContainerMethods.providerQueryParam(bad) must beFailedTry.withThrowable[BadRequestException]
    }
    
    "FAIL if provider is given more than once" in {
      val org = newOrg(id = dummyRootOrgId).get
      val (wrk,env,prv) = setupenv(org.id)
      val bad = Map("provider" -> Seq(prv.toString, uuid.toString, uuid.toString))
      
      ContainerMethods.providerQueryParam(bad) must beFailedTry.withThrowable[BadRequestException]
    }
    
    "FAIL if provider is not a valid UUID" in {
      val bad = Map("provider" -> Seq("foo"))
      ContainerMethods.providerQueryParam(bad) must beFailedTry.withThrowable[BadRequestException]
    }
    
    "FAIL if provider doesn't exist" in {
      val bad = Map("provider" -> Seq(uuid.toString))
      ContainerMethods.providerQueryParam(bad) must beFailedTry.withThrowable[BadRequestException]
    }
    
  }
  
  
}