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
import org.joda.time.DateTime

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
      
      val param = ContainerMethodsImpl.providerQueryParam(good)
      param must beSuccessfulTry
      param.get === prv
    }
    
    "FAIL if provider parameter is missing" in {
      val org = newOrg(id = dummyRootOrgId).get
      val (wrk,env,prv) = setupenv(org.id)
      val bad: Map[String,Seq[String]] = Map.empty
      
      ContainerMethodsImpl.providerQueryParam(bad) must beFailedTry.withThrowable[BadRequestException]
    }
    
    "FAIL if provider is given more than once" in {
      val org = newOrg(id = dummyRootOrgId).get
      val (wrk,env,prv) = setupenv(org.id)
      val bad = Map("provider" -> Seq(prv.toString, uuid.toString, uuid.toString))
      
      ContainerMethodsImpl.providerQueryParam(bad) must beFailedTry.withThrowable[BadRequestException]
    }
    
    "FAIL if provider is not a valid UUID" in {
      val bad = Map("provider" -> Seq("foo"))
      ContainerMethodsImpl.providerQueryParam(bad) must beFailedTry.withThrowable[BadRequestException]
    }
    
    "FAIL if provider doesn't exist" in {
      val bad = Map("provider" -> Seq(uuid.toString))
      ContainerMethodsImpl.providerQueryParam(bad) must beFailedTry.withThrowable[BadRequestException]
    }
    
  }
  
  "updateWithStats" should {
    
    "make NO CHANGES if the resource already has a 'status' property" in {
      val given = "MIGRATING"
      val props = Map(
        "container_type" -> "foo",
        "image" -> "bar",
        "provider" -> "{}",
        "status" -> given)      
      
      val org = newOrg(id = dummyRootOrgId).get
      val (_,env) = createWorkspaceEnvironment(org.id)
      val c = newDummyContainer(env, props)
      c must beSuccessfulTry
      
      val test = ContainerMethodsImpl.updateWithStats(c.get, None)
      val status = test.properties.get.get("status")
      
      status must beSome
      status.get === given
    }
    
    "use the supplied status when property is missing and ContainerStats is Some" in {
      
      val given = "LEAPING"
      val stats = com.galacticfog.gestalt.marathon.ContainerStats(
            id = uuid.toString,
            containerType = "foo",
            status = given,
            cpus = 0.5,
            memory = 0.5,
            image = "image",
            age = DateTime.now,
            numInstances = 1,
            tasksStaged = 0,
            tasksRunning = 1,
            tasksHealthy = 1,
            tasksUnhealthy = 0)
                    
      val org = newOrg(id = dummyRootOrgId).get
      val (_,env) = createWorkspaceEnvironment(org.id)
      val c = newDummyContainer(env)
      c must beSuccessfulTry

      val test = ContainerMethodsImpl.updateWithStats(c.get, Option(stats))
      val result = test.properties.get.get("status")
      
      result must beSome
      result.get === given
    }
    
    "set status to LOST when property is missing and ContainerStats is None" in {
      val org = newOrg(id = dummyRootOrgId).get
      val (_,env) = createWorkspaceEnvironment(org.id)
      val c = newDummyContainer(env)
      c must beSuccessfulTry
      
      val test = ContainerMethodsImpl.updateWithStats(c.get, None)
      val status = test.properties.get.get("status")
      
      status must beSome
      status.get === "LOST"
    }
  }
  
}