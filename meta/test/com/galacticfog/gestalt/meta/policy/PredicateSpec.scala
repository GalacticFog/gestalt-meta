//package com.galacticfog.gestalt.meta.policy
//
//
//import com.galacticfog.gestalt.meta.api.sdk._
//import com.galacticfog.gestalt.data._
//import com.galacticfog.gestalt.data.models._
//
//import org.specs2.mutable._
//import org.specs2.specification._
//import org.joda.time.DateTime
//
//import org.specs2.specification.Scope
//import java.util.UUID
//
//import scala.util.{Try,Success,Failure}
//import controllers.util.RequestOptions
//import com.galacticfog.gestalt.meta.test.ResourceScope
//
//
//class PredicateSpec extends Specification with ResourceScope {
//
//  sequential
//  
//  controllers.util.db.EnvConfig.getConnection()
//  
//  val containerProps = Map(
//    "cpus" -> "0.2",
//    "memory" -> "1024",
//    "num_instances" -> "2",
//    "image" -> "nginx",
//    "user" -> "gestalt-dev",
//    "acceptedResourceRoles" -> """[ "production" ]""",
//    "network" -> "BRIDGE",
//    "container_type" -> "DOCKER",
//    "provider" -> """{ "id" : "foo" }""",
//    "boolean_property" -> "true")
//    
//
//  val dummyAccount = dummyAuthAccountWithCreds()
//  val fakeContainer = newInstance(ResourceIds.Container, "fake-container", properties = Option(containerProps))
//  val fakeUser = "fake-user"
//  val dummyOpts = RequestOptions(dummyAccount, None, None, None)
//  
//  "decide()" should {
//    
//    "be successful when making a valid comparison" in {
//      
//      val cpuEquals  = Predicate("container.properties.cpus", "equals", "0.2")
//      val nameEquals = Predicate("container.name", "==", "fake-container")
//      
//      decideJson(fakeUser, fakeContainer, nameEquals, None, dummyOpts) must beRight
//      decideJson(fakeUser, fakeContainer, cpuEquals, None, dummyOpts) must beRight
//    }
//    
//    "fail when making an invalid comparison" in {
//      
//      val org = getOrg("galacticfog")
//      org must beSome
//      val (wrk,env) = this.createWorkspaceEnvironment(org.get.id)
//      
//      val envProps = Option(Map(
//          "environment_type" -> "461302be-df8d-4345-a91d-3e426133583d",
//          "workspace" -> uuid.toString))
//      val res = newInstance(ResourceIds.Environment, "TEST", properties = envProps)
//      
//      val environment = ResourceFactory.findById(env).get
//      
//      
//      val p1 = Predicate("name", "==", "NOT-THIS")
//      val p2 = Predicate("environment.properties.environment_type", "equals", "dev")
//      
//      decideJson(fakeUser, environment, p1, None, dummyOpts) must beLeft
//      decideJson(fakeUser, environment, p2, None, dummyOpts) must beLeft
//    }
//    
//    "be successful when operator 'is negative' and the property is missing" in {
//      val p1 = Predicate("missing_property", "!=", "foo")
//      
//      decideJson(fakeUser, fakeContainer, p1, None, dummyOpts) must beRight      
//    }
//    
//    "fail when operator 'is NOT negative' and the property is missing" in {
//      val p1 = Predicate("missing_property", "==", "foo")
//      
//      decideJson(fakeUser, fakeContainer, p1, None, dummyOpts) must beLeft    
//    }
//    
//    "match JsString -> JsArray" in {
//      val p1 = Predicate("container.properties.image", "inlist", "nginx,haproxy")
//      val p2 = Predicate("container.properties.image", "inlist", "httpd,tomcat")
//      
//      decideJson(fakeUser, fakeContainer, p1, None, dummyOpts) must beRight
//      decideJson(fakeUser, fakeContainer, p2, None, dummyOpts) must beLeft
//    }
//    
//    "match JsNumber -> JsArray" in {
//      val p1 = Predicate("container.properties.num_instances", "inlist", "1,2,3")
//      val p2 = Predicate("container.properties.num_instances", "inlist", "4,5,6")
//      
//      decideJson(fakeUser, fakeContainer, p1, None, dummyOpts) must beRight
//      decideJson(fakeUser, fakeContainer, p2, None, dummyOpts) must beLeft 
//    }
//    
//    "match JsBoolean -> JsArray" in {
//      val p1 = Predicate("container.properties.boolean_property", "inlist", "true,false")
//      val p2 = Predicate("container.properties.boolean_property", "inlist", "false,false")
//      
//      decideJson(fakeUser, fakeContainer, p1, None, dummyOpts) must beRight
//      decideJson(fakeUser, fakeContainer, p2, None, dummyOpts) must beLeft      
//    }.pendingUntilFixed("Need resource with boolean property")
//    
//  }
//  
//}