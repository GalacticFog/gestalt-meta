package controllers.util


import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Step
import play.api.libs.json._

import java.util.UUID

import com.galacticfog.gestalt.meta.test._

import play.api.test._
import play.api.test.Helpers._


class JsonInputSpec extends PlaySpecification with ResourceScope with BeforeAll {
  
  override def beforeAll(): Unit = pristineDatabase
  
  val in = new JsonInput {}
  
  "safeGetInputJson(JsValue)" should {
    
    "parse valid JSON to a GestaltResourceInput" in {
      val js = Json.obj("name" -> "test")
      val input = in.safeGetInputJson(js)
      
      input must beSuccessfulTry
      input.get.name === "test"
    }
    
    "ignore resource properties when typeId is None" in {
      val js = Json.obj(
          "name" -> "test",
          "properties" -> Json.obj(
              "a" -> "foo", "b" -> "bar", "c" -> "baz"))
      val input = in.safeGetInputJson(js)
      
      input must beSuccessfulTry
      input.get.name === "test"
      input.get.properties must beSome
      input.get.properties.get.contains("a") must beTrue
      input.get.properties.get.contains("b") must beTrue
      input.get.properties.get.contains("c") must beTrue
    }
    
    import com.galacticfog.gestalt.data.EnvironmentType
    
    "validate type properties when typeId and properties are valid" in {
      val (w, e) = createWorkspaceEnvironment(dummyRootOrgId)
      val js = Json.obj(
          "name" -> "test",
          "properties" -> Json.obj(
              "environment_type" -> EnvironmentType.id("development").toString,
              "workspace" -> w.toString))
      val input = in.safeGetInputJson(js, Some(ResourceIds.Environment))
      
      input must beSuccessfulTry
    }
    
    "fail when typeId is invalid" in {
      in.safeGetInputJson(
          Json.obj("name" -> "test"), 
          Option(uuid())) must beFailedTry      
    }
    
    "fail when properties are invalid for type" in {
      val js = Json.obj(
          "name" -> "test"
          /* 'workspace' is no longer a required property, but 'environment_type' still is
          "properties" -> Json.obj(
              "environment_type" -> EnvironmentType.id("development").toString)*/)
      
      // properties.workspace is missing
      in.safeGetInputJson(js, Some(ResourceIds.Environment)) must beFailedTry
    }
  }
  
  "withInputDefaults" should {
    
    "populate default ID, ResourceOwner, and ResourceState values if none are provided" in {
      
      val typeId = ResourceIds.Environment.toString
      val input = in.safeGetInputJson(Json.obj("resource_type" -> typeId, "name" -> "test"))
      
      input must beSuccessfulTry
      input.get.id must beNone
      input.get.owner must beNone
      input.get.resource_state must beNone
      
      val creator = this.dummyAuthAccountWithCreds()
      val res = in.withInputDefaults(dummyRootOrgId, input.get, creator)
      
      // owner defaults to 'creator'
      res.owner.id === creator.account.id.toString
      res.state    === ResourceState.id(ResourceStates.Active)
    }
    
    "leave ID, ResourceOwner, and ResourceState if any are given" in {
      
      val id     = uuid.toString()
      val typeId = ResourceIds.Environment.toString
      val owner  = dummyOwner
      val state  = ResourceStates.Disabled

      val js = Json.obj(
          "id" -> id,
          "resource_type" -> typeId,
          "resource_state" -> state,
          "name" -> "test")
          
      val input = in.safeGetInputJson(js)
      
      input must beSuccessfulTry
      
      val input2 = input.get.copy(owner = Option(owner))
      
      input2.id must beSome
      input2.owner must beSome
      input2.resource_type must beSome
      input2.resource_state must beSome
      
      val creator = dummyAuthAccountWithCreds()
      val res = in.withInputDefaults(dummyRootOrgId, input2, creator)
      
      res.id       === UUID.fromString(id)
      res.state    === ResourceState.id(state)    
      res.owner.id === owner.id
    }
  }

}

