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
import com.galacticfog.gestalt.data.EnvironmentType


class JsonInputSpec extends PlaySpecification with ResourceScope with BeforeAll {
  
  override def beforeAll(): Unit = pristineDatabase
  
  val in = new JsonInput {}
  
  "toInput(JsValue)" should {
    
    "parse valid JSON to a GestaltResourceInput" in {
      val js = Json.obj("name" -> "test")
      val input = in.toInput(js)
      
      input must beSuccessfulTry
      input.get.name === "test"
    }
    
    "requireTypeId == TRUE" >> {
      
      "succeed when type-id is given in the input" >> {
        val expected = ResourceIds.Workspace
        val js = Json.obj(
            "name" -> uuid.toString,
            "resource_type" -> expected)

        val test = in.toInput(js, None, requireTypeId = true) 
        test must beSuccessfulTry
        test.get.resource_type.get === expected
      }
      
      "succeed when type-id is given in the variable" >> {
        val expected = ResourceIds.Workspace
        val js = Json.obj("name" -> uuid.toString)
        
        val test = in.toInput(js, Some(expected), requireTypeId = true) 
        test must beSuccessfulTry
        test.get.resource_type.get === expected
      }
      
      "use ID given in the input if found in both places" >> {
        val expected = ResourceIds.Org
        val js = Json.obj(
          "name" -> uuid.toString,
          "resource_type" -> expected)
        
        val test = in.toInput(js, typeId = Some(ResourceIds.Workspace), requireTypeId = true)
        test must beSuccessfulTry
        test.get.resource_type.get === expected  
      }
      
      
      "fail if no type-id is found" >> {
        val js = Json.obj("name" -> uuid.toString)
        in.toInput(js, requireTypeId = true) must beFailedTry
      }
      
      
      "fail if an invalid type-id is found" >> {
        val js = Json.obj("name" -> uuid.toString)
        in.toInput(js, typeId = Some(uuid()), requireTypeId = true) must beFailedTry
      }
    }
    
    "requireTypeId == FALSE" >> {
      
      "ignore missing type-id" >> {
        val js = Json.obj("name" -> uuid.toString)
        in.toInput(js, requireTypeId = false) must beSuccessfulTry
      }

      "ignore an invalid type-id" >> {
        val js = Json.obj("name" -> uuid.toString)
        in.toInput(js, Some(uuid()), requireTypeId = false) must beSuccessfulTry
      }      
    }    
  }
  
  "toInputValidated" >> {
    
    "succeed when type properties are valid" >> {
      //val (w, e) = createWorkspaceEnvironment(dummyRootOrgId)
      val js = Json.obj(
          "name" -> "test",
          "properties" -> Json.obj(
            "environment_type" -> EnvironmentType.id("development").toString))
      val input = in.toInputValidated(js, Some(ResourceIds.Environment))
      
      input must beSuccessfulTry
    }
    
    "fail when type-properties are invalid" >> {
      val js = Json.obj("name" -> "test")
      val input = in.toInputValidated(js, Some(ResourceIds.Environment))
      
      input must beFailedTry      
    }
  }  
  
  
  "withInputDefaults" should {
    
    "populate default ID, ResourceOwner, and ResourceState values if none are provided" in {
      
      val typeId = ResourceIds.Environment.toString
      val input = in.toInput(Json.obj("resource_type" -> typeId, "name" -> "test"))
      
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
          
      val input = in.toInput(js)
      
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

