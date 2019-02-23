package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data._
//import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk._
//import com.galacticfog.gestalt.meta.api.sdk.{GestaltTypePropertyInput, ResourceIds}
import com.galacticfog.gestalt.meta.test._
import play.api.test.PlaySpecification
import play.api.libs.json._

import org.specs2.matcher.JsonMatchers

//import org.specs2.specification.Scope

class UserProfileMethodsSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {
  
  "readProfileFavorites" should {
    
    "read a seq of ResourceFavorite objects from JSON" >> {
      val (w, env) = this.createWorkspaceEnvironment(dummyRootOrgId)
      val nickname = "favorite-1"
      val profile = Json.parse(s"""
      {
      	"name": "root-default-profile",
      	"properties": {
      		"resource_favorites": [
      			{
      				"resource_id": "${env}",
      				"nickname": "${nickname}"
      			}
      		]
      	}
      }""")

      val favs = UserProfileMethods.readProfileFavorites(profile, false)
      favs must beSuccessfulTry
      favs.get.size === 1
      favs.get.head.resource_id === env
      favs.get.head.nickname must beSome(nickname)
    }
    
    "throw BadRequestException if a resource_favorite is malformed" >> {
      val (w, env) = this.createWorkspaceEnvironment(dummyRootOrgId)
      val nickname = "favorite-1"
      val profile = Json.parse(s"""
      {
      	"name": "root-default-profile",
      	"properties": {
      		"resource_favorites": [
      			{
      				"nickname": "${nickname}"
      			}
      		]
      	}
      }""")

      val favs = UserProfileMethods.readProfileFavorites(profile, false)
      favs must beFailedTry.withThrowable[BadRequestException]
    }
    
    
    "validatFavs == TRUE: throw ResourceNotFoundException if favorite points to invalid resource" >> {
      val notExist = "ab6460ca-ae90-44be-a580-5e0a156dbc38"
      val nickname = "favorite-1"
      val profile = Json.parse(s"""
      {
      	"name": "root-default-profile",
      	"properties": {
      		"resource_favorites": [
      			{
      				"resource_id": "${notExist}",
      				"nickname": "${nickname}"
      			}
      		]
      	}
      }""")
      val favs = UserProfileMethods.readProfileFavorites(profile, validateFavs = true)
      favs must beFailedTry.withThrowable[ResourceNotFoundException]
    }
    
    "validatFavs == FALSE: ignore when invalid resources are given" >> {
      val notExist = "ab6460ca-ae90-44be-a580-5e0a156dbc38"
      val nickname = "favorite-1"
      val profile = Json.parse(s"""
      {
      	"name": "root-default-profile",
      	"properties": {
      		"resource_favorites": [
      			{
      				"resource_id": "${notExist}",
      				"nickname": "${nickname}"
      			}
      		]
      	}
      }""")
      val favs = UserProfileMethods.readProfileFavorites(profile, validateFavs = false)
      favs must beSuccessfulTry
      favs.get.size === 1
      favs.get.head.resource_id === UUID.fromString(notExist)
    }
  }
  
  import migrations.V30._
  
  "addFavorite" should {
  //  def addFavorite(userId: UUID, profileId: UUID, newFavoriteJson: JsValue): Try[GestaltResourceInstance]
  
   "add a new ResourceFavorite when the UserProfile is empty (zero favorites)" >> {
     val (_, env) = createWorkspaceEnvironment()
     
     val user1 = createNewUser()
     
     val profile = createInstance(USERPROFILE_TYPE_ID, uuid.toString, parent = Some(user1.id))
     profile must beSuccessfulTry
     profile.get.properties.isEmpty === true

     val updated = UserProfileMethods.addFavorite(user1.id, profile.get.id, Json.obj("resource_id" -> env.toString))
     updated must beSuccessfulTry
     updated.get.properties.nonEmpty === true
     updated.get.properties.get("resource_favorites").nonEmpty === true
     
   }
   "add a new ResourceFavorite to a UserProfile if favorite does not exist" >> {
     val (wrk, env) = createWorkspaceEnvironment()
     val user1 = createNewUser()
     
     val favs = Seq(ResourceFavorite(env, nickname = Some("My Environment")))
     val profile = createInstance(USERPROFILE_TYPE_ID, uuid.toString, 
         properties = Some(Map("resource_favorites" -> Json.stringify(Json.toJson(favs)))),
         parent = Some(user1.id))
     profile must beSuccessfulTry
     profile.get.properties.nonEmpty === true
     profile.get.properties.get("resource_favorites").nonEmpty === true
     
     val updated = UserProfileMethods.addFavorite(user1.id, profile.get.id, Json.obj("resource_id" -> wrk.toString))
     updated must beSuccessfulTry
     updated.get.properties.nonEmpty === true
     updated.get.properties.get("resource_favorites").nonEmpty === true
     
     val parsed = Json.parse(updated.get.properties.get("resource_favorites")).validate[Seq[ResourceFavorite]]
     parsed.get.size === 2
   }
   
   "return the UserProfile unchanged if the ResourceFavorite already exists" >> {
     val (wrk, env) = createWorkspaceEnvironment()
     val user1 = createNewUser()
     
     val favs = Seq(
         ResourceFavorite(env, nickname = Some("My Environment")),
         ResourceFavorite(wrk, nickname = Some("My Workspace"))
     )
     val profile = createInstance(USERPROFILE_TYPE_ID, uuid.toString, 
         properties = Some(Map("resource_favorites" -> Json.stringify(Json.toJson(favs)))),
         parent = Some(user1.id))
     profile must beSuccessfulTry
     profile.get.properties.nonEmpty === true
     profile.get.properties.get("resource_favorites").nonEmpty === true
     
     val updated = UserProfileMethods.addFavorite(user1.id, profile.get.id, Json.obj("resource_id" -> wrk.toString))
     updated must beSuccessfulTry
     updated.get.properties.nonEmpty === true
     updated.get.properties.get("resource_favorites").nonEmpty === true
     
     val parsed = Json.parse(updated.get.properties.get("resource_favorites")).validate[Seq[ResourceFavorite]]
     parsed.get.size === 2
   }
    
   "throw ResourceNotFound if user does not exist" >> {
     val (wrk, env) = createWorkspaceEnvironment()
     val user1 = createNewUser()
     val badUser = UUID.fromString("8e8d2f82-7a7f-42aa-b6fa-2f0ee55df402")
     
     val favs = Seq(
         ResourceFavorite(env, nickname = Some("My Environment")),
         ResourceFavorite(wrk, nickname = Some("My Workspace"))
     )
     val profile = createInstance(USERPROFILE_TYPE_ID, uuid.toString, 
         properties = Some(Map("resource_favorites" -> Json.stringify(Json.toJson(favs)))),
         parent = Some(user1.id))
     profile must beSuccessfulTry
     profile.get.properties.nonEmpty === true
     profile.get.properties.get("resource_favorites").nonEmpty === true
     
     UserProfileMethods.addFavorite(badUser, profile.get.id, Json.obj("resource_id" -> wrk.toString)) must beFailedTry.withThrowable[ResourceNotFoundException]
   }
    
   "throw ResourceNotFound if UserProfile does not exist" >> {
     val (wrk, env) = createWorkspaceEnvironment()
     val user1 = createNewUser()
     val badProfile = uuid()
     
     UserProfileMethods.addFavorite(
         user1.id, badProfile, Json.obj("id" -> wrk.toString)) must beFailedTry.withThrowable[ResourceNotFoundException]
   }
    
   "throw BadRequest if ResourceFavorite JSON is missing 'id'" >> {
     val (wrk, env) = createWorkspaceEnvironment()
     val user1 = createNewUser()
     
     val favs = Seq(
         ResourceFavorite(env, nickname = Some("My Environment")),
         ResourceFavorite(wrk, nickname = Some("My Workspace"))
     )
     val profile = createInstance(USERPROFILE_TYPE_ID, uuid.toString, 
         properties = Some(Map("resource_favorites" -> Json.stringify(Json.toJson(favs)))),
         parent = Some(user1.id))
     profile must beSuccessfulTry
     profile.get.properties.nonEmpty === true
     profile.get.properties.get("resource_favorites").nonEmpty === true
     
     UserProfileMethods.addFavorite(
       user1.id, profile.get.id, Json.obj("nickname" -> "My Workspace")) must beFailedTry.withThrowable[BadRequestException]
   }
    
   "throw ResourceNotFound if named favorite does not exist" >> {
     val (wrk, env) = createWorkspaceEnvironment()
     val user1 = createNewUser()
     
     val favs = Seq(
         ResourceFavorite(env, nickname = Some("My Environment")),
         ResourceFavorite(wrk, nickname = Some("My Workspace"))
     )
     val profile = createInstance(USERPROFILE_TYPE_ID, uuid.toString, 
         properties = Some(Map("resource_favorites" -> Json.stringify(Json.toJson(favs)))),
         parent = Some(user1.id))
     profile must beSuccessfulTry
     profile.get.properties.nonEmpty === true
     profile.get.properties.get("resource_favorites").nonEmpty === true
     
     UserProfileMethods.addFavorite(
       user1.id, profile.get.id, Json.obj("resource_id" -> uuid.toString)) must beFailedTry.withThrowable[ResourceNotFoundException]
    }
  }
  
  "deleteFavorite" should {
 
  	"return a UserProfile with the given favorite removed" >> {
      val (wrk, env) = createWorkspaceEnvironment()
      val user1 = createNewUser()

      val favs = Seq(
          ResourceFavorite(env, nickname = Some("My Environment")),
          ResourceFavorite(wrk, nickname = Some("My Workspace"))
      )
      val profile = createInstance(USERPROFILE_TYPE_ID, uuid.toString,
        properties = Some(Map("resource_favorites" -> Json.stringify(Json.toJson(favs)))),
        parent = Some(user1.id))
      profile must beSuccessfulTry
      profile.get.properties.nonEmpty === true
      profile.get.properties.get("resource_favorites").nonEmpty === true
      Json.parse(profile.get.properties.get("resource_favorites")).validate[Seq[ResourceFavorite]].get.size === 2
      
      val updated = UserProfileMethods.deleteFavorite(user1.id, profile.get.id, wrk)
      updated must beSuccessfulTry
      updated.get.properties.nonEmpty === true
      updated.get.properties.get("resource_favorites").nonEmpty === true

      val parsed = Json.parse(updated.get.properties.get("resource_favorites")).validate[Seq[ResourceFavorite]]
      parsed.get.size === 1
  	}    
    
  	"throw ResourceNotFound if User does not exist" >> {
  	  failure
  	}.pendingUntilFixed("write test")
  	
  	"throw ResourceNotFound if UserProfile does not exist" >> {
  	  failure
  	}.pendingUntilFixed("write test")
  
  	"throw BadRequest if given favorite ID does not exist" >> {
  	  failure
  	}.pendingUntilFixed("write test")
  
  }
  
  import scala.util.{Try,Success}
  
 "buildContextMap" should {
   
   "Map Org, Workspace, and Environment when ID is Environment" >> {
     val (wrk, env) = createWorkspaceEnvironment()
     val Success(m) = UserProfileMethods.buildContextMap(env)
     
     m.size === 3
     m.contains(ResourceIds.Org) === true
     m.contains(ResourceIds.Workspace) === true
     m.contains(ResourceIds.Environment) === true
     m(ResourceIds.Workspace).id === wrk
     m(ResourceIds.Environment).id === env
     m(ResourceIds.Org).id === dummyRootOrgId
   }
   
   "Map Org/Workspace/Environment when ID is a child of Environment" >> {
     val (wrk, env) = createWorkspaceEnvironment()
     val container = newDummyContainer(env)
     container must beSuccessfulTry
     
     val Success(m) = UserProfileMethods.buildContextMap(container.get.id)
     m.size === 3
     m.contains(ResourceIds.Org) === true
     m.contains(ResourceIds.Workspace) === true
     m.contains(ResourceIds.Environment) === true
     m(ResourceIds.Workspace).id === wrk
     m(ResourceIds.Environment).id === env
     m(ResourceIds.Org).id === dummyRootOrgId     
   }
   
   "Map Org/Workspace when ID is a Workspace" >> {
     val (wrk, _) = createWorkspaceEnvironment()
     val Success(m) = UserProfileMethods.buildContextMap(wrk)
     
     m.size === 2
     m.contains(ResourceIds.Org) === true
     m.contains(ResourceIds.Workspace) === true
     m(ResourceIds.Workspace).id === wrk
     m(ResourceIds.Org).id === dummyRootOrgId
   }
   
   "Map Org/Workspace when ID is a child of Workspace (but not an Environment)" >> {
     val (wrk, _) = createWorkspaceEnvironment()
     val (policy, _) = createPolicyRule(wrk)
     
     ResourceFactory.findById(policy) must beSome
     val Success(m) = UserProfileMethods.buildContextMap(policy)
     
     m.size === 2
     m.contains(ResourceIds.Org) === true
     m.contains(ResourceIds.Workspace) === true
     m(ResourceIds.Workspace).id === wrk
     m(ResourceIds.Org).id === dummyRootOrgId
   }
   
   "Map Org when ID is an Org" >> {
     val org = dummyRootOrgId
     val Success(m) = UserProfileMethods.buildContextMap(org)
     
     m.size === 1
     m.contains(ResourceIds.Org) === true
     m(ResourceIds.Org).id === org
   }
   
   "Map Org when ID is a child of Org (but is not a Workspace)" >> {
     val org = dummyRootOrgId
     val (policy, _) = createPolicyRule(org)
     
     ResourceFactory.findById(policy) must beSome
     val Success(m) = UserProfileMethods.buildContextMap(policy)
     
     m.size === 1
     m.contains(ResourceIds.Org) === true
     m(ResourceIds.Org).id === dummyRootOrgId
   }
   
   "Map a single Org when target Org is a child Org" >> {
     val orgId = uuid()
     val org = createOrg("foo", orgId.toString, parent = Some(dummyRootOrgId))
     org must beSuccessfulTry
     
     val Success(m) = UserProfileMethods.buildContextMap(org.get.id)
     m.size === 1
     m.contains(ResourceIds.Org) === true
     m(ResourceIds.Org).id === org.get.id     
   }
 }
 
 "makeContextJson" should {
   
   "build Org/Workspace/Environment context" >> {
     val workspaceName = uuid.toString
     val environmentName = uuid.toString
     val org = dummyRootOrgId
     
     val (wrk, env) = createWorkspaceEnvironment(wrkName = workspaceName, envName = environmentName)
     val Success(json) = UserProfileMethods.makeContextJson(env)
     val Success(m) = Try(json.validate[Map[String, JsValue]].get)

     m.size === 3
     m.contains("org") === true
     m.contains("workspace") === true
     m.contains("environment") === true
     
     m("org").toString         must /("id" -> org.toString) and /("name" -> "root")
     m("workspace").toString   must /("id" -> wrk.toString) and /("name" -> workspaceName)
     m("environment").toString must /("id" -> env.toString) and /("name" -> environmentName)
   }
   
   "build Org/Workspace context" >> {
     val workspaceName = uuid.toString
     val environmentName = uuid.toString
     val org = dummyRootOrgId
     
     val (wrk, _) = createWorkspaceEnvironment(wrkName = workspaceName, envName = environmentName)
     val Success(json) = UserProfileMethods.makeContextJson(wrk)
     val Success(m) = Try(json.validate[Map[String, JsValue]].get)
 
     m.size === 2
     m.contains("org") === true
     m.contains("workspace") === true
     m.contains("environment") === false
     
     m("org").toString         must /("id" -> org.toString) and /("name" -> "root")
     m("workspace").toString   must /("id" -> wrk.toString) and /("name" -> workspaceName)
   }
   
   "build Org context" >> {
     val org = dummyRootOrgId

     val Success(json) = UserProfileMethods.makeContextJson(org)
     val Success(m) = Try(json.validate[Map[String, JsValue]].get)
 
     m.size === 1
     m.contains("org") === true
     m.contains("workspace") === false
     m.contains("environment") === false
     
     m("org").toString must /("id" -> org.toString) and /("name" -> "root")
   }
   
   "build Org context with nested Org" >> {
     val parentId = dummyRootOrgId
     val childId = uuid
     val grandchildId = uuid
     val grandchildName = "grandchild-1"
     val childName = "child-1"
     
     val child = createOrg(childName, childId, parent = Some(parentId))
     child must beSuccessfulTry
     val grandchild = createOrg(grandchildName, grandchildId, parent = Some(child.get.id))
     grandchild must beSuccessfulTry
     
     val p = ResourceFactory.findParent(grandchildId)
     p must beSome
     p.get.id === childId
     
     val Success(json) = UserProfileMethods.makeContextJson(grandchildId)
     val Success(m) = Try(json.validate[Map[String, JsValue]].get)
     
     m.size === 1
     m.contains("org") === true
     m.contains("workspace") === false
     m.contains("environment") === false
     
     m("org").toString must /("id" -> grandchildId.toString) and /("name" -> grandchildName) and /("fqon" -> s"$childName.$grandchildName")
   }
 }
  
}
