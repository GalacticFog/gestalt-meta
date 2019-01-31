package controllers.util

import java.util.UUID

//import com.galacticfog.gestalt.data.TypeFactory
//import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.meta.api.errors._
//import com.galacticfog.gestalt.meta.api.sdk
//import com.galacticfog.gestalt.meta.api.sdk.{GestaltTypePropertyInput, ResourceIds}
import com.galacticfog.gestalt.meta.test._
//import controllers.PropertyController
//import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.PlaySpecification
import play.api.libs.json._

//import org.specs2.specification.Scope

class UserProfileMethodsSpec extends PlaySpecification with MetaRepositoryOps {
  
  /*
   * def readProfileFavorites(userId: UUID, payload: JsValue, validateFavs: Boolean = true): Try[Seq[ResourceFavorite]]
   */
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
  
  
}