package controllers.util

import play.api.libs.json._
import java.util.UUID
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.json._
import scala.util.{Failure,Success,Try}

import migrations.V30._


object UserProfileMethods {
  
  def readProfileFavorites(payload: JsValue, validateFavs: Boolean = true): Try[Seq[ResourceFavorite]] = {
    Try {
      ((payload \ "properties" \ "resource_favorites") match {
        case e : JsUndefined => Seq.empty[JsValue]
        case f : JsDefined => f.as[Seq[JsValue]]
      }).map { f => 
        val fav = f.validate[ResourceFavorite](ResourceFavorite.resourceFavoriteFormat).getOrElse {
          throw new BadRequestException(s"Cannot parse favorite '${f}'")
        }
        if (validateFavs) {
          // Validate favorites exist if required.
          val res = ResourceFactory.findById(fav.resource_id).getOrElse {
            throw new ResourceNotFoundException(s"Resource with ID '${fav.resource_id}' not found.")
          }
          ResourceFavorite.mergeWithResource(fav, res)            
        } else fav   
      }  
    }
  }

  
  def addFavorite(userId: UUID, profileId: UUID, newFavoriteJson: JsValue): Try[GestaltResourceInstance] = Try {
    
    val user = ResourceFactory.findById(ResourceIds.User, userId).getOrElse {
      throw new ResourceNotFoundException(s"User with ID '${userId}' not found.")
    }

    val profile = ResourceFactory.findById(USERPROFILE_TYPE_ID, profileId).getOrElse {
      throw new ResourceNotFoundException(s"UserProfile with ID '${profileId}' not found.")
    }

    val favoriteId = (newFavoriteJson \ "resource_id") match {
      case a: JsDefined => UUID.fromString(a.as[String])
      case b: JsUndefined => throw new BadRequestException(s"Payload property 'resource_id' is missing.")
    }

    val nickName = (newFavoriteJson \ "nickname") match {
      case a: JsDefined => Some(a.as[String])
      case b: JsUndefined => None
    }
    
    val favoriteResource = ResourceFactory.findById(favoriteId).getOrElse {
      throw new ResourceNotFoundException(s"Bad favorite. Resource with ID '${favoriteId}' not found.")
    }
    
    makeContextJson(favoriteId) match {
      case Failure(e) => throw e
      case Success(context) => {
        val newFavorite = ResourceFavorite(resource_id = favoriteId, nickname = nickName, context = Some(context))            
        UserProfile.withResourceFavorite(profile, newFavorite).get
      }
    }
  }
  
  
  def makeContextJson(id: UUID): Try[JsValue] = {
    
    def typeLabel(typeId: UUID): String = typeId match {
      case ResourceIds.Org => "org"
      case ResourceIds.Workspace => "workspace"
      case ResourceIds.Environment => "environment"
      case _ => throw new RuntimeException(s"Unexpected type ID: ${typeId}")
    }
    
    def res2json(res: GestaltResourceInstance): JsValue = {
      val link = Json.obj("id" -> res.id.toString, "name" -> res.name)
      val extra = if (res.typeId == ResourceIds.Org) 
        Json.obj("fqon" -> res.properties.get("fqon")) else Json.obj()
        
      link ++ extra
    }

    buildContextMap(id).map {
      _.foldLeft(Json.obj()) { case (acc, (tpe, res)) =>
        acc ++ Json.obj(typeLabel(tpe) -> res2json(res))
      }      
    }    
  }
  
  def buildContextMap(id: UUID): Try[Map[UUID, GestaltResourceInstance]] = Try {
 
    val family: List[(Int, GestaltResourceInstance)] = ResourceFactory.getInstanceAncestors(id)
    val mappable = Seq(ResourceIds.Org, ResourceIds.Workspace, ResourceIds.Environment)
    
    val hi = family.foldLeft(Map[UUID, (Int, GestaltResourceInstance)]()) { case (acc, (depth, res)) =>      
      if (!mappable.contains(res.typeId)) acc
      else {
        if (!acc.keySet.contains(res.typeId)) {
          acc ++ Map(res.typeId -> (depth -> res))
        } else if (acc(res.typeId)._1 > depth) {
          acc ++ Map(res.typeId -> (depth -> res))
        } else acc
      }
    }
    hi.map { case (k, (_, v)) => (k -> v) }
  }
  
  def deleteFavorite(userId: UUID, profileId: UUID, favoriteId: UUID): Try[GestaltResourceInstance] = {
    Try {
      val rss = mapAll(Map("user" -> userId, "profile" -> profileId, "target" -> favoriteId))
      if (rss.get("user").isEmpty) 
        throw new ResourceNotFoundException(s"User with ID '${userId}' not found.")
      if (rss.get("profile").isEmpty) 
        throw new ResourceNotFoundException(s"UserProfile with ID '${profileId}' not found.")
      if (rss.get("target").isEmpty) 
        throw new BadRequestException(s"Must provide query-param 'id' identifying the favorite to remove.")
      
      val profile = rss("profile")
      UserProfile.removeResourceFavorite(profile, rss("target").id)
    }
  }
  
  def mapAll[A](resources: Map[A, UUID]): Map[A, GestaltResourceInstance] = {
    val ids = resources.values.toList
    val rev = resources.map { case (k, v) => (v, k) }.toMap
    ResourceFactory.findAllIn(ids).map(r => (rev(r.id) -> r)).toMap
  }    
}  


//case class ResourceContext(org: JsValue, workspace: Option[JsValue], environment: Option[JsValue])
case class ResourceFavorite(
    resource_id: UUID,
    resource_type_id: Option[UUID] = None,
    resource_name: Option[String] = None,
    resource_display_name: Option[String] = None,
    resource_description: Option[String] = None,
    nickname: Option[String] = None,
    context: Option[JsValue] = None)
    
object ResourceFavorite {
  implicit lazy val resourceFavoriteFormat = Json.format[ResourceFavorite]
  def mergeWithResource(fav: ResourceFavorite, res: GestaltResourceInstance): ResourceFavorite = {
    val displayName = for {
      ps <- res.properties
      dn <- ps.get("display_name")
    } yield dn
    ResourceFavorite(res.id, Some(res.typeId), Some(res.name), displayName, res.description, fav.nickname)
  }
}

case class UserProfile(name: String, resource_favorites: Seq[ResourceFavorite]) 
object UserProfile {
  
  implicit lazy val userProfileFormat = Json.format[UserProfile]
  
  def withResourceFavorite(profile: GestaltResourceInstance, favorite: ResourceFavorite): Try[GestaltResourceInstance] = {
    Try {
      // Ensure fav id exists
      val favoriteResource = ResourceFactory.findById(favorite.resource_id).getOrElse {
        throw new ResourceNotFoundException(s"Bad favorite. Resource with ID '${favorite.resource_id}' not found.")
      }
//      // Build list of updated favorites
//      val newFavorite = ResourceFavorite(
//          resource_id = favoriteResource.id, 
//          resource_name = Some(favoriteResource.name), 
//          resource_type_id = Some(favoriteResource.typeId), 
//          nickname = favorite.nickname)
      
      val oldFavorites: Seq[ResourceFavorite] = UserProfile.getFavorites(profile)
      if (oldFavorites.exists(_.resource_id == favorite.resource_id)) {
        profile
      } else {
        updateFavoritesProperty(profile, (favorite +: oldFavorites))
      }
    }
  }
  
  def removeResourceFavorite(profile: GestaltResourceInstance, favoriteId: UUID): GestaltResourceInstance = {
    val oldFavorites = UserProfile.getFavorites(profile)
    val newFavorites = oldFavorites.filter(_.resource_id != favoriteId)
    updateFavoritesProperty(profile, newFavorites)
  }
  
  def updateFavoritesProperty(profile: GestaltResourceInstance, newFavorites: Seq[ResourceFavorite]): GestaltResourceInstance = {
    val oldProperties = profile.properties.getOrElse(Map.empty)
    val newProperties = oldProperties ++ Map("resource_favorites" -> Json.stringify(Json.toJson(newFavorites)))
    profile.copy(properties = Some(newProperties))
  }
  
  def getFavorites(res: GestaltResourceInstance): Seq[ResourceFavorite] = {
    (for {
      ps <- res.properties
      raw <- ps.get("resource_favorites")
      json = Json.parse(raw).as[Seq[JsValue]]
      favs = json.map(Js.parse[ResourceFavorite](_).get)
      output = {
        // Get IDs of all resources the user pinned as favorites.
        val asPersisted: Map[UUID, ResourceFavorite] = favs.map(f => (f.resource_id -> f)).toMap
        
        // This will be all of those favorites that actually exist (may have been deletes)
        val allExisting = ResourceFactory.findAllIn(asPersisted.keySet.toList)
        
        //
        // TODO: Test if anything was deleted, log as info, and proceed.
        //
        allExisting.map(r => ResourceFavorite.mergeWithResource(asPersisted(r.id), r))          
      }
    } yield output).getOrElse(Seq.empty[ResourceFavorite])      
  }
}