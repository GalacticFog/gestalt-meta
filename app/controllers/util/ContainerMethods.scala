package controllers.util



import java.util.UUID

import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.security.api.errors.UnauthorizedAPIException
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.security.play.silhouette.{OrgContextRequestUUID, OrgContextRequest, GestaltFrameworkSecuredController, AuthAccountWithCreds}
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator

import controllers.util._
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.ws.WS
import com.galacticfog.gestalt.meta.api.sdk.ResourceLabel
import com.galacticfog.gestalt.laser._
import play.api.{ Logger => log }
import scala.concurrent.{ ExecutionContext, Future, Promise, Await }
import scala.concurrent.duration._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.events._
import com.galacticfog.gestalt.meta.policy._
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.marathon._

import com.galacticfog.gestalt.keymgr.GestaltFeature
import com.galacticfog.gestalt.meta.auth.Actions

import com.galacticfog.gestalt.meta.api.{Resource,ResourcePath}

import controllers.MarathonController


object ContainerMethods {

  /**
   * Make a best effort to get updated stats from the Marathon provider and to update the resource with them  
   */
  def lookupContainer(path: ResourcePath, user: AuthAccountWithCreds): Option[GestaltResourceInstance] = {
    
    Resource.fromPath(path.path) map { metaContainer =>
    
      val providerId  = getProviderId(metaContainer.properties.get("provider")) getOrElse {
        throw new RuntimeException(s"Could not parse provider ID from Meta Container.")
      }
      val provider    = MarathonController.marathonProvider(providerId)
      val client      = MarathonController.marathonClient(provider)
      val marathonId  = metaContainer.properties.get("external_id")
      val application = Await.result(client.getApplication_marathon_v3(marathonId)(global), 5 seconds)
      val stats       = MarathonClient.marathon2Container(application)
      
      updateWithStats(metaContainer, stats)
    }
  }
  
  private[util] def getProviderId(jstring: String): Option[UUID] = {    
    JsonUtil.getJsonField(Json.parse(jstring), "id") map { _.as[UUID] }
  }
  
  private[util] def updateWithStats(metaCon: GestaltResourceInstance, stats: Option[ContainerStats]) = {
    val newStats = stats match {
      
      case Some(stats) => Seq(
        "age"             -> stats.age.toString,
        "status"          -> stats.status,
        "num_instances"   -> stats.numInstances.toString,
        "tasks_running"   -> stats.tasksRunning.toString,
        "tasks_healthy"   -> stats.tasksHealthy.toString,
        "tasks_unhealthy" -> stats.tasksUnhealthy.toString,
        "tasks_staged"    -> stats.tasksStaged.toString)
        
      case None => Seq(
        "status"          -> "LOST",
        "num_instances"   -> "0",
        "tasks_running"   -> "0",
        "tasks_healthy"   -> "0",
        "tasks_unhealthy" -> "0",
        "tasks_staged"    -> "0")
    }
    
    metaCon.copy( properties = metaCon.properties map { _ ++ newStats } orElse {
      Some(newStats toMap)
    })
  }    
  
}