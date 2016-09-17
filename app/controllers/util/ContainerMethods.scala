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
import play.api.Logger
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

import play.api.mvc.RequestHeader

object ContainerMethods extends MetaController {
  
  //private val log = Logger(this.getClass)
  
  
  def setupMigrateRequest(
      fqon: String, 
      env: UUID, 
      container: GestaltResourceInstance,
      user: AuthAccountWithCreds,
      metaUrl: String,
      queryString: QueryString) = {

    val action = "container.migrate"
    val operations = List(
        controllers.util.Authorize(action),
        controllers.util.PolicyCheck(action),
        controllers.util.EventsPre(action))
        
    val options = RequestOptions(
        user, 
        authTarget = Option(env), 
        policyOwner = Option(env), 
        policyTarget = Option(container),
        data = Option(Map(
          "fqon"           -> fqon,
          "meta_url"       -> metaUrl,
          "environment_id" -> env.toString,
          "provider_id"    -> providerQueryParam(queryString).get.toString)))
          
    (operations,options)
  }
  
  
  /**
   * Extract and validate the 'provider' querystring parameter. 
   * Used by the {@link #migrateContainer(String,UUID,UUID) migrateContainer} method.
   * 
   * @param qs the complete, unmodified queryString from the original request.
   */  
  protected [controllers] def providerQueryParam(qs: Map[String,Seq[String]]): Try[UUID] = Try {
    val PROVIDER_KEY = "provider"
    
    if (!qs.contains(PROVIDER_KEY) || qs(PROVIDER_KEY)(0).trim.isEmpty)
      throw badRequest(
          "'provider' parameter not found. (i.e. */migrate?provider={UUID})")
    else Try{
      if (qs(PROVIDER_KEY).size > 1) {
        throw badRequest(s"Multiple provider IDs found. found: [${qs("provider").mkString(",")}]")
      } else {
        
        val pid = UUID.fromString(qs(PROVIDER_KEY)(0))
        ResourceFactory.findById(ResourceIds.MarathonProvider, pid).fold {
          throw badRequest(s"Provider with ID '$pid' not found.")
        }{ _ => pid }
      }
    } match {
      case Success(id) => id
      case Failure(e)  => e match {
        case i: IllegalArgumentException => 
          throw badRequest(s"Invalid provider UUID. found: '${qs(PROVIDER_KEY)(0)}'")
        case e: Throwable => throw e
      }
    }
  }
  
  
  private def badRequest(message: String) = {
    new BadRequestException(message)
  }

  def lookupContainer(path: ResourcePath, user: AuthAccountWithCreds): Option[GestaltResourceInstance] = {
    Resource.fromPath(path.path) map transformMetaContainer
  }
  
  def lookupContainers(path: ResourcePath, account: AuthAccountWithCreds, qs: QueryString): List[GestaltResourceInstance] = {
    val rs = Resource.listFromPath(path.path)
    if (getExpandParam(qs)) rs map transformMetaContainer else rs
  }
  
  private[util] def transformMetaContainer(c: GestaltResourceInstance) = {
      val providerId  = getProviderId(c.properties.get("provider")) getOrElse {
        throw new RuntimeException(s"Could not parse provider ID from Meta Container.")
      }
      val provider    = MarathonController.marathonProvider(providerId)
      val client      = MarathonController.marathonClient(provider)
      val marathonId  = c.properties.get("external_id")

      val stats = Try {
        val application = Await.result(client.getApplication_marathon_v3(marathonId)(global), 5 seconds)
        MarathonClient.marathon2Container(application)
      } match {
        case Failure(e) => if (e.isInstanceOf[ResourceNotFoundException]) None else throw e
        case Success(s) => s
      }
      
      updateWithStats(c, stats)
  }
  
  private[util] def getProviderId(jstring: String): Option[UUID] = {    
    JsonUtil.getJsonField(Json.parse(jstring), "id") map { _.as[UUID] }
  }
  
  /**
   * Make a best effort to get updated stats from the Marathon provider and to update the resource with them  
   */  
  private[util] def updateWithStats(metaCon: GestaltResourceInstance, stats: Option[ContainerStats]) = {

    if (metaCon.properties.get.get("status").isDefined) metaCon
    else {
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
  
}