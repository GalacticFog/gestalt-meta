package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.events.{AmqpEndpoint, PolicyEvent, AmqpClient, AmqpConnection}

import play.api.libs.ws.WS
import play.api.Play.current
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._

import com.galacticfog.gestalt.data.{HardDeleteWorkspace, Instance, ResourceFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.api.{ContainerSpec, Resource, ResourcePath}
import com.galacticfog.gestalt.events._

trait ContainerService extends MetaController {

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

  def deleteContainer(fqon: String, workspace: Instance, environment: Instance, containerId: UUID): Future[ContainerSpec] = {
    ???
  }

  def listEnvironmentContainers(fqon: String, workspace: Instance, environment: Instance): Future[Seq[ContainerSpec]] = {
    ???
  }

  def findEnvironmentContainerByName(fqon: String, workspace: Instance, environment: Instance, containerName: String): Future[Option[ContainerSpec]] = {
    ???
  }

  def launchContainer(fqon: String,
                      workspace: GestaltResourceInstance,
                      environment: GestaltResourceInstance,
                      name: String,
                      inputProperties: ContainerSpec): Future[ContainerSpec] = {
    ???
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

  protected [controllers] def updateMetaContainerWithStats(metaCon: GestaltResourceInstance, stats: Option[ContainerStats], creatorId: UUID) = {
    // TODO: do not overwrite status if currently MIGRATING: https://gitlab.com/galacticfog/gestalt-meta/issues/117
    val newStats = stats match {
      case Some(stats) => Seq(
        "age" -> stats.age.toString,
        "status" -> stats.status,
        "num_instances" -> stats.numInstances.toString,
        "tasks_running" -> stats.tasksRunning.toString,
        "tasks_healthy" -> stats.tasksHealthy.toString,
        "tasks_unhealthy" -> stats.tasksUnhealthy.toString,
        "tasks_staged" -> stats.tasksStaged.toString
      )
      case None => Seq(
        "status" -> "LOST",
        "num_instances" -> "0",
        "tasks_running" -> "0",
        "tasks_healthy" -> "0",
        "tasks_unhealthy" -> "0",
        "tasks_staged" -> "0"
      )
    }
    val updatedMetaCon = metaCon.copy(
      properties = metaCon.properties map {
        _ ++ newStats
      } orElse {
        Some(newStats toMap)
      })
    Future{ ResourceFactory.update(updatedMetaCon, creatorId) } onComplete {
      case Success(Success(updatedContainer)) =>
        log.trace(s"updated container ${updatedContainer.id} with info from marathon")
      case Success(Failure(e)) =>
        log.warn(s"failure to update container ${updatedMetaCon.id}",e)
      case Failure(e) =>
        log.warn(s"failure to update container ${updatedMetaCon.id}",e)
    }
    updatedMetaCon
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
    val provider    = marathonProvider(providerId)
    val client      = marathonClient(provider)
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

  def marathonClient(provider: GestaltResourceInstance): MarathonClient = {
    val providerUrl = (Json.parse(provider.properties.get("config")) \ "url").as[String]
    log.debug("Marathon URL: " + providerUrl)
    MarathonClient(WS.client, providerUrl)
  }

  def marathonProvider(provider: UUID): GestaltResourceInstance = {
    ResourceFactory.findById(ResourceIds.MarathonProvider, provider) getOrElse {
      throw new ResourceNotFoundException(s"MarathonProvider with ID '$provider' not found.")
    }
  }

  def deleteMarathonApp(fqon: String, workspaceName: String, environmentName: String, container: GestaltResourceInstance): Future[JsValue] = {
    val provider = marathonProvider(containerProviderId(container))
    marathonClient(provider).deleteApplication( fqon, workspaceName, environmentName, container.name)
  }


  def appComponents(environment: UUID) = Try {
    val we = findWorkspaceEnvironment(environment).get
    (we._1, we._2)
  }


  def eventsClient() = {
    AmqpClient(AmqpConnection(RABBIT_HOST, RABBIT_PORT, heartbeat = 300))
  }

  def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }

  def publishEvent(event: PolicyEvent) = {
    eventsClient.publish(AmqpEndpoint(RABBIT_EXCHANGE, RABBIT_ROUTE), event.toJson)
  }

  def containerProviderId(c: GestaltResourceInstance): UUID = {
    val pid = (Json.parse(c.properties.get("provider")) \ "id").as[String]
    log.debug("Provider-ID : " + pid)
    UUID.fromString(pid)
  }

  def findWorkspaceEnvironment(envId: UUID) = Try {
    val p = ResourceFactory.findParent(ResourceIds.Workspace, envId) getOrElse {
      throw new ResourceNotFoundException(s"Could not find parent Workspace for Environment '$envId'.")
    }
    val c = ResourceFactory.findById(ResourceIds.Environment, envId) getOrElse {
      throw new ResourceNotFoundException(s"Environment with ID '$envId' not found.")
    }
    (p -> c)
  }

}

object ContainerServiceImpl extends ContainerService {}
