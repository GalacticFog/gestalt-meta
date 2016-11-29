package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.events.{AmqpEndpoint, PolicyEvent, AmqpClient, AmqpConnection}

import play.api.Logger

import play.api.libs.ws.WS
import play.api.Play.current
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

import com.galacticfog.gestalt.data.{Instance, ResourceFactory}
import com.galacticfog.gestalt.data.models.{ResourceLike, GestaltResourceInstance}
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.sdk.{GestaltResourceInput, ResourceIds}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.api.{ContainerInstance, ContainerSpec, Resource, ResourcePath}
import com.galacticfog.gestalt.events._
import scala.language.postfixOps


trait ContainerService extends MetaController {

  def futureToFutureTry[T](f: Future[T]): Future[Try[T]] = f.map(Success(_)).recover({case x => Failure(x)})

  def containerRequestOperations(action: String) = List(
    controllers.util.Authorize(action),
    controllers.util.EventsPre(action),
    controllers.util.PolicyCheck(action),
    controllers.util.EventsPost(action)
  )

  def containerRequestOptions(user: AuthAccountWithCreds,
                              environment: UUID,
                              container: GestaltResourceInstance,
                              data: Option[Map[String, String]] = None) = RequestOptions(
    user = user,
    authTarget = Option(environment),
    policyOwner = Option(environment),
    policyTarget = Option(container),
    data = data
  )

  def setupMigrateRequest(fqon: String,
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

  def deleteContainer(container: GestaltResourceInstance): Future[Unit] = {
    val providerId = Json.parse(container.properties.get("provider")) \ "id"
    val provider   = ResourceFactory.findById(UUID.fromString(providerId.as[String])) getOrElse {
      throw new RuntimeException("Could not find Provider : " + providerId)
    }
    // TODO: what to do if there is no external_id ? delete the resource? attempt to reconstruct external_id from resource?
    val maybeExternalId = for {
      props <- container.properties
      eid <- props.get("external_id")
    } yield eid

    maybeExternalId match {
      case Some(eid) => marathonClient(provider).deleteApplication(eid) map { js =>
        log.debug(s"response from MarathonClient.deleteApplication:\n${Json.prettyPrint(js)}")
      }
      case None =>
        log.debug(s"no external_id property in container ${container.id}, will not attempt delete against provider")
        Future.successful(())
    }
  }

  def findEnvironmentContainerByName(fqon: String, environment: UUID, containerName: String): Future[Option[(GestaltResourceInstance,Seq[ContainerInstance])]] = {
    val maybeContainerSpec = for {
      r <- ResourceFactory.findChildByName(parent = environment, childType = ResourceIds.Container, name = containerName)
      s <- ContainerSpec.fromResourceInstance(r).toOption
    } yield (r -> s)
    val maybeStatsFromMarathon = for {
      metaContainerSpec <- maybeContainerSpec
      provider <- Try {
        marathonProvider(metaContainerSpec._2.provider.id)
      }.toOption
      extId <- metaContainerSpec._2.external_id
      marathonApp = for {
        client <- Future.fromTry(Try(
          marathonClient(provider)
        ))
        js <- client.getApplicationByAppId(extId)
        stats <- Future.fromTry(Try {
          MarathonClient.marathon2Container(js).get
        })
      } yield stats
    } yield marathonApp
    maybeStatsFromMarathon match {
      case None => Future {
        maybeContainerSpec.map(containerSpec =>
          updateMetaContainerWithStats(containerSpec._1, None) -> Seq.empty
        )
      }
      case Some(fStatsFromMarathon) => fStatsFromMarathon.map(stats =>
        Some(updateMetaContainerWithStats(maybeContainerSpec.get._1, Some(stats)) -> Seq.empty)
      ) recover {
        case e: Throwable =>
          maybeContainerSpec.map(containerSpec =>
            updateMetaContainerWithStats(containerSpec._1, None) -> Seq.empty
          )
      }
    }
  }

  def listEnvironmentContainers(fqon: String, environment: UUID): Future[Seq[(GestaltResourceInstance,Seq[ContainerInstance])]] = {
    // make a best effort to get updated stats from the Marathon provider and to update the resource with them
    val env = ResourceFactory.findById(ResourceIds.Environment, environment) getOrElse throwBadRequest("UUID did not correspond to an environment")
    val wrk = (for {
      props <- env.properties
      parentId <- props.get("workspace")
      parentUUID <- Try{UUID.fromString(parentId)}.toOption
      workspaceResource <- ResourceFactory.findById(ResourceIds.Workspace, parentUUID)
    } yield workspaceResource) getOrElse throwBadRequest("could not find parent workspace for environment")

    val containerSpecsByProvider = ResourceFactory.findChildrenOfType(ResourceIds.Container, env.id) flatMap {
      r => ContainerSpec.fromResourceInstance(r).toOption zip(Some(r)) map (_.swap)
    } groupBy ( _._2.provider.id )

    val fStatsFromAllRelevantProviders = Future.traverse(containerSpecsByProvider.keys) { pid =>
      val pidAndStats = for {
        stats <- marathonClient(marathonProvider(pid)).listApplicationsInEnvironment(fqon, wrk.name, env.name)
        statsMap = stats map (stat => stat.id -> stat) toMap
      } yield (pid -> statsMap)
      futureToFutureTry(pidAndStats)
    } map (_ collect {case Success(x) => x} toMap)

    fStatsFromAllRelevantProviders map { pid2id2stats =>
      val allSpecs = containerSpecsByProvider map {
        case (pid, cspecs) =>
          cspecs map { case (cRes,cSpec) =>
            val maybeUpdate = for {
              eid <- cSpec.external_id
              providerStatList <- pid2id2stats.get(cSpec.provider.id)
              update <- providerStatList.get(eid)
            } yield update
            updateMetaContainerWithStats(cRes, maybeUpdate) -> Seq.empty
          }
      }
      allSpecs.flatten.toSeq
    }
  }

  def launchContainer(fqon: String,
                      workspace: GestaltResourceInstance,
                      environment: GestaltResourceInstance,
                      user: AuthAccountWithCreds,
                      containerSpec: ContainerSpec): Future[(GestaltResourceInstance, Seq[ContainerInstance])] = {

    def updateSuccessfulLaunch(resource: GestaltResourceInstance)(marathonResponse: JsValue): (GestaltResourceInstance, Seq[ContainerInstance]) = {
      val marathonAppId = (marathonResponse \ "id").as[String]
      val updatedResource = upsertProperties(resource,
        "external_id" -> marathonAppId,
        "status" -> "LAUNCHED"
      )
      ResourceFactory.update(updatedResource, user.account.id).get -> Seq.empty
    }

    def updateFailedLaunch(resource: GestaltResourceInstance)(t: Throwable): Throwable = {
      val updatedResource = upsertProperties(resource,
        "status" -> "LAUNCH_FAILED"
      )
      ResourceFactory.update(updatedResource, user.account.id).get -> Seq.empty
      log.error("launch failed", t)
      new BadRequestException(s"launch failed: ${t.getMessage}")
    }

    val orgId = orgFqon(fqon)
      .map(_.id)
      .getOrElse(throw new BadRequestException("launchContainer called with invalid fqon"))

    val containerResourceInput: GestaltResourceInput = ContainerSpec.toResourcePrototype(containerSpec)
    // log.trace("GestaltResourceInput from ContainerSpec: %s".format(Json.prettyPrint(Json.toJson(containerResourceInput))))
    val containerResourcePre: GestaltResourceInstance = withInputDefaults(orgId, containerResourceInput, user, None)
    // log.trace("GestaltResourceInstance from inputWithDefaults: %s".format(Json.prettyPrint(Json.toJson(containerResourcePre))))
    val operations = containerRequestOperations("container.create")
    val options = containerRequestOptions(user, environment.id, containerResourcePre)

    SafeRequest (operations, options) ProtectAsync { maybeState =>
      val provider = marathonProvider(containerSpec.provider.id)
      ResourceFactory.create(ResourceIds.User, user.account.id)(containerResourcePre, Some(environment.id)) match {
        case Failure(t) => Future.failed(t)
        case Success(resource) =>
          val marathonApp = toMarathonLaunchPayload(
            props = containerSpec,
            provider = provider
          )
          val marathonAppCreatePayload = Json.toJson(marathonApp).as[JsObject]
          marathonClient(provider).launchApp(
            fqon = fqon,
            wrkName = workspace.name,
            envName = environment.name,
            name = containerSpec.name,
            marPayload = marathonAppCreatePayload
          ).transform( updateSuccessfulLaunch(resource), updateFailedLaunch(resource) )
      }
    }
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

  protected [controllers] def updateMetaContainerWithStats(metaCon: GestaltResourceInstance, stats: Option[ContainerStats]) = {
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
      }
    )
    // this update is passive... mark the "modifier" as the last person to have actively modified the container, or the creator...
    (metaCon.modified.flatMap(_.get("id")) orElse metaCon.created.flatMap(_.get("id"))) flatMap {s => Try(UUID.fromString(s)).toOption} match {
      case None => metaCon // TODO: not sure what else to do here
      case Some(updater) =>
        Future{ ResourceFactory.update(updatedMetaCon, updater) } onComplete {
          case Success(Success(updatedContainer)) =>
            log.trace(s"updated container ${updatedContainer.id} with info from marathon")
          case Success(Failure(e)) =>
            log.warn(s"failure to update container ${updatedMetaCon.id}",e)
          case Failure(e) =>
            log.warn(s"failure to update container ${updatedMetaCon.id}",e)
        }
        updatedMetaCon
    }
  }

  private def badRequest(message: String) = {
    new BadRequestException(message)
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

  def findWorkspaceEnvironment(environmentId: UUID): Try[(GestaltResourceInstance, GestaltResourceInstance)] = Try {
    val p = ResourceFactory.findParent(ResourceIds.Workspace, environmentId) getOrElse {
      throw new ResourceNotFoundException(s"Could not find parent Workspace for Environment '$environmentId'.")
    }
    val c = ResourceFactory.findById(ResourceIds.Environment, environmentId) getOrElse {
      throw new ResourceNotFoundException(s"Environment with ID '$environmentId' not found.")
    }
    (p -> c)
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

}
