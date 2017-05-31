package controllers.util

import java.util.UUID

import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.util.{Failure, Success, Try}
import scala.concurrent.{Await, Future}
import com.galacticfog.gestalt.data.{Instance, ResourceFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.api.patch.PatchInstance
import com.galacticfog.gestalt.meta.api.{ContainerInstance, ContainerSpec}
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.patch.PatchDocument
import com.google.inject.Inject
import controllers.DeleteController
import play.api.http.HttpVerbs
import play.api.mvc.RequestHeader
import play.api.test.FakeRequest
import services.{FakeURI, ProviderContext}

import scala.concurrent.duration._
import scala.language.postfixOps

trait ContainerService extends JsonInput {

  def deleteContainer(container: GestaltResourceInstance, identity: AuthAccountWithCreds, request: RequestHeader): Future[Unit]

  def getEnvironmentContainer(fqon: String, environment: UUID, containerId: UUID): Future[Option[(GestaltResourceInstance,Seq[ContainerInstance])]]

  def listEnvironmentContainers(fqon: String, environment: UUID): Future[Seq[(GestaltResourceInstance,Seq[ContainerInstance])]]

  def createContainer(context: ProviderContext,
                      user: AuthAccountWithCreds,
                      containerSpec: ContainerSpec,
                      userRequestedId : Option[UUID] = None ): Future[GestaltResourceInstance]

  def patchContainer(container: GestaltResourceInstance, patch: PatchDocument, user: AuthAccountWithCreds, request: RequestHeader): Future[GestaltResourceInstance]

}

object ContainerService {

  def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*): Instance = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }

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

  def setupPromoteRequest(fqon: String,
                          env: UUID,
                          container: Instance,
                          user: AuthAccountWithCreds,
                          metaUrl: String,
                          queryString: QueryString) = {
    val action = "container.promote"
    val operations = List(
      controllers.util.Authorize(action),
      controllers.util.PolicyCheck(action),
      controllers.util.EventsPre(action)
    )
    val options = RequestOptions(
      user,
      authTarget = Option(env),
      policyOwner = Option(env),
      policyTarget = Option(container),
      data = Option(Map(
        "fqon"           -> fqon,
        "meta_url"       -> System.getenv().getOrDefault("META_POLICY_CALLBACK_URL",metaUrl),
        "environment_id" -> env.toString,
        "target_env_id"  -> targetEnvQueryParam(queryString).get.toString
      ))
    )
    (operations,options)
  }

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
        "meta_url"       -> System.getenv().getOrDefault("META_POLICY_CALLBACK_URL",metaUrl),
        "environment_id" -> env.toString,
        "provider_id"    -> providerQueryParam(queryString).get.toString))
    )
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
        ResourceFactory.findById(pid).fold {
          throw badRequest(s"Provider with ID '$pid' not found.")
        }{ res =>
          import com.galacticfog.gestalt.data.CoVariant
          // Ensure resource we got is a sub-type of CaasProvider
          val caasids = ResourceFactory.findTypesWithVariance(CoVariant(ResourceIds.CaasProvider)) map { _.id }
          if (caasids.contains(res.typeId)) pid
          else throw badRequest(s"Given ID '$pid' is not a CaaS Provider. No changes made.")
        }
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

  /**
    * Extract and validate the 'targetEnv' querystring parameter.
    * Used by the {@link #promoteContainer(String,UUID,UUID) migrateContainer} method.
    *
    * @param qs the complete, unmodified queryString from the original request.
    */
  protected [controllers] def targetEnvQueryParam(qs: Map[String,Seq[String]]): Try[UUID] = {
    val TGT_KEY = "target"
    qs.get(TGT_KEY) match {
      case Some(Seq(tgt)) if tgt.trim.nonEmpty =>
        for {
          eid <- Try{UUID.fromString(tgt)}
          resId <- ResourceFactory.findById(ResourceIds.Environment, eid).fold[Try[UUID]] {
            Failure(badRequest(s"Environment with ID '$eid' not found."))
          } { r => Success(r.id) }
        } yield resId
      case None => Failure(badRequest(s"'${TGT_KEY}' parameter not found. (i.e. */promote?${TGT_KEY}={UUID})"))
      case _    => Failure(badRequest(s"Multiple target IDs found. found: [${qs(TGT_KEY).mkString(",")}]"))
    }
  }

  private def badRequest(message: String) = {
    new BadRequestException(message)
  }

  def containerProviderId(c: GestaltResourceInstance): UUID = {
    val pid = (Json.parse(c.properties.get("provider")) \ "id").as[String]
    UUID.fromString(pid)
  }

  def caasProvider(provider: UUID): GestaltResourceInstance = {
    ResourceFactory.findById(provider) filter {
      Set(ResourceIds.DcosProvider,ResourceIds.KubeProvider) contains _.typeId
    } getOrElse {
      throw new ResourceNotFoundException(s"CaaS provider with ID '$provider' not found.")
    }
  }

}

class ContainerServiceImpl @Inject() ( providerManager: ProviderManager, deleteController: DeleteController )
  extends ContainerService with MetaControllerUtils {

  import ContainerService._

  val CAAS_PROVIDER_TIMEOUT_MS = 5000


  override def getEnvironmentContainer(fqon: String, environment: UUID, containerId: UUID): Future[Option[(Instance, Seq[ContainerInstance])]] = {
    log.debug("***Finding container by id...")
    // Find container resource in Meta, convert to ContainerSpec
    log.debug(s"***ENVIRONMENT: $environment, id: $containerId")
    val maybeContainerSpec = for {
      r <- ResourceFactory.findChildrenOfType(parentId = environment, typeId = ResourceIds.Container) find {_.id == containerId}
      s <- ContainerSpec.fromResourceInstance(r).toOption
    } yield (r -> s)

    log.debug("***Getting stats...")
    val fMaybeUpdate = (for {
      metaContainerSpec <- maybeContainerSpec
      provider <- Try { caasProvider(metaContainerSpec._2.provider.id) }.toOption
      saasProvider <- providerManager.getProviderImpl(provider.typeId).toOption
      ctx = ProviderContext(new FakeURI(s"/${fqon}/environments/${environment}/containers"), provider.id, Some(metaContainerSpec._1))
      stats = saasProvider.find(ctx, metaContainerSpec._1)
    } yield stats) getOrElse Future.successful(None) recover { case e: Throwable => None }

    fMaybeUpdate map {
      maybeUpdate => maybeContainerSpec map {
        case (containerResource,containerSpec) => updateMetaContainerWithStats(containerResource, maybeUpdate) -> Seq.empty
      }
    }
  }

  def listEnvironmentContainers(fqon: String, environment: UUID): Future[Seq[(GestaltResourceInstance,Seq[ContainerInstance])]] = {
    // make a best effort to get updated stats from the CaaS provider and to update the resource with them

    log.debug(s"CaaSService::listEnvironmentContainers($fqon, $environment)")

    val env = ResourceFactory.findById(ResourceIds.Environment, environment) getOrElse throwBadRequest("UUID did not correspond to an environment")

    log.debug("Found environment...Looking up workspace...")

    val wrk = (for {
      props <- env.properties
      parentId <- props.get("workspace")
      parentUUID <- Try{UUID.fromString(parentId)}.toOption
      workspaceResource <- ResourceFactory.findById(ResourceIds.Workspace, parentUUID)
    } yield workspaceResource) getOrElse throwBadRequest("could not find parent workspace for environment")

    log.debug(s"Found workspace: ${wrk.name}[${wrk.id}]")

    val containerSpecsByProvider = ResourceFactory.findChildrenOfType(ResourceIds.Container, env.id) flatMap {
      r => ContainerSpec.fromResourceInstance(r).toOption zip(Some(r)) map (_.swap)
    } groupBy ( _._2.provider.id )

    val fStatsFromAllRelevantProviders: Future[Map[UUID, Map[String, ContainerStats]]] = Future.traverse(containerSpecsByProvider.keys) { pid =>
      val cp = caasProvider(pid)
      val ctx = ProviderContext(new FakeURI(s"/${fqon}/environments/${environment}/containers"), cp.id, None)
      val pidAndStats = for {
        caasP <- Future.fromTry(providerManager.getProviderImpl(cp.typeId))
        stats <- caasP.listInEnvironment(ctx)
        statsMap = stats map (stat => stat.external_id -> stat) toMap
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

  protected [controllers] def updateMetaContainerWithStats(metaCon: GestaltResourceInstance, stats: Option[ContainerStats]): Instance = {
    // TODO: do not overwrite status if currently MIGRATING: https://gitlab.com/galacticfog/gestalt-meta/issues/117
    val newStats = stats match {
      case Some(stats) => Seq(
        "age" -> stats.age.toString,
        "status" -> stats.status,
        "num_instances" -> stats.numInstances.toString,
        "tasks_running" -> stats.tasksRunning.toString,
        "tasks_healthy" -> stats.tasksHealthy.toString,
        "tasks_unhealthy" -> stats.tasksUnhealthy.toString,
        "tasks_staged" -> stats.tasksStaged.toString,
        "instances"       -> stats.taskStats.map{Json.toJson(_).toString}.getOrElse("[]")
      )
      case None => Seq(
        "status" -> "LOST",
        "num_instances" -> "0",
        "tasks_running" -> "0",
        "tasks_healthy" -> "0",
        "tasks_unhealthy" -> "0",
        "tasks_staged" -> "0",
        "instances"       -> "[]",
        "service_addresses" -> "[]"
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
            log.trace(s"updated container ${updatedContainer.id} with info from CaaS provider")
          case Success(Failure(e)) =>
            log.warn(s"failure to update container ${updatedMetaCon.id}",e)
          case Failure(e) =>
            log.warn(s"failure to update container ${updatedMetaCon.id}",e)
        }
        updatedMetaCon
    }
  }

  def createContainer(context: ProviderContext,
                      user: AuthAccountWithCreds,
                      containerSpec: ContainerSpec,
                      userRequestedId : Option[UUID] = None ): Future[GestaltResourceInstance] = {

    val input = ContainerSpec.toResourcePrototype(containerSpec).copy( id = userRequestedId )
    val proto = withInputDefaults(context.workspace.orgId, input, user, None)
    val operations = containerRequestOperations("container.create")
    val options    = containerRequestOptions(user, context.environmentId, proto)

    SafeRequest (operations, options) ProtectAsync { _ =>

      // TODO: we need an entitlement check (for now: Read; later: Execute/Launch) before allowing the user to use this provider
      val provider = caasProvider(containerSpec.provider.id)
      val containerResourcePre = upsertProperties(proto, "provider" -> Json.obj(
        "name" -> provider.name,
        "id" -> provider.id
      ).toString)

      for {
        metaResource <- Future.fromTry{
          log.debug("Creating container resource in Meta")
          ResourceFactory.create(ResourceIds.User, user.account.id)(containerResourcePre, Some(context.environmentId))
        }
        _ = log.info("Meta container created: " + metaResource.id)
        service      <- Future.fromTry{
          log.debug("Retrieving CaaSService from ProviderManager")
          providerManager.getProviderImpl(context.provider.typeId)
        }
        instanceWithUpdates <- {
          log.info("Creating container in backend CaaS...")
          service.create(context, metaResource)
        }
        updatedInstance <- Future.fromTry(ResourceFactory.update(instanceWithUpdates, user.account.id))
      } yield updatedInstance
    }
  }

  def deleteContainer(container: GestaltResourceInstance, identity: AuthAccountWithCreds, request: RequestHeader): Future[Unit] = {
    // just a convenient interface for testing... we'll let DeleteController do this for us
    deleteController.deleteResource(container, identity)(request)
  }

  override def patchContainer(origContainer: Instance, patch: PatchDocument, user: AuthAccountWithCreds, request: RequestHeader): Future[GestaltResourceInstance] = {
    for {
      patched <- Future.fromTry(PatchInstance.applyPatch(origContainer, patch)).mapTo[GestaltResourceInstance]
      containerSpec <- Future.fromTry(ContainerSpec.fromResourceInstance(patched))
      provider <- Future.fromTry(Try(caasProvider(containerSpec.provider.id)))
      context <- Future.fromTry(Try(ProviderContext(request, provider.id, Some(patched))))
      service <- Future.fromTry(providerManager.getProviderImpl(context.provider.typeId))
      updated <- service.update(context, patched)
    } yield updated
  }
}
