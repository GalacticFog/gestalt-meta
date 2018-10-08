package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.data.{Instance, ResourceFactory}
import com.galacticfog.gestalt.meta.api.ContainerSpec.{ExistingVolumeMountSpec, InlineVolumeMountSpec}
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.meta.api.errors.{BadRequestException, InternalErrorException, ResourceNotFoundException}
import com.galacticfog.gestalt.meta.api.patch.PatchInstance
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceStates}
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.patch.PatchDocument
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.google.inject.Inject
import controllers.DeleteController
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc.RequestHeader
import services.{FakeURI, ProviderContext}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

trait ContainerService extends JsonInput {

  def createSecret(context: ProviderContext,
                   user: AuthAccountWithCreds,
                   secretSpec: SecretSpec,
                   userRequestedId: Option[UUID]): Future[GestaltResourceInstance]

  def getEnvironmentContainer(fqon: String, environment: UUID, containerId: UUID): Future[Option[(GestaltResourceInstance, Seq[ContainerInstance])]]

  def listEnvironmentContainers(fqon: String, environment: UUID): Future[Seq[(GestaltResourceInstance, Seq[ContainerInstance])]]

  def createContainer(context: ProviderContext,
                      user: AuthAccountWithCreds,
                      containerSpec: ContainerSpec,
                      userRequestedId: Option[UUID] = None): Future[GestaltResourceInstance]

  def createVolume(context: ProviderContext,
                   user: AuthAccountWithCreds,
                   volumeSpec: VolumeSpec,
                   userRequestedId: Option[UUID] = None): Future[GestaltResourceInstance]

  def patchContainer(container: GestaltResourceInstance, patch: PatchDocument, user: AuthAccountWithCreds, request: RequestHeader): Future[GestaltResourceInstance]

  def patchVolume(volume: GestaltResourceInstance, patch: PatchDocument, user: AuthAccountWithCreds, request: RequestHeader): Future[GestaltResourceInstance]

  def updateContainer(context: ProviderContext, container: GestaltResourceInstance, user: AuthAccountWithCreds, request: RequestHeader): Future[GestaltResourceInstance]

}

object ContainerService {

  val log = Logger(this.getClass)

  def upsertProperties(resource: GestaltResourceInstance, values: (String, String)*): Instance = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }

  def futureToFutureTry[T](f: Future[T]): Future[Try[T]] = f.map(Success(_)).recover({ case x => Failure(x) })

  def caasObjectRequestOperations(action: String) = List(
    controllers.util.Authorize(action),
    controllers.util.EventsPre(action),
    controllers.util.PolicyCheck(action),
    controllers.util.EventsPost(action))

  def caasObjectRequestOptions(user: AuthAccountWithCreds,
                               environment: UUID,
                               caasObject: GestaltResourceInstance,
                               data: Option[Map[String, String]] = None) = RequestOptions(
    user = user,
    authTarget = Option(environment),
    policyOwner = Option(environment),
    policyTarget = Option(caasObject),
    data = data)

  def setupPromoteRequest(fqon: String,
                          source_env_id: UUID,
                          container: Instance,
                          user: AuthAccountWithCreds,
                          metaUrl: String,
                          target_env_id: UUID) = {
    val action = "container.promote"
    val operations = List(
      controllers.util.Authorize(action),
      controllers.util.PolicyCheck(action),
      controllers.util.EventsPre(action))
    val options = RequestOptions(
      user,
      authTarget = Option(target_env_id),
      policyOwner = Option(target_env_id),
      policyTarget = Option(container),
      data = Option(Map(
        "fqon" -> fqon,
        "meta_url" -> System.getenv().getOrDefault("META_POLICY_CALLBACK_URL", metaUrl),
        "environment_id" -> source_env_id.toString,
        "target_env_id" -> target_env_id.toString)))
    (operations, options)
  }

  def setupMigrateRequest(fqon: String,
                          env: UUID,
                          container: GestaltResourceInstance,
                          user: AuthAccountWithCreds,
                          metaUrl: String,
                          queryString: Map[String, Seq[String]]) = {
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
        "fqon" -> fqon,
        "meta_url" -> System.getenv().getOrDefault("META_POLICY_CALLBACK_URL", metaUrl),
        "environment_id" -> env.toString,
        "provider_id" -> providerQueryParam(queryString).get.toString)))
    (operations, options)
  }

  /**
   * Extract and validate the 'provider' querystring parameter.
   * Used by the {@link #migrateContainer(String,UUID,UUID) migrateContainer} method.
   *
   * @param qs the complete, unmodified queryString from the original request.
   */
  protected[controllers] def providerQueryParam(qs: Map[String, Seq[String]]): Try[UUID] = Try {
    val PROVIDER_KEY = "provider"

    if (!qs.contains(PROVIDER_KEY) || qs(PROVIDER_KEY)(0).trim.isEmpty)
      throw badRequest(
        "'provider' parameter not found. (i.e. */migrate?provider={UUID})")
    else Try {
      if (qs(PROVIDER_KEY).size > 1) {
        throw badRequest(s"Multiple provider IDs found. found: [${qs("provider").mkString(",")}]")
      } else {

        val pid = UUID.fromString(qs(PROVIDER_KEY)(0))
        ResourceFactory.findById(pid).fold {
          throw badRequest(s"Provider with ID '$pid' not found.")
        } { res =>
          import com.galacticfog.gestalt.data.CoVariant
          // Ensure resource we got is a sub-type of CaasProvider
          val caasids = ResourceFactory.findTypesWithVariance(CoVariant(ResourceIds.CaasProvider)) map { _.id }
          if (caasids.contains(res.typeId)) pid
          else throw badRequest(s"Given ID '$pid' is not a CaaS Provider. No changes made.")
        }
      }
    } match {
      case Success(id) => id
      case Failure(e) => e match {
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
  protected[controllers] def targetEnvQueryParam(qs: Map[String, Seq[String]]): Try[UUID] = {
    val TGT_KEY = "target"
    qs.get(TGT_KEY) match {
      case Some(Seq(tgt)) if tgt.trim.nonEmpty =>
        for {
          eid <- Try { UUID.fromString(tgt) }
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

  def containerProviderId(c: ResourceLike): UUID = {
    val pid = for {
      props <- c.properties
      provider <- props.get("provider")
      parsed <- Try(Json.parse(provider)).toOption
      pid <- (parsed \ "id").asOpt[String]
      uuid <- Try(UUID.fromString(pid)).toOption
    } yield uuid
    pid getOrElse {
      throw new ResourceNotFoundException(
        s"Could not parse provider ID from container '${c.id}'")
    }
  }

  def caasProvider(provider: UUID): GestaltResourceInstance = {
    ResourceFactory.findById(provider) filter {
      // TODO: this should just check that its a sub-type of ::CaaS provider
      Set(ResourceIds.DcosProvider, ResourceIds.KubeProvider, ResourceIds.DockerProvider, migrations.V14.ECS_PROVIDER_TYPE_ID) contains _.typeId
    } getOrElse {
      throw new BadRequestException(s"Provider with ID '$provider' is absent or not a recognized CaaS provider. Associated container may be corrupt.")
    }
  }

  /**
   * Lookup and return the Provider configured for the given Container.
   */
  def containerProvider(container: ResourceLike): GestaltResourceInstance = {
    val providerId = containerProviderId(container)
    caasProvider(providerId)
  }

  /**
   * Lookup and return the external_id property
   */
  def resourceExternalId(resource: ResourceLike): Option[String] = {
    for {
      props <- resource.properties
      eid <- props.get("external_id")
    } yield eid
  }

  def getProviderConfig(provider: ResourceLike): Option[JsValue] = {
    for {
      props <- provider.properties
      configProp <- props.get("config")
      config <- Try { Json.parse(configProp) }.toOption
    } yield config
  }

  def getProviderProperty[T](provider: ResourceLike, propName: String)(implicit rds: Reads[T]): Option[T] = for {
    config <- getProviderConfig(provider)
    prop <- (config \ propName).validate[T].asOpt
  } yield prop

  def deleteSecretHandler(providerManager: ProviderManager, res: Instance): Try[Unit] = {
    val provider = containerProvider(res)
    providerManager.getProviderImpl(provider.typeId) map { service =>
      Await.result(service.destroySecret(res), 10.seconds)
    }
  }

  def deleteVolumeHandler(providerManager: ProviderManager, res: Instance): Try[Unit] = {
    val provider = containerProvider(res)
    providerManager.getProviderImpl(provider.typeId) map { service =>
      Await.result(service.destroyVolume(res), 10.seconds)
    }
  }

  def deleteContainerHandler(providerManager: ProviderManager, res: Instance): Try[Unit] = {
    val result = for {
      provider <- Try{containerProvider(res)}
      service <-  providerManager.getProviderImpl(provider.typeId)
      result <- Try {
        log.info(s"Attempting to delete container ${res.id} from CaaS Provider ${provider.id}")
        Await.result(service.destroy(res), 5 seconds)
      }
    } yield result
    result recoverWith {
      case _: scala.concurrent.TimeoutException => Failure(new InternalErrorException("timed out waiting for external CaaS service to respond"))
    }
  }


}

class ContainerServiceImpl @Inject() (providerManager: ProviderManager, deleteController: DeleteController)
    extends ContainerService with MetaControllerUtils with JsonInput {

  import ContainerService._

  val CAAS_PROVIDER_TIMEOUT_MS = 5000

  override def getEnvironmentContainer(fqon: String, environment: UUID, containerId: UUID): Future[Option[(Instance, Seq[ContainerInstance])]] = {
    val maybeMetaContainer = for {
      r <- ResourceFactory.findChildrenOfType(parentId = environment, typeId = ResourceIds.Container) find { _.id == containerId }
      s <- ContainerSpec.fromResourceInstance(r).toOption
    } yield (r -> s)

    val fMaybeUpdate = (for {
      (metaContainer, metaContainerSpec) <- maybeMetaContainer
      provider <- Try { caasProvider(metaContainerSpec.provider.id) }.toOption
      caasProviderImpl <- providerManager.getProviderImpl(provider.typeId).toOption
      ctx = ProviderContext(new FakeURI(s"/${fqon}/environments/${environment}/containers"), provider.id, Some(metaContainer))
      stats = caasProviderImpl.find(ctx, metaContainer)
    } yield stats).getOrElse(Future.successful(None)) recover {

      case ce: java.net.ConnectException =>
        log.error("Error connecting to CaaS provider", ce)
        None

      case e: Throwable =>
        log.warn(s"error fetching stats for container ${containerId} from provider", e)
        None
    }

    fMaybeUpdate map {
      maybeUpdate =>
        maybeMetaContainer map {
          case (containerResource, _) => updateMetaContainerWithStats(containerResource, maybeUpdate) -> Seq.empty
        }
    }
  }

  def listEnvironmentContainers(fqon: String, environment: UUID): Future[Seq[(GestaltResourceInstance, Seq[ContainerInstance])]] = {
    // make a best effort to get updated stats from the CaaS provider and to update the resource with them

    log.debug(s"CaaSService::listEnvironmentContainers($fqon, $environment)")

    val env = ResourceFactory.findById(ResourceIds.Environment, environment) getOrElse throwBadRequest("UUID did not correspond to an environment")

    log.debug("Found environment...Looking up workspace...")

    val wrk = (for {
      props <- env.properties
      parentId <- props.get("workspace")
      parentUUID <- Try { UUID.fromString(parentId) }.toOption
      workspaceResource <- ResourceFactory.findById(ResourceIds.Workspace, parentUUID)
    } yield workspaceResource) getOrElse throwBadRequest("could not find parent workspace for environment")

    log.debug(s"Found workspace: ${wrk.name}[${wrk.id}]")

    val containerSpecsByProvider = ResourceFactory.findChildrenOfType(ResourceIds.Container, env.id) flatMap {
      r => ContainerSpec.fromResourceInstance(r).toOption zip (Some(r)) map (_.swap)
    } groupBy (_._2.provider.id)

    val fStatsFromAllRelevantProviders: Future[Map[UUID, Map[String, ContainerStats]]] = Future.traverse(containerSpecsByProvider.keys) { pid =>
      val provider = caasProvider(pid)
      val ctx = ProviderContext(new FakeURI(s"/${fqon}/environments/${environment}/containers"), provider.id, None)
      val pidAndStats = for {
        caasProviderImpl <- Future.fromTry(providerManager.getProviderImpl(provider.typeId))
        stats <- caasProviderImpl.listInEnvironment(ctx)
        statsMap = stats map (stat => stat.external_id -> stat) toMap
      } yield (pid -> statsMap)
      futureToFutureTry(pidAndStats)
    } map (_ collect { case Success(x) => x } toMap)

    fStatsFromAllRelevantProviders map { pid2id2stats =>
      val allSpecs = containerSpecsByProvider map {
        case (_, cspecs) =>
          cspecs map {
            case (cRes, cSpec) =>
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

  protected[controllers] def updateMetaContainerWithStats(metaCon: GestaltResourceInstance, stats: Option[ContainerStats]): Instance = {
    // TODO: do not overwrite status if currently MIGRATING: https://gitlab.com/galacticfog/gestalt-meta/issues/117
    val newProperties = stats match {
      case Some(stats) =>
        val pms = Try{
          Json.parse(metaCon.properties.get("port_mappings")).as[Seq[ContainerSpec.PortMapping]]
        }.getOrElse(Seq.empty).map {
          _ match {
            case lbPm if lbPm.expose_endpoint.contains(true) && lbPm.`type`.contains("loadBalancer") && lbPm.container_port.orElse(lbPm.lb_port).isDefined =>
              lbPm.copy(
                lb_address = stats.lb_address.map {
                  a => ContainerSpec.ServiceAddress(
                    host = a,
                    protocol = Some("http"),
                    port = lbPm.lb_port.orElse(lbPm.container_port).getOrElse(0)
                  )
                }
              )
            case other => other.copy(
              lb_address = None
            )
          }
        }
        Seq(
          "age" -> stats.age.toString,
          "status" -> stats.status,
          "num_instances" -> stats.numInstances.toString,
          "tasks_running" -> stats.tasksRunning.toString,
          "tasks_healthy" -> stats.tasksHealthy.toString,
          "tasks_unhealthy" -> stats.tasksUnhealthy.toString,
          "tasks_staged" -> stats.tasksStaged.toString,
          "instances" -> stats.taskStats.map { Json.toJson(_).toString }.getOrElse("[]"),
          "port_mappings" -> Json.toJson(pms).toString,
          "events" -> stats.events.map { Json.toJson(_).toString() }.getOrElse("[]"),
          "status_detail" -> stats.states.map { Json.toJson(_).toString() }.getOrElse("[]")
        )
      case None if metaCon.state == ResourceStates.Failed => Seq(
        "status" -> "FAILED",
        "num_instances" -> "0",
        "tasks_running" -> "0",
        "tasks_healthy" -> "0",
        "tasks_unhealthy" -> "0",
        "tasks_staged" -> "0",
        "instances" -> "[]",
        "service_addresses" -> "[]"
      )
      case None => Seq(
        "status" -> "LOST",
        "num_instances" -> "0",
        "tasks_running" -> "0",
        "tasks_healthy" -> "0",
        "tasks_unhealthy" -> "0",
        "tasks_staged" -> "0",
        "instances" -> "[]",
        "service_addresses" -> "[]"
      )
    }
    metaCon.copy(
      properties = Some(metaCon.properties.getOrElse(Map.empty) ++ newProperties.toMap)
    )
  }

  /**
    * From a gestalt container resource, parse .properties.volumes, and convert any InlineVolumeMountSpec into ExistingVolumeMountSpec by creating volume resources
    *
    * @param container
    * @return all of the ExistingVolumeMountSpec object, for the previous and newly-created volumes
    */
  def createInlineContainerVolumes(context: ProviderContext, user: AuthAccountWithCreds, container: Either[GestaltResourceInstance,ContainerSpec]): Future[Seq[ContainerSpec.ExistingVolumeMountSpec]] = {
    val containerSpec = container.fold[ContainerSpec](
      r => ContainerSpec.fromResourceInstance(r).get, identity
    )
    val inlineVolMounts = containerSpec.volumes collect {
      case i: InlineVolumeMountSpec =>
        val spec = VolumeSpec.fromResourceInstance(resourceWithDefaults(
          org = context.workspace.orgId,
          input = i.volume_resource,
          creator = user,
          typeId = Some(migrations.V13.VOLUME_TYPE_ID)
        )).get
        spec.provider match {
          case Some(provider) if provider.id != containerSpec.provider.id =>
            throw new BadRequestException("inline volume specification must have the same provider as the container that is being created")
          case _ =>
            i.mount_path -> spec.copy(
              provider = Some(containerSpec.provider)
            )
        }
    }
    val existingVolMounts = containerSpec.volumes collect {
      case e: ExistingVolumeMountSpec =>
        val ok = for {
          v <- ResourceFactory.findById(migrations.V13.VOLUME_TYPE_ID, e.volume_id)
          vs <- VolumeSpec.fromResourceInstance(v).toOption
          prov <- vs.provider
          if prov.id == containerSpec.provider.id
        } yield true
        if (ok.contains(true)) e else throw new BadRequestException("container can only mount volumes from the same CaaS provider")
    }
    // convert all inlineVolMounts to existingVolMounts by creating volume instances from them
    val v = Future.traverse(inlineVolMounts) {
      case (mntPath,inlineSpec) =>
        this.createVolume(context, user, inlineSpec, None) map {
          volumeInstance => ExistingVolumeMountSpec(
            mount_path = mntPath,
            volume_id = volumeInstance.id
          )
        }
    }
    v map {
      _ ++ existingVolMounts
    }
  }

  def updateContainer(context: ProviderContext,
                      container: Instance,
                      user: AuthAccountWithCreds,
                      request: RequestHeader): Future[Instance] = {
    import ContainerSpec.existingVMSFmt

    val operations = caasObjectRequestOperations("container.update")
    val options = caasObjectRequestOptions(user, context.environmentId, container)
    SafeRequest(operations, options) ProtectAsync { _ =>

      for {
        volMounts <- createInlineContainerVolumes(context, user, Left(container))
        containerWithMounts = upsertProperties(
          container,
          "volumes" -> Json.toJson(volMounts).toString
        )
        service <- Future.fromTry {
          log.debug("Retrieving CaaSService from ProviderManager")
          providerManager.getProviderImpl(context.provider.typeId)
        }
        instanceWithUpdates <- {
          log.info("Updating container in backend CaaS...")
          service.update(context, containerWithMounts)
        }
        updatedInstance <- Future.fromTry(ResourceFactory.update(instanceWithUpdates, user.account.id))
      } yield updatedInstance
    }
  }

  def createContainer(context: ProviderContext,
                      user: AuthAccountWithCreds,
                      containerSpec: ContainerSpec,
                      userRequestedId: Option[UUID] = None): Future[GestaltResourceInstance] = {

    import ContainerSpec.existingVMSFmt

    val input = ContainerSpec.toResourcePrototype(containerSpec).copy(id = userRequestedId)
    val proto = resourceWithDefaults(context.workspace.orgId, input, user, None)
    val containerOps = caasObjectRequestOperations("container.create")
    // TODO: should also have provider.view for the selected provider, as well as (maybe) volume.create for inline volume creation
    val options = caasObjectRequestOptions(user, context.environmentId, proto)

    SafeRequest(containerOps, options) ProtectAsync { _ =>

      val provider = caasProvider(containerSpec.provider.id)

      for {
        volMounts <- createInlineContainerVolumes(context, user, Right(containerSpec))
        containerResourcePre = upsertProperties(
          proto,
          "provider" -> Json.obj(
            "name" -> provider.name,
            "id" -> provider.id,
            "resource_type" -> sdk.ResourceName(provider.typeId)
          ).toString,
          "volumes" -> Json.toJson(volMounts).toString
        )
        service <- Future.fromTry {
          log.debug("Retrieving CaaSService from ProviderManager")
          providerManager.getProviderImpl(context.provider.typeId)
        }
        metaResource <- Future.fromTry {
          log.debug("Creating container resource in Meta")
          CreateWithEntitlements(containerResourcePre.orgId, user, containerResourcePre, Some(context.environmentId))
        }
        _ = log.info("Meta container created: " + metaResource.id)
        instanceWithUpdates <- {
          log.info("Creating container in backend CaaS...")
          service.create(context, metaResource)
        }
        updatedInstance <- Future.fromTry(ResourceFactory.update(instanceWithUpdates, user.account.id))
      } yield updatedInstance
    }
  }

  override def createSecret(context: ProviderContext, user: AuthAccountWithCreds, secretSpec: SecretSpec, userRequestedId: Option[UUID]): Future[Instance] = {

    val input = SecretSpec.toResourcePrototype(secretSpec).copy(id = userRequestedId)
    val proto = resourceWithDefaults(context.environment.orgId, input, user, None)
    val operations = caasObjectRequestOperations("secret.create")
    val options = caasObjectRequestOptions(user, context.environmentId, proto)

    SafeRequest(operations, options) ProtectAsync { _ =>

      val provider = caasProvider(secretSpec.provider.id)
      val secretResourcePre = upsertProperties(
        resource = proto,
        "provider" -> Json.obj(
          "name" -> provider.name,
          "id" -> provider.id,
          "resource_type" -> sdk.ResourceName(provider.typeId)).toString)

      for {
        metaResource <- Future.fromTry {
          log.debug("Creating secret resource in Meta")
          ResourceFactory.create(ResourceIds.User, user.account.id)(secretResourcePre, Some(context.environmentId))
        }
        _ = log.info("Meta secret created: " + metaResource.id)
        service <- Future.fromTry {
          log.debug("Retrieving CaaSService from ProviderManager")
          providerManager.getProviderImpl(context.provider.typeId)
        }
        instanceWithUpdates <- {
          log.info("Creating secret in backend CaaS...")
          service.createSecret(context, metaResource, secretSpec.items)
        }
        updatedInstance <- Future.fromTry(ResourceFactory.update(instanceWithUpdates, user.account.id))
      } yield updatedInstance

    }

  }

  override def createVolume(context: ProviderContext, user: AuthAccountWithCreds, volumeSpec: VolumeSpec, userRequestedId: Option[UUID]): Future[Instance] = {

    val providerId = volumeSpec.provider.map(_.id).getOrElse(
      throw new BadRequestException("Volume payload did not include '.properties.provider.id'")
    )

    val input = VolumeSpec.toResourcePrototype(volumeSpec).copy(id = userRequestedId)
    val proto = resourceWithDefaults(context.environment.orgId, input, user, None)
    val operations = caasObjectRequestOperations("volume.create")
    val options = caasObjectRequestOptions(user, context.environmentId, proto)

    SafeRequest(operations, options) ProtectAsync { _ =>

      val provider = caasProvider(providerId)
      val volumeResourcePre = upsertProperties(
        resource = proto,
        "provider" -> Json.obj(
          "name" -> provider.name,
          "id" -> provider.id,
          "resource_type" -> sdk.ResourceName(provider.typeId)
        ).toString
      )

      for {
        metaResource <- Future.fromTry {
          log.debug("Creating volume resource in Meta")
          ResourceFactory.create(ResourceIds.User, user.account.id)(volumeResourcePre, Some(context.environmentId))
        }
        _ = log.info("Meta volume created: " + metaResource.id)
        service <- Future.fromTry {
          log.debug("Retrieving CaaSService from ProviderManager")
          providerManager.getProviderImpl(context.provider.typeId)
        }
        instanceWithUpdates <- {
          log.info("Creating volume in backend CaaS...")
          service.createVolume(context, metaResource)
        }
        updatedInstance <- Future.fromTry(ResourceFactory.update(instanceWithUpdates, user.account.id))
      } yield updatedInstance

    }

  }

  override def patchContainer(origContainer: Instance, patch: PatchDocument, user: AuthAccountWithCreds, request: RequestHeader): Future[GestaltResourceInstance] = {
    for {
      patched <- PatchInstance.applyPatch(origContainer, patch) match {
        case Success(r) if r.name == origContainer.name => Future.successful(r.asInstanceOf[GestaltResourceInstance])
        case Success(r) if r.name != origContainer.name => Future.failed(new BadRequestException("renaming containers is not supported"))
        case Failure(t) => Future.failed(t)
      }
      containerSpec <- Future.fromTry(ContainerSpec.fromResourceInstance(patched))
      provider <- Future.fromTry(Try(caasProvider(containerSpec.provider.id)))
      context <- Future.fromTry(Try(ProviderContext(request, provider.id, Some(patched))))
      service <- Future.fromTry(providerManager.getProviderImpl(context.provider.typeId))
      updated <- service.update(context, patched)
    } yield updated
  }

  override def patchVolume(volume: Instance, patch: PatchDocument, user: AuthAccountWithCreds, request: RequestHeader): Future[Instance] = ???

}