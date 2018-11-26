package controllers.util

import java.util.UUID

import actors.SystemConfigActor
import akka.actor.ActorRef
import akka.pattern.ask
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.{HardDeleteInstanceManager, ResourceFactory}
import com.galacticfog.gestalt.meta.api.Resource
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
import com.galacticfog.gestalt.meta.providers.ProviderManager
import javax.inject.{Inject, Named}
import play.api.libs.json.{Format, JsObject, Json}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.util.{Try, Failure}

trait UpgraderService {
  import UpgraderService._

  def deleteUpgrader(creator: AccountLike)
                    (implicit ec: ExecutionContext): Future[UpgradeStatus]

  def launchUpgrader(creator: AccountLike, payload: UpgradeLaunch)
                    (implicit ec: ExecutionContext): Future[UpgradeStatus]
}

class DefaultUpgraderService @Inject() ( @Named(SystemConfigActor.name) configActor: ActorRef,
                                         providerManager: ProviderManager,
                                         gatewayMethods: GatewayMethods)
  extends UpgraderService with JsonInput with AuthorizationMethods {

  import UpgraderService._
  implicit val askTimeout: akka.util.Timeout = 15.seconds

  lazy val rootId = Resource.findFqon("root").map(_.id).getOrElse(throw new RuntimeException("could not find root org"))

  private[this] def wrapUnauthedHandler(f: (GestaltResourceInstance) => Try[Unit])(r: GestaltResourceInstance, a: AccountLike) = f(r)
  private val deleteMgr = new HardDeleteInstanceManager[AccountLike](
    external = Map(
      ResourceIds.Container   -> wrapUnauthedHandler { resourceLike =>
       Try(Await.result(ContainerService.deleteContainerHandler(providerManager, resourceLike), 10 .seconds)) recoverWith {
          case _: scala.concurrent.TimeoutException => Failure(new InternalErrorException("timed out waiting for external CaaS service to respond"))
          case throwable => {
            throwable.printStackTrace()
            Failure(throwable)
          }
        }
      },
      ResourceIds.Api         -> wrapUnauthedHandler { resourceLike =>
        Try(Await.result(gatewayMethods.deleteApiHandler(resourceLike), 5 .seconds))
      },
      ResourceIds.ApiEndpoint -> wrapUnauthedHandler { resourceLike =>
        Try(Await.result(gatewayMethods.deleteEndpointHandler(resourceLike), 5 .seconds))
      }
    )
  )

  override def deleteUpgrader(creator: AccountLike)
                             (implicit ec: ExecutionContext): Future[UpgradeStatus] = {
    for {
      maybeProviderId <- (configActor ? SystemConfigActor.GetKey("upgrade_provider")).mapTo[Option[String]]
      _ = log.info(s"upgrade_provider: ${maybeProviderId}")
      maybeProvider = maybeProviderId
        .flatMap(s => Try(UUID.fromString(s)).toOption)
        .flatMap(ResourceFactory.findById(ResourceIds.Provider, _))
      _ <- maybeProvider match {
        case None =>
          log.info("upgrade_provider does not exist")
          Future.successful(())
        case Some(provider) =>
          log.info("upgrade_provider exists; will delete")
          Future.fromTry(
            deleteMgr.delete(provider, creator, true)
              .map(_ => ())
          )
      }
      newStatus <- updateStatus(creator.id, UpgradeStatus.inactive, None)
    } yield newStatus
  }

  override def launchUpgrader(creator: AccountLike, payload: UpgradeLaunch)
                             (implicit ec: ExecutionContext): Future[UpgradeStatus] = {
    val providerId = java.util.UUID.randomUUID()
    for {
      // get gwm provider
      gwmProvider <- Future.fromTry(Try(
        ResourceFactory.findById(ResourceIds.GatewayManager, payload.gwmProviderId)
          .getOrElse(throw new RuntimeException(s"could not locate GatewayManager provider with id '${payload.gwmProviderId}'"))
      ))
      // compute provider prototype
      providerProto <- Future.fromTry(
        jsonToResource(rootId, creator, getProviderPayload(payload), Some(ResourceIds.Provider))
      )
      // save providerId, just in case it gets a partial create
      _ <- updateStatus(
        creator = creator.id,
        newStatus = UpgradeStatus(
          active = true,
          endpoint = None
        ),
        providerId = Some(providerId)
      )
      // create provider (i.e., environment and container)
      provider <- Future.fromTry(ResourceFactory.create(ResourceIds.User, creator.id)(
        r = providerProto.copy(
          id = providerId
        ),
        parentId = Some(rootId)
      ))
      // set entitlements on the provider
      _ <- Future.fromTry(Try(setNewResourceEntitlements(
        org = rootId,
        resource = provider.id,
        creator = creator,
        parent = None  // parent=None means no inheritance, granting entitlements only to creator
      )))
      // load the provider to create the upgrader service container
      Seq(container) <- providerManager.triggerProvider(provider)
      env = providerManager.getOrCreateProviderEnvironment(provider, creator)
      // create the API
      apiJson = getApiPayload(payload)
      api <- gatewayMethods.createApi(rootId, env.id, apiJson, creator)
      endpointJson = getEndpointPayload(api, container, payload)
      endpoint <- gatewayMethods.createEndpoint(rootId, api, endpointJson, creator)
      publicUrl = gatewayMethods.getPublicUrl(endpoint)
      status <- updateStatus(
        creator = creator.id,
        newStatus = UpgradeStatus(
          active = true,
          endpoint = publicUrl
        ),
        providerId = Some(provider.id))
    } yield status
  }

  private[this] def updateStatus(creator: UUID, newStatus: UpgradeStatus, providerId: Option[UUID])
                                (implicit ec: ExecutionContext): Future[UpgradeStatus] = {
    (configActor ? SystemConfigActor.SetKeys(
      creator = creator,
      pairs = Map(
        "upgrade_status" -> Some(Json.toJson(newStatus).toString),
        "upgrade_lock" -> Some(newStatus.active.toString),
        "upgrade_provider" -> providerId.map(_.toString)
      )
    )).map { _ => newStatus }
  }

}

case object UpgraderService {

  def getEndpointPayload(api: GestaltResourceInstance, container: GestaltResourceInstance, launchPayload: UpgradeLaunch): JsObject = Json.obj(
    "id" -> java.util.UUID.randomUUID().toString,
    "name" -> "upgrader",
    "properties" -> Json.obj(
      "parent" -> api.id.toString,
      "location_id" -> launchPayload.kongProviderId.toString,
      "implementation_type" -> "container",
      "implementation_id" -> container.id.toString,
      "container_port_name" -> "api",
      "methods" -> Seq("GET", "POST", "OPTIONS"),
      "resource" -> "/upgrader",
      "provider" -> api.properties.get("provider")
    )
  )

  def getApiPayload(launchPayload: UpgradeLaunch): JsObject = Json.obj(
    "id" -> java.util.UUID.randomUUID().toString,
    "name" -> java.util.UUID.randomUUID().toString,
    "properties" -> Json.obj(
      "provider" -> Json.obj(
        "id" -> launchPayload.gwmProviderId.toString,
        "locations" -> Seq(launchPayload.kongProviderId.toString)
      )
    )
  )

  def getProviderPayload(launchPayload: UpgradeLaunch): JsObject = Json.obj(
    "id" -> java.util.UUID.randomUUID().toString,
    "name" -> "gestalt-upgrade",
    "properties" -> Json.obj(
      "config" -> Json.obj(
        "auth" -> Json.obj(),
        "env" -> Json.obj(
          "private" -> Json.obj(),
          "public" -> Json.obj()
        )
      ),
      "linked_providers" -> Seq(
        Json.obj(
          "id" -> launchPayload.dbProviderId,
          "name" -> "DB"
        ),
        Json.obj(
          "id" -> launchPayload.secProviderId,
          "name" -> "SEC"
        ),
        Json.obj(
          "id" -> launchPayload.caasProviderId,
          "name" -> "CAAS"
        )
      ),
      "services" -> Seq(Json.obj(
        "container_spec" -> Json.obj(
          "name" -> "upgrader",
          "properties" -> Json.obj(
            "container_type" -> "DOCKER",
            "cpus" -> 1.0,
            "force_pull" -> true,
            "image" -> launchPayload.image,
            "memory" -> 512,
            "env" -> Json.obj(
              "META_CALLBACK_URL" -> sys.env.get("META_POLICY_CALLBACK_URL")
            ),
            "network" -> "BRIDGE",
            "num_instances" -> 1,
            "port_mappings" -> Seq(Json.obj(
              "container_port" -> 9000,
              "expose_endpoint" -> true,
              "name" -> "api",
              "protocol" -> "tcp"
            )),
            "provider" -> Json.obj(
              "id" -> launchPayload.caasProviderId
            ),
            "volumes" -> Seq(Json.obj(
              "container_path" -> "persistence",
              "mode" -> "RW",
              "persistent" -> Json.obj("size" -> launchPayload.persistenceSize.getOrElse[Long](1024))
            ))
          )
        ),
        "init" -> Json.obj(
          "binding" -> "eager",
          "singleton" -> true
        )
      ))
    )
  )

  case class UpgradeLaunch( image: String,
                            dbProviderId: UUID,
                            secProviderId: UUID,
                            caasProviderId: UUID,
                            gwmProviderId: UUID,
                            kongProviderId: UUID,
                            persistenceSize: Option[Long] = None)

  case class UpgradeStatus(active: Boolean, endpoint: Option[String])
  case object UpgradeStatus {
    def inactive = UpgradeStatus(false,None)
  }

  implicit val usFmt: Format[UpgradeStatus] = Json.format[UpgradeStatus]
  implicit val ulFmt: Format[UpgradeLaunch] = Json.format[UpgradeLaunch]
}
