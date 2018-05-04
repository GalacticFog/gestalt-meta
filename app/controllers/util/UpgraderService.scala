package controllers.util

import java.util.UUID

import actors.SystemConfigActor
import akka.actor.ActorRef
import akka.pattern.ask
import com.galacticfog.gestalt.data.{HardDeleteInstanceManager, HardDeleteResourceDefault, ResourceFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.Resource
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
import com.galacticfog.gestalt.meta.providers.{ProviderManager, ProviderMap}
import controllers.DeleteController
import javax.inject.{Inject, Named}
import play.api.libs.json.{Format, JsObject, Json}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

trait UpgraderService {
  import UpgraderService._

  def deleteUpgrader(creator: AccountLike)
                    (implicit ec: ExecutionContext): Future[UpgradeStatus]

  def launchUpgrader(creator: AccountLike, payload: UpgradeLaunch)
                    (implicit ec: ExecutionContext): Future[UpgradeStatus]
}

class DefaultUpgraderService @Inject() ( @Named(SystemConfigActor.name) configActor: ActorRef,
                                         providerManager: ProviderManager,
                                         gatewayMethods: GatewayMethods )
  extends UpgraderService with JsonInput with AuthorizationMethods {

  import UpgraderService._
  implicit val askTimeout: akka.util.Timeout = 15.seconds

  lazy val rootId = Resource.findFqon("root").map(_.id).getOrElse(throw new RuntimeException("could not find root org"))

  private[this] def wrapUnauthedHandler(f: (GestaltResourceInstance) => Try[Unit])(r: GestaltResourceInstance, a: AccountLike) = f(r)
  private val deleteMgr = new HardDeleteInstanceManager[AccountLike](
    external = Map(
      ResourceIds.Container   -> wrapUnauthedHandler(ContainerService.deleteContainerHandler(providerManager,_)),
      ResourceIds.Api         -> wrapUnauthedHandler(gatewayMethods.deleteApiHandler),
      ResourceIds.ApiEndpoint -> wrapUnauthedHandler(gatewayMethods.deleteEndpointHandler)
    )
  )

  override def deleteUpgrader(creator: AccountLike)
                             (implicit ec: ExecutionContext): Future[UpgradeStatus] = {
    for {
      maybeProviderId <- (configActor ? SystemConfigActor.GetKey("upgrade_provider")).mapTo[Option[String]]
      maybeProvider = maybeProviderId
        .flatMap(s => Try(UUID.fromString(s)).toOption)
        .flatMap(ResourceFactory.findById(ResourceIds.Provider, _))
      _ <- maybeProvider.fold(Future.successful(()))({
        provider => Future.fromTry(deleteMgr.delete(provider, creator, true).map(_ => ()))
      })
      newStatus <- updateStatus(creator.id, UpgradeStatus.inactive, None)
    } yield newStatus
  }

  override def launchUpgrader(creator: AccountLike, payload: UpgradeLaunch)
                             (implicit ec: ExecutionContext): Future[UpgradeStatus] = {
    for {
      providerProto <- Future.fromTry(
        jsonToResource(rootId, creator, getProviderPayload(payload), Some(ResourceIds.Provider))
      )
      provider <- Future.fromTry(ResourceFactory.create(ResourceIds.User, creator.id)(
        r = providerProto,
        parentId = Some(rootId)
      ))
      _ <- Future.fromTry(
        // parent=None means no inheritance, granting entitlements only to creator
        Try(setNewResourceEntitlements(
          org = rootId,
          resource = provider.id,
          creator = creator,
          parent = None
        ).map(_.get))
      )
      (_, Seq(container)) <- providerManager.processProvider(ProviderMap(provider))
      env = providerManager.getOrCreateProviderEnvironment(provider, creator)
      apiProto <- Future.fromTry(
        jsonToResource(rootId, creator, getApiPayload(payload), Some(ResourceIds.Api))
      )
      api <- Future.fromTry(ResourceFactory.create(ResourceIds.User, creator.id)(
        r = apiProto,
        parentId = Some(env.id)
      ))
      endpointJson = getEndpointPayload(api, container, payload)
      endpointProto <- Future.fromTry(
        jsonToResource(rootId, creator, endpointJson, Some(ResourceIds.ApiEndpoint))
      )
      endpointNative <- Future.fromTry(GatewayMethods.toGatewayEndpoint(endpointJson, api.id))
      endpoint <- Future.fromTry(ResourceFactory.create(ResourceIds.User, creator.id)(
        r = endpointProto,
        parentId = Some(api.id)
      ))
      apiEndpoint <- gatewayMethods.createEndpoint(
        api = api,
        metaEndpoint = endpoint,
        gatewayEndpoint = endpointNative
      )
      publicUrl = GatewayMethods.getPublicUrl(apiEndpoint)
      newStatus = UpgradeStatus(
        active = true,
        endpoint = publicUrl
      )
      status <- updateStatus(creator.id, newStatus, Some(provider.id))
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
      "methods" -> Seq("GET", "POST"),
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
