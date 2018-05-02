package controllers.util

import java.util.UUID

import actors.SystemConfigActor
import akka.actor.ActorRef
import javax.inject.{Inject, Named}
import play.api.libs.json.{Format, JsValue, Json}
import akka.pattern.ask

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

trait UpgraderService {
  import UpgraderService._

  def deleteUpgrader(creator: UUID, status: UpgradeStatus)
                    (implicit ec: ExecutionContext): Future[UpgradeStatus]

  def launchUpgrader(creator: UUID, payload: UpgradeLaunch)
                    (implicit ec: ExecutionContext): Future[UpgradeStatus]
}

class DefaultUpgraderService @Inject() (@Named(SystemConfigActor.name) configActor: ActorRef)
  extends UpgraderService {

  import UpgraderService._
  implicit val askTimeout: akka.util.Timeout = 15.seconds

  override def deleteUpgrader(creator: UUID, status: UpgradeStatus)
                             (implicit ec: ExecutionContext): Future[UpgradeStatus] = {
    // FINISH
    val newStatus = UpgradeStatus.inactive
    updateStatus(creator, newStatus)
  }

  override def launchUpgrader(creator: UUID, payload: UpgradeLaunch)
                             (implicit ec: ExecutionContext): Future[UpgradeStatus] = {
    // FINISH
    val newStatus = UpgradeStatus(
      active = true,
      endpoint = Some("https://gtw1.test.galacticfog.com/test-upgrader")
    )
    updateStatus(creator, newStatus)
  }

  private[this] def updateStatus(creator: UUID, newStatus: UpgradeStatus)
                                (implicit ec: ExecutionContext): Future[UpgradeStatus] = {
    (configActor ? SystemConfigActor.SetKeys(
      creator = creator,
      pairs = Map(
        "upgrade_status" -> Some(Json.toJson(newStatus).toString),
        "upgrade_lock" -> Some(newStatus.active.toString)
      )
    )).map { _ => newStatus }
  }

}

case object UpgraderService {

  case class UpgradeLaunch( image: String,
                            dbProviderId: UUID,
                            secProviderId: UUID,
                            caasProviderId: UUID,
                            kongProviderId: UUID )

  case class UpgradeStatus(active: Boolean, endpoint: Option[String])
  case object UpgradeStatus {
    def inactive = UpgradeStatus(false,None)
  }

  implicit val usFmt: Format[UpgradeStatus] = Json.format[UpgradeStatus]
  implicit val ulFmt: Format[UpgradeLaunch] = Json.format[UpgradeLaunch]
}
