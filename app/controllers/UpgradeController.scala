package controllers

import actors.SystemConfigActor
import akka.actor.ActorRef
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import javax.inject.{Named, Singleton}
import play.api.i18n.MessagesApi
import play.api.libs.json.{Format, Json}

import scala.concurrent.Future
import akka.pattern.ask
import com.galacticfog.gestalt.meta.api.errors.InternalErrorException

import scala.util.Try
import scala.concurrent.duration._
import play.api.libs.concurrent.Execution.Implicits.defaultContext

@Singleton
class UpgradeController @Inject()( messagesApi: MessagesApi,
                                   env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
                                   @Named(SystemConfigActor.name) configActor: ActorRef )
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {

  implicit val askTimeout: akka.util.Timeout = 15.seconds

  import UpgradeController._

  def check() = AsyncAuditedAny() { implicit request =>
    SafeRequest(upgradeOps, upgradeOptions(request)) ProtectAsync { _ =>
      getStatus map { status => Ok(Json.toJson(status)) }
    }
  }

  def setAndTest()(implicit request: SecuredRequest[_]): Future[Boolean] = {
    for {
      maybeLocked <- (configActor ? SystemConfigActor.SetKey(request.identity.account.id, "upgrade_lock",  Some("true")))
          .mapTo[Option[String]]
      locked = maybeLocked.map(_.toBoolean).getOrElse(false)
    } yield locked
  }

  def launch() = AsyncAudited() { implicit request =>
    SafeRequest(upgradeOps, upgradeOptions(request)) ProtectAsync { _ =>
      setAndTest flatMap { alreadyLocked =>
        if (alreadyLocked) {
          Future.successful(BadRequest(Json.obj(
            "message" -> "upgrade is already active"
          )))
        } else {
          val newStatus = UpgradeStatus(
            active = true,
            endpoint = Some("https://gtw1.test.galacticfog.com/test-upgrader")
          )
          updateStatus(newStatus) map {_ => Accepted(Json.toJson(newStatus))}
        }
      }
    }
  }

  def delete() = AsyncAuditedAny() { implicit request =>
    SafeRequest(upgradeOps, upgradeOptions(request)) ProtectAsync { _ =>
      getStatus flatMap { status =>
        if (status.active) {
          val newStatus = UpgradeStatus.inactive
          updateStatus(newStatus) map {_ => Accepted(Json.toJson(newStatus))}
        } else {
          Future.successful(Accepted(Json.toJson(status)))
        }
      }
    }
  }

  lazy val rootId = orgFqon("root").map(_.id).getOrElse(throw new RuntimeException("could not find root org"))

  private[this] def upgradeOps() = List(controllers.util.Authorize("gestalt.upgrade"))

  private[this] def upgradeOptions(req: SecuredRequest[_]) = RequestOptions(
    user = req.identity,
    authTarget = Some(rootId),
    policyOwner = None,
    policyTarget = None,
    data = None
  )

  private[this] def getStatus: Future[UpgradeStatus] = {
    (configActor ? SystemConfigActor.GetKey("upgrade_status")).mapTo[Option[String]].map(_.fold(UpgradeStatus.inactive){
      str => Try {
        Json.parse(str).as[UpgradeStatus]
      } getOrElse UpgradeStatus.inactive
    })
  }

  private[this] def updateStatus(newStatus: UpgradeStatus)(implicit request: SecuredRequest[_]): Future[UpgradeStatus] = {
    (configActor ? SystemConfigActor.SetKey(
      caller = request.identity.account.id,
      "upgrade_status",
      Some(Json.toJson(newStatus).toString)
    )).mapTo[Option[String]].map { _ => newStatus }
  }


}

case object UpgradeController {

  case class UpgradeStatus(active: Boolean, endpoint: Option[String])
  case object UpgradeStatus {
    def inactive = UpgradeStatus(false,None)
  }

  implicit val usFmt: Format[UpgradeStatus] = Json.format[UpgradeStatus]

}