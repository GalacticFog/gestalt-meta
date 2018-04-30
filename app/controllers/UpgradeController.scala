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
import play.api.libs.json.Json

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

  case class UpgradeStatus(active: Boolean, endpoint: Option[String])
  case object UpgradeStatus {
    def inactive = UpgradeStatus(false,None)
  }
  implicit val usFmt = Json.format[UpgradeStatus]
  implicit val askTimeout: akka.util.Timeout = 15.seconds

  def getStatus: Future[UpgradeStatus] = {
    (configActor ? SystemConfigActor.GetKey("upgrade")).mapTo[Option[String]].map(_.fold(UpgradeStatus.inactive){
      str => Try {
        Json.parse(str).as[UpgradeStatus]
      } getOrElse UpgradeStatus.inactive
    })
  }

  def updateStatus(newStatus: UpgradeStatus)(implicit request: SecuredRequest[_]): Future[Unit] = {
    (configActor ? SystemConfigActor.SetKeys(
      caller = request.identity.account.id,
      "upgrade" -> Json.toJson(newStatus).toString
    )).mapTo[Boolean].flatMap {
      case true => Future.successful(())
      case false => Future.failed(new InternalErrorException("failed to set upgrade status"))
    }
  }

  def check() = AsyncAuditedAny() { implicit request =>
    SafeRequest(upgradeOps, upgradeOptions(request)) ProtectAsync { _ =>
      getStatus map { status => Ok(Json.toJson(status)) }
    }
  }

  def launch() = AsyncAudited() { implicit request =>
    SafeRequest(upgradeOps, upgradeOptions(request)) ProtectAsync { _ =>
      getStatus flatMap { status =>
        if (! status.active) {
          val newStatus = status.copy(
            active = true,
            endpoint = Some("https://gtw1.test.galacticfog.com/test-upgrader")
          )
          updateStatus(newStatus) map {_ => Accepted(Json.toJson(newStatus))}
        } else {
          Future.successful(BadRequest(Json.obj(
            "message" -> "upgrade is already active"
          )))
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

}