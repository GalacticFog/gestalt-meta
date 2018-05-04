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

import scala.util.Try
import scala.concurrent.duration._
import play.api.libs.concurrent.Execution.Implicits.defaultContext

@Singleton
class UpgradeController @Inject()( messagesApi: MessagesApi,
                                   env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
                                   upgraderService: UpgraderService,
                                   @Named(SystemConfigActor.name) configActor: ActorRef )
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {

  implicit val askTimeout: akka.util.Timeout = 15.seconds

  import UpgraderService._

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
    request.body.validate[UpgraderService.UpgradeLaunch].fold({
      errs =>
        val details = errs.map({
          case (path, e) => path.toString -> e.mkString(",")
        }).toMap
        Future.successful(BadRequest(Json.obj(
          "message" -> "invalid payload",
          "details" -> details
        )))
    },{ payload =>
      SafeRequest(upgradeOps, upgradeOptions(request)) ProtectAsync { _ =>
        setAndTest flatMap { alreadyLocked =>
          if (alreadyLocked) {
            Future.successful(BadRequest(Json.obj(
              "message" -> "upgrade is already active"
            )))
          } else {
            upgraderService.launchUpgrader(request.identity, payload)
              .map {s => Accepted(Json.toJson(s))}
          }
        }
      }
    })
  }

  def delete() = AsyncAuditedAny() { implicit request =>
    SafeRequest(upgradeOps, upgradeOptions(request)) ProtectAsync { _ =>
      getStatus flatMap { status =>
        if (status.active) {
          upgraderService.deleteUpgrader(request.identity, status)
            .map {s => Accepted(Json.toJson(s))}
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


}
