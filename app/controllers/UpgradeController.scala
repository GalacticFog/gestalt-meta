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

import scala.concurrent.Future

@Singleton
class UpgradeController @Inject()( messagesApi: MessagesApi,
                                   env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
                                   @Named(SystemConfigActor.name) configActor: ActorRef )
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {

  def check() = AsyncAuditedAny() { implicit request =>
    SafeRequest(upgradeOps, upgradeOptions(request)) ProtectAsync { _ =>
      Future.successful(Ok(""))
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