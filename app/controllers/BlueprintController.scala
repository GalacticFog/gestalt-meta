package controllers

import java.util.UUID
import javax.inject.Singleton

import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import play.api.i18n.MessagesApi

@Singleton
class BlueprintController @Inject()(messagesApi: MessagesApi,
                                    env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {

  def createBlueprintFqon(fqon: String) = createBlueprint(fqon, "org", fqid(fqon))

  def createBlueprint(fqon: String, parentType: String, parentId: UUID) = AsyncAudited(fqon) { implicit request =>
    ???
  }

  def deployBlueprint(fqon: String, envid: UUID, id: UUID) = AsyncAudited(fqon) { implicit request =>
    ???
  }
}
