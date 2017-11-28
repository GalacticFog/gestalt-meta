package controllers

import java.util.UUID
import javax.inject.Singleton

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.actions.{ActionContext, ActionInvocation, ActionProviderManager}
import com.galacticfog.gestalt.meta.api.errors.{BadRequestException, InternalErrorException}
import com.galacticfog.gestalt.meta.api.sdk
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import play.api.i18n.MessagesApi
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future
import scala.util.Try

@Singleton
class BlueprintController @Inject()( messagesApi: MessagesApi,
                                     env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
                                     actionProviderManager: ActionProviderManager )
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {

  def createBlueprintFqon(fqon: String) = createBlueprint(fqon, "org", fqid(fqon))

  def createBlueprint(fqon: String, parentType: String, parentId: UUID) = AsyncAudited(fqon) { implicit request =>
    def fTry[T](t: => T): Future[T] = Future.fromTry(Try{t})
    val blueprint: GestaltResourceInstance = null
    val created = for {
      org <- fTry(orgFqon(fqon) getOrElse (
        throw new InternalErrorException("could not locate org resource after authentication")
      ))
      providerId <- fTry((request.body \ "properties" \ "provider" \ "id").asOpt[UUID] getOrElse (
        throw new BadRequestException("Blueprint creation requires a provider")
      ))
      providerResource <- fTry(ResourceFactory.findById(sdk.ResourceIds.BlueprintProvider, providerId) getOrElse (
        throw new BadRequestException(s"provider of type ${sdk.ResourceLabel(sdk.ResourceIds.BlueprintProvider)} '${providerId}' not found")
      ))
      parent <- fTry(parentType match {
        case "environment" => ResourceFactory.findById(sdk.ResourceIds.Environment, parentId) getOrElse (
          throw new BadRequestException(s"parent of type ${sdk.ResourceLabel(sdk.ResourceIds.Environment)} with '${parentId}' not found")
        )
        case "workspace" => ResourceFactory.findById(sdk.ResourceIds.Workspace, parentId) getOrElse (
          throw new BadRequestException(s"parent of type ${sdk.ResourceLabel(sdk.ResourceIds.Workspace)} with '${parentId}' not found")
        )
        case "org" => org
      })
      provider <- Future.fromTry(actionProviderManager.getProvider(providerResource))
      invocation <- fTry(ActionInvocation(
        action = "blueprint.create",
        context = ActionContext.fromParent(parent),
        provider = providerResource,
        resource = Some(blueprint)
      ))
      result <- provider.invokeAction(invocation)
    } yield Created(RenderSingle(result))
    created recover { case e => HandleExceptions(e) }
  }

  def deployBlueprint(fqon: String, envid: UUID, id: UUID) = AsyncAudited(fqon) { implicit request =>
    ???
  }
}
