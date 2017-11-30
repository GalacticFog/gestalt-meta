package controllers

import java.util.UUID
import javax.inject.Singleton

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.InternalErrorException
import com.galacticfog.gestalt.meta.api.sdk
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.meta.genericactions.GenericProviderManager
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import play.api.i18n.MessagesApi

import scala.concurrent.Future
import scala.util.Try

@Singleton
class BlueprintController @Inject()( messagesApi: MessagesApi,
                                     env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
                                     genericResourceMethods: GenericResourceMethods,
                                     genericProviderManager: GenericProviderManager )
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization with MetaController {

  private[this] def fTry[T](t: => T): Future[T] =
    Future.fromTry(Try{t})

  private[this] def findOrgOrFail(fqon: String): Future[GestaltResourceInstance] =
    fTry(orgFqon(fqon) getOrElse {
      throw new InternalErrorException("could not locate org resource after authentication")
    })

  private[this] def findParentOrFail(parentType: UUID, parentId: UUID): Future[GestaltResourceInstance] = {
    fTry(ResourceFactory.findById(parentType, parentId) getOrElse {
      throwBadRequest(s"parent of type ${sdk.ResourceLabel(parentType)} with '${parentId}' not found")
    })
  }

  def createBlueprintFqon(fqon: String) = AsyncAudited(fqon) { implicit request =>
    for {
      org <- findOrgOrFail(fqon)
      result <- genericResourceMethods.createGenericProviderBackedResource(
        org = org,
        identity = request.identity,
        body = request.body,
        parent = org,
        resourceType = sdk.ResourceIds.Blueprint,
        providerType = sdk.ResourceIds.BlueprintProvider
      )
    } yield result
  }

  def createWorkspaceBlueprint(fqon: String, workspaceId: UUID) = AsyncAudited(fqon) { implicit request =>
    for {
      org <- findOrgOrFail(fqon)
      workspace <- findParentOrFail(sdk.ResourceIds.Workspace, workspaceId)
      result <- genericResourceMethods.createGenericProviderBackedResource(
        org = org,
        identity = request.identity,
        body = request.body,
        parent = workspace,
        resourceType = sdk.ResourceIds.Blueprint,
        providerType = sdk.ResourceIds.BlueprintProvider
      )
    } yield result
  }

  def createEnvironmentBlueprint(fqon: String, environmentId: UUID) = AsyncAudited(fqon) { implicit request =>
    for {
      org <- findOrgOrFail(fqon)
      environment <- findParentOrFail(sdk.ResourceIds.Environment, environmentId)
      result <- genericResourceMethods.createGenericProviderBackedResource(
        org = org,
        identity = request.identity,
        body = request.body,
        parent = environment,
        resourceType = sdk.ResourceIds.Blueprint,
        providerType = sdk.ResourceIds.BlueprintProvider
      )
    } yield result
  }

  def blueprintAction(fqon: String, id: UUID, action: String) = AsyncAuditedAny(fqon) { implicit request =>
    for {
      org <- findOrgOrFail(fqon)
      result <- genericResourceMethods.performGenericProviderBackedAction(
        org = org,
        identity = request.identity,
        body = request.body,
        resourceType = sdk.ResourceIds.Blueprint,
        resourceId = id,
        providerType = sdk.ResourceIds.BlueprintProvider,
        actionVerb = action
      )
    } yield result
  }


}
