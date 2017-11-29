package controllers

import java.util.UUID
import javax.inject.Singleton

import com.galacticfog.gestalt.data.{ResourceFactory, models}
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
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.JsValue
import play.api.mvc.{ResponseHeader, Result}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

@Singleton
class BlueprintController @Inject()( messagesApi: MessagesApi,
                                     env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
                                     actionProviderManager: ActionProviderManager )
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization with MetaController {

  private[this] def fTry[T](t: => T): Future[T] =
    Future.fromTry(Try{t})

  private[this] def getOrFail[A](maybeA: Option[A], msg: String): Future[A] =
    fTry(maybeA.getOrElse{throwBadRequest(msg)})

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
      result <- createGenericProviderBackedResource(
        org = org,
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
      result <- createGenericProviderBackedResource(
        org = org,
        parent = workspace,
        resourceType = sdk.ResourceIds.Blueprint,
        providerType = sdk.ResourceIds.BlueprintProvider
      )
    } yield result
  }

  def createEnvironmentBlueprint(fqon: String, environmentId: UUID) = AsyncAudited(fqon) { implicit request =>
    for {
      org <- findOrgOrFail(fqon)
      workspace <- findParentOrFail(sdk.ResourceIds.Environment, environmentId)
      result <- createGenericProviderBackedResource(
        org = org,
        parent = workspace,
        resourceType = sdk.ResourceIds.Blueprint,
        providerType = sdk.ResourceIds.BlueprintProvider
      )
    } yield result
  }

  def deployBlueprint(fqon: String, id: UUID) = AsyncAudited(fqon) { implicit request =>
    // FINISH: BLUEPRINT-SPECIFIC/NON-GENERIC, SHOULD BE COMPUTED FROM URI/ROUTING AND META SCHEMA
    val resourceType = sdk.ResourceIds.Blueprint
    val providerType = sdk.ResourceIds.BlueprintProvider
    val actionVerb = "deploy"

    val response = for {
      org <- findOrgOrFail(fqon)
      resource <- getOrFail(
        ResourceFactory.findById(resourceType, id),
        s"resource of type ${sdk.ResourceLabel(resourceType)} with id '${id}' does not exist"
      )
      providerId <- getOrFail(
        (request.body \ "properties" \ "provider").asOpt[UUID],
        s"${sdk.ResourceLabel(resourceType)} creation requires a provider to be specified in 'obj.properties.provider'"
      )
      providerResource <- getOrFail(
        ResourceFactory.findById(providerType, providerId),
        s"provider of type ${sdk.ResourceLabel(providerType)} '${providerId}' not found"
      )
      provider <- Future.fromTry(
        actionProviderManager.getProvider(providerResource)
      )
      parent <- getOrFail(
        ResourceFactory.findParent(resource.id),
        s"could not locate parent for ${sdk.ResourceLabel(resourceType)} with id '${resource.id}'"
      )
      metaReq = metaRequest(org.id, request.body, resourceType, parent.id, actionVerb)
      (operations, options) = requestArgs(metaReq)
      response <- SafeRequest(operations, options).ExecuteAsync {
        input => for {
          invocation <- fTry(ActionInvocation(
            action = metaReq.action,
            context = ActionContext.fromParent(org, parent),
            provider = providerResource,
            resource = Some(input)
          ))
          output <- provider.invokeAction(invocation)
          response = output.fold(
            { resourceResponse =>
              ResourceFactory.update(resourceResponse, request.identity.account.id, updateTimestamp = true) match {
                case Success(r) => Ok(RenderSingle(r))
                case Failure(ex) => HandleExceptions(ex)
              }
            }, {
              case (status,contentType,contentBody) => Result(
                ResponseHeader(status.getOrElse(200), contentType.map(ct => Map(CONTENT_TYPE -> ct)).getOrElse(Map.empty)),
                Enumerator(contentBody.getOrElse("").getBytes)
              )
            }
          )
        } yield response
      }
    } yield response
    response recover { case e => HandleExceptions(e) }
  }


  ////////////////////////////////////////////////////////////////////////////////////////////////////
  //
  //  Generic stuff, to be eventually moved over to Meta.scala
  //
  ////////////////////////////////////////////////////////////////////////////////////////////////////

  def createGenericProviderBackedResource( org: GestaltResourceInstance,
                                           parent: GestaltResourceInstance,
                                           resourceType: UUID,
                                           providerType: UUID )
                                         ( implicit request: SecuredRequest[JsValue] ) : Future[Result] = {
    val response = for {
      providerId <- getOrFail(
        (request.body \ "properties" \ "provider").asOpt[UUID],
        s"${sdk.ResourceLabel(resourceType)} creation requires a provider to be specified in 'obj.properties.provider'"
      )
      providerResource <- getOrFail(
        ResourceFactory.findById(providerType, providerId),
        s"provider of type ${sdk.ResourceLabel(providerType)} '${providerId}' not found"
      )
      json <- Future.fromTry({
        normalizeResourceType(request.body, resourceType)
      })
      provider <- Future.fromTry(
        actionProviderManager.getProvider(providerResource)
      )
      metaRequest = newResourceRequest(org.id, resourceType, resourceParent = parent.id, payload = Some(json))
      (operations, options) = requestArgs(metaRequest)
      response <- SafeRequest(operations, options).ExecuteAsync {
        input => for {
          invocation <- fTry(ActionInvocation(
            action = metaRequest.action,
            context = ActionContext.fromParent(org, parent),
            provider = providerResource,
            resource = Some(input)
          ))
          output  <- provider.invokeAction(invocation)
          persisted = output.fold(x => x, _ => input)
          created <- Future.fromTry(CreateWithEntitlements(
            org.id, request.identity, persisted, Some(parent.id)
          ))
        } yield Created(RenderSingle(created))
      }
    } yield response
    response recover { case e => HandleExceptions(e) }
  }

}
