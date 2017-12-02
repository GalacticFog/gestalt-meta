package controllers.util

import java.util.UUID
import javax.inject.Singleton

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.{BadRequestException, InternalErrorException}
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk
import com.galacticfog.gestalt.meta.auth.{ActionMethods, AuthorizationMethods}
import com.galacticfog.gestalt.meta.genericactions.{GenericActionContext, GenericActionInvocation, GenericProviderManager}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.google.inject.Inject
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{JsString, JsValue}
import play.api.mvc.{AnyContent, RequestHeader, ResponseHeader, Result}
import play.api.http.HeaderNames._
import play.api.mvc.Results._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

////////////////////////////////////////////////////////////////////////////////////////////////////
//
//  Generic provider-backed resource stuff
//
////////////////////////////////////////////////////////////////////////////////////////////////////
trait GenericResourceMethods {

  def deleteProviderBackedResource( org: GestaltResourceInstance,
                                    identity: AuthAccountWithCreds,
                                    resource: GestaltResourceInstance,
                                    actionVerb: String = "delete" )
                                  ( implicit request: RequestHeader ): Future[Unit]

  def performProviderBackedAction( org: GestaltResourceInstance,
                                   identity: AuthAccountWithCreds,
                                   body: AnyContent,
                                   resourceType: UUID,
                                   resourceId: UUID,
                                   providerType: UUID,
                                   actionVerb: String )
                                 ( implicit request: RequestHeader ) : Future[Result]

  def createProviderBackedResource( org: GestaltResourceInstance,
                                    identity: AuthAccountWithCreds,
                                    body: JsValue,
                                    parent: GestaltResourceInstance,
                                    resourceType: UUID,
                                    providerType: UUID,
                                    actionVerb: String = "create" )
                                  ( implicit request: RequestHeader ) : Future[Result]

  def updateProviderBackedResource( org: GestaltResourceInstance,
                                    identity: AuthAccountWithCreds,
                                    updatedResource: GestaltResourceInstance )
                                  ( implicit request: RequestHeader ): Future[GestaltResourceInstance]
}

@Singleton
class GenericResourceMethodsImpl @Inject()( genericProviderManager: GenericProviderManager )
  extends GenericResourceMethods with MetaControllerUtils with JsonInput with AuthorizationMethods {

  private[this] def fTry[T](t: => T): Future[T] =
    Future.fromTry(Try{t})

  private[this] def getOrFail[A](maybeA: Option[A], msg: String): Future[A] =
    fTry(maybeA.getOrElse{throw new BadRequestException(msg)})

  private[this] def findOrgOrFail(fqon: String): Future[GestaltResourceInstance] =
    fTry(orgFqon(fqon) getOrElse {
      throw new InternalErrorException("could not locate org resource after authentication")
    })

  private[this] def findParentOrFail(parentType: UUID, parentId: UUID): Future[GestaltResourceInstance] = {
    fTry(ResourceFactory.findById(parentType, parentId) getOrElse {
      throw new BadRequestException(s"parent of type ${sdk.ResourceLabel(parentType)} with '${parentId}' not found")
    })
  }

  object actions extends ActionMethods

  def deleteProviderBackedResource(org: GestaltResourceInstance,
                                   identity: AuthAccountWithCreds,
                                   resource: GestaltResourceInstance,
                                   actionVerb: String = "delete" )
                                  (implicit request: RequestHeader) : Future[Unit] = {

    for {
      providerId <- getOrFail(
        resource.properties.getOrElse(Map.empty).get("provider").flatMap(s => Try(UUID.fromString(s)).toOption),
        s"Could not location 'obj.properties.provider' on ${sdk.ResourceLabel(resource.typeId)} '${resource.id}'"
      )
      providerResource <- getOrFail(
        ResourceFactory.findById(providerId),
        s"Provider '${providerId}' not found"
      )
      provider <- Future.fromTry(
        genericProviderManager.getProvider(providerResource)
      )
      parent <- getOrFail(
        ResourceFactory.findParent(resource.id),
        s"Could not locate parent for ${sdk.ResourceLabel(resource.typeId)} with id '${resource.id}'"
      )
      action <- getOrFail (
        actions.prefixFromResource(resource).map { prefix => "%s.%s".format(prefix,actionVerb) },
        s"Could not find action prefix for type '${sdk.ResourceLabel(resource.typeId)}'"
      )
      invocation <- fTry(GenericActionInvocation(
        action = action,
        context = GenericActionContext.fromParent(org, parent),
        provider = providerResource,
        resource = Some(resource)
      ))
      output <- provider.invokeAction(invocation) map (_ => ())
    } yield output
  }

  def updateProviderBackedResource( org: GestaltResourceInstance,
                                    identity: AuthAccountWithCreds,
                                    updatedResource: GestaltResourceInstance )
                                  ( implicit request: RequestHeader ): Future[GestaltResourceInstance] = {
    for {
      providerId <- getOrFail(
        updatedResource.properties.getOrElse(Map.empty).get("provider").flatMap(s => Try(UUID.fromString(s)).toOption),
        s"Could not location 'obj.properties.provider' on ${sdk.ResourceLabel(updatedResource.typeId)} '${updatedResource.id}'"
      )
      providerResource <- getOrFail(
        ResourceFactory.findById(providerId),
        s"Provider '${providerId}' not found"
      )
      provider <- Future.fromTry(
        genericProviderManager.getProvider(providerResource)
      )
      parent <- getOrFail(
        ResourceFactory.findParent(updatedResource.id),
        s"Could not locate parent for ${sdk.ResourceLabel(updatedResource.typeId)} with id '${updatedResource.id}'"
      )
      action <- getOrFail (
        actions.prefixFromResource(updatedResource).map { prefix => "%s.update".format(prefix) },
        s"Could not find action prefix for type '${sdk.ResourceLabel(updatedResource.typeId)}'"
      )
      invocation <- fTry(GenericActionInvocation(
        action = action,
        context = GenericActionContext.fromParent(org, parent),
        provider = providerResource,
        resource = Some(updatedResource)
      ))
      output <- provider.invokeAction(invocation)
      providerUpdates = output.left.toOption getOrElse updatedResource
    } yield providerUpdates
  }

  def performProviderBackedAction(org: GestaltResourceInstance,
                                  identity: AuthAccountWithCreds,
                                  body: AnyContent,
                                  resourceType: UUID,
                                  resourceId: UUID,
                                  providerType: UUID,
                                  actionVerb: String )
                                 ( implicit request: RequestHeader ) : Future[Result] = {

    val response = for {
      resource <- getOrFail(
        ResourceFactory.findById(resourceType, resourceId),
        s"Resource of type ${sdk.ResourceLabel(resourceType)} with id '${resourceId}' does not exist"
      )
      providerId <- getOrFail(
        resource.properties.getOrElse(Map.empty).get("provider").flatMap(s => Try(UUID.fromString(s)).toOption),
        s"Could not location 'obj.properties.provider' on ${sdk.ResourceLabel(resourceType)} '${resourceId}'"
      )
      providerResource <- getOrFail(
        ResourceFactory.findById(providerType, providerId),
        s"Provider of type ${sdk.ResourceLabel(providerType)} '${providerId}' not found"
      )
      provider <- Future.fromTry(
        genericProviderManager.getProvider(providerResource)
      )
      parent <- getOrFail(
        ResourceFactory.findParent(resource.id),
        s"Could not locate parent for ${sdk.ResourceLabel(resourceType)} with id '${resource.id}'"
      )
      action <- getOrFail (
        actions.prefixFromResource(resource).map { prefix => "%s.%s".format(prefix, actionVerb) },
        s"Could not find action prefix for type '${sdk.ResourceLabel(resourceType)}'"
      )
      operations = List(
        controllers.util.Authorize(action),
        controllers.util.PolicyCheck(action),
        controllers.util.EventsPre(action),
        controllers.util.EventsPost(action)
      )
      options = RequestOptions(identity,
        authTarget   = Option(parent.id),
        policyOwner  = Option(parent.id),
        policyTarget = Option(resource),
        data = Option(Map(
          "host"     -> META_URL,
          "parentId" -> parent.id.toString,
          "typeId"   -> resource.typeId.toString))
      )
      response <- SafeRequest(operations, options).ExecuteAsync {
        input => for {
          invocation <- fTry(GenericActionInvocation(
            action = action,
            context = GenericActionContext.fromParent(org, parent),
            provider = providerResource,
            resource = Some(input),
            actionPayload = body.asText.map(JsString(_)) orElse body.asJson
          ))
          output <- provider.invokeAction(invocation)
          response = output.fold(
            { resourceResponse =>
              ResourceFactory.update(resourceResponse, identity.account.id, updateTimestamp = true) match {
                case Success(r) => Ok(Output.renderInstance(r, Some(META_URL)))
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

  def createProviderBackedResource(org: GestaltResourceInstance,
                                   identity: AuthAccountWithCreds,
                                   body: JsValue,
                                   parent: GestaltResourceInstance,
                                   resourceType: UUID,
                                   providerType: UUID,
                                   actionVerb: String = "create"
                                  )
                                  ( implicit request: RequestHeader ) : Future[Result] = {
    val response = for {
      providerId <- getOrFail(
        (body \ "properties" \ "provider").asOpt[UUID],
        s"${sdk.ResourceLabel(resourceType)} creation requires a provider to be specified in 'obj.properties.provider'"
      )
      providerResource <- getOrFail(
        ResourceFactory.findById(providerType, providerId),
        s"Provider of type ${sdk.ResourceLabel(providerType)} '${providerId}' not found"
      )
      json <- Future.fromTry({
        normalizeResourceType(body, resourceType)
      })
      provider <- Future.fromTry(
        genericProviderManager.getProvider(providerResource)
      )
      metaRequest = {
        val resource = j2r(org.id, identity, body, Some(resourceType))
        actions.prefixFromResource(resource).fold {
          throw new RuntimeException(s"Could not find action prefix for type '${resourceType}'")
        }{ prefix =>
          val fqaction = "%s.%s".format(prefix, actionVerb)
          MetaRequest(identity, resource, parent.id, fqaction, Some(META_URL))
        }
      }
      (operations, options) = newResourceRequestArgs(metaRequest)
      response <- SafeRequest(operations, options).ExecuteAsync {
        input => for {
          invocation <- fTry(GenericActionInvocation(
            action = metaRequest.action,
            context = GenericActionContext.fromParent(org, parent),
            provider = providerResource,
            resource = Some(input)
          ))
          output  <- provider.invokeAction(invocation)
          persisted = output.fold(x => x, _ => input)
          created <- Future.fromTry(CreateWithEntitlements(
            org.id, identity, persisted, Some(parent.id)
          ))
        } yield Created(Output.renderInstance(created, Some(META_URL)))
      }
    } yield response
    response recover { case e => HandleExceptions(e) }
  }

}
