package controllers.util

import java.util.UUID
import scala.util.Try
import scala.concurrent.Future
import play.api.Logger
import play.api.libs.json._
import play.api.libs.ws.WSClient
import cats.syntax.either._
import com.google.inject.Inject
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.{ResourceFactory,ResourceState}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.patch.PatchInstance
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.util.Either._
import com.galacticfog.gestalt.util.ResourceSerde
import com.galacticfog.gestalt.patch.PatchDocument
import com.galacticfog.gestalt.meta.providers.gwm._

class GatewayMethods @Inject() (
  ws: WSClient,
  providerMethods: ProviderMethods,
  awsapiImpl: AWSAPIGatewayProvider,
  gwmImpl: GatewayManagerProvider
 ) extends AuthorizationMethods {

  import play.api.libs.concurrent.Execution.Implicits.defaultContext
  import GwmSpec.Implicits._

  private[this] val log = Logger(this.getClass)

  def getGwmProviderAndImpl(providerId: UUID): Either[String,(GestaltResourceInstance,GwmProviderImplementation[Future])] = {
    val gmProvider = ResourceFactory.findById(ResourceIds.GatewayManager, providerId)
    val awsapiProvider = ResourceFactory.findById(migrations.V24.AWS_API_GATEWAY_PROVIDER_TYPE_ID, providerId)
    (gmProvider, awsapiProvider) match {
      case (Some(provider), None) => Right((provider, gwmImpl))
      case (None, Some(provider)) => Right((provider, awsapiImpl))
      case (None, None) => Left(s"No provider found for ${providerId}")
      case _ => ???
    }
  }

  def getPublicUrl(resource: GestaltResourceInstance): Option[String] = {
    for(
      endpointProperties <- ResourceSerde.deserialize[ApiEndpointProperties](resource).toOption;
      pi <- getGwmProviderAndImpl(endpointProperties.provider.id).toOption;
      (_, impl) = pi;
      publicUrl <- impl.getPublicUrl(resource)
    )yield publicUrl
  }

  def createApi(org: UUID, parentId: UUID, requestBody: JsValue,
   caller: AccountLike): Future[GestaltResourceInstance] = {
    (for(
      gri <- eitherFromJsResult(requestBody.validate[GestaltResourceInput]);
      typeId = gri.resource_type.getOrElse(ResourceIds.Api);
      environment <- Either.fromOption(ResourceFactory.findById(ResourceIds.Environment, parentId),
       s"Environment with id ${parentId} not found");
      ownerLink = toOwnerLink(ResourceIds.User, caller.id, name=Some(caller.name), orgId=caller.orgId);
      apiId = gri.id.getOrElse(UUID.randomUUID);
      payload = gri.copy(
        id=Some(apiId),
        owner=gri.owner.orElse(Some(ownerLink)),
        resource_state=gri.resource_state.orElse(Some(ResourceStates.Active)),
        resource_type=Some(typeId)
      );
      resource = inputToInstance(org, payload);
      apiProperties <- ResourceSerde.deserialize[ApiProperties](resource);
      pi <- getGwmProviderAndImpl(apiProperties.provider.id);
      (provider, impl) = pi;
      created <- eitherFromTry(ResourceFactory.create(ResourceIds.User, caller.id)(resource, Some(parentId)));
      _ <- eitherFromTry(Try(setNewResourceEntitlements(org, resource.id, caller, Some(parentId))))
    ) yield {
      impl.createApi(provider, created) recoverWith { case e =>
        e.printStackTrace()
        log.error(s"Error creating API in backend: ${e.getMessage()}")
        log.error(s"Setting state of resource '${created.id}' to FAILED")
        val failstate = ResourceState.id(ResourceStates.Failed)
        ResourceFactory.update(created.copy(state = failstate), caller.id)
        Future.failed(e)
      }
    }) valueOr { errorMessage =>
      log.error(s"Failed to create Api: $errorMessage")
      Future.failed(new RuntimeException(errorMessage))
    }
  }

  def deleteApiHandler(resource: GestaltResourceInstance): Future[Unit] = {
    (for(
      apiProperties <- ResourceSerde.deserialize[ApiProperties](resource);
      pi <- getGwmProviderAndImpl(apiProperties.provider.id);
      (provider, impl) = pi
    ) yield {
      impl.deleteApi(provider, resource) recoverWith { case e: Throwable =>
        log.error(s"Error deleting API from backend: " + e.getMessage)
        Future.failed(e)
      }
    }) valueOr { errorMessage =>
      log.error(s"Failed to delete Api: $errorMessage")
      Future.failed(new RuntimeException(errorMessage))
    }
  }

  def createEndpoint(org: UUID, api: GestaltResourceInstance, requestBody: JsValue,
   caller: AccountLike): Future[GestaltResourceInstance] = {
    (for(
      apiProperties <- ResourceSerde.deserialize[ApiProperties](api);
      gri <- eitherFromJsResult(requestBody.validate[GestaltResourceInput]);
      typeId = gri.resource_type.getOrElse(ResourceIds.ApiEndpoint);
      ownerLink = toOwnerLink(ResourceIds.User, caller.id, name=Some(caller.name), orgId=caller.orgId);
      endpointId = gri.id.getOrElse(UUID.randomUUID);
      rawProperties <- Either.fromOption(gri.properties, s"Properties not set on resource ${gri.id}");
      _ = log.debug(s"rawProperties: $rawProperties");
      payload = gri.copy(
        id=Some(endpointId),
        owner=gri.owner.orElse(Some(ownerLink)),
        resource_state=gri.resource_state.orElse(Some(ResourceStates.Active)),
        resource_type=Some(typeId),
        properties=Some(rawProperties ++ Map(
          "parent" -> Json.toJson(api.id),
          "location_id" -> Json.toJson(apiProperties.provider.locations.headOption.getOrElse("")),
          "provider" -> Json.toJson(apiProperties.provider)
        ))
      );
      resource = inputToInstance(org, payload);
      endpointProperties <- ResourceSerde.deserialize[ApiEndpointProperties](resource);
      pi <- getGwmProviderAndImpl(apiProperties.provider.id);
      (provider, impl) = pi;
      created <- eitherFromTry(ResourceFactory.create(ResourceIds.User, caller.id)(resource, Some(api.id)));
      _ <- eitherFromTry(Try(setNewResourceEntitlements(org, created.id, caller, Some(api.id))))
    ) yield {
      log.info("Endpoint created in Meta... creating Endpoint in backend...")
      impl.createEndpoint(provider, created) map { updatedResource =>
        ResourceFactory.update(updatedResource, caller.id).get
      } recoverWith { case e =>
        e.printStackTrace()
        log.error(s"Error creating Endpoint in backend: ${e.getMessage()}")
        log.error(s"Setting state of resource '${created.id}' to FAILED")
        val failstate = ResourceState.id(ResourceStates.Failed)
        ResourceFactory.update(created.copy(state = failstate), caller.id).get
        Future.failed(e)
      }
    }) valueOr { errorMessage =>
      log.error(s"Failed to create Endpoint: $errorMessage")
      Future.failed(new RuntimeException(errorMessage))
    }
  }

  def deleteEndpointHandler(resource: GestaltResourceInstance): Future[Unit] = {
    (for(
      endpointProperties <- ResourceSerde.deserialize[ApiEndpointProperties](resource);
      api <- Either.fromOption(ResourceFactory.findParent(ResourceIds.Api, resource.id),
       s"Could not find API parent of ApiEndpoint ${resource.id}");
      apiProperties <- ResourceSerde.deserialize[ApiProperties](api);
      pi <- getGwmProviderAndImpl(apiProperties.provider.id);
      (provider, impl) = pi
    ) yield {
      log.info("Deleting Endpoint from backend...")
      impl.deleteEndpoint(provider, resource) recoverWith { case e: Throwable =>
        e.printStackTrace()
        log.error(s"Error deleting Endpoint from backend: " + e.getMessage)
        Future.successful(())
      }
    }) valueOr { errorMessage =>
      log.error(s"Failed to delete Endpoint: $errorMessage")
      Future.failed(new RuntimeException(errorMessage))
    }
  }

  def updateEndpoint(originalResource: GestaltResourceInstance, patch: PatchDocument): Future[GestaltResourceInstance] = {

    (for(
      resource <- eitherFromTry(PatchInstance.applyPatch(originalResource, patch));
      endpointProperties <- ResourceSerde.deserialize[ApiEndpointProperties](resource);
      api <- Either.fromOption(ResourceFactory.findParent(ResourceIds.Api, resource.id),
       s"Could not find API parent of ApiEndpoint ${resource.id}");
      apiProperties <- ResourceSerde.deserialize[ApiProperties](api);
      pi <- getGwmProviderAndImpl(apiProperties.provider.id);
      (provider, impl) = pi
    ) yield {
      impl.updateEndpoint(provider, resource, patch)// map { updatedResource =>
        // no access to caller id – this works fine as is though
        // ResourceFactory.update(updatedResource, user.account.id).get
      // }
    }) valueOr { errorMessage =>
      log.error(s"Failed to update Endpoint: $errorMessage")
      Future.failed(new RuntimeException(errorMessage))
    }
  }
}