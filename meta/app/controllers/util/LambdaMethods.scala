package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.patch.PatchDocument
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.util.Either._
import com.galacticfog.gestalt.meta.providers.faas._
import cats.syntax.either._
import javax.inject.Inject
import play.api.Logger
import play.api.mvc.RequestHeader

import play.api.libs.concurrent.Execution.Implicits.defaultContext

class LambdaMethods @Inject()(
    laserProviderImpl: LaserProvider,
    awsLambdaProvider: AWSLambdaProvider
  ) extends AuthorizationMethods {
  
  override val log = Logger(this.getClass)

  import LambdaSpec.Implicits._

  private def findLambdaProviderById(id: UUID): Either[String,GestaltResourceInstance] = {
    log.debug("findLambdaProviderById")
    val opt = ResourceFactory.findById(ResourceIds.LambdaProvider, id) orElse
     ResourceFactory.findById(migrations.V20.AWS_LAMBDA_PROVIDER_TYPE_ID, id)
    Either.fromOption(opt, s"No LambdaProvider or AWSLambdaProvider found with id `$id`")
  }

  private def getLambdaProvider(res: GestaltResourceInstance): Either[String,GestaltResourceInstance] = {
    log.debug("getLambdaProvider")
    for {
      rawProperties <- Either.fromOption(res.properties, s"Properties not set on resource ${res.id}");
      rawProvider <- Either.fromOption(rawProperties.get("provider"), s"Provider id not set on resource ${res.id}");
      inlineProvider <- eitherFromJsResult(Json.parse(rawProvider).validate[InlineLambdaProvider]);
      provider <- findLambdaProviderById(inlineProvider.id)
    } yield provider
  }

  private def getProviderImpl[A](provider: GestaltResourceInstance): Either[String,FaasProviderImplementation[Future]] = {
    log.debug("getProviderImpl")
    if(provider.typeId == migrations.V20.AWS_LAMBDA_PROVIDER_TYPE_ID) {
      Right(awsLambdaProvider)
    }else if(provider.typeId == ResourceIds.LambdaProvider) {
      Right(laserProviderImpl)
    }else {
      Left(s"No faas implementation exists for provider `${provider.id}` with typeId `${provider.typeId}`")
    }
  }

  def getLambdaStreams(streamSpec: GestaltResourceInstance, user: AuthAccountWithCreds): Future[JsValue] = {
    /*
     * Get lambda from stream to get provider.
    */

    val eitherF: Either[String,Future[JsValue]] = for(
      rawProperties <- Either.fromOption(streamSpec.properties,
       s"Could not get lambda streams for resource ${streamSpec.id} with unset properties");
      rawJsonProperties = unstringmap(Some(rawProperties)).get;
      streamSpecProperties <- eitherFromJsResult(JsObject(rawJsonProperties).validate[StreamSpecProperties]);
      lambdaId = streamSpecProperties.processor.lambdaId;
      _ = log.debug(s"Found Lambda ID : ${lambdaId}");
      lambdaResource <- Either.fromOption(ResourceFactory.findById(ResourceIds.Lambda, lambdaId),
       s"Lambda with ID '${lambdaId}' not found.");
      lambdaProvider <- getLambdaProvider(lambdaResource)
    ) yield laserProviderImpl.getStreamDefinitions(lambdaProvider, streamSpec)

    eitherF valueOr { errorMessage =>
      Future.failed(new RuntimeException(errorMessage))
    }
  }
  
  def deleteLambdaHandler(r: GestaltResourceInstance): Future[Unit] = {
    (for(
      provider <- getLambdaProvider(r);
      impl <- getProviderImpl(provider)
    ) yield {
      impl.deleteLambda(provider, r) recoverWith { case throwable =>
        throwable.printStackTrace()
        log.error(s"Failed to delete backend lambda: ${throwable.getMessage()}")
        Future.successful(())
      }
    }) valueOr { errorMessage =>
      // Future.failed(new RuntimeException(errorMessage))
      log.error(s"Failed to delete backend lambda: $errorMessage")
      Future.successful(())
    }
  }

  def patchLambdaHandler( r: GestaltResourceInstance,
                          patch: PatchDocument,
                          user: AuthAccountWithCreds,
                          request: RequestHeader ): Future[GestaltResourceInstance] = {
    (for(
      provider <- getLambdaProvider(r);
      impl <- getProviderImpl(provider)
    ) yield {
      impl.updateLambda(provider, r, patch) map { updatedLambdaResource =>
        ResourceFactory.update(updatedLambdaResource, user.account.id)
        updatedLambdaResource
      }
    }) valueOr { errorMessage =>
      Future.failed(new RuntimeException(errorMessage))
    }
  }
  
  def createLambda(
    org: UUID, 
    parent: GestaltResourceInstance,
    requestBody: JsValue,
    caller: AccountLike): Future[GestaltResourceInstance] = {

    log.debug("createLambda")

    (for(
      gri <- eitherFromJsResult(requestBody.validate[GestaltResourceInput]);
      typeId = gri.resource_type.getOrElse(ResourceIds.Lambda);
      parentLink = Json.toJson(toLink(parent, None));
      lambdaId = gri.id.getOrElse(UUID.randomUUID);
      ownerLink = toOwnerLink(ResourceIds.User, caller.id, name=Some(caller.name), orgId=caller.orgId);
      rawProperties <- Either.fromOption(gri.properties, "Properties not set");
      payload = gri.copy(
        id=Some(lambdaId),
        owner=gri.owner.orElse(Some(ownerLink)),
        resource_state=gri.resource_state.orElse(Some(ResourceStates.Active)),
        resource_type=Some(typeId),
        properties=Some(rawProperties ++ Map("parent" -> parentLink))
      );
      resource = inputToInstance(org, payload);
      lambdaProvider <- getLambdaProvider(resource);
      impl <- getProviderImpl(lambdaProvider);
      check = PolicyCheck("lambda.create").proceed(RequestOptions(
        user = null,
        authTarget = Option(parent.id),
        policyOwner = Option(parent.id),
        policyTarget = Option(resource),
        providerIdOpt = Option(lambdaProvider.id))).toTry().get;
      lambdaResource <- eitherFromTry(ResourceFactory.create(ResourceIds.User, caller.id)(resource, Some(parent.id)));
      _ <- eitherFromTry(Try(setNewResourceEntitlements(org, lambdaResource.id, caller, Some(parent.id))))
    ) yield {
      impl.createLambda(lambdaProvider, lambdaResource) map { createdLambdaResource =>
        log.info("Successfully created Lambda in backend system.")
        setNewResourceEntitlements(org, createdLambdaResource.id, caller, Some(parent.id))
        
        ResourceFactory.update(createdLambdaResource, caller.id).get
        
        createdLambdaResource
      } recoverWith {
        case throwable: Throwable => {
          throwable.printStackTrace()
          log.error(s"Error creating Lambda in backend system.")
          
          log.error(s"Setting state of resource '${lambdaResource.id}' to FAILED")
          ResourceFactory.update(lambdaResource.copy(state=ResourceState.id(ResourceStates.Failed)), caller.id).get

          Future.failed(throwable)
        }
      }
    }) valueOr { errorMessage =>
      log.error(s"Failed to create Lambda in Meta: $errorMessage")
      Future.failed(new RuntimeException(errorMessage))
    }
  }

  def importLambda(
    org: UUID, 
    parent: GestaltResourceInstance,
    requestBody: JsValue,
    caller: AccountLike): Future[GestaltResourceInstance] = {

    log.debug("importLambda")

    (for(
      gri <- eitherFromJsResult(requestBody.validate[GestaltResourceInput]);
      typeId = gri.resource_type.getOrElse(ResourceIds.Lambda);
      parentLink = Json.toJson(toLink(parent, None));
      lambdaId = gri.id.getOrElse(UUID.randomUUID);
      ownerLink = toOwnerLink(ResourceIds.User, caller.id, name=Some(caller.name), orgId=caller.orgId);
      rawProperties <- Either.fromOption(gri.properties, "Properties not set");
      payload = gri.copy(
        id=Some(lambdaId),
        owner=gri.owner.orElse(Some(ownerLink)),
        resource_state=gri.resource_state.orElse(Some(ResourceStates.Active)),
        resource_type=Some(typeId),
        properties=Some(rawProperties ++ Map("parent" -> parentLink))
      );
      resource = inputToInstance(org, payload);
      lambdaProvider <- getLambdaProvider(resource);
      impl <- getProviderImpl(lambdaProvider);
      lambdaResource <- eitherFromTry(ResourceFactory.create(ResourceIds.User, caller.id)(resource, Some(parent.id)));
      _ <- eitherFromTry(Try(setNewResourceEntitlements(org, lambdaResource.id, caller, Some(parent.id))))
    ) yield {
      impl.importLambda(lambdaProvider, lambdaResource) map { importedLambdaResource =>
        log.info("Found Lambda in backend system.")
        setNewResourceEntitlements(org, importedLambdaResource.id, caller, Some(parent.id))
        
        ResourceFactory.update(importedLambdaResource, caller.id).get
        
        importedLambdaResource
      } recoverWith {
        case throwable: Throwable => {
          throwable.printStackTrace()
          log.error(s"Error creating Lambda in backend system.")
          
          log.error(s"Setting state of resource '${lambdaResource.id}' to FAILED")
          ResourceFactory.update(lambdaResource.copy(state=ResourceState.id(ResourceStates.Failed)), caller.id).get

          Future.failed(throwable)
        }
      }
    }) valueOr { errorMessage =>
      log.error(s"Failed to create Lambda in Meta: $errorMessage")
      Future.failed(new RuntimeException(errorMessage))
    }
  }

  /**
   * Find the LambdaProvider backing the given Lambda
   */
  def findLambdaProvider(lambda: GestaltResourceInstance): Option[GestaltResourceInstance] = {
    getLambdaProvider(lambda).toOption
  }
  
  /**
   * Find the Message (rabbit) provider linked to the given LambdaProvider
   */
  def findMessageProvider(lambdaProvider: GestaltResourceInstance): Option[GestaltResourceInstance] = {
    (for(
      rawProperties <- Either.fromOption(lambdaProvider.properties,
       s"Could not get message provider for resource ${lambdaProvider.id} with unset properties");
      rawJsonProperties = rawProperties map { case(k, v) => (k, Json.parse(v)) };
      properties <- eitherFromJsResult(JsObject(rawJsonProperties).validate[LaserLambdaProperties]);
      linkedProviders = properties.linked_providers.getOrElse(Seq());
      messageProvider <- Either.fromOption(linkedProviders.find(_.name == "RABBIT"),
       s"No MessageProvider configured for LambdaProvider '${lambdaProvider.id}'. Cannot invoke action.")
    ) yield ResourceFactory.findById(messageProvider.id)) valueOr { errorMessage =>
      throw new RuntimeException(errorMessage)
    }
  }
}