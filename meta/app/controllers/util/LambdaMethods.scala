package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import play.api.libs.json._

import scala.concurrent.Future
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.TypeFactory
import com.galacticfog.gestalt.patch.PatchDocument
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
// import com.galacticfog.gestalt.meta.api.ContainerSpec
// import com.galacticfog.gestalt.meta.api.patch.PatchInstance
// import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods  
// import com.galacticfog.gestalt.meta.providers._  
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.data.ResourceState
// import com.galacticfog.gestalt.meta.providers.LinkedProvider
import com.galacticfog.gestalt.util.Either._
import com.galacticfog.gestalt.meta.providers.faas._
import cats.syntax.either._
import cats.syntax.traverse._
import cats.instances.try_._
// import cats.instances.either._
import cats.instances.vector._
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
    val opt = ResourceFactory.findById(ResourceIds.LambdaProvider, id) orElse
     ResourceFactory.findById(migrations.V20.AWS_LAMBDA_PROVIDER_TYPE_ID, id)
    Either.fromOption(opt, s"No LambdaProvider or AWSLambdaProvider found with id `$id`")
  }

  private def getLambdaProvider(res: GestaltResourceInstance): Either[String,GestaltResourceInstance] = {
    for {
      rawProperties <- Either.fromOption(res.properties,
       s"Could not get LambdaProvider for resource ${res.id} with unset properties");
      rawJsonProperties = unstringmap(Some(rawProperties)).get
      properties <- eitherFromJsResult(JsObject(rawJsonProperties).validate[LambdaProperties])
      provider <- findLambdaProviderById(properties.provider.id)
    } yield provider
  }

  private def getProviderImpl[A](provider: GestaltResourceInstance): Either[String,FaasProviderImplementation[Future]] = {
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

    log.debug("Finding lambda in backend system...")

    (for(
      provider <- getLambdaProvider(r);
      impl <- getProviderImpl(provider)
    ) yield impl.deleteLambda(provider, r)) valueOr { errorMessage =>
      Future.failed(new RuntimeException(errorMessage))
    }

    // getLambdaProvider(r) match {
    //   case Left(errorMessage) => Future.failed(new RuntimeException(errorMessage))
    //   case Right(provider) => {

    //     val client = providerMethods.configureWebClient(provider, Some(ws))

    //     client.delete(s"/lambdas/${r.id.toString}") map { result =>
    //       log.info("Deleting API from Lambda backend...")
    //       log.debug("Response from Lambda backend: " + result.body)
    //     } recover {
    //       case e: Throwable => {
    //         log.error(s"Error deleting lambda from Lambda backend: " + e.getMessage)
    //         throw e
    //       }
    //     }
    //   }
    // }
  }

  def patchLambdaHandler( r: GestaltResourceInstance,
                          patch: PatchDocument,
                          user: AuthAccountWithCreds,
                          request: RequestHeader ): Future[GestaltResourceInstance] = {

    val eitherFR: Either[String,Future[GestaltResourceInstance]] = for(
      provider <- getLambdaProvider(r);
      impl <- getProviderImpl(provider)//;
      // updatedLambda <- eitherFromTry(PatchInstance.applyPatch(r, patch));
      // patchedLaserLambda <- toLaserLambda(updatedLambda, provider.id)
    ) yield {
      impl.updateLambda(provider, r, patch)
      // val client = providerMethods.configureWebClient(provider, Some(ws))
      // client.put(s"/lambdas/${r.id}", Some(Json.toJson(patchedLaserLambda))) map { response =>
      //   if(response.status == 200) {
      //     log.info(s"Successfully PUT Lambda in lambda provider.")
      //     ()
      //   }else {
      //     log.error(s"Error PUTting Lambda in lambda provider: ${response}")
      //     throw ApiError(response.status, response.body).throwable
      //   }
      // } flatMap { _ =>
      //   Future.fromTry(PatchInstance.applyPatch(r, patch))
      //   // we don't actually use the result from laser, though we probably should
      // }
    }

    eitherFR valueOr { errorMessage =>
      Future.failed(new RuntimeException(errorMessage))
    }

    // val eitherFR: Either[String,Future[GestaltResourceInstance]] = for(
    //   provider <- getLambdaProvider(r);
    //   updatedLambda <- eitherFromTry(PatchInstance.applyPatch(r, patch));
    //   patchedLaserLambda <- toLaserLambda(updatedLambda, provider.id)
    // ) yield {
    //   val client = providerMethods.configureWebClient(provider, Some(ws))
    //   client.put(s"/lambdas/${r.id}", Some(Json.toJson(patchedLaserLambda))) map { response =>
    //     if(response.status == 200) {
    //       log.info(s"Successfully PUT Lambda in lambda provider.")
    //       ()
    //     }else {
    //       log.error(s"Error PUTting Lambda in lambda provider: ${response}")
    //       throw ApiError(response.status, response.body).throwable
    //     }
    //   } flatMap { _ =>
    //     Future.fromTry(PatchInstance.applyPatch(r, patch))
    //     // we don't actually use the result from laser, though we probably should
    //   }
    // }

    // eitherFR valueOr { errorMessage =>
    //   Future.failed(new RuntimeException(errorMessage))
    // }
  }
  
  def createLambdaCommon(
    org: UUID, 
    parent: GestaltResourceInstance,
    requestBody: JsValue,
    caller: AccountLike): Future[GestaltResourceInstance] = {

    val eitherFR: Either[String,Future[GestaltResourceInstance]] = for(
      gri <- eitherFromJsResult(requestBody.validate[GestaltResourceInput]);
      typeId = gri.resource_type.getOrElse(ResourceIds.Lambda);
      _ <- Either.fromOption(TypeFactory.findById(typeId), Errors.TYPE_NOT_FOUND(typeId));
      rawProperties <- Either.fromOption(gri.properties, "Provider properties not set");
      properties0 <- eitherFromJsResult(JsObject(rawProperties).validate[LambdaProperties]);
      parentLink = Json.toJson(toLink(parent, None));
      properties = properties0.copy(parent=Some(parentLink));
      lambdaId = gri.id.getOrElse(UUID.randomUUID);
      ownerLink = toOwnerLink(ResourceIds.User, caller.id, name=Some(caller.name), orgId=caller.orgId);
      payload = gri.copy(
        id=Some(lambdaId),
        owner=gri.owner.orElse(Some(ownerLink)),
        resource_state=gri.resource_state.orElse(Some(ResourceStates.Active)),
        resource_type=Some(typeId),
        properties=Some(Json.toJson(properties).as[Map[String,JsValue]])
      );
      instance = inputToInstance(org, payload);
      lambdaProvider <- findLambdaProviderById(properties.provider.id);
      impl <- getProviderImpl(lambdaProvider);
      lambdaResource <- eitherFromTry(ResourceFactory.create(ResourceIds.User, caller.id)(instance, Some(parent.id)));
      _ <- eitherFromTry(setNewResourceEntitlements(org, lambdaResource.id, caller, Some(parent.id)).toVector.sequence)//;
      // laserLambda <- toLaserLambda(lambdaResource, lambdaProvider.id)
    ) yield {
      impl.createLambda(lambdaProvider, lambdaResource) map { createdLambdaResource =>
        log.info("Successfully created Lambda in backend system.")
        setNewResourceEntitlements(org, createdLambdaResource.id, caller, Some(parent.id))
        createdLambdaResource
        // should also save createdLambdaResource to db
      } recoverWith {
        case throwable: Throwable => {
          log.error(s"Error creating Lambda in backend system.")
          
          log.error(s"Setting state of resource '${lambdaResource.id}' to FAILED")
          ResourceFactory.update(lambdaResource.copy(state=ResourceState.id(ResourceStates.Failed)), caller.id)

          Future.failed(throwable)
        }
      }

      // val client = providerMethods.configureWebClient(lambdaProvider, Some(ws))

      // log.debug("Creating lambda in Laser...")
      // client.post("/lambdas", Option(Json.toJson(laserLambda))) map { result =>
      //   if (Seq(200, 201).contains(result.status)) {
      //     log.info("Successfully created Lambda in backend system.")
      //     setNewResourceEntitlements(org, lambdaResource.id, caller, Some(parent.id))
      //     lambdaResource
      //   } else {
      //     throw ApiError(result.status, result.body).throwable
      //   }
      // } recoverWith {
      //   case throwable: Throwable => {
      //     log.error(s"Error creating Lambda in backend system.")
          
      //     log.error(s"Setting state of resource '${lambdaResource.id}' to FAILED")
      //     ResourceFactory.update(lambdaResource.copy(state=ResourceState.id(ResourceStates.Failed)), caller.id)

      //     Future.failed(throwable)
      //   }
      // }
    }

    eitherFR valueOr { errorMessage =>
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
      properties <- eitherFromJsResult(JsObject(rawJsonProperties).validate[LambdaProperties]);
      linkedProviders = properties.linked_providers.getOrElse(Seq());
      messageProvider <- Either.fromOption(linkedProviders.find(_.name == "RABBIT"),
       s"No MessageProvider configured for LambdaProvider '${lambdaProvider.id}'. Cannot invoke action.")
    ) yield ResourceFactory.findById(messageProvider.id)) valueOr { errorMessage =>
      throw new RuntimeException(errorMessage)
    }
  }
}