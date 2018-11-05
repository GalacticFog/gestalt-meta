package controllers.util

import java.util.UUID

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import com.galacticfog.gestalt.data.ResourceFactory
import play.api.libs.json._
import play.api.libs.ws.WSClient

import scala.concurrent.Future
import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.data.TypeFactory
import com.galacticfog.gestalt.patch.PatchDocument
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.patch.PatchInstance
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods  
import com.galacticfog.gestalt.meta.providers._  
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.meta.providers.LinkedProvider
import com.galacticfog.gestalt.util.Either._
import cats.syntax.either._
import cats.syntax.traverse._
import cats.instances.try_._
import cats.instances.either._
import cats.instances.vector._
import javax.inject.Inject
import play.api.Logger
import play.api.mvc.RequestHeader

object LambdaMethods {
  case class InlineSecretProvider(
    id: UUID
  )
  case class SecretProperties(
    provider: InlineSecretProvider
  )

  implicit val inlineSecretProviderFormat = Json.format[InlineSecretProvider]
  implicit val secretPropertiesFormat = Json.format[SecretProperties]

  case class LambdaProviderPropertiesConfigEnv(
    public: Map[String,String]
  )
  case class LambdaProviderPropertiesConfig(
    env: LambdaProviderPropertiesConfigEnv
  )
  case class LambdaProviderProperties(
    schema: Option[String],
    config: Option[LambdaProviderPropertiesConfig],
    linked_providers: Option[Seq[LinkedProvider]],
    services: Option[Seq[JsValue]],
    environments: Option[Seq[String]],
    config_schema: Option[JsValue],
    provider_actions: Option[Seq[JsValue]],
    environment_types: Option[Seq[String]]
  )

  implicit val lambdaProviderPropertiesConfigEnvFormat = Json.format[LambdaProviderPropertiesConfigEnv]
  implicit val lambdaProviderPropertiesConfigFormat = Json.format[LambdaProviderPropertiesConfig]
  implicit val linkedProviderFormat = Json.format[LinkedProvider]
  implicit val lambdaProviderPropertiesFormat = Json.format[LambdaProviderProperties]

  case class InlineLambdaProvider(
    id: UUID,
    locations: Option[Seq[JsValue]]
  )
  case class LambdaProperties(
    public: Boolean,
    runtime: String,
    code_type: String,
    memory: Int,
    cpus: Double,
    timeout: Int,
    provider: InlineLambdaProvider,
    handler: String,
    messaging: Option[JsValue],
    package_url: Option[String],
    code: Option[String],
    parent: Option[JsValue],
    headers: Option[Map[String,String]],
    compressed: Option[Boolean],
    periodic_info: Option[JsValue],
    apiendpoints: Option[JsValue],
    pre_warm: Option[Int],
    secrets: Option[Seq[ContainerSpec.SecretMount]],
    linked_providers: Option[Seq[LinkedProvider]],

    isolate: Option[Boolean]    // not present in /root/resourcetypes/... for me
  )

  implicit val inlineLambdaProviderFormat = Json.format[InlineLambdaProvider]
  implicit val lambdaPropertiesFormat = Json.format[LambdaProperties]

  case class StreamSpecProcessor(
    lambdaId: UUID
  )

  case class StreamSpecProperties(
    processor: StreamSpecProcessor
  )

  implicit val streamSpecProcessorFormat = Json.format[StreamSpecProcessor]
  implicit val streamSpecPropertiesFormat = Json.format[StreamSpecProperties]

  case class LaserArtifactDescription(
      artifactUri: Option[String],
      runtime: String,
      handler: String,
      memorySize: Int,
      cpus: Double,
      description: Option[String] = None,
      compressed: Boolean = false,
      publish: Boolean = false,
      role: String = "none",
      timeoutSecs: Int = 180,
      code: Option[String] = None,
      periodicInfo : Option[JsValue] = None,
      headers : Map[String,String] = Map.empty,
      computePathOverride: Option[String] = None,
      secrets: Option[Seq[JsObject]] = None,
      preWarm: Option[Int] = None,
      isolate: Option[Boolean] = None)

  case class LaserLambda(
      id: Option[String], 
      eventFilter: Option[String],
      public: Boolean,
      provider: Option[JsValue],
      artifactDescription: LaserArtifactDescription)
  
  implicit val laserArtifactDescriptionFormat = Json.format[LaserArtifactDescription]
  implicit val laserLambdaFormat = Json.format[LaserLambda]
}

class LambdaMethods @Inject()( ws: WSClient,
                               providerMethods: ProviderMethods ) extends AuthorizationMethods {
  
  override val log = Logger(this.getClass)

  import LambdaMethods._

  private def getLambdaProvider(res: ResourceLike): Either[String,GestaltResourceInstance] = {
    for {
      rawProperties <- Either.fromOption(res.properties,
       s"Could not get LambdaProvider for resource ${res.id} with unset properties");
      rawJsonProperties = unstringmap(Some(rawProperties)).get
      properties <- eitherFromJsResult(JsObject(rawJsonProperties).validate[LambdaProperties])
      provider <- Either.fromOption(ResourceFactory.findById(ResourceIds.LambdaProvider, properties.provider.id),
       "Could not parse LambdaProvider ID from API.")
    } yield provider
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
    ) yield {
      val client = providerMethods.configureWebClient(lambdaProvider, Some(ws))
      val streamUrl = s"/streamDefinitions/${streamSpec.id.toString}/streams"
      log.debug(s"Stream URL : $streamUrl")
      client.get(streamUrl) map { result =>
        Json.parse(result.body)
      } recover {
        case e: Throwable => {
          log.error(s"Error looking up streams: ${e.getMessage}")
          throw e
        }      
      }
    }

    eitherF valueOr { errorMessage =>
      Future.failed(new RuntimeException(errorMessage))
    }
  }
  
  def deleteLambdaHandler( r: ResourceLike ): Future[Unit] = {

    log.debug("Finding lambda in backend system...")

    getLambdaProvider(r) match {
      case Left(errorMessage) => Future.failed(new RuntimeException(errorMessage))
      case Right(provider) => {
        val client = providerMethods.configureWebClient(provider, Some(ws))

        client.delete(s"/lambdas/${r.id.toString}") map { result =>
          log.info("Deleting API from Lambda backend...")
          log.debug("Response from Lambda backend: " + result.body)
        } recover {
          case e: Throwable => {
            log.error(s"Error deleting lambda from Lambda backend: " + e.getMessage)
            throw e
          }
        }
      }
    }
  }

  def patchLambdaHandler( r: GestaltResourceInstance,
                          patch: PatchDocument,
                          user: AuthAccountWithCreds,
                          request: RequestHeader ): Future[GestaltResourceInstance] = {

    val eitherFR: Either[String,Future[GestaltResourceInstance]] = for(
      provider <- getLambdaProvider(r);
      updatedLambda <- eitherFromTry(PatchInstance.applyPatch(r, patch));
      patchedLaserLambda <- toLaserLambda(updatedLambda, provider.id)
    ) yield {
      val client = providerMethods.configureWebClient(provider, Some(ws))
      client.put(s"/lambdas/${r.id}", Some(Json.toJson(patchedLaserLambda))) map { response =>
        if(response.status == 200) {
          log.info(s"Successfully PUT Lambda in lambda provider.")
          ()
        }else {
          log.error(s"Error PUTting Lambda in lambda provider: ${response}")
          throw ApiError(response.status, response.body).throwable
        }
      } flatMap { _ =>
        Future.fromTry(PatchInstance.applyPatch(r, patch))
        // we don't actually use the result from laser, though we probably should
      }
    }

    eitherFR valueOr { errorMessage =>
      Future.failed(new RuntimeException(errorMessage))
    }
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
      lambdaProvider <- Either.fromOption(ResourceFactory.findById(ResourceIds.LambdaProvider, properties.provider.id),
       s"Lambda Provider with ID '${properties.provider.id}' not found.");
      lambdaResource <- eitherFromTry(ResourceFactory.create(ResourceIds.User, caller.id)(instance, Some(parent.id)));
      _ <- eitherFromTry(setNewResourceEntitlements(org, lambdaResource.id, caller, Some(parent.id)).toVector.sequence);
      laserLambda <- toLaserLambda(lambdaResource, lambdaProvider.id)
    ) yield {
      val client = providerMethods.configureWebClient(lambdaProvider, Some(ws))

      log.debug("Creating lambda in Laser...")
      client.post("/lambdas", Option(Json.toJson(laserLambda))) map { result =>
        if (Seq(200, 201).contains(result.status)) {
          log.info("Successfully created Lambda in backend system.")
          setNewResourceEntitlements(org, lambdaResource.id, caller, Some(parent.id))
          lambdaResource
        } else {
          throw ApiError(result.status, result.body).throwable
        }
      } recoverWith {
        case throwable: Throwable => {
          log.error(s"Error creating Lambda in backend system.")
          
          log.error(s"Setting state of resource '${lambdaResource.id}' to FAILED")
          ResourceFactory.update(lambdaResource.copy(state=ResourceState.id(ResourceStates.Failed)), caller.id)

          Future.failed(throwable)
        }
      }
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

  def toLaserLambda(lambda: GestaltResourceInstance, providerId: UUID): Either[String,LaserLambda] = {

    log.debug("toLaserLambda(...)")

    type EitherString[A] = Either[String,A]

    for(
      rawProperties <- Either.fromOption(lambda.properties, s"Cannot use lambda resource ${lambda.id} with unset properties map");
      rawJsonProperties = unstringmap(Some(rawProperties)).get;
      properties <- eitherFromJsResult(JsObject(rawJsonProperties).validate[LambdaProperties]);

      lambdaProvider <- Either.fromOption(ResourceFactory.findById(ResourceIds.LambdaProvider, properties.provider.id),
       s"Lambda '${properties.provider.id}' provider did not exist");
      rawLambdaProviderProperties <- Either.fromOption(lambdaProvider.properties, s"Cannot use lambda provider resource ${lambdaProvider.id} with unset properties map");
      rawJsonLambdaProviderProperties = unstringmap(Some(rawLambdaProviderProperties)).get;
      lambdaProviderProperties <- eitherFromJsResult(JsObject(rawJsonLambdaProviderProperties).validate[LambdaProviderProperties]);

      fqon <- Either.fromOption(OrgCache.getFqon(lambda.orgId), s"Could not determine FQON for lambda ${lambda.id}.");
      lambdaCaasProviderId <- Either.fromOption(lambdaProviderProperties.config.flatMap(_.env.public.get("META_COMPUTE_PROVIDER_ID")).map(UUID.fromString(_)),
       s"Lambda '${lambda.id}' provider '${lambdaProvider.id}' did not have '.properties.config.env.public.META_COMPUTE_PROVIDER_ID'");
      lambdaEnvironment <- Either.fromOption(ResourceFactory.findParent(parentType=ResourceIds.Environment, childId=lambda.id),
       s"Could not locate parent Environment for Lambda '${lambda.id}'");

      secrets <- properties.secrets.getOrElse(Seq()).toVector.traverse[EitherString,GestaltResourceInstance] { secretMount => 
        Either.fromOption(ResourceFactory.findById(ResourceIds.Secret, secretMount.secret_id),
          s"Secret '${secretMount.secret_id}' does not exist")
      };
      secretProperties <- secrets.traverse[EitherString,SecretProperties] { secret =>
        for(
          rawProperties <- Either.fromOption(secret.properties, s"Cannot use secret resource ${secret.id} with unset properties");
          rawJsonProperties = unstringmap(Some(rawProperties)).get;
          properties <- eitherFromJsResult(JsObject(rawJsonProperties).validate[SecretProperties])
        ) yield properties
      };
      _ <- secretProperties.map(_.provider.id).distinct match {
        case Vector() => Right(())
        case Vector(providerId) if providerId == lambdaCaasProviderId => Right(())
        case Vector(_) => Left(s"Lambda '${lambda.id}' provider '${properties.provider.id}' did not have same CaaS provider as mounted Secrets")
        case _ => Left("Secrets must have the same CaaS provider")
      };
      secretEnvironments <- secrets.traverse[EitherString,GestaltResourceInstance] { secret =>
        Either.fromOption(ResourceFactory.findParent(parentType=ResourceIds.Environment, childId=secret.id),
         s"Could not locate parent Environment for Secret '${secret.id}'")
      };
      _ <- secretEnvironments.map(_.id).distinct match {
        case Vector() => Right(())
        case Vector(environmentId) if environmentId == lambdaEnvironment.id => Right(())
        case Vector(_) => Left(s"Lambda '${lambda.id}' must belong to the same Environment as all mounted Secrets")
        case _ => Left("All mounted Secrets must belong to the same Environment")
      }
    ) yield {
      val executorEnvironmentOptId = if(properties.pre_warm.exists(_ > 0)) {
        Some(lambdaEnvironment.id)
      }else {
        secretEnvironments.headOption.map(_.id)
      }
      val computePathOpt = executorEnvironmentOptId.map(controllers.routes.ContainerController.postContainer(fqon, _).url)
      LaserLambda(
        id = Some(lambda.id.toString),
        eventFilter = Some(UUID.randomUUID.toString),
        public = properties.public,
        provider = Some(Json.obj(
          "id" -> properties.provider.id.toString,
          "location" -> "",
          "href" -> "/foo/bar"
        )),
        LaserArtifactDescription(
          artifactUri = properties.package_url,
          description = None,     // no such property
          handler = properties.handler,
          memorySize = properties.memory.toInt,
          cpus = properties.cpus.toDouble,
          publish = false,     // <- currently not used
          role = "none",    // <- currently not used
          preWarm = properties.pre_warm,
          runtime = properties.runtime,
          timeoutSecs = properties.timeout,
          compressed = properties.compressed.getOrElse(false),
          periodicInfo = properties.periodic_info,
          code = properties.code,
          headers = properties.headers.getOrElse(Map()),
          isolate = properties.isolate,
          secrets = Some(properties.secrets.getOrElse(Seq()).map(Json.toJson(_).as[JsObject])),
          computePathOverride = computePathOpt
        )
      )
    }
  }
}