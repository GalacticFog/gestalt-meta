package controllers


import java.util.UUID

import com.galacticfog.gestalt.data.{EnvironmentType, ResourceFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceLabel, ResourceStates, GestaltResourceInput}
import com.galacticfog.gestalt.meta.api.{ContainerSpec, SecretSpec, VolumeSpec}
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltFrameworkSecurity}
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.Resource
import com.galacticfog.gestalt.util.Error
import com.galacticfog.gestalt.util.EitherWithErrors._
import com.galacticfog.gestalt.util.ResourceSerde
import com.galacticfog.tracking.CaasTrackingProvider
import com.google.inject.Inject
import controllers.util._
import cats.syntax.either._
import javax.inject.Singleton
import play.api.i18n.MessagesApi
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import services._

import scala.concurrent.Future
import scala.util.Try


@Singleton
class ContainerController @Inject()(
     messagesApi: MessagesApi,
     sec: GestaltFrameworkSecurity,
     containerService: ContainerService,
     providerManager: ProviderManager,
     genericResourceMethods: GenericResourceMethods,
     resourceController: ResourceController,
     db: play.api.db.Database,
     trackingProvider: CaasTrackingProvider)
    extends SecureController(messagesApi = messagesApi, sec = sec) with Authorization {

  type MetaCreate = (AuthAccountWithCreds, GestaltResourceInstance, UUID) => Try[GestaltResourceInstance]
  type BackendCreate = (ProviderContext, GestaltResourceInstance) => Future[GestaltResourceInstance]
  
  
  /*
   * TODO: ??? Create a custom result type for CaasService to return:
   *  - SuccessWithUpdates
   *  - SuccessNoChange
   *  - Failed
   */

  /**
   * Test that the given Provider is compatible with the given Environment.
   */
  def assertCompatibleEnvType(provider: GestaltResourceInstance, env: GestaltResourceInstance): EitherError[Unit] = {
    
    // These are the values tha Meta will accept for 'environment_type'
    val knownTypes = Set("development", "test", "production")

    for(
      providerProperties <- Either.fromOption(provider.properties, Error.Default("Empty provider properties"));
      envTypesJson <- eitherFromTry(Try(Json.parse(providerProperties.get("environment_types").getOrElse("[]"))));
      providerEnvTypes <- eitherFromJsResult(envTypesJson.validate[Seq[String]]);
      envProperties <- Either.fromOption(env.properties, Error.Default("Empty environment properties"));
      typeId <- Either.fromOption(envProperties.get("environment_type"),
       Error.Default(s"Environment with ID '${env.id}' does not contain 'environment_type' property. Data is corrupt"));
      envType = EnvironmentType.name(UUID.fromString(typeId)).trim.toLowerCase;
      _ <- if(knownTypes.contains(envType)) { Right(()) }else {
        Left(Error.UnprocessableEntity(s"Invalid environment_type. Expected: one of (${knownTypes.mkString(",")}. found: '$envType'"))
      };
      _ <- if(providerEnvTypes.isEmpty || providerEnvTypes.contains(envType)) { Right(()) }else {
        Left(Error.Conflict(s"Incompatible Environment type. Expected one of (${providerEnvTypes.mkString(",")}). found: '$envType'"))
      }
    ) yield ()  
  }

  case class ProviderProperties(id: UUID)
  case class ImportPayloadProperties(provider: ProviderProperties)
  case class ImportPayload(properties: ImportPayloadProperties)

  implicit val formatProviderProperties = Json.format[ProviderProperties]
  implicit val formatImportPayloadProperties = Json.format[ImportPayloadProperties]
  implicit val formatImportPayload = Json.format[ImportPayload]

  private def notFoundMessage(typeId: UUID, id: UUID) = s"${ResourceLabel(typeId)} with ID '$id' not found."

  private def mkResource(requestBody: JsValue, caller: AccountLike, environment: GestaltResourceInstance, resourceType: UUID): EitherError[GestaltResourceInstance] = {
    for(
      // apparently resource_type is usually passed in `Gestalt::Resource::Container` format, not as an uuid,
      // but ContainerController#normalizeInput*** used to overwrite it anyway
      modifiedRequestBody <- eitherFromJsResult(requestBody.validate[JsObject]).map(_ - "resource_type");
      gri <- eitherFromJsResult(modifiedRequestBody.validate[GestaltResourceInput]);
      ownerLink = toOwnerLink(ResourceIds.User, caller.id, name=Some(caller.name), orgId=caller.orgId);
      parentLink = Json.toJson(toLink(environment, None));
      rawProperties <- Either.fromOption(gri.properties, "Properties not set").liftTo[EitherError];
      payload = gri.copy(
        id=gri.id.orElse(Some(UUID.randomUUID())),
        owner=gri.owner.orElse(Some(ownerLink)),
        resource_state=gri.resource_state.orElse(Some(ResourceStates.Active)),
        resource_type=gri.resource_type.orElse(Some(resourceType)),
        properties=Some(rawProperties ++ Map("parent" -> parentLink))
      )
    ) yield inputToInstance(caller.orgId, payload)
  }

  def postContainer(fqon: String, environment: UUID) = AsyncAudited(fqon) { implicit request =>
    def createContainer(org: GestaltResourceInstance, env: GestaltResourceInstance): EitherError[Future[GestaltResourceInstance]] = {
      for(
        resource <- mkResource(request.body, request.identity, env, ResourceIds.Container);
        modifiedResource = resource.copy(properties=Some(resource.properties.getOrElse(Map()) ++ Map(
          "name" -> resource.name,
          "description" -> resource.description.getOrElse("")
        )));    // why not pass this explicitly from ui? I'd prefer verbosity over inconsistence
        containerProperties <- ResourceSerde.deserialize[ContainerSpec](modifiedResource);
        provider <- Either.fromOption(ResourceFactory.findById(containerProperties.provider.id),
         Error.NotFound(s"CaasProvider with ID '${containerProperties.provider.id}' not found"));
        _ <- assertCompatibleEnvType(provider, env)
      ) yield {
        val context = ProviderContext(request, containerProperties.provider.id, None)
        containerService.createContainer(context, request.identity, containerProperties, Some(resource.id)) map { res =>
          trackingProvider.reportCreate(
            resource.id.toString,
            containerProperties.cpus.toFloat,
            containerProperties.memory.toFloat,
            containerProperties.num_instances,
            containerProperties.image,
            containerProperties.name
          )
          res
        }
      }
    }

    val action = request.getQueryString("action").getOrElse("create")
    (for(
      org <- Either.fromOption(Resource.findFqon(fqon), "could not locate org resource after authentication").liftTo[EitherError];
      env <- Either.fromOption(ResourceFactory.findById(ResourceIds.Environment, environment),
       Error.NotFound(notFoundMessage(ResourceIds.Environment, environment)));
      futureRes <- action match {
        case "create" => createContainer(org, env)
        case "import" => for(
          importPayload <- eitherFromJsResult(request.body.validate[ImportPayload]);
          provider <- Either.fromOption(ResourceFactory.findById(importPayload.properties.provider.id),
           Error.NotFound(s"CaasProvider with ID '${importPayload.properties.provider.id}' not found"));
          _ <- assertCompatibleEnvType(provider, env)
        ) yield {
          genericResourceMethods.createProviderBackedResource(
            org = org,
            identity = request.identity,
            body = request.body,
            parent = env,
            resourceType = ResourceIds.Container,
            providerType = provider.typeId,
            actionVerb = "import"
          )
        }
        case _ => Left(Error.BadRequest("invalid 'action' on container create: must be 'create' or 'import'"))
      }
    ) yield {
      futureRes map { res =>
        Created(RenderSingle(res))
      }
    }) valueOr { error =>
      Future.successful(errorToResult(error))
    }
  }

  def postSecret(fqon: String, environment: java.util.UUID) = AsyncAudited(fqon) { implicit request =>
    (for(
      env <- Either.fromOption(ResourceFactory.findById(ResourceIds.Environment, environment),
       Error.NotFound(notFoundMessage(ResourceIds.Environment, environment)));
      resource <- mkResource(request.body, request.identity, env, ResourceIds.Secret);
      modifiedResource = resource.copy(properties=Some(resource.properties.getOrElse(Map()) ++ Map(
        "name" -> resource.name,
        "description" -> resource.description.getOrElse("")
      )));    // why not pass this explicitly from ui? I'd prefer verbosity over inconsistence
      secretProperties <- ResourceSerde.deserialize[SecretSpec](modifiedResource);
      provider <- Either.fromOption(ResourceFactory.findById(secretProperties.provider.id),
       Error.NotFound(s"CaasProvider with ID '${secretProperties.provider.id}' not found"));
      _ <- assertCompatibleEnvType(provider, env)
    ) yield {
      val context = ProviderContext(request, secretProperties.provider.id, None)
      containerService.createSecret(context, request.identity, secretProperties, Some(resource.id)) map { created =>
        Accepted(RenderSingle(resourceController.transformResource(created).get))
      }
    }) valueOr { error =>
      Future.successful(errorToResult(error))
    }
  }

  def postVolume(fqon: String, environment: java.util.UUID) = AsyncAudited(fqon) { implicit request =>
    (for(
      env <- Either.fromOption(ResourceFactory.findById(ResourceIds.Environment, environment),
       Error.NotFound(notFoundMessage(ResourceIds.Environment, environment)));
      resource <- mkResource(request.body, request.identity, env, migrations.V13.VOLUME_TYPE_ID);
      modifiedResource = resource.copy(properties=Some(resource.properties.getOrElse(Map()) ++ Map(
        "name" -> resource.name,
        "description" -> resource.description.getOrElse("")
      )));    // why not pass this explicitly from ui? I'd prefer verbosity over inconsistence
      volumeProperties <- ResourceSerde.deserialize[VolumeSpec](modifiedResource);
      volumeProvider <- Either.fromOption(volumeProperties.provider, Error.BadRequest("provider not specified"));
      provider <- Either.fromOption(ResourceFactory.findById(volumeProvider.id),
       Error.NotFound(s"CaasProvider with ID '${volumeProvider.id}' not found"));
      _ <- assertCompatibleEnvType(provider, env)
    ) yield {
      val context = ProviderContext(request, volumeProvider.id, None)
      containerService.createVolume(context, request.identity, volumeProperties, Some(resource.id)) map { created =>
        Accepted(RenderSingle(resourceController.transformResource(created).get))
      }
    }) valueOr { error =>
      Future.successful(errorToResult(error))
    }
  }

  def updateContainer(fqon: String, cid: UUID) = AsyncAudited(fqon) { implicit request =>
    (for(
      prevContainer <- Either.fromOption(ResourceFactory.findById(ResourceIds.Container, cid),
       Error.NotFound(notFoundMessage(ResourceIds.Container, cid)));
      environment <- Either.fromOption(ResourceFactory.findParent(ResourceIds.Environment, prevContainer.id),
       Error.NotFound(s"could not find Environment parent for container ${prevContainer.id}"));
      prevContainerProperties <- ResourceSerde.deserialize[ContainerSpec](prevContainer);
      provider <- eitherFromTry(Try(ContainerService.caasProvider(prevContainerProperties.provider.id)));
      resource <- mkResource(request.body, request.identity, environment, ResourceIds.Container);
      resourceWithId = resource.copy(id=cid);
      resourceWithProvider = resourceWithId.copy(properties=Some(resourceWithId.properties.get
       ++ Map("provider" -> Json.toJson(prevContainerProperties.provider).toString)));
      containerProperties <- ResourceSerde.deserialize[ContainerSpec](resourceWithProvider);
      cleanContainerProperties = containerProperties.copy(external_id=prevContainerProperties.external_id);
      _ <- if(prevContainer.name == resourceWithProvider.name) { Right(()) }else {
       Left(Error.BadRequest("renaming containers is not supported")) };
      _ <- if(prevContainerProperties.external_id.isDefined) { Right(()) }else {
       Left(Error.BadRequest(s"Container with ID '$cid' missing 'external_id'")) };
      updatedResource <- ResourceSerde.serialize[ContainerSpec](resourceWithProvider, cleanContainerProperties)
    ) yield {
      val context = ProviderContext(
        request.copy(uri = s"/${fqon}/environments/${environment.id}/containers/${prevContainer.id}"),
        prevContainerProperties.provider.id,
        Some(prevContainer)
      )
      containerService.updateContainer(context, updatedResource, request.identity, request) map { updated =>
        trackingProvider.reportUpdate(
          updated.id.toString,
          cleanContainerProperties.cpus.toFloat,
          cleanContainerProperties.memory.toFloat,
          cleanContainerProperties.num_instances
        )
        Ok(RenderSingle(resourceController.transformResource(updated).get))
      }
    }) valueOr { error =>
      Future.successful(errorToResult(error))
    }
  }

  def scaleContainer(fqon: String, id: UUID, numInstances: Int) = AsyncAuditedAny(fqon) { implicit request =>
    for(
      container <- Either.fromOption(ResourceFactory.findById(ResourceIds.Container, id),
       Error.NotFound(notFoundMessage(ResourceIds.Container, id))).liftTo[Future];
      environment <- Either.fromOption(ResourceFactory.findParent(ResourceIds.Environment, container.id),
       s"could not find Environment parent for container ${container.id}").liftTo[Future];
      operations = ContainerService.caasObjectRequestOperations("container.scale");
      options = ContainerService.caasObjectRequestOptions(
        user = request.identity,
        environment = environment.id,
        caasObject = container,
        data = Some(Map("scaleTo" -> numInstances.toString))
      );
      _ <- ComposableSafeRequest2.Protect(operations, options);
      _ = log.debug(s"scaling container ${container.id} to $numInstances instances...");
      context <- (for(
        providerJson <- eitherFromTry(Try(Json.parse(container.properties.get("provider"))));
        providerId <- eitherFromJsResult((providerJson \ "id").validate[UUID])
      ) yield ProviderContext(
        request.copy(uri = s"/${fqon}/environments/${environment.id}/containers/${container.id}"),
        providerId,
        Some(container)
      )).liftTo[Future];
      service <- eitherFromTry(providerManager.getProviderImpl(context.provider.typeId)).liftTo[Future];
      updated <- service.scale(context, container, numInstances);
      updatedResource <- eitherFromTry(ResourceFactory.update(updated, request.identity.account.id)).liftTo[Future];
      transformedResource <- eitherFromTry(resourceController.transformResource(updatedResource)).liftTo[Future];
      containerProperties <- ResourceSerde.deserialize[ContainerSpec](transformedResource).liftTo[Future]
    ) yield {
      trackingProvider.reportUpdate(
        updatedResource.id.toString,
        containerProperties.cpus.toFloat,
        containerProperties.memory.toFloat,
        containerProperties.num_instances
      )
      Accepted(RenderSingle(transformedResource))
    }
  }

  def promoteContainer(fqon: String, id: UUID) = AsyncAuditedAny(fqon) { implicit request =>
    for(
      container <- Either.fromOption(ResourceFactory.findById(ResourceIds.Container, id),
       Error.NotFound(notFoundMessage(ResourceIds.Container, id))).liftTo[Future];
      environment <- Either.fromOption(ResourceFactory.findParent(ResourceIds.Environment, container.id),
       s"could not find Environment parent for container ${container.id}").liftTo[Future];
      targetEnvId = ContainerService.targetEnvQueryParam(request.queryString);
      _ <- Either.fromOption(EventMethods.findEffectiveEventRules(targetEnvId, Some("container.promote")),
       Error.Conflict("No promotion policy found for target environment.")).liftTo[Future];
      (operations, options) = ContainerService.setupPromoteRequest(fqon, environment.id, container, request.identity,
       META_URL, targetEnvId);
      _ <- ComposableSafeRequest2.Protect(operations, options);
      updated <- eitherFromTry(ResourceFactory.update(
        container.copy(properties=Some((container.properties getOrElse Map()) ++ Map("status" -> "MIGRATING"))),
        request.identity.account.id
      )).liftTo[Future]
    ) yield Accepted(RenderSingle(updated))
  }

  def migrateContainer(fqon: String, id: UUID) = AsyncAuditedAny(fqon) { implicit request =>
    for(
      container <- Either.fromOption(ResourceFactory.findById(ResourceIds.Container, id),
       Error.NotFound(notFoundMessage(ResourceIds.Container, id))).liftTo[Future];
      environment <- Either.fromOption(ResourceFactory.findParent(ResourceIds.Environment, container.id),
       s"could not find Environment parent for container ${container.id}").liftTo[Future];
      _ <- Either.fromOption(EventMethods.findEffectiveEventRules(environment.id, Some("container.migrate")),
       Error.Conflict("No promotion policy found for target environment.")).liftTo[Future];
      _ <- (if(container.properties.flatMap(_.get("status")) == Some("MIGRATING")) {
        Left(Error.Conflict(s"Container '$id' is already migrating. No changes made."))
      }else {
        Right(())
      }).liftTo[Future];
      (operations, options) = ContainerService.setupMigrateRequest(fqon, environment.id, container, request.identity,
       META_URL, request.queryString);
      _ <- ComposableSafeRequest2.Protect(operations, options);
      updated <- eitherFromTry(ResourceFactory.update(
        container.copy(properties=Some((container.properties getOrElse Map()) ++ Map("status" -> "MIGRATING"))),
        request.identity.account.id
      )).liftTo[Future]
    ) yield Accepted(RenderSingle(updated))
  }
}