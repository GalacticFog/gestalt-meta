package controllers


import java.util.UUID

import com.galacticfog.gestalt.data.{EnvironmentType, PropertyValidator, ResourceFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.json._
import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceLabel}
import com.galacticfog.gestalt.meta.api.{ContainerSpec, SecretSpec, VolumeSpec}
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltFrameworkSecurity}
import com.galacticfog.gestalt.util.FutureFromTryST._
import com.galacticfog.tracking.CaasTrackingProvider
import com.google.inject.Inject
import controllers.util._
import javax.inject.Singleton
import play.api.i18n.MessagesApi
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import services._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}


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
  
  def futureToFutureTry[T](f: Future[T]): Future[Try[T]] = f.map(Success(_)).recover({case x => Failure(x)})

  /**
   * Parse the provider ID from container.properties
   */
  def parseProvider(c: GestaltResourceInstance): UUID = {
    UUID.fromString((Json.parse(c.properties.get("provider")) \ "id").as[String])
  }
  
  import ContainerController._
  import com.galacticfog.gestalt.meta.api.errors._
  import com.galacticfog.gestalt.meta.api.sdk.GestaltResourceInput

  type MetaCreate = (AuthAccountWithCreds, GestaltResourceInstance, UUID) => Try[GestaltResourceInstance]
  type BackendCreate = (ProviderContext, GestaltResourceInstance) => Future[GestaltResourceInstance]
  
  
  /*
   * TODO: ??? Create a custom result type for CaasService to return:
   *  - SuccessWithUpdates
   *  - SuccessNoChange
   *  - Failed
   */

  /**
   * Test that the given Provider is compatible with the given Environment. An
   * exception is thrown if the given Environment is incompatible.
   */
  private[controllers] def assertCompatibleEnvType(provider: GestaltResourceInstance, env: GestaltResourceInstance): Unit = {
    
    // These are the values tha Meta will accept for 'environment_type'
    val acceptableTypes = Set("development", "test", "production")
    
    provider.properties.get.get("environment_types") foreach { types =>
      
      // Environment types the provider is compatible with.
      val allowedByProvider = Json.parse(types).as[Seq[String]]
      
      // Environment type of the given environment.
      val envType = {
        val typeId = env.properties.get.get("environment_type") getOrElse {
          val msg = s"Environment with ID '${env.id}' does not contain 'environment_type' property. Data is corrupt"
          throw new RuntimeException(msg)
        }
        val target = EnvironmentType.name(UUID.fromString(typeId))
        if (acceptableTypes.contains(target.trim.toLowerCase)) target
        else {
          val msg = s"Invalid environment_type. Expected: one of (${acceptableTypes.mkString(",")}. found: '$target'"
          throw new UnprocessableEntityException(msg)
        }
      }
      
      // Given MUST be in the allowed list.
      if (allowedByProvider.nonEmpty && !allowedByProvider.contains(envType)) {
        val msg = s"Incompatible Environment type. Expected one of (${allowedByProvider.mkString(",")}). found: '$envType'"
        throw new ConflictException(msg)
      }
    }    
  }
  
  
  private[controllers] def normalizeCaasPayload(payloadJson: JsValue, environment: UUID): Try[JsObject] = Try {

    val payload = payloadJson.as[JsObject]

    val (env, provider) = (for {
      pid <- Try(Js.find(payload, "/properties/provider/id").map(j => UUID.fromString(j.as[String])) getOrElse {
            throw new BadRequestException(s"`/properties/provider/id` not found.")
          })
      env <- Try(ResourceFactory.findById(ResourceIds.Environment, environment) getOrElse {
            throw new BadRequestException(s"Environment with ID '$environment' not found.")
          })
      prv <- Try(ResourceFactory.findById(pid) getOrElse {
            throw new BadRequestException(s"CaasProvider with ID '$pid' not found")
          })
    } yield (env, prv)).get

    /*
     *  This throws an exception if the environment is incompatible with the provider.
     */
    assertCompatibleEnvType(provider, env)

    // Inject `provider` object into container.properties
    val oldProperties = Js.find(payload, "/properties").flatMap(_.asOpt[JsObject]).getOrElse(Json.obj())
    payload ++ Json.obj(
      "properties" -> (oldProperties ++ Json.obj(
        "provider" -> Json.obj(
          "name"          -> provider.name,
          "id"            -> provider.id,
          "resource_type" -> TypeMethods.typeName(provider.typeId)
        )
      ))
    )
  }

  def postContainer(fqon: String, environment: UUID) = AsyncAudited(fqon) { implicit request =>
    val action = request.getQueryString("action").getOrElse("create")
    val created = for {
      payload   <- Future.fromTryST(normalizeCaasPayload(request.body, environment))
      container <- action match {
        //
        // CREATE
        case "create" =>
          for {
            proto     <- Future.fromTryST(jsonToResource(fqid(fqon), request.identity, normalizeInputContainer(payload), None))
            spec      <- Future.fromTryST(ContainerSpec.fromResourceInstance(proto))
            context   = ProviderContext(request, spec.provider.id, None)
            container <- containerService.createContainer(context, request.identity, spec, Some(proto.id))
          } yield {
            trackingProvider.reportCreate(
              container.id.toString,
              spec.cpus.toFloat,
              spec.memory.toFloat,
              spec.num_instances,
              spec.image,
              spec.name
            )
            container
          }
        //
        // IMPORT
        case "import" =>
          log.info("request to import container against GenericResourceMethods")
          for {
            org <- Future.fromTry(Try(orgFqon(fqon).getOrElse(
              throw new InternalErrorException("could not locate org resource after authentication")
            )))
            env <- Future.fromTry(Try(ResourceFactory.findById(ResourceIds.Environment, environment).getOrElse(
              throw new ResourceNotFoundException(s"environment with id '$environment' not found")
            )))
            providerId = (payload \ "properties" \ "provider" \ "id").as[UUID]
            provider <- Future.fromTry(Try(ResourceFactory.findById(providerId).getOrElse(
              throw new ResourceNotFoundException(s"provider with id '$providerId' not found")
            )))
            r <- genericResourceMethods.createProviderBackedResource(
              org = org,
              identity = request.identity,
              body = payload,
              parent = env,
              resourceType = ResourceIds.Container,
              providerType = provider.typeId,
              actionVerb = action
            )
          } yield r
        //
        // default
        case _=>
          Future.failed(new BadRequestException("invalid 'action' on container create: must be 'create' or 'import'"))
      }
    } yield Created(RenderSingle(container))
    created recover { case e => HandleExceptions(e) }
  }

  def postSecret(fqon: String, environment: java.util.UUID) = AsyncAudited(fqon) { implicit request =>
    val created = for {
      payload   <- Future.fromTry(normalizeCaasPayload(request.body, environment))
      proto     <- Future.fromTry(jsonToResource(fqid(fqon), request.identity, normalizeInputSecret(payload), None))
      spec      <- Future.fromTry(SecretSpec.fromResourceInstance(proto))
      context   = ProviderContext(request, spec.provider.id, None)
      secret <- containerService.createSecret(context, request.identity, spec, Some(proto.id))
    } yield Accepted(RenderSingle(resourceController.transformResource(secret).get))
    created recover { case e => HandleExceptions(e) }
  }

  def postVolume(fqon: String, environment: java.util.UUID) = AsyncAudited(fqon) { implicit request =>
    val created = for {
      payload   <- Future.fromTry(normalizeCaasPayload(request.body, environment))
      proto     <- Future.fromTry(jsonToResource(fqid(fqon), request.identity, normalizeInputVolume(payload), None))
      spec      <- Future.fromTry(VolumeSpec.fromResourceInstance(proto))
      pid       <- Future.fromTry(Try{spec.provider.map(_.id).getOrElse(
        throw new BadRequestException("Volume payload did not include '.properties.provider.id'")
      )})
      context   = ProviderContext(request, pid, None)
      volume <- containerService.createVolume(context, request.identity, spec, Some(proto.id))
    } yield Accepted(RenderSingle(resourceController.transformResource(volume).get))
    created recover { case e => HandleExceptions(e) }
  }

  def updateContainer(fqon: String, cid: java.util.UUID) = AsyncAudited(fqon) { implicit request =>
    val prevContainer = ResourceFactory.findById(ResourceIds.Container, cid) getOrElse {
      throw ResourceNotFoundException(s"Container with ID '$cid' not found")
    }
    val environment = ResourceFactory.findParent(
      parentType = ResourceIds.Environment,
      childId = prevContainer.id
    ) getOrElse {throw new RuntimeException(s"could not find Environment parent for container ${prevContainer.id}")}
    val provider = ContainerService.caasProvider(ContainerService.containerProviderId(prevContainer))
    val prevProps = prevContainer.properties.getOrElse {
      throw BadRequestException(s"Container with ID '$cid' missing properties field and is likely corrupt.")
    }
    val updated = for {
      input <- request.body.validate[GestaltResourceInput] match {
        case JsSuccess(input, _) if input.name == prevContainer.name => Future.successful(input)
        case JsSuccess(input, _) if input.name != prevContainer.name => Future.failed(new BadRequestException("renaming containers is not supported"))
        case JsError(errors) => Future.failed(new BadRequestException("could not parse resource input"))
      }
      _ = log.debug("parsed input")
      newProperties = stringmap(input.properties).map(
        _ ++ Map(
          "external_id" -> prevProps.getOrElse("external_id", throw new RuntimeException(s"Container with ID '$cid' missing 'external_id'")),
          "provider" -> prevProps.getOrElse("provider", throw new RuntimeException(s"Container with ID '$cid' missing 'external_id'"))
        )
      )
      validatedProperties <- PropertyValidator.validate(ResourceIds.Container, newProperties) match {
        case (true,_) => Future.successful(newProperties)
        case (false,msg) => Future.failed(new BadRequestException(msg getOrElse "could not validate Container properties"))
      }
      containerWithUpdates <- Future.fromTry(Try{prevContainer.copy(
        description = input.description,
        properties = validatedProperties
      )})
      _ = log.debug("created update")
      context   = ProviderContext(request.copy(
        uri = s"/${fqon}/environments/${environment.id}/containers/${prevContainer.id}"
      ), provider.id, Some(prevContainer))
      _ = log.debug("about to perform update")
      updated <- containerService.updateContainer(context, containerWithUpdates, request.identity, request)
    } yield {
      val maybeContainerSpec = ContainerSpec.fromResourceInstance(updated).toOption
      maybeContainerSpec.map(
        containerSpec => trackingProvider.reportUpdate(
          updated.id.toString,
          containerSpec.cpus.toFloat,
          containerSpec.memory.toFloat,
          containerSpec.num_instances
        )
      )
      Ok(RenderSingle(resourceController.transformResource(updated).get))
    }
    updated recover { case e => HandleExceptions(e) }
  }

  def scaleContainer(fqon: String, id: UUID, numInstances: Int) = AsyncAuditedAny(fqon) { implicit request =>
    ResourceFactory.findById(ResourceIds.Container, id).fold {
      Future.successful(NotFoundResult(s"Container with ID '$id' not found."))
    }{ c =>
      val environment = ResourceFactory.findParent(
        parentType = ResourceIds.Environment,
        childId = c.id
      ) getOrElse {throw new RuntimeException(s"could not find Environment parent for container ${c.id}")}

      val operations = ContainerService.caasObjectRequestOperations("container.scale")
      val options    = ContainerService.caasObjectRequestOptions(
        user = request.identity,
        environment = environment.id,
        caasObject = c,
        data = Option(Map("scaleTo" -> numInstances.toString))
      )

      SafeRequest (operations, options) ProtectAsync { (_:Option[UUID]) =>
        log.debug(s"scaling container ${c.id} to $numInstances instances...")
        val context = ProviderContext(request.copy(
          uri = s"/${fqon}/environments/${environment.id}/containers/${c.id}"
        ), parseProvider(c), Some(c))
        val scaled = for {
          service <- Future.fromTry(providerManager.getProviderImpl(context.provider.typeId))
          updated <- service.scale(context, c, numInstances)
          updatedResource <- Future.fromTry(ResourceFactory.update(updated, request.identity.account.id))
        } yield {
          val maybeContainerSpec = ContainerSpec.fromResourceInstance(updatedResource).toOption
          maybeContainerSpec.map(
            containerSpec => trackingProvider.reportUpdate(
              updatedResource.id.toString,
              containerSpec.cpus.toFloat,
              containerSpec.memory.toFloat,
              containerSpec.num_instances
            )
          )
          Accepted(RenderSingle(resourceController.transformResource(updatedResource).get))
        }
        scaled recover { case e => HandleExceptions(e) }
      }
    }
  }

  def promoteContainer(fqon: String, id: UUID) = Audited(fqon) { implicit request =>
    ResourceFactory.findById(ResourceIds.Container, id).fold {
      NotFoundResult(notFoundMessage(ResourceIds.Container, id))
    } { container =>
      val environment = ResourceFactory.findParent(
        parentType = ResourceIds.Environment,
        childId = container.id
      ) getOrElse {
        throw new RuntimeException(s"could not find Environment parent for container ${container.id}")
      }

      val target_env_id = ContainerService.targetEnvQueryParam(request.queryString).get

      if (findPromotionRule(target_env_id).isEmpty) {
        HandleExceptions(new ConflictException("No promotion policy found for target environment."))
      } else {
        val user = request.identity

        val (operations, options) = ContainerService.setupPromoteRequest(
          fqon, environment.id, container, user, META_URL, target_env_id
        )

        SafeRequest(operations, options) Protect { _ =>
          ResourceFactory.update(
            ContainerService.upsertProperties(container, "status" -> "MIGRATING"),
            user.account.id
          ) match {
            case Failure(e) => HandleExceptions(e)
            case Success(c) => Accepted(RenderSingle(c))
          }
        }
      }
    }
  }

  def migrateContainer(fqon: String, id: UUID) = Audited(fqon) { implicit request =>
    ResourceFactory.findById(ResourceIds.Container, id).fold {
      NotFoundResult(s"Container with ID '$id' not found.")
    } { c =>
      val environment = ResourceFactory.findParent(
        parentType = ResourceIds.Environment,
        childId = c.id
      ) getOrElse {
        throw new RuntimeException(s"could not find Environment parent for container ${c.id}")
      }

      if (findMigrationRule(environment.id).isEmpty) {
        HandleExceptions(new ConflictException("No migration policy found."))
      } else {
        val user = request.identity
        val container = getMigrationContainer(environment.id, id)
        val (operations, options) = ContainerService.setupMigrateRequest(
          fqon, environment.id, container, user, META_URL, request.queryString
        )

        SafeRequest(operations, options) Protect { _ =>
          ResourceFactory.update(
            ContainerService.upsertProperties(container, "status" -> "MIGRATING"),
            user.account.id
          ) match {
            case Failure(e) => HandleExceptions(e)
            case Success(c) => Accepted(RenderSingle(c))
          }
        }
      }
    }
  }

  /**
   * Lookup and validate a Container for migration. Verifies that the container exists
   * and that it is in a state where migration is possible.
   * 
   * @param  id the UUID of the Container to migrate.
   * @return    the requested Container as Try[GestaltResourceInstance]
   */
  protected [controllers] def getMigrationContainer(env: UUID, id: UUID) = {
    ResourceFactory.findChildOfType(ResourceIds.Container, env, id).fold {
      throw new ResourceNotFoundException(
          notFoundMessage(ResourceIds.Container, id))
    }{ container =>
      val props = container.properties.get
      if (props.contains("status") && props("status") == "MIGRATING") {
        throw new ConflictException(s"Container '$id' is already migrating. No changes made.")
      } else container
    }
  }

  /**
   * Ensure Container input JSON is well-formed and valid. Ensures that required properties
   * are given and fills in default values where appropriate.
   */
  private [this] def normalizeInputContainer(inputJson: JsValue): JsObject = {
    val defaults = containerWithDefaults(inputJson)
    val newprops = Json.toJson(defaults).as[JsObject] ++ (inputJson \ "properties").as[JsObject]
    inputJson.as[JsObject] ++
      Json.obj("resource_type" -> ResourceIds.Container.toString) ++
      Json.obj("properties" -> newprops)
  }

  /**
    * Ensure Secret input JSON is well-formed and valid. Ensures that required properties
    * are given and fills in default values where appropriate.
    */
  private [this] def normalizeInputSecret(inputJson: JsValue): JsObject = {
    inputJson.as[JsObject] ++ Json.obj(
      "resource_type" -> ResourceIds.Secret.toString
    )
  }

  /**
    * Ensure Volume input JSON is well-formed and valid. Ensures that required properties
    * are given and fills in default values where appropriate.
    */
  private [this] def normalizeInputVolume(inputJson: JsValue): JsObject = {
    inputJson.as[JsObject] ++ Json.obj(
      "resource_type" -> migrations.V13.VOLUME_TYPE_ID
    )
  }

  private def notFoundMessage(typeId: UUID, id: UUID) = s"${ResourceLabel(typeId)} with ID '$id' not found."

}

object ContainerController {
  def findPromotionRule(start: UUID) = {
    EventMethods.findEffectiveEventRules(start, Option("container.promote"))
  }

  def findMigrationRule(start: UUID) = {
    EventMethods.findEffectiveEventRules(start, Option("container.migrate"))
  }
}

