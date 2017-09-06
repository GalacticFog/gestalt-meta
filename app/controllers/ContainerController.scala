package controllers


import java.util.UUID

import com.galacticfog.gestalt.data.PropertyValidator
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import controllers.util._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.sdk.ResourceLabel

import scala.concurrent.Future
import scala.concurrent.duration._
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.providers.ProviderManager

import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi

import scala.language.postfixOps
import javax.inject.Singleton

import services._

@Singleton
class ContainerController @Inject()( 
     messagesApi: MessagesApi,
     env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
     containerService: ContainerService,
     providerManager: ProviderManager,
     db: play.api.db.Database)
    extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  def futureToFutureTry[T](f: Future[T]): Future[Try[T]] = f.map(Success(_)).recover({case x => Failure(x)})

  /**
   * Parse the provider ID from container.properties
   */
  def parseProvider(c: GestaltResourceInstance): UUID = {
    UUID.fromString((Json.parse(c.properties.get("provider")) \ "id").as[String])
  }
  
  import com.galacticfog.gestalt.meta.api.errors._
  import com.galacticfog.gestalt.meta.api.sdk.GestaltResourceInput
  import ContainerController._
  
  private[controllers] def specToInstance(
      fqon: String, 
      user: AuthAccountWithCreds, 
      containerSpec: ContainerSpec, 
      containerId: Option[UUID]): Try[GestaltResourceInstance] = Try {
    
    val org = orgFqon(fqon)
      .map(_.id)
      .getOrElse(throw new BadRequestException("launchContainer called with invalid fqon"))
      
    val containerResourceInput: GestaltResourceInput = 
      ContainerSpec.toResourcePrototype(containerSpec).copy( id = containerId )
      
    withInputDefaults(org, containerResourceInput, user, None)
  }  
  
  type MetaCreate = (AuthAccountWithCreds, GestaltResourceInstance, UUID) => Try[GestaltResourceInstance]
  type BackendCreate = (ProviderContext, GestaltResourceInstance) => Future[GestaltResourceInstance]
  
  
  /*
   * TODO: ??? Create a custom result type for CaasService to return:
   *  - SuccessWithUpdates
   *  - SuccessNoChange
   *  - Failed
   */
  
  def withProviderInfo(json: JsValue): Try[JsObject] = {
    val jprops = (json \ "properties")
    for {
      pid <- Try((jprops \ "provider" \ "id").as[String])
      provider <- ResourceFactory.findById(UUID.fromString(pid)) match {
        case None => Failure(new BadRequestException(s"provider does not exist"))
        case Some(p) => Success(p)
      }
      newprops = jprops.as[JsObject] ++ Json.obj(
        "provider" -> Json.obj(
          "name" -> provider.name,
          "id" -> provider.id
        )
      )
    } yield json.as[JsObject] ++ Json.obj("properties" -> newprops)
  }

  def postContainer(fqon: String, environment: UUID) = AsyncAudited(fqon) { implicit request =>
    val created = for {
      payload   <- Future.fromTry(withProviderInfo(request.body))
      proto     = jsonToInput(fqid(fqon), request.identity, normalizeInputContainer(payload))
      spec      <- Future.fromTry(ContainerSpec.fromResourceInstance(proto))
      context   = ProviderContext(request, spec.provider.id, None)
      container <- containerService.createContainer(context, request.identity, spec, Some(proto.id))
    } yield Created(RenderSingle(container))
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
    } yield Ok(RenderSingle(updated))
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

      val operations = ContainerService.containerRequestOperations("container.scale")
      val options    = ContainerService.containerRequestOptions(
        user = request.identity,
        environment = environment.id,
        container = c,
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
        } yield Accepted(RenderSingle(updatedResource))
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
          fqon, environment.id, container, user, META_URL.get, target_env_id
        )

        SafeRequest(operations, options) Protect { _ =>
          ResourceFactory.update(
            ContainerService.upsertProperties(container, "status" -> "MIGRATING"),
            user.account.id
          ) match {
            case Failure(e) => HandleExceptions(e)
            case Success(c) => Accepted(Output.renderInstance(c, META_URL))
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
          fqon, environment.id, container, user, META_URL.get, request.queryString
        )

        SafeRequest(operations, options) Protect { _ =>
          ResourceFactory.update(
            ContainerService.upsertProperties(container, "status" -> "MIGRATING"),
            user.account.id
          ) match {
            case Failure(e) => HandleExceptions(e)
            case Success(c) => Accepted(Output.renderInstance(c, META_URL))
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
    val newprops = (Json.toJson(defaults).as[JsObject]) ++ (inputJson \ "properties").as[JsObject]
    (inputJson.as[JsObject] ++ Json.obj("resource_type" -> ResourceIds.Container.toString)) ++ Json.obj("properties" -> newprops)
  }

  private def notFoundMessage(typeId: UUID, id: UUID) = s"${ResourceLabel(typeId)} with ID '$id' not found."

}

object ContainerController {
  def findPromotionRule(start: UUID) = {
    object Finder extends EventMethods
    Finder.findEffectiveEventRules(start, Option("container.promote"))
  }

  def findMigrationRule(start: UUID) = {
    object Finder extends EventMethods
    Finder.findEffectiveEventRules(start, Option("container.migrate"))
  }
}

