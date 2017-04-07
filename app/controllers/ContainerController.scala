package controllers


import java.util.UUID

import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output

//import play.api.libs.concurrent.Execution.Implicits.defaultContext
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

import scala.concurrent.{Await, Future}
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
class ContainerController @Inject()( messagesApi: MessagesApi,
                                     env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
                                     containerService: ContainerService,
                                     providerManager: ProviderManager )
    extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  def futureToFutureTry[T](f: Future[T]): Future[Try[T]] = f.map(Success(_)).recover({case x => Failure(x)})

  trait ResourceTransform extends GestaltProviderService
  case class CaasTransform(org: UUID, caller: AuthAccountWithCreds, json: JsValue) extends ResourceTransform {
    lazy val resource = jsonToInput(org, caller, normalizeInputContainer(json))
    lazy val spec = ContainerSpec.fromResourceInstance(resource)
  }

  trait ServiceProvider[A <: GestaltProviderService] {
    /**
     * Get a GestaltService implementation.
     */
    def get[A](provider: UUID): Try[A]
  }

  /**
   * Parse the provider ID from container.properties
   */
  def parseProvider(c: GestaltResourceInstance): UUID = {
    UUID.fromString((Json.parse(c.properties.get("provider")) \ "id").as[String])
  }
  
  import com.galacticfog.gestalt.meta.api.errors._
  import com.galacticfog.gestalt.meta.api.sdk.GestaltResourceInput
  
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
  
  def createMetaContainer(user: AuthAccountWithCreds, container: GestaltResourceInstance, env: UUID) = {
    ResourceFactory.create(ResourceIds.User, user.account.id)(container, Some(env))
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
        "provider" -> Json.obj("name" -> provider.name, "id" -> provider.id)
      )
    } yield json.as[JsObject] ++ Json.obj("properties" -> newprops)
  }

  def postContainer(fqon: String, environment: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    val created = for {
      payload   <- Future.fromTry(withProviderInfo(request.body))
      transform = CaasTransform(fqid(fqon), request.identity, payload)
      context   = ProviderContext(request, parseProvider(transform.resource), None)
      env       = context.environment
      provider  = context.provider
      r1        = transform.resource
      _ = log.info("Creating container in Meta...")
      metaResource <- Future.fromTry(createMetaContainer(request.identity, r1, env.id))
      service      <- Future.fromTry(providerManager.getProviderImpl(provider.typeId))
      _ = log.info("Meta container created: " + metaResource.id)
      _ = log.info("Creating container in backend CaaS...")
      updated   <- service.create(context, metaResource)
      container <- updateContainer(updated, request.identity.account.id)
    } yield Created(RenderSingle(container))
    created recover { case e => HandleExceptions(e) }
  }

  /*
   * TODO: This is a temporary wrapper to adapt ResourceFactory.update() to return a Future
   * as it will in a forthcoming revision.
   */
  private def updateContainer(container: GestaltResourceInstance, identity: UUID): Future[GestaltResourceInstance] =
    Future.fromTry(ResourceFactory.update(container, identity))

  def scaleContainer(fqon: String, unused: String, id: UUID, numInstances: Int) = Authenticate(fqon).async { implicit request =>
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
          uri = request.uri.replaceFirst("/scale.*$","")
        ), parseProvider(c), Some(c))
        val scaled = for {
          service <- Future.fromTry(providerManager.getProviderImpl(context.provider.typeId))
          updated <- service.scale(context, c, numInstances)
          updatedResource <- updateContainer(updated, request.identity.account.id)
        } yield Accepted(RenderSingle(updatedResource))
        scaled recover { case e => HandleExceptions(e) }
      }
    }
  }

  def findMigrationRule(start: UUID) = {
    object Finder extends EventMethods
    Finder.findEffectiveEventRules(start, Option("container.migrate"))
  }
  
  def migrateContainer(fqon: String, envId: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    if (findMigrationRule(envId).isEmpty) HandleExceptions {
      throw new ConflictException("No migration policy found.")
    } else {
      val user = request.identity
      val container = getMigrationContainer(envId, id)
      val (operations, options) = ContainerService.setupMigrateRequest(
          fqon, envId, container, user, META_URL.get, request.queryString)
      
      SafeRequest (operations, options) Protect { maybeState =>    
        ResourceFactory.update(
            ContainerService.upsertProperties(container, "status" -> "MIGRATING"),
            user.account.id) match {
          case Failure(e) => HandleExceptions(e)
          case Success(c) => Accepted(Output.renderInstance(c, META_URL))
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
