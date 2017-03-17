package controllers


import java.util.UUID

import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output

import scala.concurrent.ExecutionContext.Implicits.global
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
  
  
  import com.galacticfog.gestalt.json.Js

  def withProviderInfo(json: JsValue): JsObject = {
    val jprops = (json \ "properties")

    val pid = (jprops \ "provider" \ "id").as[String]

    // TODO: need to check that this was a Some, throw/handle appropriate error
    val p = ResourceFactory.findById(UUID.fromString(pid)).get
    val newprops = (jprops.as[JsObject] ++ Json.obj("provider" -> Json.obj("name" -> p.name, "id" -> p.id)))
    
    json.as[JsObject] ++ Json.obj("properties" -> newprops)
  }
  
  
  def postContainer(fqon: String, environment: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    
    log.debug(s"postContainer($fqon, $environment)")
    
    val payload = withProviderInfo(request.body)
    
    val transform = CaasTransform(fqid(fqon), request.identity, payload /*request.body*/)
    val context   = ProviderContext(request, parseProvider(transform.resource), None)
    
    log.info("Creating container in Meta...")

    val env      = context.environment
    val provider = context.provider
    val r1       = transform.resource
    val metaCreate = for {
      resource    <- createMetaContainer(request.identity, r1, env.id)
      serviceImpl <- providerManager.getProviderImpl(provider.typeId)
    } yield (serviceImpl, resource)
    
    val created = metaCreate match {
      case Failure(e) => {
        log.error("Failed creating container in Meta.")
        HandleExceptionsAsync(e)
      }
      case Success((service, metaResource)) => {
        
        log.info("Meta container created: " + metaResource.id)
        log.info("Creating container in backend CaaS...")
        
        for {
          updated   <- service.create(context, metaResource)
          container <- updateContainer(updated, request.identity.account.id)
        } yield Created(RenderSingle(container))
      }
    }
    created recoverWith { case e => HandleExceptionsAsync(e) }
  }

  /*
   * TODO: This is a temporary wrapper to adapt ResourceFactory.update() to return a Future
   * as it will in a forthcoming revision.
   */
  private def updateContainer(container: GestaltResourceInstance, identity: UUID): Future[GestaltResourceInstance] = {
    ResourceFactory.update(container, identity) match {
      case Failure(e) => Future.failed(e)
      case Success(r) => Future(r)
    }
  }

  def scaleContainer(fqon: String, environment: UUID, id: UUID, numInstances: Int) = Authenticate(fqon).async { implicit request =>
    log.debug("Looking up workspace and environment for container...")

    val org = fqid(fqon)

    containerService.findWorkspaceEnvironment(environment) match {
      case Failure(e) => Future.successful(HandleExceptions(e))
      case Success((wrk, env)) => {
        
        ResourceFactory.findById(ResourceIds.Container, id).fold {
          Future.successful(NotFoundResult(s"Container with ID '$id' not found."))
        }{ c =>
          
            val operations = containerService.containerRequestOperations("container.scale")
            val options    = containerService.containerRequestOptions(request.identity, environment, c,
                              data = Option(Map("scaleTo" -> numInstances.toString)))
            
            val props     = c.properties.get ++ Map("num_instances" -> numInstances.toString)
            val container = c.copy(properties = Option(props))
            
            Future {
              
              SafeRequest (operations, options) Protect { maybeState => 
                log.debug(s"Scaling Marathon App to $numInstances instances...")
                
                // TODO: Check for failure!!!
                // TODO: this needs to be converted to code against a generic CaaSService, not just marathon
                val marathonJson = scaleMarathonAppSync(c, numInstances)
                
                ResourceFactory.update(c.copy(
                  properties = transformScaleProps(c.properties, numInstances)),
                  identity = request.identity.account.id) match {
                  case Failure(error) => HandleExceptions(error)
                  case Success(updatedContainer) => Accepted(RenderSingle(updatedContainer)) 
                }
              }
            }
          }
        
        }
      }
  }

  def transformScaleProps(props: Option[Map[String,String]], numInstances: Int): Option[Map[String,String]] = {
    val scaleProps = Seq("num_instances" -> numInstances.toString, "status" -> "SCALING")
    props map { _ ++ scaleProps } orElse Option(Map(scaleProps:_*))
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
      val (operations, options) = containerService.setupMigrateRequest(
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

  protected [controllers] def scaleMarathonAppSync(container: GestaltResourceInstance, numInstances: Int) /*: Future[JsValue]*/ = {
    val provider = containerService.caasProvider(containerService.containerProviderId(container))
    container.properties.flatMap(_.get("external_id")).fold {
      throw new RuntimeException("container.properties.external_id not found.")
    }{ id =>
      Await.result(
          containerService.marathonClient(provider).scaleApplication(id, numInstances),
          5 seconds)
    }
  }

  private def notFoundMessage(typeId: UUID, id: UUID) = s"${ResourceLabel(typeId)} with ID '$id' not found."

}
