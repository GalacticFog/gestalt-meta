package controllers

import java.util.UUID

import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.security.api.errors.UnauthorizedAPIException
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import controllers.util._
import play.api.Play.current
import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.sdk.ResourceLabel
import scala.concurrent.{ Future, Await }
import scala.concurrent.duration._
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.marathon._

import com.galacticfog.gestalt.meta.auth.Actions

class ContainerController(containerService: ContainerService) extends Authorization {

  def futureToFutureTry[T](f: Future[T]): Future[Try[T]] = f.map(Success(_)).recover({case x => Failure(x)})

  /*
   * TODO: Reverse the order of container creation.  Create FIRST in Meta - that way if it fails to create
   * in Marathon, we can just delete it from Meta.
   */
  def createContainer(fqon: String, environment: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>

    containerService.findWorkspaceEnvironment(environment) match {
      case Failure(e) => Future { HandleExceptions(e) }
      case Success((wrk,env)) => {

        // This initializes all required properties if missing, implicit in the route
        val inputJson = normalizeInputContainer(request.body)

        val org = fqid(fqon)
        val user = request.identity

        /*
         * Take the input JSON payload and convert it to a GestaltResourceInstance. This is
         * needed to check any attribute/property values against policy rules (if any).
         */
        val target = inputToResource(org, user, inputJson)

        val fCreated = for {
          containerSpec <- Future.fromTry {
            ContainerSpec.fromResourceInstance(target)
          }
          container <- containerService.launchContainer(
            fqon = fqon,
            workspace = wrk,
            environment = env,
            containerSpec = containerSpec,
            user = request.identity
          )
        } yield Created(Output.renderInstance(container.resource.get))

        fCreated recover {case err: Throwable => HandleExceptions(err)}
      }
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
          
            val operations = containerService.containerRequestOperations(Actions.Container.Scale)
            val options    = containerService.containerRequestOptions(request.identity, environment, c,
                              data = Option(Map("scaleTo" -> numInstances.toString)))
            
            val props     = c.properties.get ++ Map("num_instances" -> numInstances.toString)
            val container = c.copy(properties = Option(props))
            
            Future {
              
              SafeRequest (operations, options) Protect { maybeState => 
                log.debug(s"Scaling Marathon App to $numInstances instances...")
                
                // TODO: Check for failure!!!
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
            containerService.upsertProperties(container, "status" -> "MIGRATING"),
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
    val provider = containerService.marathonProvider(containerService.containerProviderId(container))
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
