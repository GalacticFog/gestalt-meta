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

  def getEnvironmentContainers(fqon: String, environment: UUID) = Authenticate(fqon).async { implicit request =>
    if (!getExpandParam(request.queryString)) {
      // don't need to expand them, so we don't need to query marathon for status
      Future{Ok(Output.renderLinks(ResourceFactory.findChildrenOfType(ResourceIds.Container, environment), META_URL))}
    } else {
      // make a best effort to get updated stats from the Marathon provider and to update the resource with them
      containerService.appComponents(environment) match {
        case Failure(e) => Future.successful(HandleExceptions(e))
        case Success((wrk, env)) =>
          val appGroupPrefix = MarathonClient.metaContextToMarathonGroup(fqon, wrk.name, env.name)
          for {
            metaCons <- Future{ResourceFactory.findChildrenOfType(ResourceIds.Container, environment)}
            // map from meta resource container UUID to a Marathon container guid: (providerId, external_id)
            metaCon2guid = (for {
              metaCon <- metaCons
              props <- metaCon.properties
              providerProp <- props.get("provider")
              pobj <- Try{Json.parse(providerProp)}.toOption
              providerId <- (pobj \ "id").asOpt[UUID]
              eid <- props.get("external_id")
              // MarathonClient.listApplicationsInEnvironment strips env app group prefix
              localEid = eid.stripPrefix(appGroupPrefix).stripPrefix("/")
            } yield (metaCon.id -> (providerId,localEid))).toMap
            // all the providers we care about
            relevantProviders = (metaCon2guid.values.map{_._1} toSeq).distinct.flatMap {
              pid => Try{containerService.marathonProvider(pid)}.toOption
            }
            // id is only unique inside a marathon provider, so we have to zip these with the provider ID
            triedContainerListings <- Future.traverse(relevantProviders)({ pid =>
              val marCons = containerService.marathonClient(pid).listApplicationsInEnvironment(fqon, wrk.name, env.name)
              val pidsAndMarCons = marCons map {cs => cs.map {c => pid.id -> c}}
              // wrap this in a try so that failures don't crash the whole list in Future.traverse
              futureToFutureTry(pidsAndMarCons)
            })
            successfulContainerListings = triedContainerListings collect {case Success(x) => x}
            marCons = successfulContainerListings.flatten
            _ = log.trace(s"Found ${marCons.size} total containers over ${relevantProviders.size} Marathon providers")
            mapMarCons = marCons.map(p => (p._1, p._2.id.stripPrefix("/")) -> p._2).toMap
            outputMetaContainers = metaCons map { originalMetaCon =>
              val stats = for {
                guid <- metaCon2guid.get(originalMetaCon.id)
                marCon <- mapMarCons.get(guid)
              } yield marCon
              containerService.updateMetaContainerWithStats(originalMetaCon, stats)
            }
          } yield Ok(Json.toJson(outputMetaContainers map {Output.renderInstance(_, META_URL).as[JsObject]}))
      }
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



  /*
   * TODO: Reverse the order of container creation.  Create FIRST in Meta - that way if it fails to create
   * in Marathon, we can just delete it from Meta.
   */
  def createContainer(fqon: String, environment: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>

    containerService.appComponents(environment) match {
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

    containerService.appComponents(environment) match {
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
