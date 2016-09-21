package controllers

import java.util.UUID

import com.galacticfog.gestalt.marathon._
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

object ContainerController extends Authorization {

  import com.galacticfog.gestalt.security.api.json.JsonImports._
  import ContainerMethodsImpl.{marathonProvider, marathonClient, appComponents}

  def futureToFutureTry[T](f: Future[T]): Future[Try[T]] = f.map(Success(_)).recover({case x => Failure(x)})

  def getEnvironmentContainers(fqon: String, environment: UUID) = Authenticate(fqon).async { implicit request =>
    if (!getExpandParam(request.queryString)) {
      // don't need to expand them, so we don't need to query marathon for status
      Future{Ok(Output.renderLinks(ResourceFactory.findChildrenOfType(ResourceIds.Container, environment), META_URL))}
    } else {
      // make a best effort to get updated stats from the Marathon provider and to update the resource with them
      appComponents(environment) match {
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
              pid => Try{marathonProvider(pid)}.toOption
            }
            // id is only unique inside a marathon provider, so we have to zip these with the provider ID
            triedContainerListings <- Future.traverse(relevantProviders)({ pid =>
              val marCons = marathonClient(pid).listApplicationsInEnvironment(fqon, wrk.name, env.name)
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
              ContainerMethodsImpl.updateMetaContainerWithStats(originalMetaCon, stats, request.identity.account.id)
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

  def containerRequestOptions(user: AuthAccountWithCreds,
                              environment: UUID,
                              container: GestaltResourceInstance,
                              data: Option[Map[String, String]] = None) = RequestOptions(
    user = user,
    authTarget = Option(environment),
    policyOwner = Option(environment),
    policyTarget = Option(container),
    data = data
  )

  def containerRequestOperations(action: String) = List(
    controllers.util.Authorize(action),
    controllers.util.EventsPre(action),
    controllers.util.PolicyCheck(action),
    controllers.util.EventsPost(action)
  )

  /*
   * TODO: Reverse the order of container creation.  Create FIRST in Meta - that way if it fails to create
   * in Marathon, we can just delete it from Meta.
   */
  def createContainer(fqon: String, environment: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>

    appComponents(environment) match {
      case Failure(e) => Future { HandleExceptions(e) }
      case Success((wrk,env)) => {

        // This initializes all required properties if missing.
        val inputJson = normalizeInputContainer(request.body)
        val name = requiredJsString("name", (inputJson \ "name"))
        
        val providerId = UUID.fromString {
          requiredJsString("id", (inputJson \ "properties" \ "provider" \ "id"))
        }
  
        val org = fqid(fqon)
        val user = request.identity
        val provider = marathonProvider(providerId)
          
        /*
         * Take the input JSON payload and convert it to a GestaltResourceInstance. This is
         * needed to check any attribute/property values against policy rules (if any).
         */
        val target = inputToResource(org, user, inputJson)
        val operations = containerRequestOperations(Actions.Container.Create)
        val options = containerRequestOptions(user, environment, target)
        
        Future {
          
          SafeRequest (operations, options) Protect { maybeState => 
            
            log.debug("Creating Container in Marathon:\n" + Json.prettyPrint(inputJson))
            // TODO: convert this to async, and build the meta container from the Marathon response: inputJson should be used only once, it's not authoritative
            createMarathonAppSync(fqon, name, wrk.name, env.name, inputJson, provider)
            
            // Inject external_id property and full provider info
            val marGroupId = groupId(fqon, wrk.name, env.name)
            val metaContainerJson = toMetaContainerJson(inputJson, name, marGroupId, provider)
            
            log.debug("Creating Container in Meta:\n" + Json.prettyPrint(metaContainerJson))
            
            createResourceInstance(org, metaContainerJson, Some(ResourceIds.Container), Some(environment)) match {
              case Failure(err) => HandleExceptions(err)
              case Success(container) => Created(Output.renderInstance(container)) 
            }
          }
        
        }
      } 
      
    }
  }


  def scaleContainer(fqon: String, environment: UUID, id: UUID, numInstances: Int) = Authenticate(fqon).async { implicit request =>
    log.debug("Looking up workspace and environment for container...")

    val org = fqid(fqon)

    appComponents(environment) match {
      case Failure(e) => Future.successful(HandleExceptions(e))
      case Success((wrk, env)) => {
        
        ResourceFactory.findById(ResourceIds.Container, id).fold {
          Future.successful(NotFoundResult(s"Container with ID '$id' not found."))
        }{ c =>
          
            val operations = containerRequestOperations(Actions.Container.Scale)
            val options    = containerRequestOptions(request.identity, environment, c, 
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

  def toMetaContainerJson(
      inputJson: JsObject, 
      containerName: String,
      marathonGroupId: String,
      provider: GestaltResourceInstance) = {
    Seq(
      "external_id" -> externalIdJson(marathonGroupId, containerName),
      "provider"    -> providerJson(provider) ).foldLeft(inputJson) { (json, prop) => 
        JsonUtil.withJsonPropValue(json, prop) 
    }      
  }
  

  def providerJson(provider: GestaltResourceInstance) = {
    Json.obj("id" -> provider.id.toString, "name" -> provider.name)
  }
  
  def externalIdJson(marathonGroupId: String, containerName: String): JsString = {
    JsString(marathonGroupId.stripSuffix("/") + "/" + containerName.stripPrefix("/"))
  }
  
  def groupId(fqon: String, workspaceName: String, environmentName: String) = {
    MarathonClient.metaContextToMarathonGroup(fqon, workspaceName, environmentName)
  }

  def createMarathonAppSync(fqon: String,
                            appName: String,
                            workspaceName: String,
                            environmentName: String,
                            inputJson: JsObject,
                            provider: GestaltResourceInstance): JsValue = {
    // Create app in Marathon
    val props = (inputJson \ "properties").validate[InputContainerProperties].map {
      case ps: InputContainerProperties => ps
    }.recoverTotal { e =>
      throw new IllegalArgumentException(
        "Could not parse container properties: " + JsError.toFlatJson(e).toString)
    }
    val app = toMarathonApp(appName, props, provider)
    val marathonPayload = Json.toJson(app).as[JsObject]
    // TODO: Parse result JsValue for error response.
    log.debug("Creating App in Marathon:\n" + Json.prettyPrint(marathonPayload))
    Await.result(
      marathonClient(provider).launchContainer_marathon_v2(fqon, workspaceName, environmentName, marathonPayload),
      5 seconds
    )
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
      val (operations, options) = ContainerMethodsImpl.setupMigrateRequest(
          fqon, envId, container, user, META_URL.get, request.queryString)
      
      SafeRequest (operations, options) Protect { maybeState =>    
        ResourceFactory.update(
            ContainerMethodsImpl.upsertProperties(container, "status" -> "MIGRATING"),
            user.account.id) match {
          case Failure(e) => HandleExceptions(e)
          case Success(c) => Accepted(Output.renderInstance(c, META_URL))
        }
      }
    
    }
  }
  
  def hardDeleteContainerFqon(fqon: String, environment: UUID, id: UUID) = Authenticate(fqon) { implicit request =>

    appComponents(environment) match {
      case Failure(e) => HandleExceptions(e)
      case Success((wrk, env)) => {

        ResourceFactory.findById(ResourceIds.Container, id) match {
          case None => NotFoundResult(s"Container with ID '$id' not found.")
          case Some(c) => {

            log.debug(s"Deleting Marathon App...")
            ContainerMethodsImpl.deleteMarathonApp(fqon, wrk.name, env.name, c) map { js =>
              log.info("marathon return from app deletion: " + js.toString)
            }
            log.debug(s"Deleting Meta Container...")
            ResourceFactory.hardDeleteResource(c.id) match {
              case Success(_) => NoContent
              case Failure(e) => HandleRepositoryExceptions(e)
            }
          }
        }
      }
    }
  }
  
  /**
   * Get a unique list (Set) of marathon providers in the given environment's scope.
   * Uniqueness is determined by provider URL.
   */
  protected [controllers] def findAllMarathonProvidersInScope(environment: UUID): Map[String,GestaltResourceInstance] = {
    
    def go(ps: Seq[GestaltResourceInstance], acc: Map[String, GestaltResourceInstance]): Map[String,GestaltResourceInstance] = {
      ps match {
        case Nil => acc
        case h :: t => {
          val url = (Json.parse(h.properties.get("config")) \ "url").as[String]
          val m = if (acc.contains(url)) acc else acc ++ Map(url -> h)
          go(t, m)
        }
      }
    }
    val providers = ResourceFactory.findAncestorProviders(environment) filter { p =>
      p.typeId == ResourceIds.MarathonProvider
    }
    go(providers, Map())
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
   * Get all Event-Rules that are in effect for the given parent Resource.
   * 
   * @param parentId UUID of the resource to get event rules for
   * @param event optional event name to filter for rules applying to a specific event.
   */
  
  protected [controllers] def effectiveEventRules(parentId: UUID, event: Option[String] = None): Option[GestaltResourceInstance] = {
    val rs = for {
      p <- ResourceFactory.findChildrenOfType(ResourceIds.Policy, parentId)
      r <- ResourceFactory.findChildrenOfType(ResourceIds.RuleEvent, p.id)
    } yield r
    
    val fs = if (event.isEmpty) rs else {
      rs filter { _.properties.get("actions").contains(event.get) }
    }
    // TODO: This is temporary. Need a strategy for multiple matching rules.
    if (fs.isEmpty) None else Some(fs(0))
  }
  
  protected [controllers] def scaleMarathonApp(container: GestaltResourceInstance, numInstances: Int): Future[JsValue] = {
    val provider = marathonProvider(ContainerMethodsImpl.containerProviderId(container))
    container.properties flatMap {_.get("external_id")} match {
      case Some(externalId) =>
        marathonClient(provider).scaleApplication(externalId, numInstances)
      case None =>
        throw new RuntimeException("container did not have external_id")
    }
  }

  protected [controllers] def scaleMarathonAppSync(container: GestaltResourceInstance, numInstances: Int) /*: Future[JsValue]*/ = {
    val provider = marathonProvider(ContainerMethodsImpl.containerProviderId(container))
    
    container.properties.flatMap(_.get("external_id")).fold {
      throw new RuntimeException("container.properties.external_id not found.")
    }{ id =>
      Await.result(
          marathonClient(provider).scaleApplication(id, numInstances), 
          5 seconds)
    }
  }

  private def notFoundMessage(typeId: UUID, id: UUID) = s"${ResourceLabel(typeId)} with ID '$id' not found."

}
