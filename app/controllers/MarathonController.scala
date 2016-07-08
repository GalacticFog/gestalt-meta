package controllers


import java.util.UUID

import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.api.output.Output
import play.api.mvc.AnyContent
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.laser.MarathonClient
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator

import controllers.util._
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.ws.WS
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.laser._
import play.api.{ Logger => log }
import scala.concurrent.{ ExecutionContext, Future, Promise, Await }
import scala.concurrent.duration._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.events._
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.meta.policy._


object MarathonController extends GestaltFrameworkSecuredController[DummyAuthenticator]
  with MetaController with SecurityResources {

  type ProxyAppFunction = (String,String,String) => Future[JsValue]

  /**
    * GET /{fqon}/environments/{eid}/providers/{pid}/v2/deployments
    */
  def getDeployments(fqon: String, parentType: String, environment: UUID, providerId: UUID) = Authenticate(fqon).async { implicit request =>
    val provider = marathonProvider(providerId)
    execAppFunction(fqon, parentType, environment, provider, "v2/deployments") {
      marathonClient(provider).listDeploymentsAffectingEnvironment_marathon_v2
    } map { Ok(_) } recover {
      case e: Throwable => BadRequest(e.getMessage)
    }
  }

  /**
   * GET /{fqon}/environments/{eid}/providers/{pid}/v2/info
   */
  def getInfo(fqon: String, environment: UUID, provider: UUID) = Authenticate(fqon).async { implicit request =>
    marathonClient(marathonProvider(provider)).getInfo.map { Ok( _ ) } recover {
      case e: Throwable => HandleExceptions(e)
    }
  }

  /**
   * GET /{fqon}/environments/{eid}/containers
   * TODO: is this obsolete? or maybe code from here needs to be moved into ResourceController.getEnvironmentContainersFqon2
   */
  def getMarathonAppsAll(fqon: String, parentType: String, environment: UUID) = Authenticate(fqon).async { implicit request =>

    def go(
        ps: Seq[GestaltResourceInstance], wrk: String, env: String,
        acc: Seq[Future[Seq[GestaltResourceInstance]]]): Seq[Future[Seq[GestaltResourceInstance]]] = {

      ps match {
        case Nil => acc
        case h #:: t => {
          val containers = marathonClient(h).listApplicationsInEnvironment(fqon, wrk, env).map { cs =>
            cs.map { ResourceController.toGestaltContainer(fqon, _, Some(h)) }
          }
          go(t, wrk, env, acc :+ containers)
        }
      }
    }

    appComponents(environment) match {
      case Failure(e) => Future( HandleExceptions(e) )
      case Success((wrk, env)) => {

        val providers  = findAllMarathonProvidersInScope(environment)
        val containers = go(providers.values.toSeq, wrk.name, env.name, Seq())

        for {
          a <- (Future sequence containers)
          b = a.flatten
          c = handleExpansion(b, request.queryString, META_URL)
        } yield c

      }
    }
  }


  /**
   * GET /{fqon}/environments/{eid}/providers/{pid}/v2/apps
   */
  def getMarathonApps(fqon: String, parentType: String, environment: UUID, providerId: UUID) = Authenticate(fqon).async { implicit request =>
    // make a best effort to get updated stats from the Marathon provider and to update the resource with them
    MarathonController.appComponents(environment) match {
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
            pid => Try{MarathonController.marathonProvider(pid)}.toOption
          }
          // id is only unique inside a marathon provider, so we have to zip these with the provider ID
          triedContainerListings <- Future.traverse(relevantProviders)({ pid =>
            val marCons = MarathonController.marathonClient(pid).listApplicationsInEnvironment(fqon, wrk.name, env.name)
            val pidsAndMarCons = marCons map {cs => cs.map {c => pid.id -> c}}
            // wrap this in a try so that failures don't crash the whole list in Future.traverse
            ResourceController.futureToFutureTry(pidsAndMarCons)
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
            updateMetaContainerWithStats(originalMetaCon, stats, request.identity.account.id)
          }
          outputMarathonContainers = outputMetaContainers flatMap { metaApp =>
            (meta2Marathon(metaApp) map {Json.toJson(_).as[JsObject]}).toOption
          }
        } yield Ok(Json.obj("apps" -> outputMarathonContainers))
    }
  }

  /**
    * GET /{fqon}/environments/{eid}/providers/{pid}/v2/apps/{appId}
    */
  def getMarathonApp(fqon: String, parentType: String, envId: UUID, providerId: UUID, appId: String) = Authenticate(fqon).async { implicit request =>
    // make a best effort to get updated stats from the Marathon provider and to update the resource with them
    MarathonController.appComponents(envId) match {
      case Failure(e) => Future.successful(HandleExceptions(e))
      case Success((wrk, env)) =>
        val appGroupPrefix = MarathonClient.metaContextToMarathonGroup(fqon, wrk.name, env.name)
        for {
          metaCons <- Future{ResourceFactory.findChildrenOfType(ResourceIds.Container, envId)}
          metaCon = {
            val cons = metaCons.flatMap {testApp =>
              for {
                props <- testApp.properties
                providerProp <- props.get("provider")
                pobj <- Try{Json.parse(providerProp)}.toOption
                pid <- (pobj \ "id").asOpt[UUID]
                if pid == providerId
                eid <- props.get("external_id")
                // MarathonClient.listApplicationsInEnvironment strips env app group prefix
                localEid = eid.stripPrefix(appGroupPrefix).stripPrefix("/")
                if localEid == appId.stripPrefix("/")
              } yield testApp
            }
            if (cons.size > 1) log.warn(s"found multiple container with the same external id a single provider (${providerId}); this represents a bug.")
            cons.headOption
          } getOrElse(throw new ResourceNotFoundException(s"cannot find container with id ${appId} in environment ${envId}"))
          provider = MarathonController.marathonProvider(providerId)
          client = MarathonController.marathonClient(provider)
          marConTry <- ResourceController.futureToFutureTry(client.getApplication_marathon_v2(fqon, wrk.name, env.name, appId))
          stats = marConTry.toOption flatMap MarathonClient.marathon2Container
          outputMetaContainer = updateMetaContainerWithStats(metaCon, stats, request.identity.account.id)
          outputMarathonContainer = meta2Marathon(outputMetaContainer) map {Json.toJson(_).as[JsObject]} getOrElse(throw new RuntimeException("could not cover meta container to marathon container"))
        } yield Ok(Json.obj("app" -> outputMarathonContainer))
    }
  }

  /**
    * DELETE /{fqon}/environments/{eid}/providers/{pid}/v2/apps/{appId}
    */
  def deleteMarathonAppDcos(fqon: String, parentType: String, environment: UUID, providerId: UUID, marathonAppId: String) = Authenticate(fqon).async { implicit request =>
    log.debug("Looking up workspace and environment for container...")
    appComponents(environment) match {
      case Failure(e) => Future{HandleExceptions(e)}
      case Success((wrk, env)) => {

        log.debug(s"\nWorkspace: ${wrk.id}\nEnvironment: ${env.id}")
        ResourceFactory.findChildByName(env.id, ResourceIds.Container, marathonAppId) match {
          case None => Future{NotFound(Json.obj(
            "message" -> s"App '${marathonAppId}' does not exist"
          ))}
          case Some(c) => {

            log.debug(s"Deleting Marathon App...")
            deleteMarathonApp(fqon, wrk.name, env.name, c) recover {
              case e: Throwable =>
                log.warn("received error deleting app in marathon",e)
                Json.obj(
                  "deploymentId" -> "error",
                  "version" -> "error"
                )
            } map { js =>
              log.debug(s"Deleting Meta Container...")
              ResourceFactory.hardDeleteResource(c.id) match {
                case Success(_) => Ok(js)
                case Failure(e) => HandleRepositoryExceptions(e)
              }
            }
          }
        }
      }
    }
  }

  /**
   * POST /{fqon}/environments/{eid}/providers/{pid}/v2/apps
   */
  def postMarathonAppDCOS(fqon: String, parentType: String, environment: UUID, providerId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    
    val inputJson = request.body.as[JsObject]
    val provider = marathonProvider(providerId)
    
    appComponents(environment) match {
      case Failure(e) => throw e
      case Success((wrk,env)) => {
        
        // TODO: Parse result for error...
        log.debug("Transforming JSON to Meta Container format...")
        val metaContainerJson = marathonApp2MetaContainer(inputJson: JsObject, providerId: UUID)
        val appGroupPrefix = MarathonClient.metaContextToMarathonGroup(fqon, wrk.name, env.name)
        for {
          f1 <- marathonClient(provider).launchContainer_marathon_v2(fqon, wrk.name, env.name, inputJson)
          metaContainerWithExtId = JsonUtil.withJsonPropValue(
            obj = metaContainerJson,
            propName = "external_id",
            propValue = JsString(appGroupPrefix.stripSuffix("/") + "/" + (f1 \ "id").as[String].stripPrefix("/"))
          )
          f2 <- createResourceD(fqid(fqon), metaContainerWithExtId, Some(ResourceIds.Container), Some(environment))
        } yield Created(f1)
        
      }
    }
  }
  
  
  protected [controllers] def updateMetaContainerWithStats(metaCon: GestaltResourceInstance, stats: Option[ContainerStats], creatorId: UUID) = {
    val newStats = stats match {
      case Some(stats) => Seq(
        "age" -> stats.age.toString,
        "status" -> stats.status,
        "num_instances" -> stats.numInstances.toString,
        "tasks_running" -> stats.tasksRunning.toString,
        "tasks_healthy" -> stats.tasksHealthy.toString,
        "tasks_unhealthy" -> stats.tasksUnhealthy.toString,
        "tasks_staged" -> stats.tasksStaged.toString
      )
      case None => Seq(
        "status" -> "LOST",
        "num_instances" -> "0",
        "tasks_running" -> "0",
        "tasks_healthy" -> "0",
        "tasks_unhealthy" -> "0",
        "tasks_staged" -> "0"
      )
    }
    val updatedMetaCon = metaCon.copy(
      properties = metaCon.properties map {
        _ ++ newStats
      } orElse {
        Some(newStats toMap)
      })
    Future{ ResourceFactory.update(updatedMetaCon, creatorId) } onComplete {
      case Success(Success(updatedContainer)) =>
        log.trace(s"updated container ${updatedContainer.id} with info from marathon")
      case Success(Failure(e)) =>
        log.warn(s"failure to update container ${updatedMetaCon.id}",e)
      case Failure(e) =>
        log.warn(s"failure to update container ${updatedMetaCon.id}",e)
    }
    updatedMetaCon
  }
  
  /**
   * Ensure Container input JSON is well-formed and valid. Ensures that required properties
   * are given and fills in default values where appropriate.
   */
  protected [controllers] def normalizeInputContainer(inputJson: JsValue): JsObject = {
    val defaults = containerWithDefaults(inputJson)
    val newprops = (Json.toJson(defaults).as[JsObject]) ++ (inputJson \ "properties").as[JsObject]
    (inputJson.as[JsObject] ++ Json.obj("resource_type" -> ResourceIds.Container.toString)) ++ Json.obj("properties" -> newprops)
  }

  def inputToResource(org: UUID, creator: AuthAccountWithCreds, json: JsValue) = {
    controllers.util.MetaController.inputWithDefaults(
          org = org, 
          input = safeGetInputJson(json).get, 
          creator = creator)
  }
  
  def postMarathonApp(fqon: String, environment: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>

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
        
        
        Policy(user, env, Option(target), ContainerEvents.Create) {
        
          /*
           * TODO: Reverse the order of container creation.  Create FIRST in Meta - that way if it fails to create
           * in Marathon, we can just delete it from Meta.
           */
          
          log.debug("NEW : About to create Container in Marathon...")
          // Create app in Marathon
          createMarathonApp(fqon, name, wrk.name, env.name, inputJson, provider) flatMap { r =>
            
            log.debug("Marathon App created:\n" + Json.prettyPrint(r))

            // Inject external_id property and full provider info
            val marGroupId = groupId(fqon, wrk.name, env.name)
            val resourceJson = Seq(
              "external_id" -> externalIdJson(marGroupId, name),
              "provider"    -> providerJson(provider) 
            ).foldLeft(inputJson) { (json, prop) => JsonUtil.withJsonPropValue(json, prop) }
            
            // Create app in Meta
            log.debug("Creating Container in Meta:\n" + Json.prettyPrint(resourceJson))
            
            createResourceInstance(org, resourceJson, Some(ResourceIds.Container), Some(environment)) match {
              case Failure(err) => Future { HandleExceptions(err) }
              case Success(container) => {

                processContainerEvent(env, container, provider.id, ContainerEvents.PostCreate) match {
                  case Failure(e) => log.error("Failed Publishing Event: " + e.getMessage)
                  case Success(_) => log.debug(s"Success: Published ${ContainerEvents.PostCreate}")
                }
                
                Future { Created(Output.renderInstance(container)) }
                
              }
            }
              
          } recover {
            case e: Throwable => HandleExceptions(e)
          }

        }
      }
    }
  }
  
  
  def Policy(
      user: AuthAccountWithCreds, 
      policyOwner: GestaltResourceInstance, 
      target: Option[GestaltResourceInstance], 
      action: String)(block: => Future[play.api.mvc.Result]): Future[play.api.mvc.Result]  = {
    
    Try {
      effectiveRules(policyOwner.id, Some(ResourceIds.RuleLimit), Seq(action)) foreach { r =>
        val predicate = toPredicate(r.properties.get("eval_logic"))
        val decision = decide(user, (target getOrElse policyOwner), predicate, None)
        
        def predicateMessage(p: Predicate[Any]) = {
          val value = p.value.toString.replaceAll("\"", "")
          "[%s %s %s]".format(p.property, p.operator, value)
        }
        
        if (decision.isLeft) {
          val message = s"Failed Policy Assertion: ${predicateMessage(predicate)}. Rule: ${r.id}"
          throw new ConflictException(message)
        }
      }
    } match {
      case Failure(e) => e match {
        case c: ConflictException =>
          Future(HandleExceptions(new ConflictException(e.getMessage)))
        case _ => Future(HandleExceptions(e))
      }
      case Success(_) => block
    }
  }

  
  def processContainerEvent(
      parent: GestaltResourceInstance, 
      container: GestaltResourceInstance,
      provider: UUID,
      eventName: String)(implicit request: SecuredRequest[_]): Try[Unit] = Try {
    
    log.debug(s"Searching for effective '${eventName}' rules...")
    
    effectiveEventRules(parent.id, Some(eventName)) match {
      case None => Success(None)  
      case Some(rule) => {
        log.debug(s"Sending '$eventName'...")
        for {
          event  <- PolicyEvent.make(eventName, parent, container, rule, provider, META_URL.get)
          status <- publishEvent(event)  
        } yield status
      }
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

  def createMarathonApp(
      fqon: String,
      appName: String,
      workspaceName: String,
      environmentName: String,
      inputJson: JsObject,
      provider: GestaltResourceInstance): Future[JsValue] = {

    // Create app in Marathon
    val app = toMarathonApp(appName, inputJson, provider)
    val marathonPayload = Json.toJson(app).as[JsObject]

    // TODO: Parse result JsValue for error response.
    log.debug("Creating App in Marathon:\n" + Json.prettyPrint(marathonPayload))
    marathonClient(provider).launchContainer_marathon_v2(fqon, workspaceName, environmentName, marathonPayload)
  }

  private def execAppFunction(
      fqon: String,
      parentType: String,
      environment: UUID,
      provider: GestaltResourceInstance, proxyUri: String)(fn: ProxyAppFunction) = {

    appComponents(environment) match {
      case Failure(e) => throw e
      case Success((parent, child)) => proxyUri match {
        case "v2/apps" =>        { fn(fqon, parent.name, child.name) }
        case "v2/deployments" => { fn(fqon, parent.name, child.name) }
        case e         => throw new BadRequestException(s"Unsupported Marathon URL : " + e)
      }
    }
  }


  
  def scaleContainer(fqon: String, environment: UUID, id: UUID, numInstances: Int) = Authenticate(fqon).async { implicit request =>
    log.debug("Looking up workspace and environment for container...")

    val org = fqid(fqon)

    appComponents(environment) match {
      case Failure(e) => Future.successful(HandleExceptions(e))
      case Success((wrk, env)) => {

        log.debug(s"\nWorkspace: ${wrk.id}\nEnvironment: ${env.id}")
        ResourceFactory.findById(ResourceIds.Container, id) match {
          case None => Future.successful(NotFoundResult(s"Container with ID '$id' not found."))
          case Some(c) => {
            val props = c.properties.get ++ Map("num_instances" -> numInstances.toString)
            val container = c.copy(properties = Option(props))
            //val containerJson = Option(Json.toJson(container))
            
            
            
            Policy(request.identity, env, Option(container), ContainerEvents.Scale) {
              
              log.debug(s"Scaling Marathon App...")
              
              scaleMarathonApp(c, numInstances) map {
                marathonJson =>
                  Accepted(Output.renderInstance(c.copy(
                    properties = c.properties map {
                      _ ++ Seq(
                        "num_instances" -> numInstances.toString,
                        "status" -> "SCALING")
                    } orElse Some(Map(
                      "num_instances" -> numInstances.toString,
                      "status" -> "SCALING"))), META_URL))
              } recover {
                case e: Throwable => HandleExceptions(e)
              }
            }
        
          }
        }
      }
    }

  }
  def migrateContainer(fqon: String, envId: UUID, id: UUID) = Authenticate(fqon) { implicit request =>

    val updated = for {
      rule         <- Try(effectiveEventRules(envId, Some("container.migrate")) getOrElse {
                        throw new ConflictException("There are no migration rules in scope. No changes made.")})

      provider     <- providerQueryParam(request.queryString)
      
      environment  <- Try(ResourceFactory.findById(ResourceIds.Environment, envId) getOrElse {
                        throw new ResourceNotFoundException(notFoundMessage(ResourceIds.Environment, envId))})
      
      container    <- getMigrationContainer(id)
      
      message      <- PolicyEvent.make("container.migrate", environment, container, rule, provider, META_URL.get)
      
      publish      <- publishEvent(message)
      
      newcontainer <- ResourceFactory.update(upsertProperties(container, "status" -> "MIGRATING"), request.identity.account.id)
      
    } yield newcontainer

    updated match {
      case Failure(e) => HandleExceptions(e)
      case Success(c) => Accepted(Output.renderInstance(c, META_URL))
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
            deleteMarathonApp(fqon, wrk.name, env.name, c) map { js =>
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
  protected [controllers] def getMigrationContainer(id: UUID) = Try {
    ResourceFactory.findById(ResourceIds.Container, id) match {
      case None => throw new ResourceNotFoundException(notFoundMessage(ResourceIds.Container, id))
      case Some(container) => {  
        val props = container.properties.get
        if (props.contains("status") && props("status") == "MIGRATING") {
          throw new ConflictException(s"Container '$id' is already migrating. No changes made.")
        } else container
      }
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
    val provider = marathonProvider(containerProviderId(container))
    container.properties flatMap {_.get("external_id")} match {
      case Some(externalId) =>
        marathonClient(provider).scaleApplication(externalId, numInstances)
      case None =>
        throw new RuntimeException("container did not have external_id")
    }
  }

  protected [controllers] def deleteMarathonApp(fqon: String, workspaceName: String, environmentName: String, container: GestaltResourceInstance): Future[JsValue] = {
    val provider = marathonProvider(containerProviderId(container))
    marathonClient(provider).deleteApplication( fqon, workspaceName, environmentName, container.name)
  }

  
  protected [controllers] def appComponents(environment: UUID /*, provider: UUID*/) = Try {
    val we = ResourceController.findWorkspaceEnvironment(environment).get
    (we._1, we._2)
  }

  protected [controllers] def marathonClient(provider: GestaltResourceInstance): MarathonClient = {
    val providerUrl = (Json.parse(provider.properties.get("config")) \ "url").as[String]
    log.debug("Marathon URL: " + providerUrl)
    MarathonClient(WS.client, providerUrl)
  }
  
  protected [controllers] def marathonProvider(provider: UUID): GestaltResourceInstance = {
    ResourceFactory.findById(ResourceIds.MarathonProvider, provider) getOrElse {
      throw new ResourceNotFoundException(s"MarathonProvider with ID '$provider' not found.")
    }
  }  
  
  protected [controllers] def eventsClient() = {  
    AmqpClient(AmqpConnection(RABBIT_HOST, RABBIT_PORT, heartbeat = 300))
  }
  
  
  /**
   * Extract and validate the 'provider' querystring parameter. 
   * Used by the {@link #migrateContainer(String,UUID,UUID) migrateContainer} method.
   * 
   * @param qs the complete, unmodified queryString from the origina request.
   */
  protected [controllers] def providerQueryParam(qs: Map[String,Seq[String]]) = Try {
    val PROVIDER_KEY = "provider"
    if (!qs.contains(PROVIDER_KEY) || qs(PROVIDER_KEY)(0).trim.isEmpty)
      throw new BadRequestException("You must supply a provider-id in the query-string, i.e., .../migrate?provider={UUID}")
    else {
      try {
       val pid = UUID.fromString(qs(PROVIDER_KEY)(0))
       ResourceFactory.findById(ResourceIds.MarathonProvider, pid) match {
         case None => throw new ResourceNotFoundException(
             s"Invalid querystring. Provider with ID '$pid' not found.")
         case Some(_) => pid
       }
      } catch {
        case i: IllegalArgumentException => 
          throw new BadRequestException(s"Invalid provider UUID. found: '${qs(PROVIDER_KEY)(0)}'")
        case e: Throwable => throw e
      }
    }
  }    
  
  protected [controllers] def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }
  
  protected [controllers] def publishEvent(event: PolicyEvent) = {
    eventsClient.publish(AmqpEndpoint(RABBIT_EXCHANGE, RABBIT_ROUTE), event.toJson)
  }
  
  protected [controllers] def containerProviderId(c: GestaltResourceInstance): UUID = {
    val pid = (Json.parse(c.properties.get("provider")) \ "id").as[String]
    log.debug("Provider-ID : " + pid)
    UUID.fromString(pid)
  }  

  private def notFoundMessage(typeId: UUID, id: UUID) = s"${ResourceLabel(typeId)} with ID '$id' not found."

  private object ContainerEvents {
    val Migrate = "container.migrate"
    val PreCreate = "container.precreate"
    val Create = "container.create"
    val PostCreate = "container.postcreate"
    val Scale = "container.scale"
  }  
}