package controllers

import java.util.UUID

import com.galacticfog.gestalt.marathon._
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
  
object MarathonController extends GestaltFrameworkSecuredController[DummyAuthenticator]
  with MetaController with SecurityResources {

  type ProxyAppFunction = (String,String,String) => Future[JsValue]

  /**
    * GET /{fqon}/environments/{eid}/providers/{pid}/v2/deployments
    */
  def getDeployments(fqon: String, parentType: String, environment: UUID, providerId: UUID) = Authenticate(fqon).async { implicit request =>
    val provider = marathon(providerId)

    execAppFunction(fqon, parentType, environment, provider, "v2/deployments") {
      client(provider).listDeploymentsAffectingEnvironment_marathon_v2
    } map { Ok(_) } recover {
      case e: Throwable => BadRequest(e.getMessage)
    }
  }

  /**
   * GET /{fqon}/environments/{eid}/providers/{pid}/v2/info
   */
  def getInfo(fqon: String, environment: UUID, provider: UUID) = Authenticate(fqon).async { implicit request =>
    client(marathon(provider)).getInfo.map { Ok( _ ) } recover {
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
          val containers = client(h).listApplicationsInEnvironment(fqon, wrk, env).map { cs =>
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
  def getMarathonApps(fqon: String, parentType: String, environment: UUID, providerId: UUID, proxyUri: String) = Authenticate(fqon).async { implicit request =>

    val provider = marathon(providerId)

    execAppFunction(fqon, parentType, environment, provider, proxyUri) {
      client(provider).listApplicationsInEnvironment_marathon_v2
    } map { Ok(_) } recover {
      case e: Throwable => BadRequest(e.getMessage)
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
  def postMarathonAppDcos(fqon: String, parentType: String, environment: UUID, providerId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    
    val inputJson = request.body.as[JsObject]
    val provider = marathon(providerId)

    appComponents(environment) match {
      case Failure(e) => throw e
      case Success((wrk,env)) => {
        
        // TODO: Parse result for error...
        log.debug("Transforming JSON to Meta Container format...")
        val metaContainerJson = marathonApp2MetaContainer(inputJson: JsObject, providerId: UUID)
        
        for {
          f1 <- client(provider).launchContainer_marathon_v2(fqon, wrk.name, env.name, inputJson)
          f2 <- createResourceD(fqid(fqon), metaContainerJson, Some(ResourceIds.Container), Some(environment))
        } yield Created(f1)

      }
    }
  }


  def postMarathonApp(fqon: String, environment: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    
    log.debug("RECEIVED :\n" + Json.prettyPrint(request.body))
    
    appComponents(environment) match {
      case Failure(e) => Future { HandleExceptions(e) }
      case Success((wrk,env)) => {
        
        // This initializes all required properties if missing.
        val inputJson = normalizeInputContainer(request.body)
        val name = requiredJsString("name", (inputJson \ "name"))
        
        val providerId = UUID.fromString {
          requiredJsString("id", (inputJson \ "properties" \ "provider" \ "id"))
        }
        
        val provider = marathon(providerId)
        
        // Create app in Marathon
        createMarathonApp(fqon, name, wrk.name, env.name, inputJson, provider) match {
          case Failure(e) => Future { HandleExceptions(e) }
          case Success(r) => {
            
            log.debug("Marathon App created:\n" + Json.prettyPrint(r))
            
            // Inject external_id property
            val marathonGroupId = groupId(fqon, wrk.name, env.name)
            val resourceJson = JsonUtil.withJsonPropValue(inputJson,
              "external_id", JsString(marathonGroupId.stripSuffix("/") + "/" + name.stripPrefix("/")))

            // Create app in Meta
            log.debug("Marathon-Group-ID : " + marathonGroupId)
            log.debug("Creating Container in Meta:\n" + Json.prettyPrint(resourceJson))
            createResourceD(fqid(fqon), resourceJson, Some(ResourceIds.Container), Some(environment))
          }
        }
      }
    }
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
      provider: GestaltResourceInstance): Try[JsValue] = Try {
        
    // Create app in Marathon
    val app = toMarathonApp(appName, inputJson)
    val marathonPayload = Json.toJson(app).as[JsObject]

    log.debug("Creating App in Marathon:\n" + Json.prettyPrint(marathonPayload))
    Await.result(client(provider).launchContainer_marathon_v2(
      fqon, workspaceName, environmentName, marathonPayload), 5 seconds)    
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
  
  private def marathon(provider: UUID) = {
    ResourceFactory.findById(ResourceIds.MarathonProvider, provider) getOrElse {
      throw new ResourceNotFoundException(s"MarathonProvider with ID '$provider' not found.")
    }
  }  
  
  private def appComponents(environment: UUID /*, provider: UUID*/) = Try {
    val we = ResourceController.findWorkspaceEnvironment(environment).get
    (we._1, we._2)  
  }
  
  private def client(provider: GestaltResourceInstance) = {
    val providerUrl = (Json.parse(provider.properties.get("config")) \ "url").as[String]
    log.debug("Marathon URL: " + providerUrl)
    MarathonClient(WS.client, providerUrl)
  }
  
  /**
   * Get a unique list (Set) of marathon providers in the given environment's scope.
   * Uniqueness is determined by provider URL.
   */
  private def findAllMarathonProvidersInScope(environment: UUID): Map[String,GestaltResourceInstance] = {
    
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
  
  def normalizeInputContainer(inputJson: JsValue): JsObject = {
    val defaults = containerWithDefaults(inputJson)
    val newprops = (Json.toJson(defaults).as[JsObject]) ++ (inputJson \ "properties").as[JsObject]  
    inputJson.as[JsObject] ++ Json.obj("properties" -> newprops)
  }

  def scaleContainer(fqon: String, environment: UUID, id: UUID, numInstances: Int) = Authenticate(fqon).async { implicit request =>
    log.debug("Looking up workspace and environment for container...")

    appComponents(environment) match {
      case Failure(e) => Future.successful(HandleExceptions(e))
      case Success((wrk, env)) => {

        log.debug(s"\nWorkspace: ${wrk.id}\nEnvironment: ${env.id}")
        ResourceFactory.findById(ResourceIds.Container, id) match {
          case None => Future.successful(NotFoundResult(s"Container with ID '$id' not found."))
          case Some(c) => {

            log.debug(s"Scaling Marathon App...")
            scaleMarathonApp(c, numInstances) map {
              marathonJson => Accepted(marathonJson) // TODO: scaleMarathonApp returns Marathon JSON; need to return Meta JSON
            } recover {
              case e: Throwable => HandleExceptions(e)
            }
          }
        }
      }
    }
  }
  
  
  def eventsClient() = {  
    AmqpClient(AmqpConnection(RABBIT_HOST, RABBIT_PORT, heartbeat = 300))
  }
  
  def notFoundMessage(typeId: UUID, id: UUID) = s"${ResourceLabel(typeId)} with ID '$id' not found."
  
  def effectiveEventRules(parentId: UUID, event: Option[String] = None): Option[GestaltResourceInstance] = {
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

  
  def getMigrationContainer(id: UUID) = Try {
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
  
  
  def migrateContainer(fqon: String, envId: UUID, id: UUID) = Authenticate(fqon) { implicit request =>

    val updated = for {
      rule         <- Try(effectiveEventRules(envId, Some("container.migrate")) getOrElse {
                        throw new ConflictException("There are no migration rules in scope. No changes made.")})

      provider     <- providerQueryParam(request.queryString)
      
      environment  <- Try(ResourceFactory.findById(ResourceIds.Environment, envId) getOrElse {
                        throw new ResourceNotFoundException(notFoundMessage(ResourceIds.Environment, envId))})
      
      container    <- getMigrationContainer(id)
      
      message      <- MigrateEvent.make(environment, container, rule, provider, META_URL.get)
      
      publish      <- publishMigrate(message)
      
      newcontainer <- ResourceFactory.update(upsertProperties(container, "status" -> "MIGRATING"), request.identity.account.id)
      
    } yield newcontainer

    updated match {
      case Failure(e) => {
        log.debug(e.toString)
        log.debug(e.getStackTraceString)
        HandleExceptions(e)
      }
      case Success(c) => Accepted(Output.renderInstance(c, META_URL))
    }

  }

  def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }
  
  protected [controllers] def publishMigrate(event: MigrateEvent) = {
    eventsClient.publish(AmqpEndpoint(RABBIT_EXCHANGE, RABBIT_ROUTE), event.toJson)
  }
  
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
  
  def hardDeleteContainerFqon(fqon: String, environment: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    log.debug("Looking up workspace and environment for container...")
    
    appComponents(environment) match {
      case Failure(e) => HandleExceptions(e)
      case Success((wrk, env)) => {
        
        log.debug(s"\nWorkspace: ${wrk.id}\nEnvironment: ${env.id}")
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
  
  def providerId(c: GestaltResourceInstance) = {
    val pid = (Json.parse(c.properties.get("provider")) \ "id").as[String]
    log.debug("Provider-ID : " + pid)
    UUID.fromString(pid)
  }

  def scaleMarathonApp(container: GestaltResourceInstance, numInstances: Int): Future[JsValue] = {
    val provider = marathon(providerId(container))
    container.properties flatMap {_.get("external_id")} match {
      case Some(externalId) =>
        client(provider).scaleApplication(externalId, numInstances)
      case None =>
        throw new RuntimeException("container did not have external_id")
    }
  }

  def deleteMarathonApp(fqon: String, workspaceName: String, environmentName: String, container: GestaltResourceInstance): Future[JsValue] = {
    val provider = marathon(providerId(container))
    client(provider).deleteApplication( fqon, workspaceName, environmentName, container.name)
  }
  
//    targets match {
//      case Failure(e) => Future { HandleRepositoryExceptions(e) } 
//      case Success((parent, child, provider)) => {
//        val providerUrl = (provider.properties.get("config") \ "url").as[String]
//        val marathonClient = MarathonClient(WS.client, providerUrl)
//        
//        (request.method, proxyUri) match {
//          case (HttpVerbs.GET, "v2/apps") =>
//            marathonClient.listApplicationsInEnvironment_marathon_v2(fqon, wrkName = parent.name, envName = child.name)
//              .map {Ok(_)}
//              .recover {case e: Throwable => BadRequest(e.getMessage)}
//          case (HttpVerbs.POST,"v2/apps") =>
//            marathonClient.launchContainer_marathon_v2(fqon, wrkName = parent.name, envName = child.name, marPayload = request.request.body.as[JsObject])
//              .map {Created(_)}
//              .recover {case e: Throwable => BadRequest(e.getMessage)}
//          case _ => Future { BadRequest(s"Unsupported provider URL: " + proxyUri) }
//        }        
//      }
//    }
//  }

}



