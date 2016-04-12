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
  def getMarathonApps(fqon: String, parentType: String, environment: UUID, providerId: UUID, proxyUri: String) = Authenticate(fqon).async { implicit request =>

    val provider = marathonProvider(providerId)

    execAppFunction(fqon, parentType, environment, provider, proxyUri) {
      marathonClient(provider).listApplicationsInEnvironment_marathon_v2
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
    val provider = marathonProvider(providerId)

    appComponents(environment) match {
      case Failure(e) => throw e
      case Success((wrk,env)) => {

        // TODO: Parse result for error...
        log.debug("Transforming JSON to Meta Container format...")
        val metaContainerJson = marathonApp2MetaContainer(inputJson: JsObject, providerId: UUID)

        for {
          f1 <- marathonClient(provider).launchContainer_marathon_v2(fqon, wrk.name, env.name, inputJson)
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

        val provider = marathonProvider(providerId)

        // Create app in Marathon
        createMarathonApp(fqon, name, wrk.name, env.name, inputJson, provider) flatMap {
          r =>
            log.debug("Marathon App created:\n" + Json.prettyPrint(r))

            // Inject external_id property and full provider info
            val marathonGroupId = groupId(fqon, wrk.name, env.name)
            val resourceJson = Seq(
              "external_id" -> JsString(marathonGroupId.stripSuffix("/") + "/" + name.stripPrefix("/")),
              "provider" -> Json.obj(
                "id" -> provider.id.toString,
                "name" -> provider.name
              )
            ).foldLeft(inputJson) { (json, prop) => JsonUtil.withJsonPropValue(json, prop)}

            // Create app in Meta
            log.debug("Marathon-Group-ID : " + marathonGroupId)
            log.debug("Creating Container in Meta:\n" + Json.prettyPrint(resourceJson))
            createResourceD(fqid(fqon), resourceJson, Some(ResourceIds.Container), Some(environment))
        } recover {
          case e: Throwable => HandleExceptions(e)
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
      provider: GestaltResourceInstance): Future[JsValue] = {

    // Create app in Marathon
    val app = toMarathonApp(appName, inputJson)
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

  def marathonProvider(provider: UUID): GestaltResourceInstance = {
    ResourceFactory.findById(ResourceIds.MarathonProvider, provider) getOrElse {
      throw new ResourceNotFoundException(s"MarathonProvider with ID '$provider' not found.")
    }
  }

  private def appComponents(environment: UUID /*, provider: UUID*/) = Try {
    val we = ResourceController.findWorkspaceEnvironment(environment).get
    (we._1, we._2)
  }

  def marathonClient(provider: GestaltResourceInstance): MarathonClient = {
    val providerUrl = (Json.parse(provider.properties.get("config")) \ "url").as[String]
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

  def migrateContainer(fqon: String, envId: java.util.UUID, id: UUID, providerId: String) = play.mvc.Results.TODO

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

  def containerProviderId(c: GestaltResourceInstance): UUID = {
    val pid = (Json.parse(c.properties.get("provider")) \ "id").as[String]
    log.debug("Provider-ID : " + pid)
    UUID.fromString(pid)
  }

  def scaleMarathonApp(container: GestaltResourceInstance, numInstances: Int): Future[JsValue] = {
    val provider = marathonProvider(containerProviderId(container))
    container.properties flatMap {_.get("external_id")} match {
      case Some(externalId) =>
        marathonClient(provider).scaleApplication(externalId, numInstances)
      case None =>
        throw new RuntimeException("container did not have external_id")
    }
  }

  def deleteMarathonApp(fqon: String, workspaceName: String, environmentName: String, container: GestaltResourceInstance): Future[JsValue] = {
    val provider = marathonProvider(containerProviderId(container))
    marathonClient(provider).deleteApplication( fqon, workspaceName, environmentName, container.name)
  }
  
}



