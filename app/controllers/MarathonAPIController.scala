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
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.security.play.silhouette.{OrgContextRequestUUID, OrgContextRequest, GestaltFrameworkSecuredController, AuthAccountWithCreds}
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator

import controllers.util._
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.ws.WS
import com.galacticfog.gestalt.meta.api.sdk.ResourceLabel
import com.galacticfog.gestalt.laser._
import play.api.{Logger => log, Play}
import scala.concurrent.{ ExecutionContext, Future, Promise, Await }
import scala.concurrent.duration._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.events._
import com.galacticfog.gestalt.meta.policy._
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.marathon._

import com.galacticfog.gestalt.keymgr.GestaltFeature
import com.galacticfog.gestalt.meta.auth.Actions


class MarathonAPIController(containerService: ContainerService) extends Authorization {

  import com.galacticfog.gestalt.security.api.json.JsonImports._
  import ContainerController.futureToFutureTry

  class MarAuth(maybeGenFQON: Option[RequestHeader => Option[String]] = None) extends ActionBuilder[SecuredRequest] {
    def invokeBlock[B](request: Request[B], block: SecuredRequest[B] => Future[Result]) = {
      val ocr = OrgContextRequest(maybeGenFQON flatMap {_(request)}, request)
      SecuredRequestHandler(ocr) { securedRequest =>
        Future.successful(HandlerResult(Ok, Some(securedRequest)))
      }.flatMap {
        case HandlerResult(r, Some(sr)) => block(sr)
        case HandlerResult(r, None) => Future{
          lazy val org = ocr.orgFQON getOrElse "root"
          lazy val defRealm = s"${securityConfig.protocol}://${securityConfig.hostname}:${securityConfig.port}/${org}/oauth/issue"
          val realm: String = securityRealmOverride(org) getOrElse defRealm
          val challenge: String = "acsjwt realm=\"" + realm
          Unauthorized(Json.toJson(UnauthorizedAPIException("","Authentication required",""))).withHeaders(WWW_AUTHENTICATE -> challenge)
        }
      }
    }
  }

  object MarAuth {
    def apply(genFQON: => String): MarAuth = new MarAuth(Some({ rh: RequestHeader => Some(genFQON)}))
  }

  type ProxyAppFunction = (String,String,String) => Future[JsValue]

  /**
   * GET /{fqon}/environments/{eid}/providers/{pid}/v2/deployments
   */
  def getDeployments(fqon: String, parentType: String, environment: UUID, providerId: UUID) = MarAuth(fqon).async { implicit request =>
    val provider = containerService.marathonProvider(providerId)
    execAppFunction(fqon, parentType, environment, provider, "v2/deployments") {
      containerService.marathonClient(provider).listDeploymentsAffectingEnvironment_marathon_v2
    } map { Ok(_) } recover {
      case e: Throwable => BadRequest(e.getMessage)
    }
  }

  /**
   * GET /{fqon}/environments/{eid}/providers/{pid}/v2/info
   */
  def getInfo(fqon: String, environment: UUID, provider: UUID) = MarAuth(fqon).async { implicit request =>
    containerService.marathonClient(containerService.marathonProvider(provider)).getInfo.map { Ok( _ ) } recover {
      case e: Throwable => HandleExceptions(e)
    }
  }

  /**
   * GET /{fqon}/environments/{eid}/providers/{pid}/v2/apps
   */
  def getMarathonApps(fqon: String, parentType: String, environment: UUID, providerId: UUID) = MarAuth(fqon).async { implicit request =>
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
            containerService.updateMetaContainerWithStats(originalMetaCon, stats, request.identity.account.id)
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
  def getMarathonApp(fqon: String, parentType: String, envId: UUID, providerId: UUID, appId: String) = MarAuth(fqon).async { implicit request =>
    // make a best effort to get updated stats from the Marathon provider and to update the resource with them
    containerService.appComponents(envId) match {
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
            if (cons.size > 1) {
              log.warn(s"found multiple container with the same external id a single provider (${providerId}); this represents a bug.")
            }
            cons.headOption
          } getOrElse {
            throw new ResourceNotFoundException(s"cannot find container with id ${appId} in environment ${envId}")
          }

          provider = containerService.marathonProvider(providerId)
          client = containerService.marathonClient(provider)
          marConTry <- futureToFutureTry(client.getApplication_marathon_v2(fqon, wrk.name, env.name, appId))
          stats = marConTry.toOption flatMap MarathonClient.marathon2Container
          outputMetaContainer = containerService.updateMetaContainerWithStats(metaCon, stats, request.identity.account.id)
          outputMarathonContainer = meta2Marathon(outputMetaContainer) map {Json.toJson(_).as[JsObject]} getOrElse(throw new RuntimeException("could not cover meta container to marathon container"))
        } yield Ok(Json.obj("app" -> outputMarathonContainer))
    }
  }

  /**
    * DELETE /{fqon}/environments/{eid}/providers/{pid}/v2/apps/{appId}
    */
  def deleteMarathonAppDCOS(fqon: String, parentType: String, environment: UUID, providerId: UUID, marathonAppId: String) = MarAuth(fqon).async { implicit request =>
    log.debug("Looking up workspace and environment for container...")
    containerService.appComponents(environment) match {
      case Failure(e) => Future{HandleExceptions(e)}
      case Success((wrk, env)) => {

        log.debug(s"\nWorkspace: ${wrk.id}\nEnvironment: ${env.id}")
        ResourceFactory.findChildByName(env.id, ResourceIds.Container, marathonAppId) match {
          case None => Future{NotFound(Json.obj(
            "message" -> s"App '${marathonAppId}' does not exist"
          ))}
          case Some(c) => {

            log.debug(s"Deleting Marathon App...")
            containerService.deleteMarathonApp(fqon, wrk.name, env.name, c) recover {
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
  def postMarathonAppDCOS(fqon: String, parentType: String, environment: UUID, providerId: UUID) = MarAuth(fqon).async(parse.json) { implicit request =>

    val inputJson = request.body.as[JsObject]
    val provider = containerService.marathonProvider(providerId)

    containerService.appComponents(environment) match {
      case Failure(e) => throw e
      case Success((wrk,env)) => {

        // TODO: Parse result for error...
        log.debug("Transforming JSON to Meta Container format...")
        val metaContainerJson = marathonApp2MetaContainer(inputJson: JsObject, providerId: UUID)
        val appGroupPrefix = MarathonClient.metaContextToMarathonGroup(fqon, wrk.name, env.name)
        for {
          f1 <- containerService.marathonClient(provider).launchContainer_marathon_v2(fqon, wrk.name, env.name, inputJson)
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



  private def execAppFunction(
      fqon: String,
      parentType: String,
      environment: UUID,
      provider: GestaltResourceInstance, proxyUri: String)(fn: ProxyAppFunction) = {

    containerService.appComponents(environment) match {
      case Failure(e) => throw e
      case Success((parent, child)) => proxyUri match {
        case "v2/apps" =>        { fn(fqon, parent.name, child.name) }
        case "v2/deployments" => { fn(fqon, parent.name, child.name) }
        case e         => throw new BadRequestException(s"Unsupported Marathon URL : " + e)
      }
    }
  }

  def handleMarathonAPIRequest(method: String, fqon: String, parentType: String, environment: UUID, providerId: UUID, path: String) = MarAuth(fqon).async { implicit request =>
    val AppPath = "apps/(.*)".r
    (method,path) match {
      case ("GET","apps") => ???             // controllers.MarathonController.getMarathonApps(fqon: String, parentType = "46b28076-9d94-4860-9aca-e8483a56d32c", envId: java.util.UUID, id: java.util.UUID)
      case ("POST","apps") => ???            // controllers.MarathonController.postMarathonAppDCOS(fqon: String, parentType = "46b28076-9d94-4860-9aca-e8483a56d32c", envId: java.util.UUID, id: java.util.UUID)
      case ("GET", AppPath(appId)) => ???    // controllers.MarathonController.getMarathonApp(fqon: String, parentType = "46b28076-9d94-4860-9aca-e8483a56d32c", envId: java.util.UUID, id: java.util.UUID, appId: String)
      case ("GET", "info") => ???            // controllers.MarathonController.getInfo(fqon: String, envId: java.util.UUID, id: java.util.UUID)
      case ("GET", "deployments") => ???     // controllers.MarathonController.getDeployments(fqon: String, parentType = "46b28076-9d94-4860-9aca-e8483a56d32c", envId: java.util.UUID, id: java.util.UUID)
      case ("DELETE", AppPath(appId)) => ??? // controllers.MarathonController.deleteMarathonAppDcos(fqon: String, parentType = "46b28076-9d94-4860-9aca-e8483a56d32c", envId: java.util.UUID, prvId: java.util.UUID, marathonAppId: String)
    }
  }

}
