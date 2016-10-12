package controllers


import java.util.UUID

import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.security.api.errors.UnauthorizedAPIException
import org.joda.time.{DateTimeZone, DateTime}
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
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
  def listDeployments(fqon: String, parentType: String, environment: UUID, providerId: UUID) = MarAuth(fqon).async { implicit request =>
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
  def listApps(fqon: String, parentType: String, environment: UUID, providerId: UUID) = MarAuth(fqon).async { implicit request =>
    containerService.appComponents(environment) match {
      case Failure(e) => throw e
      case Success((wrk,env)) => {
        // TODO: Needs a lot of error handling
        for {
          metaContainers <- containerService.listContainers(fqon, workspace = wrk, environment = env)
          marv2ContainerTries = metaContainers map (mcs => metaToMarathonAppInfo(metaContainerSpec = mcs, instances = Some(Seq()), deploymentIDs = None))
          marv2Containers <- Future.fromTry(
            Try{marv2ContainerTries map (_.get)}
          )
        } yield Ok(Json.toJson(Json.obj(
          "apps" -> marv2Containers
        )))
      }
    }
  }

  /**
    * GET /{fqon}/environments/{eid}/providers/{pid}/v2/apps/{appId}
    */
  def getApp(fqon: String, parentType: String, environment: UUID, providerId: UUID, appId: String) = MarAuth(fqon).async { implicit request =>
    containerService.appComponents(environment) match {
      case Failure(e) => throw e
      case Success((wrk,env)) => {
        // TODO: Needs a lot of error handling
        containerService.findContainer(fqon, workspace = wrk, environment = env, containerName = appId) flatMap {
          _ match {
            case Some(metaContainer) => Future.fromTry(
              metaToMarathonAppInfo(metaContainer, instances = Some(Seq()), deploymentIDs = None) map (
                appInfo => Ok( Json.obj("app" -> appInfo) )
              )
            )
            case None => Future.successful(NotFound(Json.obj(
              "message" -> s"App '/${appId}' does not exist"
            )))
          }
        }
      }
    }
  }

  /**
    * DELETE /{fqon}/environments/{eid}/providers/{pid}/v2/apps/{appId}
    */
  def deleteApp(fqon: String, parentType: String, environment: UUID, providerId: UUID, appId: String) = MarAuth(fqon).async { implicit request =>
    containerService.appComponents(environment) match {
      case Failure(e) => throw e
      case Success((wrk,env)) => {
        // TODO: Needs a lot of error handling
        containerService.deleteContainer(fqon, workspace = wrk, environment = env, containerName = appId) flatMap {
          _ match {
            case Some(metaContainer) => Future.fromTry(
              metaToMarathonAppInfo(metaContainer, instances = Some(Seq()), deploymentIDs = None) map (
                appInfo => Ok( Json.obj(
                  "deployment" -> UUID.randomUUID(),
                  "version" -> DateTime.now(DateTimeZone.UTC).toString
                ) ) )
            )
            case None => Future.successful(NotFound(Json.obj(
              "message" -> s"App '/${appId}' does not exist"
            )))
          }
        }
      }
    }
  }

  /**
   * POST /{fqon}/environments/{eid}/providers/{pid}/v2/apps
   */
  def createApp(fqon: String, parentType: String, environment: UUID, providerId: UUID) = MarAuth(fqon).async(parse.json) { implicit request =>
    containerService.appComponents(environment) match {
      case Failure(e) => throw e
      case Success((wrk,env)) => {
        val provider = containerService.marathonProvider(providerId)
        // TODO: Needs a lot of error handling
        for {
          (name,props) <- Future.fromTry(marAppPayloadToMetaContainerSpec(request.body, provider) flatMap {
            case (maybeName,cspec) => maybeName match {
              case None => Failure(BadRequestException("payload did not include app name"))
              case Some(name) => Success((name,cspec))
            }
          })
          metaContainer <- containerService.launchContainer(fqon, wrk, env, name, props)
          marv2Container <- Future.fromTry(metaToMarathonAppInfo(
            metaContainerSpec = metaContainer,
            instances = Some(Seq()),
            deploymentIDs = None
          ))
        } yield Created(Json.toJson(marv2Container))
      }
    }
  }

  private def execAppFunction(fqon: String,
                              parentType: String,
                              environment: UUID,
                              provider: GestaltResourceInstance, proxyUri: String)
                             (fn: ProxyAppFunction) =
  {
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
