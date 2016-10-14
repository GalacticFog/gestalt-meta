package controllers

import java.util.UUID

import com.galacticfog.gestalt.security.api.errors.UnauthorizedAPIException
import org.joda.time.{DateTimeZone, DateTime}
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.security.play.silhouette.OrgContextRequest

import controllers.util._
import play.api.libs.json._
import scala.concurrent.Future
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.marathon._

class MarathonAPIController(containerService: ContainerService) extends Authorization {

  import com.galacticfog.gestalt.security.api.json.JsonImports._

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

  /**
   * GET /{fqon}/environments/{eid}/providers/{pid}/v2/deployments
   */
  def listDeployments(fqon: String, environment: UUID, providerId: UUID) = MarAuth(fqon).async { implicit request =>
    val provider = containerService.marathonProvider(providerId)
    containerService.appComponents(environment) match {
      case Failure(e) => throw e
      case Success((wrk,env)) =>
        containerService.marathonClient(provider).listDeploymentsAffectingEnvironment_marathon_v2(fqon, wrk.name, env.name) map { Ok(_) }
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
  def listApps(fqon: String, environment: UUID, providerId: UUID) = MarAuth(fqon).async { implicit request =>
    containerService.appComponents(environment) match {
      case Failure(e) => throw e
      case Success((wrk,env)) => {
        // TODO: Needs a lot of error handling
        for {
          metaContainers <- containerService.listEnvironmentContainers(fqon, workspace = wrk, environment = env)
          marv2ContainerTries = metaContainers map (mcs => metaToMarathonAppInfo(spec = mcs, instances = Some(Seq()), deploymentIDs = None))
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
  def getApp(fqon: String, environment: UUID, providerId: UUID, appId: String) = MarAuth(fqon).async { implicit request =>
    containerService.appComponents(environment) match {
      case Failure(e) => throw e
      case Success((wrk,env)) => {
        // TODO: Needs a lot of error handling
        containerService.findEnvironmentContainerByName(fqon, workspace = wrk, environment = env, containerName = appId) flatMap {
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
  def deleteApp(fqon: String, environment: UUID, providerId: UUID, appId: String) = MarAuth(fqon).async { implicit request =>
    containerService.appComponents(environment) match {
      case Failure(e) => throw e
      case Success((wrk,env)) => {
        // TODO: Needs a lot of error handling
        for {
          container <- containerService.findEnvironmentContainerByName(fqon, workspace = wrk, environment = env, containerName = appId)
          response <- container match {
            case Some(c) => containerService.deleteContainer(fqon, workspace = wrk, environment = env, containerId = c.id.get) map {
              _ => Ok(Json.obj(
                "deployment" -> UUID.randomUUID(),
                "version" -> DateTime.now(DateTimeZone.UTC).toString
              ))
            }
            case None => Future.successful(NotFound(Json.obj(
              "message" -> s"App '/${appId}' does not exist"
            )))
          }
        } yield response
      }
    }
  }

  /**
   * POST /{fqon}/environments/{eid}/providers/{pid}/v2/apps
   */
  def createApp(fqon: String, environment: UUID, providerId: UUID) = MarAuth(fqon).async { implicit request =>
    containerService.appComponents(environment) match {
      case Failure(e) => throw e
      case Success((wrk,env)) => {
        val provider = containerService.marathonProvider(providerId)
        // TODO: Needs a lot of error handling
        for {
          body <- Future.fromTry {
            request.body.asJson.fold[Try[JsValue]](Failure(BadRequestException("requires json body")))(Success(_))
          }
          app <- Future.fromTry {body.validate[AppUpdate] match {
            case JsSuccess(app,_) => Success(app)
            case JsError(_) => Failure(new BadRequestException("Invalid JSON"))
          }}
          (name,props) <- Future.fromTry(marathonToMetaContainerSpec(app, provider) flatMap {
            case (maybeName,cspec) => maybeName match {
              case None => Failure(BadRequestException("payload did not include app name"))
              case Some(name) => Success((name,cspec))
            }
          })
          metaContainer <- containerService.launchContainer(fqon, wrk, env, name, props)
          marv2Container <- Future.fromTry(metaToMarathonAppInfo(
            spec = metaContainer,
            instances = Some(Seq()),
            deploymentIDs = None
          ))
        } yield Created(Json.toJson(marv2Container))
      }
    }
  }

  def handleRequest(method: String, fqon: String, envId: UUID, providerId: UUID, path: String) = Action.async { implicit request =>
    val AppPath = "apps/(.*)".r
    (method,path) match {
      case ("GET", "apps")            => listApps(fqon, envId, providerId)(request)
      case ("POST","apps")            => createApp(fqon, envId, providerId)(request)
      case ("GET", AppPath(appId))    => getApp(fqon, envId, providerId, appId)(request)
      case ("GET", "info")            => getInfo(fqon, envId, providerId)(request)
      case ("GET", "deployments")     => listDeployments(fqon, envId, providerId)(request)
      case ("DELETE", AppPath(appId)) => deleteApp(fqon, envId, providerId, appId)(request)
    }
  }

}
