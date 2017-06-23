package controllers

import java.util.UUID

import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.security.api.errors.UnauthorizedAPIException
import org.joda.time.{DateTime, DateTimeZone}
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.meta.api.errors.{BadRequestException, ResourceNotFoundException}
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment, OrgContextRequest}
import controllers.util._
import play.api.libs.json._

import scala.concurrent.Future
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.marathon._
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi
import javax.inject.Singleton

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import services.{FakeURI, MarathonClientFactory, ProviderContext}

@Singleton
class MarathonAPIController @Inject()( messagesApi: MessagesApi,
                                       env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
                                       marathonClientFactory: MarathonClientFactory,
                                       containerService: ContainerService )
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {

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
          lazy val defRealm = s"${securityClient.protocol}://${securityClient.hostname}:${securityClient.port}/${org}/oauth/issue"
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
  def listDeployments(fqon: String, environment: UUID, providerId: UUID) = MarAuth(fqon) { implicit request =>
    Ok(Json.arr())
  }

  /**
   * GET /{fqon}/environments/{eid}/providers/{pid}/v2/info
   */
  def getInfo(fqon: String, environment: UUID, provider: UUID) = MarAuth(fqon).async { implicit request =>
    val resp = for {
      marClient <- marathonClientFactory.getClient(ContainerService.caasProvider(provider))
      info <- marClient.getInfo
    } yield Ok(info)
    resp recover {
      case e: Throwable => HandleExceptions(e)
    }
  }

  /**
   * GET /{fqon}/environments/{eid}/providers/{pid}/v2/apps
   * TODO: this is not protected by policy
   */
  def listApps(fqon: String, environment: UUID, providerId: UUID) = MarAuth(fqon).async { implicit request =>
    for {
      metaContainers <- containerService.listEnvironmentContainers(fqon, environment)
      marv2ContainerTries = metaContainers map { case (cRes,cInsts) => metaToMarathonAppInfo(
        spec = ContainerSpec.fromResourceInstance(cRes).get,
        instances = Some(cInsts),
        deploymentIDs = None
      ) }
      successes = marv2ContainerTries collect {case Success(appInfo) => appInfo}
      _ = marv2ContainerTries collect {case Failure(ex) => ex} foreach {
        ex => log.error("Error converting Container resource to Marathon AppInfo",ex)
      }
    } yield Ok(Json.toJson(Json.obj(
      "apps" -> successes
    )))
  }

  /**
    * GET /{fqon}/environments/{eid}/providers/{pid}/v2/apps/{appId}
    */
  def getApp(fqon: String, environment: UUID, providerId: UUID, appId: String) = MarAuth(fqon).async { implicit request =>
    val fMaybeResponse = for {
      maybeResource <- Future {
        ResourceFactory.findChildByName(environment, ResourceIds.Container, appId)
      }
      maybeUpdates <- maybeResource match {
        case None => Future.successful(None)
        case Some(r) => containerService.getEnvironmentContainer(fqon, environment, r.id)
      }
      response = for {
        (resource, instances) <- maybeUpdates
        spec <- ContainerSpec.fromResourceInstance(resource).toOption
        app <- metaToMarathonAppInfo(
          spec = spec,
          instances = Some(instances),
          deploymentIDs = None
        ).toOption
      } yield Ok(Json.obj("app" -> app))
    } yield response

    fMaybeResponse map {
      _.getOrElse(NotFound(Json.obj(
        "message" -> s"App '/${appId}' does not exist"
      )))
    } recover {
      case e: Throwable => InternalServerError(Json.obj(
        "message" -> e.getMessage
      ))
    }
  }

  /**
    * DELETE /{fqon}/environments/{eid}/providers/{pid}/v2/apps/{appId}
    */
  def deleteApp(fqon: String, environment: UUID, providerId: UUID, appId: String) = MarAuth(fqon).async { implicit request =>
    val deleted = for {
      container <- ResourceFactory.findChildByName(environment, ResourceIds.Container, appId)
      response = containerService.deleteContainer(container, request.identity, request) map {
        _ =>
          Ok(Json.obj(
            "deployment" -> UUID.randomUUID(),
            "version" -> DateTime.now(DateTimeZone.UTC).toString
          ))
      }
    } yield response
    deleted getOrElse (Future.successful(NotFound(Json.obj(
      "message" -> s"App '/${appId}' does not exist"
    ))))
  }

  /**
   * POST /{fqon}/environments/{eid}/providers/{pid}/v2/apps
   */
  def createApp(fqon: String, environment: UUID, providerId: UUID) = MarAuth(fqon).async { implicit request =>
    ResourceFactory.findById(ResourceIds.Environment, environment) match {
      case None    => Future.successful(BadRequest(Json.obj(
        "message" -> s"Could not find environment corresponding to UUID ${environment}, check your client configuration"
      )))
      case Some(_) =>
        val provider = ContainerService.caasProvider(providerId)
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
          metaContainer <- containerService.createContainer(
            context = ProviderContext(FakeURI(s"/${fqon}/environments/${environment}/containers"), providerId, None),
            user = request.identity,
            containerSpec = props,
            userRequestedId = None
          )
          marv2Container <- Future.fromTry(metaToMarathonAppInfo(
            spec = ContainerSpec.fromResourceInstance(metaContainer).get,
            instances = Some(Seq.empty),
            deploymentIDs = None
          ))
        } yield Created(Json.toJson(marv2Container))

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
      case _                          => Future.successful(BadRequest(Json.obj("message" -> "endpoint not supported")))
    }
  }

}
