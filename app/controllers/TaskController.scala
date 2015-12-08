package controllers


import play.api.{Logger => log}
import play.api.Play.current
import play.api.libs.ws._
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import scala.concurrent.Future
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.mvc.RequestHeader
import play.api.mvc.AnyContent
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Try, Success, Failure}
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.services._
import com.galacticfog.gestalt.tasks.io.TaskStatus
import com.galacticfog.gestalt.tasks.play.actors.TaskEventMessage
import com.galacticfog.gestalt.tasks.play.io._
import controllers.util._
import controllers.util.db._
import play.mvc.Result
import java.util.UUID
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltBaseAuthProvider
import com.galacticfog.gestalt.security.play.silhouette.GestaltSecuredController
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.mohiva.play.silhouette.api.services.AuthenticatorService
import com.mohiva.play.silhouette.impl.authenticators.{DummyAuthenticatorService, DummyAuthenticator}
import com.galacticfog.gestalt.security.api._
import com.galacticfog.gestalt.security.api.json.JsonImports
import play.api.libs.json._
import com.galacticfog.gestalt.security.api.json.JsonImports.{orgFormat,linkFormat}
import com.mohiva.play.silhouette.api.util.Credentials
import com.galacticfog.gestalt.meta.api.sdk.{ResourceLink => GestaltLink}
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._

object TaskController extends GestaltFrameworkSecuredController[DummyAuthenticator] 
  with MetaController with NonLoggingTaskEvents {
  
  implicit lazy val resourceLinkFormat = Json.format[GestaltLink]
  implicit lazy val gestaltTaskFormat = Json.format[GestaltTask]
  
  
  def getAllTasks(org: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    unwrapAll(org, TaskFactory.findAll(org))
  }

  def getTaskById(org: UUID, id: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    unwrap(TaskFactory.findById(id))
  }

  def getTaskByService(org: UUID, service: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    unwrapAll(org, TaskFactory.findAllByService(service))
  }

  def getTaskByOwner(org: UUID, owner: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    unwrapAll(org, TaskFactory.findAllByOwner(owner))
  }
  
  
  def createTask(org: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    Future {
      TaskFactory.create(request.body.as[GestaltTask]) match {
        case Success(t) => Created(pretty(Option(t)))
        case Failure(e) => InternalServerError(e.getMessage)
      }
    }
  }
  
  def deleteTask(org: UUID, id: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    TaskFactory.delete(id) match {
      case Success(int) => NoContent
      case Failure(err) => InternalServerError(err.getMessage)
    }
  }

  
  private def unwrap(t: Try[Option[GestaltTask]]) = {
    t match {
      case Success(t) => t match {
        case Some(task) => Ok(pretty(t))
        case None => NotFound("")
      }
      case Failure(ex) => InternalServerError( ex.getMessage )
    }
  }
  
  private def unwrapAll(org: UUID, ts: Try[Seq[GestaltTask]]) = {
    ts match {
      case Success(ts) => Ok(pretty(org, ts))
      case Failure(ex) => InternalServerError( ex.getMessage )
    }
  }
  
  private def pretty(t: Option[GestaltTask]) = { 
    Json.prettyPrint(Json.toJson(t)) 
  }
  
  private def pretty(org: UUID, ts: Seq[GestaltTask]) = { 
    Json.prettyPrint(Json.toJson( ts map { toLink( org, _ ) } )) 
  }
  
  private def toLink(org: UUID, t: GestaltTask) = {
    GestaltLink(ResourceIds.Task, t.id, Some(t.name), 
        Some(s"http://{host}/orgs/${org.toString}/tasks/${t.id.toString}"))
  }
  
}