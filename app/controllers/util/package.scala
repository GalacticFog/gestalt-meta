package controllers

import com.galacticfog.gestalt.data.models._

import scala.util.{Try,Success,Failure}
import play.api.{Logger => log}
import play.api.mvc._
import play.api.mvc.Results._
import play.api.mvc.Action
import play.api.mvc.Controller

//import com.galacticfog.gestalt.meta.services._

import com.galacticfog.gestalt.meta.api._

import play.api.libs.json._

import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.data._

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.sdk.{ResourceLink => GestaltLink}
import com.galacticfog.gestalt.meta.api.errors._

import com.galacticfog.gestalt.security.api.errors.SecurityRESTException

import com.galacticfog.gestalt.security.api.errors.{ BadRequestException => SecurityBadRequestException }
import com.galacticfog.gestalt.security.api.errors.{ UnauthorizedAPIException => SecurityUnauthorizedAPIException }
import com.galacticfog.gestalt.security.api.errors.{ ForbiddenAPIException => SecurityForbiddenAPIException }
import com.galacticfog.gestalt.security.api.errors.{ ResourceNotFoundException => SecurityResourceNotFoundException }
import com.galacticfog.gestalt.security.api.errors.{ ConflictException => SecurityConflictException }
import com.galacticfog.gestalt.security.api.errors.{ UnknownAPIException => SecurityUnknownAPIException }
import com.galacticfog.gestalt.security.api.errors.{ APIParseException => SecurityAPIParseException }
import java.util.UUID


package object util {
  
  protected[controllers] object Errors {
    def ORG_NOT_FOUND(id: String) = s"Org '${id}' not found."
    def PROPERTY_NOT_FOUND(id: String) = s"TypeProperty '${id}' not found."
    def TYPE_NOT_FOUND(id: UUID) = s"ResourceType '${id.toString}' not found."
    def RESOURCE_NOT_FOUND(id: String) = s"Resource '${id}' not found."
    def INVALID_OWNER_TYPE(id: UUID) = s"Type ID must be Gestalt Org or User. Found: ${id.toString}"
    def INVALID_RESOURCE_TYPE(identifier: String) = NotFoundResult(s"Invalid resource type '${identifier}'")
    def USER_SYNCHRONIZATION(id: UUID) = s"User ID ${id.toString} was found in Security but not in Meta - this may be a synchronization error. Contact an administrator."
    def USER_GROUP_LOOKUP_FAILED(userId: UUID, msg: String) = s"Failed looking up groups for user '${userId}': ${msg}"
    
    val LAMBDA_NO_IMPLEMENTATION = "No value for implementation.function was found."
    val RESOURCE_TYPE_NOT_GIVEN = "resource_type must be specified."
  }
  
  /* Return HTTP Results containing formatted system error JSON */
  def NotFoundResult(message: String) = NotFound(new ResourceNotFoundException(message).asJson)
  def BadRequestResult(message: String) = BadRequest(new BadRequestException(message).asJson)
  def ConflictResult(message: String) = Conflict(new ConflictException(message).asJson)
  def ForbiddenResult(message: String) = Forbidden(new ForbiddenException(message).asJson)
  def UnauthorizedResult(message: String) = Unauthorized(new UnauthorizedException(message).asJson)
  def GenericErrorResult(code: Int, message: String) = InternalServerError(new GenericApiException(code, message).asJson)
  
def HandleExceptions(e: Throwable) = {
    log.error(e.getMessage)
    (metaApiExceptions orElse securityApiExceptions orElse genericApiException)(e)
  }
  
  val metaApiExceptions: PartialFunction[Throwable, play.api.mvc.Result] = {
    case e: ResourceNotFoundException     => NotFound(e.asJson)
    case e: BadRequestException           => BadRequest(e.asJson)
    case e: UnrecognizedResourceException => BadRequest(e.asJson)
    case e: NotAcceptableException        => NotAcceptable(e.asJson)
    case e: ConflictException             => Conflict(e.asJson)    
  }
  
  val securityApiExceptions: PartialFunction[Throwable, play.api.mvc.Result] = {
    case e: SecurityBadRequestException       => BadRequestResult(e.getMessage)
    case e: SecurityResourceNotFoundException => NotFoundResult(e.getMessage)
    case e: SecurityConflictException         => ConflictResult(e.getMessage)
    case e: SecurityUnknownAPIException       => BadRequestResult(e.getMessage)
    case e: SecurityAPIParseException         => GenericErrorResult(500, e.getMessage)
    case e: SecurityUnauthorizedAPIException  => Unauthorized(e.getMessage)
    case e: SecurityForbiddenAPIException     => Forbidden(e.getMessage)      
  }
  
  val genericApiException: PartialFunction[Throwable, play.api.mvc.Result] = {
    case x => GenericErrorResult(500, x.getMessage)
  }
  
  def HandleRepositoryExceptions(e: Throwable) = e match {
    case e: ResourceNotFoundException     => NotFound(e.asJson)
    case e: BadRequestException           => BadRequest(e.asJson)
    case e: UnrecognizedResourceException => BadRequest(e.asJson)
    case e: ConflictException             => Conflict(e.asJson)
    case x => {
      if (x.isInstanceOf[SecurityRESTException])
        HandleSecurityApiException(x)
      else GenericErrorResult(500, x.getMessage)
    }
  }
  
  def HandleSecurityApiException(e: Throwable) = e.asInstanceOf[SecurityRESTException] match {
    case e: SecurityBadRequestException       => BadRequestResult(e.getMessage)
    case e: SecurityResourceNotFoundException => NotFoundResult(e.getMessage)
    case e: SecurityConflictException         => ConflictResult(e.getMessage)
    case e: SecurityUnknownAPIException       => BadRequestResult(e.getMessage)
    case e: SecurityAPIParseException         => GenericErrorResult(500, e.getMessage)
    case e: SecurityUnauthorizedAPIException  => Unauthorized(e.getMessage)
    case e: SecurityForbiddenAPIException     => Forbidden(e.getMessage)  
  }    
  
  def trace(method: String) = {
    log.debug("%s::%s".format(this.getClass.getSimpleName, method))
  }
  
  def trimquotes(s: String) = {
    s.trim
     .stripPrefix("\"")
     .stripSuffix("\"")
  }
  
  def stringmap(m: Option[Map[String,JsValue]]): Option[Hstore] = {
//    log.debug("stringmap(...) => " + m)
//    m map { x => 
//      x map { x =>
//        val normalized = x._2 match {
//          case s: JsString => trimquotes(s.toString)
//          case _ => x._2.toString
//        }
//        (x._1, normalized)
//      } 
//    }
    
    m map { _ map { case (k,v) =>
        (k -> (v match {
          case JsString(value) => value
          case other => other.toString
        }))  
      }
    }
  }
  
  abstract class TryHandler[A,B](success: A => B)(failure: Throwable => B) {
    def handle(in: Try[A]) = in match {
      case Success(out) => success(out)
      case Failure(err) => failure(err)
    }
  }
  
  private def trySuccess(in: JsValue) = Ok(in)
  private def trySuccessNoResult(in: Unit) = Ok("")
  
  private def tryNotFoundFailure(err: Throwable) = {    
    log.debug("***tryNotFoundFailure : " + err.getMessage)
    err match {
      case rnf: ResourceNotFoundException => {
        log.info( rnf.getMessage )
        NotFound( rnf.asJson )
      }
      case unknown => {
        log.error( err.getMessage )
        InternalServerError( err.getMessage )
      }
    }
  }
  
  class OkNotFoundNoResultHandler 
    extends TryHandler[Unit,Result](trySuccessNoResult)(tryNotFoundFailure)
  
  class OkNotFoundHandler 
    extends TryHandler[JsValue,Result](trySuccess)(tryNotFoundFailure)
  
  def okNotFound(f: Try[JsValue]) = new OkNotFoundHandler().handle( f )
  def okNotFoundNoResult(f: Try[Unit]) = new OkNotFoundNoResultHandler().handle( f )
  
}