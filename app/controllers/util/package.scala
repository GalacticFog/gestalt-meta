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

package object util {
  
  /* Return HTTP Results containing formatted system error JSON */
  def NotFoundResult(message: String) = NotFound(new ResourceNotFoundException(message).asJson)
  def BadRequestResult(message: String) = BadRequest(new BadRequestException(message).asJson)
  def ConflictResult(message: String) = Conflict(new ConflictException(message).asJson)
  def GenericErrorResult(code: Int, message: String) = InternalServerError(new GenericApiException(code, message).asJson)
  
def HandleExceptions(e: Throwable) = {
    log.error(e.getMessage)
    (metaApiExceptions orElse securityApiExceptions orElse genericApiException)(e)
  }
  
  val metaApiExceptions: PartialFunction[Throwable,play.api.mvc.Result] = {
    case e: ResourceNotFoundException     => NotFound(e.asJson)
    case e: BadRequestException           => BadRequest(e.asJson)
    case e: UnrecognizedResourceException => BadRequest(e.asJson)
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
    val trimmed = s.trim
    val t1 = if (trimmed.startsWith("\"")) trimmed.drop(1) else trimmed
    if (t1.endsWith("\"")) t1.dropRight(1) else t1
  }
  
  def stringmap(m: Option[Map[String,JsValue]]): Option[Hstore] = {
    m map { x => 
      x map { x =>
        /* this bit removes the quotes the play parser includes with strings */
        val normalized = x._2 match {
          case s: JsString => trimquotes(s.toString)
          case _ => x._2.toString
        }
        (x._1, normalized)
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
  
  
  
//  private def securedGetListFailure(err: Throwable) = {
//    err match {
//      case unauth: UnauthorizedException => {
//        log.info( unauth.getMessage )
//        Unauthorized( unauth.toErrorString )
//      }
//    }
//  }
  
  class OkNotFoundNoResultHandler 
    extends TryHandler[Unit,Result](trySuccessNoResult)(tryNotFoundFailure)
  
  class OkNotFoundHandler 
    extends TryHandler[JsValue,Result](trySuccess)(tryNotFoundFailure)
  
  def okNotFound(f: Try[JsValue]) = new OkNotFoundHandler().handle( f )
  def okNotFoundNoResult(f: Try[Unit]) = new OkNotFoundNoResultHandler().handle( f )

  
//  abstract class ansi(text: String) {
//    val code: String
//    def colorize(code: String) = s"\u001B[${code}m${text}\u001B[0m"
//    override def toString = colorize(code)
//  }
//  
//  case class red(text: String) extends ansi(text) {
//    override val code = "0;31"
//  }
//  case class blue(text: String) extends ansi(text) {
//    override val code = "0;34"
//  }
//  case class green(text: String) extends ansi(text) {
//    override val code = "0;32"
//  }  
  
}