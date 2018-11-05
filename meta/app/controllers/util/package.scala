package controllers

import java.io.{PrintWriter, StringWriter}

import com.galacticfog.gestalt.data.models._

import scala.util.{Failure, Success, Try}
import play.api.{Logger => log}
import play.api.mvc._
import play.api.mvc.Results._

//import com.galacticfog.gestalt.meta.services._


import play.api.libs.json._

import com.galacticfog.gestalt.data._

import com.galacticfog.gestalt.meta.api.sdk.{ResourceLink => _}
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
import scala.concurrent.ExecutionContext

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
  def UnprocessableEntityResult(message: String) = UnprocessableEntity(new UnprocessableEntityException(message).asJson)
  def GenericErrorResult(code: Int, message: String) = InternalServerError(new GenericApiException(code, message).asJson)
  
  def HandleExceptions(e: Throwable): Result = {
    val err = new StringWriter()
    e.printStackTrace(new PrintWriter(err))
    log.debug(err.toString)
    (metaApiExceptions orElse securityApiExceptions orElse genericApiException)(e)
  }
  
  def HandleExceptionsAsync(e: Throwable)(implicit ec: ExecutionContext) = 
    scala.concurrent.Future.successful(HandleExceptions(e))
  
  val metaApiExceptions: PartialFunction[Throwable, play.api.mvc.Result] = {
    case e: ResourceNotFoundException     => NotFound(e.asJson)
    case e: BadRequestException           => BadRequest(e.asJson)
    case e: UnrecognizedResourceException => BadRequest(e.asJson)
    case e: NotAcceptableException        => NotAcceptable(e.asJson)
    case e: ConflictException             => Conflict(e.asJson)
    case e: ForbiddenException            => Forbidden(e.asJson)
    case e: UnprocessableEntityException  => UnprocessableEntity(e.asJson)
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

  /*
  I don't see a way to unfuck-up this for example:
  Map("a" -> JsString("true"))
  -> stringmap ->
  Map("a" -> "true")
  -> unstringmap ->
  Map("a" -> JsBool(true))
  */
  def unstringmap(m: Option[Hstore]): Option[Map[String,JsValue]] = {
    m map { hstore =>
      hstore map { case(k, v) =>
        val trueV: JsValue = try {
          Json.parse(v)
        } catch {
          case _: Throwable => JsString(v)
        }
        (k, trueV)
      }
    }
  }
  
  // this was truly not the best of ideas
  def stringmap(m: Option[Map[String,JsValue]]): Option[Hstore] = {
    m map {
      _ mapValues { _ match {
        case JsString(value) => value
        case other => other.toString
      } }
    }
  }
  
  
  /*
   * TODO: This only handles true | false. Extend to allow for expansion
   * of individual resource attributes and properties.
   */
  def getExpandParam(qs: Map[String, Seq[String]]): Boolean = {
    QueryString.singleBoolean(qs,"expand")
  }

  def extractQueryParameters(qs: Map[String, Seq[String]]): Try[Seq[(String, String)]] = {
    val pairs = (qs - "expand" - "embed") map {
      case (name, Seq(value)) => Success(name -> value)
      case (name, Nil)        => Failure(BadRequestException(s"Query parameter '${name}' did not include a search term."))
      case (name, _)          => Failure(BadRequestException(s"Query parameter '${name}' included multiple search terms."))
    }
    Try{pairs.toSeq.map(_.get)}
  }


  abstract class TryHandler[A,B](success: A => B)(failure: Throwable => B) {
    def handle(in: Try[A]) = in match {
      case Success(out) => success(out)
      case Failure(err) => failure(err)
    }
  }
  
  private def trySuccess(in: JsValue) = Ok(in)
  private def trySuccessNoResult(in: Unit) = Ok("")
  
  private def tryNotFoundFailure(err: Throwable): Result = {
    log.debug("***tryNotFoundFailure : " + err.getMessage)
    err match {
      case rnf: ResourceNotFoundException => {
        log.info( rnf.getMessage )
        NotFound( rnf.asJson )
      }
      case unknown => {
        HandleExceptions(unknown)
      }
    }
  }
  
  class OkNotFoundNoResultHandler 
    extends TryHandler[Unit,Result](trySuccessNoResult)(tryNotFoundFailure)
  
  class OkNotFoundHandler 
    extends TryHandler[JsValue,Result](trySuccess)(tryNotFoundFailure)
  
  def okNotFound(f: Try[JsValue]) = new OkNotFoundHandler().handle( f )
  def okNotFoundNoResult(f: Try[Unit]) = new OkNotFoundNoResultHandler().handle( f )
 
  
  import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
  
  /**
   * Get a List of 'standard' request operations. Includes:
   * - Authorization
   * - Policy Checking
   * - Pre-Operation Event
   * - Post-Operation Event
   */
  def standardMethods(typeId: UUID, action: String): List[Operation[Seq[String]]] = {
    List(
      controllers.util.Authorize(action),
      controllers.util.Validate(action),
      controllers.util.PolicyCheck(action),
      controllers.util.EventsPre(action),
      controllers.util.EventsPost(action))
  }
  
  def requestOpts(
      user: AuthAccountWithCreds, 
      policyOwner: UUID, 
      target: GestaltResourceInstance): RequestOptions = {
    
    RequestOptions(user, 
        authTarget = Option(policyOwner), 
        policyOwner = Option(policyOwner), 
        policyTarget = Option(target))    
  }

}