package com.galacticfog.gestalt.util

import scala.util.{Try,Success,Failure}
import scala.concurrent.{Future,ExecutionContext}
import cats.syntax.either._
import cats.ApplicativeError
import cats.instances.future._
import play.api.libs.json._
import play.api.mvc._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.security.api.errors.{ BadRequestException => SecurityBadRequestException }
import com.galacticfog.gestalt.security.api.errors.{ UnauthorizedAPIException => SecurityUnauthorizedAPIException }
import com.galacticfog.gestalt.security.api.errors.{ ForbiddenAPIException => SecurityForbiddenAPIException }
import com.galacticfog.gestalt.security.api.errors.{ ResourceNotFoundException => SecurityResourceNotFoundException }
import com.galacticfog.gestalt.security.api.errors.{ ConflictException => SecurityConflictException }
import com.galacticfog.gestalt.security.api.errors.{ UnknownAPIException => SecurityUnknownAPIException }
// import com.galacticfog.gestalt.security.api.errors.{ APIParseException => SecurityAPIParseException }

object Error {
  sealed trait Error {
    def message: String
  }
  case class NotFound(message: String) extends Error
  case class BadRequest(message: String) extends Error
  case class Conflict(message: String) extends Error
  case class Forbidden(message: String) extends Error
  case class Unauthorized(message: String) extends Error
  case class UnprocessableEntity(message: String) extends Error
  case class NotAcceptable(message: String) extends Error
  case class Default(message: String) extends Error

  trait ToError[A <: Error] {
    def make(message: String): A
  }
}

object EitherWithErrors {
  type EitherError[A] = Either[Error.Error,A]
  
  implicit val toNotFound: Error.ToError[Error.NotFound] = new Error.ToError[Error.NotFound] {
    def make(message: String): Error.NotFound = Error.NotFound(message)
  }
  implicit val toBadRequest: Error.ToError[Error.BadRequest] = new Error.ToError[Error.BadRequest] {
    def make(message: String): Error.BadRequest = Error.BadRequest(message)
  } 
  implicit val toConflict: Error.ToError[Error.Conflict] = new Error.ToError[Error.Conflict] {
    def make(message: String): Error.Conflict = Error.Conflict(message)
  } 
  implicit val toForbidden: Error.ToError[Error.Forbidden] = new Error.ToError[Error.Forbidden] {
    def make(message: String): Error.Forbidden = Error.Forbidden(message)
  } 
  implicit val toUnauthorized: Error.ToError[Error.Unauthorized] = new Error.ToError[Error.Unauthorized] {
    def make(message: String): Error.Unauthorized = Error.Unauthorized(message)
  } 
  implicit val toUnprocessableEntity: Error.ToError[Error.UnprocessableEntity] = new Error.ToError[Error.UnprocessableEntity] {
    def make(message: String): Error.UnprocessableEntity = Error.UnprocessableEntity(message)
  } 
  implicit val toNotAcceptable: Error.ToError[Error.NotAcceptable] = new Error.ToError[Error.NotAcceptable] {
    def make(message: String): Error.NotAcceptable = Error.NotAcceptable(message)
  } 
  implicit val toDefault: Error.ToError[Error.Default] = new Error.ToError[Error.Default] {
    def make(message: String): Error.Default = Error.Default(message)
  } 

  def eitherFromJsResult[A](jsResult: JsResult[A]): Either[Error.Error,A] = {
    jsResult match {
      case JsError(errors) => {
        val errorMessage = errors map { case(path, errors) =>
          val allErrors = errors.map(_.message).mkString(", ")
          s"${path}: $allErrors"
        } mkString("; ")
        Left(Error.BadRequest(s"Failed to parse payload: ${errorMessage}"))
      }
      case JsSuccess(value, _) => Right(value)
    }
  }

  private def throwableToError(throwable: Throwable): Error.Error = {
    throwable match {
      case e: ResourceNotFoundException     => Error.NotFound(e.getMessage())
      case e: BadRequestException           => Error.BadRequest(e.getMessage())
      case e: UnrecognizedResourceException => Error.BadRequest(e.getMessage())
      case e: NotAcceptableException        => Error.NotAcceptable(e.getMessage())
      case e: ConflictException             => Error.Conflict(e.getMessage())
      case e: ForbiddenException            => Error.Forbidden(e.getMessage())
      case e: UnprocessableEntityException  => Error.UnprocessableEntity(e.getMessage())
      case e: SecurityBadRequestException       => Error.BadRequest(e.getMessage())
      case e: SecurityResourceNotFoundException => Error.NotFound(e.getMessage())
      case e: SecurityConflictException         => Error.Conflict(e.getMessage())
      case e: SecurityUnknownAPIException       => Error.BadRequest(e.getMessage())
      case e: SecurityUnauthorizedAPIException  => Error.Unauthorized(e.getMessage())
      case e: SecurityForbiddenAPIException     => Error.Forbidden(e.getMessage())      
      case e => Error.Default(e.getMessage())
    }
  }

  private def errorToThrowable(error: Error.Error) = {
    error match {
      case Error.NotFound(message: String) => new ResourceNotFoundException(message)
      case Error.BadRequest(message: String) => new BadRequestException(message)
      case Error.Conflict(message: String) => new ConflictException(message)
      case Error.Forbidden(message: String) => new ForbiddenException(message)
      case Error.Unauthorized(message: String) => new GenericApiException(500, message)
      case Error.UnprocessableEntity(message: String) => new UnprocessableEntityException(message)
      case Error.NotAcceptable(message: String) => new NotAcceptableException(message)
      case Error.Default(message: String) => new GenericApiException(500, message)
    }
  }

  def eitherFromTry[A](tryBlock: Try[A]): EitherError[A] = {
    tryBlock match {
      case Success(v) => Right(v)
      case Failure(throwable) => {
        throwable.printStackTrace()
        Left(throwableToError(throwable))
      }
    }
  }

  def eitherFrom[B <: Error.Error](implicit toError: Error.ToError[B]) = {
    new {
      def option[A](option: Option[A], ifNone: => String): EitherError[A] = {
        Either.fromOption(option, toError.make(ifNone))
      }
    }
  }

  def eitherFromOption[A](option: Option[A], ifNone: => String): EitherError[A] = {
    eitherFrom[Error.Default].option[A](option, ifNone)
  }

  def errorToResult(error: Error.Error): Result = {
    val message = errorToThrowable(error).asJson
    error match {
      case e: Error.NotFound => Results.NotFound(message)
      case e: Error.BadRequest => Results.BadRequest(message)
      case e: Error.Conflict => Results.Conflict(message)
      case e: Error.Forbidden => Results.Forbidden(message)
      case e: Error.Unauthorized => Results.Forbidden(message)
      case e: Error.UnprocessableEntity => Results.UnprocessableEntity(message)
      case e: Error.NotAcceptable => Results.NotAcceptable(message)
      case e: Error.Default => Results.InternalServerError(message)
    }
  }

  implicit def futureStringApplicativeError(implicit ec: ExecutionContext) = new ApplicativeError[Future,String] {
    def ap[A, B](ff: Future[(A) => B])(fa: Future[A]): Future[B] = {
      ApplicativeError[Future,Throwable].ap(ff)(fa)
    }
    def pure[A](x: A): Future[A] = {
      ApplicativeError[Future,Throwable].pure(x)
    }
    def handleErrorWith[A](fa: Future[A])(f: String => Future[A]): Future[A] = {
      ApplicativeError[Future,Throwable].handleErrorWith(fa) { e =>
        e.printStackTrace()
        f(e.getMessage())
      }
    }
    def raiseError[A](e: String): Future[A] = {
      ApplicativeError[Future,Throwable].raiseError(new RuntimeException(e))
    }
  }

  implicit def futureErrorApplicativeError(implicit ec: ExecutionContext) = new ApplicativeError[Future,Error.Error] {
    def ap[A, B](ff: Future[(A) => B])(fa: Future[A]): Future[B] = {
      ApplicativeError[Future,Throwable].ap(ff)(fa)
    }
    def pure[A](x: A): Future[A] = {
      ApplicativeError[Future,Throwable].pure(x)
    }
    def handleErrorWith[A](fa: Future[A])(f: Error.Error => Future[A]): Future[A] = {
      ApplicativeError[Future,Throwable].handleErrorWith(fa) { e =>
        e.printStackTrace()
        f(throwableToError(e))
      }
    }
    def raiseError[A](e: Error.Error): Future[A] = {
      ApplicativeError[Future,Throwable].raiseError(errorToThrowable(e))
    }
  }

  implicit def tryErrorApplicativeError = new ApplicativeError[Try,Error.Error] {
    def ap[A, B](ff: Try[(A) => B])(fa: Try[A]): Try[B] = {
      for(
        f <- ff;
        a <- fa
      ) yield f(a)
    }
    def pure[A](x: A): Try[A] = Success(x)
    def handleErrorWith[A](fa: Try[A])(f: Error.Error => Try[A]): Try[A] = {
      fa recoverWith { case e: Throwable =>
        f(throwableToError(e))
      }
    }
    def raiseError[A](e: Error.Error): Try[A] = Failure(errorToThrowable(e))
  }
}