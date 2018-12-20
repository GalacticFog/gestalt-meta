package com.galacticfog.gestalt.util

import scala.util.{Try,Success,Failure}
import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.json._
import cats.syntax.either._
import cats.ApplicativeError
import cats.instances.future._

/** deprecated */
object Either {
  type EitherString[A] = Either[String,A]

  def eitherFromJsResult[A](jsResult: JsResult[A]): Either[String,A] = {
    jsResult match {
      case JsError(errors) => {
        val errorMessage = errors map { case(path, errors) =>
          val allErrors = errors.map(_.message).mkString(", ")
          s"${path}: $allErrors"
        } mkString("; ")
        Left(s"Failed to parse payload: ${errorMessage}")
      }
      case JsSuccess(value, _) => Right(value)
    }
  }

  def eitherFromTry[A](tryBlock: Try[A]): Either[String,A] = {
    tryBlock match {
      case Success(v) => Right(v)
      case Failure(throwable) => {
        throwable.printStackTrace()
        Left(throwable.getMessage())
      }
    }
  }

  implicit def eitherStringEitherErrorApplicativeError(implicit ec: ExecutionContext) = new ApplicativeError[EitherString,Error.Error] {
    def ap[A, B](ff: EitherString[(A) => B])(fa: EitherString[A]): EitherString[B] = fa.ap(ff)
    def pure[A](x: A): EitherString[A] = Right(x)
    def handleErrorWith[A](fa: EitherString[A])(f: Error.Error => EitherString[A]): EitherString[A] = {
      fa match {
        case Right(v) => Right(v)
        case Left(error) => f(Error.Default(error))
      }
    }
    def raiseError[A](e: Error.Error): EitherString[A] = Left(e.message)
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
}