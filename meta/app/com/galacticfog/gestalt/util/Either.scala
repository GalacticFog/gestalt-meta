package com.galacticfog.gestalt.util

import scala.util.{Try,Success,Failure}
import scala.concurrent.{Future,ExecutionContext}
import play.api.libs.json._
import cats.ApplicativeError
import cats.instances.future._

object Either {
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