package com.galacticfog.gestalt.util

import scala.util.{Try,Success,Failure}
import play.api.libs.json._

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
}