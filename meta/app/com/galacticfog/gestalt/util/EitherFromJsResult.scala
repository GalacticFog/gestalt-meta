package com.galacticfog.gestalt.util

import play.api.libs.json._

object EitherFromJsResult {
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
}