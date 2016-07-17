package com.galacticfog.gestalt.meta.auth

import java.util.UUID
import org.joda.time.DateTime
import play.api.libs.json.Json

case class AuthorizationMessage(timestamp: DateTime = DateTime.now, status: String, user: UUID, resource: UUID, action: String, message: Option[String] = None)

object AuthorizationMessage {
  implicit lazy val authorizationMessageFormat = Json.format[AuthorizationMessage]
}