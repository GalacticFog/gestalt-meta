package com.galacticfog.gestalt.meta.providers.gwm

import java.util.UUID
import play.api.libs.json._

case class InlineApiProvider(
  id: UUID,
  locations: Seq[String]
)

case class ApiProperties(
  provider: InlineApiProvider,
  endpoints: Option[Seq[JsValue]]
)

case class ApiEndpointProperties(
  resource: Option[String],
  upstream_url: Option[String],
  implementation_id: UUID,
  implementation_type: String,    // `container` or `lambda`
  container_port_name: Option[String],    // container_port_name for container or rest api id for aws lambda
  auth_type: Option[JsValue],
  http_method: Option[String],
  location_id: Option[String],
  parent: Option[String],
  synchronous: Option[Boolean],
  provider: InlineApiProvider,
  methods: Option[Seq[String]],
  plugins: Option[JsObject],
  // gestaltSecurity: Option[JsValue],    // present in com.galacticfog.gestalt.data.bootstrap.MetSystemTypes, but not used apparently
  public_url: Option[String],
  hosts: Option[Seq[String]],
  actions: Option[JsValue]
)

object GwmSpec {
  object Implicits {
    implicit val formatInlineApiProvider = Json.format[InlineApiProvider]
    implicit val formatApiProperties = Json.format[ApiProperties]
    implicit val formatApiEndpointProperties = Json.format[ApiEndpointProperties]
  }
}