package com.galacticfog.gestalt.meta.api.output

import play.api.libs.json._
import java.util.UUID

case class GestaltTypePropertyOutput(
  id: UUID = UUID.randomUUID(),
  name: String,
  resource_type: JsValue, // name
  resource_state: JsValue, //name
  org: JsValue, //link
  owner: JsValue, //link
  description: Option[String],
  created: JsValue,
  modified: JsValue,
  
  applies_to: Option[JsValue],
  datatype: JsValue, //name
  default_value: Option[JsString],  
  is_sealed: Boolean,
  is_system: Boolean,
  requirement_type: JsValue, //name
  visibility_type: JsValue, //name
  refers_to: Option[JsValue], //link
  
  properties: Option[JsValue],
  variables: Option[JsValue],
  tags: Option[JsValue],
  auth: Option[JsValue]) extends ResourceOutput