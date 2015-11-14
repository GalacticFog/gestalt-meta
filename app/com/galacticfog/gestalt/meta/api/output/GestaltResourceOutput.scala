package com.galacticfog.gestalt.meta.api.output

import play.api.libs.json.JsValue
import java.util.UUID

case class GestaltResourceOutput(
  id: UUID,
  name: String,
  resource_type: JsValue,    // name
  resource_state: JsValue,   // name
  org: JsValue,               // link
  owner: JsValue,            // link 
  description: Option[String],
  created: JsValue,            //link
  modified: JsValue,           //link
  properties: Option[JsValue],
  variables: Option[JsValue],
  tags: Option[JsValue],
  auth: Option[JsValue]) extends ResourceOutput