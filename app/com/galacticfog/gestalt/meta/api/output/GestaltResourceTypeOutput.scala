package com.galacticfog.gestalt.meta.api.output

import play.api.libs.json.JsValue
import java.util.UUID

case class GestaltResourceTypeOutput(
  id: UUID,
  name: String,
  extend: Option[JsValue], //link
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
  auth: Option[JsValue],
  property_defs: Option[JsValue] = None) extends ResourceOutput