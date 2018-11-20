package com.galacticfog.gestalt.util

import play.api.libs.json._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import controllers.util.{unstringmap, stringmap}

object ResourceSerde {
  // oops
  import scala.util.Either
  import cats.syntax.either._
  import com.galacticfog.gestalt.util.Either._
  def deserialize[A: Reads](resource: GestaltResourceInstance): Either[String,A] = {
    for(
      rawProperties <- Either.fromOption(resource.properties, s"Could not parse resource ${resource.id} with unset properties");
      rawJsonProperties = unstringmap(Some(rawProperties)).get;
      properties <- eitherFromJsResult(JsObject(rawJsonProperties).validate[A])
    ) yield properties
  }

  def serialize[A: Writes](resource: GestaltResourceInstance, properties: A): Either[String,GestaltResourceInstance] = {
    for(
      serializedProperties <- Either.fromOption(stringmap(Json.toJson(properties).asOpt[Map[String,JsValue]]),
       s"Failed to serialize resource properties: $properties")
    ) yield {
      resource.copy(properties=Some(resource.properties.getOrElse(Map()) ++ serializedProperties))
    }
  }
}