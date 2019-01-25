package com.galacticfog.gestalt.util

import play.api.libs.json._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import controllers.util.{unstringmap, stringmap}
import cats.syntax.either._

object ResourceSerde {
  def deserialize[A: Reads](resource: GestaltResourceInstance): Either[Error.Error,A] = {
    for(
      rawProperties <- Either.fromOption(resource.properties, Error.BadRequest(s"Could not parse resource ${resource.id} with unset properties"));
      rawJsonProperties = unstringmap(Some(rawProperties)).get;
      properties <- EitherWithErrors.eitherFromJsResult(JsObject(rawJsonProperties).validate[A])
    ) yield properties
  }

  def serialize[A: Writes](resource: GestaltResourceInstance, properties: A): Either[Error.Error,GestaltResourceInstance] = {
    for(
      serializedProperties <- Either.fromOption(stringmap(Json.toJson(properties).asOpt[Map[String,JsValue]]),
       Error.Default(s"Failed to serialize resource properties: $properties"))
    ) yield {
      // !!! will not correctly remove missing/deleted properties:
      resource.copy(properties=Some(resource.properties.getOrElse(Map()) ++ serializedProperties))
    }
  }
}