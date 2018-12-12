package com.galacticfog.gestalt.util

import play.api.libs.json._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import controllers.util.{unstringmap, stringmap}

object ResourceSerde {
  // oops
  import scala.util.Either
  import cats.syntax.either._
  import com.galacticfog.gestalt.util.Either._
  def deserialize[A: Reads](resource: GestaltResourceInstance): Either[Error.Error,A] = {
    val r: Either[String,A] = for(
      rawProperties <- Either.fromOption(resource.properties, s"Could not parse resource ${resource.id} with unset properties");
      rawJsonProperties = unstringmap(Some(rawProperties)).get;
      properties <- eitherFromJsResult(JsObject(rawJsonProperties).validate[A])
    ) yield properties
    r.left.map { errorMessage =>
      Error.BadRequest(errorMessage)
    }
  }

  def serialize[A: Writes](resource: GestaltResourceInstance, properties: A): Either[Error.Error,GestaltResourceInstance] = {
    val r: Either[String,GestaltResourceInstance] = for(
      serializedProperties <- Either.fromOption(stringmap(Json.toJson(properties).asOpt[Map[String,JsValue]]),
       s"Failed to serialize resource properties: $properties")
    ) yield {
      // !!! will not correctly remove missing/deleted properties:
      resource.copy(properties=Some(resource.properties.getOrElse(Map()) ++ serializedProperties))
    }
    r.left.map { errorMessage =>
      Error.Default(errorMessage)
    }
  }
}