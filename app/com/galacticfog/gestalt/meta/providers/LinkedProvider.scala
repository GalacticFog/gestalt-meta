package com.galacticfog.gestalt.meta.providers

import play.api.libs.json._
import com.galacticfog.gestalt.json.Js
import play.api.libs.json.Reads._ // Custom validation helpers
import play.api.libs.functional.syntax._ // Combinator syntax
import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models._

import scala.util.{Try,Success,Failure}

import com.galacticfog.gestalt.meta.api.errors._

case class LinkedProvider(name: String, id: UUID, location: Option[String])

object LinkedProvider {
  lazy implicit val linkedProviderFormat = Json.format[LinkedProvider]  
  
  /**
   * Get list of linked providers from Meta Provider Resource.
   */
  def fromResource(r: GestaltResourceInstance): Seq[LinkedProvider] = {
    val props = r.properties getOrElse {
      throw new UnprocessableEntityException(s"Resource '${r.id}' does not have properties.")
    }

    props.get("linked_providers").fold(Seq[LinkedProvider]()) { p =>
      val jsarr = Try(Json.parse(p).as[JsArray]).getOrElse {
        throw new UnprocessableEntityException("Could not parse linked_providers array.")
      }
      jsarr.value map { v =>
        Js.parse[LinkedProvider](v.as[JsObject]).getOrElse {
          throw new UnprocessableEntityException("Could not parse linked providers array.")
        }
      }
    }
  }
  
  /**
   * 
   */
  def fromJson(js: JsValue): Seq[LinkedProvider] = {
    val jsarr = Try(js.as[JsArray]).getOrElse {
      throw new UnprocessableEntityException("Could not parse linked_providers array.")
    }
    jsarr.value map { v =>
      Js.parse[LinkedProvider](v.as[JsObject]).getOrElse {
        throw new UnprocessableEntityException("Could not parse linked providers array.")
      }
    }
  }
  
}