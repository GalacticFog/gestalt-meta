package com.galacticfog.gestalt.meta.providers

import play.api.libs.json._
import com.galacticfog.gestalt.json.Js
import play.api.libs.json.Reads._ // Custom validation helpers
import play.api.libs.functional.syntax._ // Combinator syntax
import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models._

import scala.util.{Try,Success,Failure}

case class ProviderEnv(public: Option[Map[String, String]], privatev: Option[Map[String, String]])
object ProviderEnv {
  lazy implicit val providerEnvReads: Reads[ProviderEnv] = (
    (JsPath \ "public").readNullable[Map[String,String]] and
    (JsPath \ "private").readNullable[Map[String,String]]
  )(ProviderEnv.apply _)
  
  lazy implicit val providerEnvWrites: Writes[ProviderEnv] = (
    (JsPath \ "public").writeNullable[Map[String,String]] and
    (JsPath \ "private").writeNullable[Map[String,String]]
  )(unlift(ProviderEnv.unapply))  
  
  lazy implicit val providerEnvFormat: Format[ProviderEnv] =
    Format(providerEnvReads, providerEnvWrites)  
}