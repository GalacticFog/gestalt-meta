package com.galacticfog.gestalt.meta.providers


import play.api.libs.json._
import com.galacticfog.gestalt.json.Js
import play.api.libs.json.Reads._ // Custom validation helpers
import play.api.libs.functional.syntax._ // Combinator syntax
import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models._

import scala.util.{Try,Success,Failure}


case class ProviderEnv(public: Option[Map[String, String]], privatev: Option[Map[String, String]]) {
  def flatten() = {
    getmap(this.public) ++ getmap(this.privatev)
  }
  
  def ++(that: ProviderEnv): ProviderEnv = {
    val pub = getmap(this.public) ++ getmap(that.public)
    val prv = getmap(this.privatev) ++ getmap(that.privatev)
    
    ProviderEnv(Some(pub), Some(prv))
  }
  
  private def getmap(m: Option[Map[String,String]]): Map[String, String] = m getOrElse Map.empty
}

object ProviderEnv {
  
  def fromResource(r: GestaltResourceInstance): Option[ProviderEnv] = for {
    ps  <- r.properties
    cf  <- ps.get("config")
    js = Json.parse(cf)
    env <- Js.find(js.as[JsObject], "/env")
    out = Js.parse[ProviderEnv](env).get
  } yield out  
  
  def merge(a: ProviderEnv, b: ProviderEnv): ProviderEnv = {
    val npublic = a.public map { pub => pub ++ b.public.getOrElse(Map.empty) }
    val nprivate = a.privatev map { priv => priv ++ b.privatev.getOrElse(Map.empty) }
    ProviderEnv(npublic, nprivate)
  }
  
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

