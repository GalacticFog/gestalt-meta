package com.galacticfog.gestalt.meta.providers


import play.api.libs.json._

import com.galacticfog.gestalt.json.Js
import play.api.libs.json.Reads._ // Custom validation helpers
import play.api.libs.functional.syntax._ // Combinator syntax
import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models._

import scala.util.{Try,Success,Failure}
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.json._
import com.galacticfog.gestalt.meta.api.errors._

case class ServiceBinding(
    binding: Option[String] = Some("eager"), 
    singleton: Option[Boolean] = Some(true))
    
object ServiceBinding {
  implicit lazy val providerServiceBindingFormat = Json.format[ServiceBinding]
  
  def fromJson(js: JsValue): Try[ServiceBinding] = Js.parse[ServiceBinding](js)
}

case class ProviderService(init: ServiceBinding, container_spec: JsValue)
object ProviderService {
  implicit lazy val providerService = Json.format[ProviderService]
  
  def fromJson(js: JsValue): Try[ProviderService] = Js.parse[ProviderService](js)
  
  def fromResource(r: GestaltResourceInstance): Seq[ProviderService] = {
    val services = r.properties.get.get("services")
    if (services.isEmpty) Seq.empty else {
    val svcs = (Json.parse(r.properties.get("services"))).as[JsArray]
      svcs.value map { s => fromJson(s).get }
    }
  }
  
  def providerId(ps: ProviderService): Option[UUID] = {
    Js.find(ps.container_spec.as[JsObject], "/properties/provider/id") map {
      jid => UUID.fromString(jid.as[String])
    }
  }
}


case class ProviderMap(root: GestaltResourceInstance, idx: Int = 0, env: Option[ProviderEnv] = None) {

  def this(id: UUID, idx: Int) = this{
    ResourceFactory.findById(id) getOrElse {
      throw new IllegalArgumentException(s"Provider with ID '$id' not found.")
    }
  }
  
  val id = root.id
  val org = root.orgId
  val name = root.name
  val properties = root.properties
  val resource = root
  
  private val linkedProviders: Seq[LinkedProvider] = LinkedProvider.fromResource(root)
  
  lazy val services: Seq[ProviderService] = ProviderService.fromResource(root)
  
  /**
   * The original properties.config.env - not merged with linked vars.
   */
  lazy val envConfig: Option[ProviderEnv] = {
    if (env.isDefined) env else ProviderMap.parseEnvironment(root)
  }
  
  /**
   * These are the linked providers that this provider depends on.
   */
  lazy val dependencies: Seq[ProviderMap] = {
    val pids = linkedProviders.map(_.id)
    val providers = ResourceFactory.findAllIn(pids)
    
    if (pids.size != providers.size) {
      val found = providers map { _.id }
      val missing = pids.diff(found).mkString(",")
      throw new UnprocessableEntityException(
          s"${found.size} of ${pids.size} linked_providers found. missing: [$missing]")
    }
    providers map { ProviderMap(_) }
  }
  
  /**
   * This maps the UUID of linked providers to the 'prefix'
   * used to name linked provider variables.
   */
  lazy val prefixMap: Map[UUID, String] = {
    (linkedProviders map { lp => (lp.id, lp.name) }).toMap
  }
}


object ProviderMap {
  
  /**
   * Replace the [properties.config] for a given resource with the given ProviderEnv.
   */
  def replaceEnvironmentConfig(r: GestaltResourceInstance, env: ProviderEnv): Try[GestaltResourceInstance] = Try{
    val newconfig = Json.obj("env" -> Json.toJson(env)).toString
    val newprops = (r.properties.get - "config") ++ Map("config" -> newconfig)
    val out = r.copy(properties = Some(newprops))
    out
  }
  
  def parseEnvironment(r: GestaltResourceInstance): Option[ProviderEnv] = {
    val props = r.properties getOrElse {
      throw new IllegalArgumentException(s"Resource '${r.id}' does not have properties.")
    }

    props.get("config").fold {
      val none: Option[ProviderEnv] = None; none
    }{ config =>
      Try(Json.parse(config).as[JsObject]) match {
        case Failure(e) =>
          throw new IllegalArgumentException("Could not parse 'properties.config': " + e.getMessage)
        case Success(configjs) => {
          Js.find(configjs, "/env") flatMap { ev => Js.parse[ProviderEnv](ev).toOption }
        }
      }      
    }
  }
}

