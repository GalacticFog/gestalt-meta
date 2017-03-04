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


case class ServiceBinding(
    binding: Option[String] = Some("eager"), 
    singleton: Option[Boolean] = Some(true))
object ServiceBinding {
  implicit lazy val providerServiceBindingFormat = Json.format[ServiceBinding]
  
  def fromJson(js: JsValue): Try[ServiceBinding] = Js.parse[ServiceBinding](js)
}

case class ProviderService(init: ServiceBinding, container_spec: JsValue /*ContainerSpec*/)
object ProviderService {
  implicit lazy val providerService = Json.format[ProviderService]
  
  def fromJson(js: JsValue): Try[ProviderService] = Js.parse[ProviderService](js)
  
  def fromResource(r: GestaltResourceInstance): Seq[ProviderService] = {
    val svcs = (Json.parse(r.properties.get("services"))).as[JsArray]
    svcs.value map { fromJson(_).get }
  }
  
  def providerId(ps: ProviderService): Option[UUID] = {
    Js.find(ps.container_spec.as[JsObject], "/properties/provider/id") map {
      jid => UUID.fromString(jid.as[String])
    }
  }
}

case class ProviderMap(root: GestaltResourceInstance, idx: Int = 0) {

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
  lazy val envConfig: Option[ProviderEnv] = ProviderMap.parseEnvironment(root)

  /**
   * These are the linked providers that this provider depends on.
   */
  lazy val dependencies: Seq[ProviderMap] = {
    val pids = linkedProviders.map(_.id)
    val providers = ResourceFactory.findAllIn(pids)

    if (pids.size != providers.size) {
      // TODO: throw exception, report missing providers.
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

  def parseEnvironment(r: GestaltResourceInstance): Option[ProviderEnv] = {
    val props = r.properties getOrElse {
      throw new IllegalArgumentException(s"Resource '${r.id}' does not have properties.")
    }

    val config = props.get("config") getOrElse {
      throw new IllegalArgumentException("Could not find 'properties.config'")
    }

    Try(Json.parse(config).as[JsObject]) match {
      case Failure(e) =>
        throw new IllegalArgumentException("Could not parse 'properties.config': " + e.getMessage)
      case Success(configjs) => {
        Js.find(configjs, "/env") flatMap { ev => Js.parse[ProviderEnv](ev).toOption }
      }
    }
  }
}

