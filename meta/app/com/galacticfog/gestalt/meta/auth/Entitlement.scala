package com.galacticfog.gestalt.meta.auth


import java.util.UUID

import scala.{Either,Left,Right}
import scala.Right

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds,ResourceStates}
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink

import play.api.libs.json.JsArray
import play.api.libs.json.Json
import play.api.libs.json._

case class Entitlement(
    id: UUID,
    org: UUID,
    name: String,
    description: Option[String] = None,  
    properties: EntitlementProps,
    variables: Option[Hstore] = None,
    tags: Option[List[String]] = None) {
  
  def withIdentities(ids: Seq[UUID]) = {
    val props = this.properties.copy(identities = Some(ids))
    this.copy(properties = props)
  }
}

object Entitlement {
  
  /**
   * Convert a GestaltResourceInstance to an Entitlement object.
   */
  def make(r: GestaltResourceInstance) = {
    Entitlement(
      id = r.id,
      org = r.orgId,
      name = r.name,
      description = r.description,
      properties = EntitlementProps.make(r),
      variables = r.variables,
      tags = r.tags
    )
  }
  
  /**
   * Convert an Entitlement object to a GestaltResourceInstance.
   */
  def toGestalt(creator: UUID, ent: Entitlement) = {
    
    GestaltResourceInstance(
      id = ent.id,
      typeId = ResourceIds.Entitlement,
      orgId = ent.org, // this needs to be org from URI
      owner = ResourceOwnerLink(ResourceIds.User, creator),
      name = ent.name,
      description = ent.description,
      state = ResourceState.id(ResourceStates.Active),
      properties = Option(EntitlementProps.toMap(ent.properties)) 
    ) 
  }  
  
  implicit lazy val entitlementPropsFormat = Json.format[EntitlementProps]
  implicit lazy val entitlementFormat = Json.format[Entitlement]
}

object EntitlementProps {

  /**
   * Convert GestaltResourceInstance.properties to EntitlementProps object.
   */
  def make(r: GestaltResourceInstance): EntitlementProps = {
    val props = r.properties.get
    val action = props("action")
    val value = if (props.contains("value")) Some(props("value")) else None
    val identities = if (props.contains("identities")) Some(props("identities")) else None
    
    def parseIdentities(str: Option[String]): Option[Seq[UUID]] = { 
      str map { 
        Json.parse(_).as[JsArray].value map { s =>  
          UUID.fromString(s.as[String]) 
        }
      }
    }
    EntitlementProps(action, value, parseIdentities(identities))
  }
  
 /**
  * Convert EntitlementProps object to Map[String,String]
  */
  def toMap(props: EntitlementProps) = {
    val v = props.value map { v => Map("value" -> v) } getOrElse Map()
    val i = props.identities map { ids => 
      Map(
        "identities" -> Json.toJson(ids).as[JsArray].toString.replaceAll("\\\\", "")
      )
    } getOrElse Map()
    
    Map("action" -> props.action) ++ v ++ i
  }
}

case class EntitlementProps(
    action: String, 
    value: Option[String] = None,
    identities: Option[Seq[UUID]] = None,
    parent: Option[JsValue] = None) {

  /**
   * Validates that action name is valid for the resource type and that all identities given are valid.
   */
  def validate(): Either[String,EntitlementProps] = {
    if (identities.isEmpty) Right((this))
    else {
      val given = identities.get
      val found = ResourceFactory.findAllIn(given) map { _.id }
      val notFound = given.diff(found)
      
      if (notFound.isEmpty) Right((this))
      else Left(s"Invalid identities : [ ${notFound.mkString(",")} ]")
    }
  }
  
  def addIdentities(ids: UUID*): EntitlementProps = {
    this.copy(identities = identities map { ids ++ _ })
  }
  
  def removeIdentities(ids: UUID*): EntitlementProps = {
    val newids = (identities getOrElse Seq.empty) filter { !ids.contains(_) }
    this.copy(identities = if (newids.nonEmpty) Some(newids) else None)
  }
}

