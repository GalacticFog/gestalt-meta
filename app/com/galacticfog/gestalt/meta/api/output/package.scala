package com.galacticfog.gestalt.meta.api

import com.galacticfog.gestalt.data.models._

import play.api.libs.json._
import java.util.UUID

package object output {
  
  implicit lazy val resourceLinkFormat = Json.format[com.galacticfog.gestalt.data.models.ResourceLink]
  implicit lazy val gestaltResourceOwnerLinkFormat = Json.format[ResourceOwnerLink]

  implicit lazy val gestaltReferenceDataFormat = Json.format[GestaltReferenceData]
  implicit lazy val gestaltTypePropertyFormat = Json.format[GestaltTypeProperty]
  implicit lazy val gestaltResourceInstanceFormat = Json.format[GestaltResourceInstance]
  implicit lazy val gestaltResourceType = Json.format[GestaltResourceType]
  implicit lazy val gestaltTaskFormat = Json.format[GestaltTask]
  
  /* Input */
  implicit lazy val gestaltResourceInputFormat = Json.format[GestaltResourceInput]
  implicit lazy val gestaltTypePropertyInputFormat = Json.format[GestaltTypePropertyInput]
  implicit lazy val gestaltResourceTypeInputFormat = Json.format[GestaltResourceTypeInput]
  
  /* Output */
  implicit lazy val gestaltInstanceOutputFormat = Json.format[GestaltResourceOutput]
  implicit lazy val gestaltAttributeOutputFormat = Json.format[GestaltAttributeFormat]
  implicit lazy val gestaltResourceOutputFormat = Json.format[GestaltResourceFormat]
  implicit lazy val gestaltTypeOutputFormat = Json.format[GestaltResourceTypeOutput]
  implicit lazy val gestaltPropertyOutputFormat = Json.format[GestaltTypePropertyOutput]
  
  
  def toLink(typeId: UUID, id: UUID, name: Option[String]) = {
    ResourceLink(typeId, id.toString, name, Some(toHref( typeId, id )))
  }
  
  def toHref(typeId: UUID, id: UUID) = {
    val typename = resourceRestName(typeId) getOrElse { "resources" }
    "/%s/%s".format(typename, id.toString)
  }  
}