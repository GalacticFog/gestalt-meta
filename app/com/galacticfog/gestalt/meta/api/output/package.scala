package com.galacticfog.gestalt.meta.api

import com.galacticfog.gestalt.data.ResourceIds
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
  
  
  /* JSON PATCH */
  implicit lazy val patchOpFormat = Json.format[PatchOp]
  implicit lazy val patchDocFormat = Json.format[PatchDocument]  
  
  
  def toLink(typeId: UUID, id: UUID, orgId: UUID, name: Option[String], baseUri: Option[String] = None) = {
    ResourceLink(typeId, id.toString, name, Some(toHref( typeId, id, orgId, baseUri )))
  }
  
  def toOwnerLink(typeId: UUID, id: UUID, orgId: UUID, name: Option[String], baseUri: Option[String] = None) = {
    ResourceOwnerLink(typeId, id.toString, name, Some(toHref(typeId, id, orgId, baseUri)))
  }
  
  def toHref(typeId: UUID, id: UUID, orgId: UUID, baseUri: Option[String] = None) = {
    val typename = resourceRestName(typeId) getOrElse { "resources" }
    "/%s/%s".format(typename, id.toString)

    val base = if (baseUri.isDefined) baseUri.get else ""
    if (typeId == ResourceIds.Org) "%s/orgs/%s".format(base, id)
    else "%s/orgs/%s/%s/%s".format(base, orgId, typename, id)
  }
  
  
  import com.galacticfog.gestalt.data._
  
  def getResource(typeId: UUID, id: UUID) = {
    val r = typeId match {
      case ResourceIds.ResourceType => TypeFactory.findById(id)
      case ResourceIds.TypeProperty => PropertyFactory.findById(id)
      case _                        => ResourceFactory.findById(id)
    }
  }
  
}