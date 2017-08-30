package com.galacticfog.gestalt.meta.api

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data.ResourceFactory

import play.api.libs.json._
import java.util.UUID

package object output {
  
  implicit lazy val gestaltReferenceDataFormat = Json.format[GestaltReferenceData]
  implicit lazy val gestaltTypePropertyFormat = Json.format[GestaltTypeProperty]
  implicit lazy val gestaltResourceInstanceFormat = Json.format[GestaltResourceInstance]
  implicit lazy val gestaltResourceType = Json.format[GestaltResourceType]
  implicit lazy val gestaltTaskFormat = Json.format[GestaltTask]
  
  /* JSON PATCH */
//  implicit lazy val patchOpFormat = Json.format[PatchOp]
//  implicit lazy val patchDocFormat = Json.format[PatchDocument]  
  
  def toLink(res: GestaltResourceInstance, baseUri: Option[String]): ResourceLink = {
    ResourceLink(res.typeId, res.id.toString, Some(res.name), Some(toHref(res.typeId, res.id, res.orgId, baseUri )))
  }
  
  def toLink(
      typeId: UUID, id: UUID, orgId: UUID, name: Option[String], 
      baseUri: Option[String] = None, props: Option[Map[String,String]] = None) = {
    
    val href = toHref( typeId, id, orgId, baseUri )
    ResourceLink(typeId, id.toString, name, Some(toHref( typeId, id, orgId, baseUri )), properties = props)
  }
  
  def toOwnerLink(typeId: UUID, id: UUID, orgId: UUID, name: Option[String], baseUri: Option[String] = None) = {
    ResourceOwnerLink(typeId, id.toString, name, Some(toHref(typeId, id, orgId, baseUri)))
  }
  
  def toHref_old(typeId: UUID, id: UUID, orgId: UUID, baseUri: Option[String] = None) = {
    val typename = resourceRestName(typeId) getOrElse { "resources" }
    "/%s/%s".format(typename, id.toString)

    val base = if (baseUri.isDefined) baseUri.get else ""
    if (typeId == ResourceIds.Org) "%s/orgs/%s".format(base, id)
    else "%s/orgs/%s/%s/%s".format(base, orgId, typename, id)
  }
  
  def toHref(typeId: UUID, id: UUID, orgId: UUID, baseUri: Option[String] = None) = {
    val typename = resourceRestName(typeId) getOrElse { "resources" }
    "/%s/%s".format(typename, id.toString)

    ResourceFactory.findById(orgId) map { org =>
      val base = if (baseUri.isDefined) baseUri.get else ""
      val fqon = org.properties.get("fqon")

      if (typeId == ResourceIds.Org) {
        val suborg = ResourceFactory.findById(id).get
        "%s/%s".format(base, suborg.properties.get("fqon"))
      } else "%s/%s/%s/%s".format(base, fqon, typename, id)
    } getOrElse toHref_old(typeId, id, orgId, baseUri)
  }  
  
  
}