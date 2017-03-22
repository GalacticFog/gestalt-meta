package com.galacticfog.gestalt.meta.api


import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import play.api.Logger

import com.galacticfog.gestalt.meta.api.Resource.typeOrElse
import java.util.UUID


class ResourcePath(val path: String) {
  
  private[this] val log = Logger(this.getClass)
  
  def this(fqon: String, path: String) = this("%s/%s".format(fqon, path))
  
  val components = Resource.components(path)
  val fqon = components(0)
  val isOrg = components.size == 1
  val isList = components.size % 2 == 0
  val isFirstLevelList = components.size == 2
  val isFirstLevelResource = components.size == 3
  val isSecondLevelList = components.size == 4
  val isSecondLevelResource = components.size == 5
  
  val info = {
    if (isList) Resource.mapListPathData(path) else Resource.mapPathData(path)
  }
  
  // Validate the parent resource type if it exists.
//  if (info.get(Resource.ParentType).isDefined) Resource.typeOrElse(info(Resource.ParentType))

  val parentTypeId = info.get(Resource.ParentType) map { typeOrElse(_) }
  val parentId     = info.get(Resource.ParentId) map { UUID.fromString(_) }
  
  // If path has only one component, assume it's the FQON and we're pointing to an Org.
  val targetTypeId = {
    if (components.size == 1) ResourceIds.Org 
    else Resource.typeOrElse(info(Resource.TargetType)) 
  }
  val targetId = info.get(Resource.TargetId)  
}

import com.galacticfog.gestalt.data._
import play.api.libs.json._

case class LineageInfo(parent_types: Seq[UUID], child_types: Option[Seq[UUID]] = None)
object LineageInfo {
  implicit lazy val lineageInfoFormat = Json.format[LineageInfo]
}

object ResourcePath {
  
  def validate(path: String) = {
    val rp = new ResourcePath(path)
    
    rp match {
      case a if a.isOrg => {
        ResourceFactory.findByPropertyValue(ResourceIds.Org, "fqon", a.fqon).isDefined
      }
      case b if b.isFirstLevelList => {
        // lookup org and validate list type as child
        val validorg = ResourceFactory.findByPropertyValue(ResourceIds.Org, "fqon", b.fqon).isDefined
        if (validorg) {
          val orgtype = TypeFactory.findById(ResourceIds.Org)
        }
      }
      case c if c.isFirstLevelResource => {
        // lookup org and resource1
      }
      case d if d.isSecondLevelList => {
        // lookup org, resource1 and validate resource2 as child type
      }
      case e if e.isSecondLevelResource => {
        // lookup org, resource1 and resource 2
      }
      case _ => throw new RuntimeException(s"Could not determine path type. This is a bug.")
    }
    /*
     * Parent* is nothing if:
     * - path refers to an Org
     * - path refers to a first-level resource or first-level resource list
     * first-level list == 2
     * first-level resource == 3
     * second-level list == 4 (org/r1/id/r2)
     * second-level resource == 5 (/{fqon}/workspaces/{id}/environments/{id})
     */
  }
  
}
