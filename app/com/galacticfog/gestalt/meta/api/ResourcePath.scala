package com.galacticfog.gestalt.meta.api


import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import play.api.Logger


class ResourcePath(val path: String) {
  
  private[this] val log = Logger(this.getClass)
  
  def this(fqon: String, path: String) = this("%s/%s".format(fqon, path))
  
  val components = Resource.components(path)
  val fqon = components(0)
  val isList = components.size % 2 == 0
  val info = {
    if (isList) Resource.mapListPathData(path) else Resource.mapPathData(path)
  }
  
  // Validate the parent resource type if it exists.
  if (info.get(Resource.ParentType).isDefined) Resource.typeOrElse(info(Resource.ParentType))
  
  val targetTypeId = {
    if (components.size == 1) ResourceIds.Org 
    else Resource.typeOrElse(info(Resource.TargetType)) 
  }
  val targetId = info.get(Resource.TargetId)
}