package com.galacticfog.gestalt.meta.api


import java.util.UUID
import play.api.libs.json._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk._
import scala.util.Try


object Resource {

  val Fqon       = "fqon"
  val TargetType = "targetType"
  val TargetId   = "targetId"
  val ParentType = "parentType"
  val ParentId   = "parentId"
  
  
  def findByPath(path: String): Option[GestaltResourceInstance] = {
    findResource(mapPathData(path))
  }
    
  def findResource(info: Map[String,String]) = {
    
    info.size match {
      case 1 => findFqon(info)
      case 3 => find(info)
      case 5 => findChild(info)
      case _ => throw new RuntimeException("Invalid ResourceInfo map.")
    }
  }
  
  def find(info: Map[String,String]) = {
    val targetId = UUID.fromString(info(TargetId))
    val targetTypeName = info(TargetType)
    
    if (targetTypeName == "resources") ResourceFactory.findById(targetId)
    else ResourceFactory.findById(typeOrElse(targetTypeName), targetId)
  }
  
  def findFqon(info: Map[String,String]) = {
    ResourceFactory.findByPropertyValue(ResourceIds.Org, Fqon, info(Fqon))
  }
  
  def findChild(info: Map[String,String]) = {
    
    val parentId = UUID.fromString(info(ParentId))
    val targetId = UUID.fromString(info(TargetId))
    
    val parentTypeId = typeOrElse(info(ParentType))
    val targetTypeId = typeOrElse(info(TargetType))
    
    ResourceFactory.findChildOfType(targetTypeId, parentId, targetId)
  }
  
  protected[api] def mapPathData(path: String): Map[String,String] = {
    
    val cmps = { path.trim
        .stripPrefix("/")
        .stripSuffix("/")
        .split("/").toList filter { _.trim != "" }
    }

    if (cmps.isEmpty) { 
      throw illegal("Path does not identify a Resource. found: Empty String")
    } else if (cmps.size % 2 == 0) {
      throw illegal(s"Path does not identify a Resource. found: '/$path'")
    }
    
    cmps.size match {
      case 1 => Map(Fqon -> cmps(0))
      case 3 => Map(Fqon -> cmps(0), TargetType -> cmps(1), TargetId -> cmps(2))
      case 5 => Map(Fqon -> cmps(0), ParentType -> cmps(1), ParentId -> cmps(2), TargetType -> cmps(3), TargetId -> cmps(4))
      case _ => throw illegal("Invalid path.")
    }
  }  
  
  protected[api] def typeOrElse(typeName: String) = {
    resourceUUID(typeName) getOrElse {
      throw new BadRequestException(s"Invalid Resource Type name: '${typeName}'")
    }
  }  
  
  protected[api] def illegal(message: String) = new IllegalArgumentException(message)
  
}