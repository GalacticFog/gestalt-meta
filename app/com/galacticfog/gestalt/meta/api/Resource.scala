package com.galacticfog.gestalt.meta.api


import java.util.UUID
import play.api.libs.json._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk._
import scala.util.Try
import play.api.Logger
import com.galacticfog.gestalt.data.ResourceFactory.findByPropertyValue

import scala.language.postfixOps

object Resource {

  private[this] val log = Logger(this.getClass)
  
  val Fqon       = "fqon"
  val TargetType = "targetType"
  val TargetId   = "targetId"
  val ParentType = "parentType"
  val ParentId   = "parentId"
  
  private[api] val polymorphic = Seq("providers", "rules")
  
  
  /**
   * Find a list of same-typed resources by fully-qualified path
   */
  def listFromPath(p: String): List[GestaltResourceInstance] = {
    val info = mapListPathData(p)
    val f = info.size match {
      case 2 => findFirstLevelList _
      case 4 => findSecondLevelList _
      case _ => throw illegal(s"Invalid path. found '$p'")
    }
    f(info)
  }
  
  /**
   * Find  a single resource by path
   */
  def fromPath(p: String): Option[GestaltResourceInstance] = {
    log.debug(s"fromPath($p)")
    
    // Normalize the path
    val cmps = components(p)
    
    // Get target type-name from path (REST name)
    val typeName = components(p) size match {
      case 1 => "orgs"
      case _ => cmps(cmps.size - 2)
    }
    
    if (isPolymorphic(typeName)) {
      log.debug(s"Found polymorphic type s'$typeName'")
      
      val resourceId = UUID.fromString(cmps.last)
      lookupResource(typeName, resourceId)
    } else findByPath(p)
  }
  
  /**
   * Indicates whether or not the path is a list selector.
   */
  def isList(path: String): Boolean = {
    val cs = components(path)
    if (cs.size % 2 == 0) true else false
  }  
  

  private[api] def findByPath(path: String): Option[GestaltResourceInstance] = {
    findResource(mapPathData(path))
  }    
  
  /**
   * Determine if a given type is a sub-type of another.
   */
  private[api] def isSubTypeOf(superType: UUID, subType: UUID) = {
    log.debug(s"isSubTypeOf(super = $superType, sub = $subType)")
    
    ResourceFactory.findTypesWithVariance(CoVariant(superType)) exists {
      subType == _.id  
    }
  }  
  
  private[api] def isPolymorphic(typeRestName: String): Boolean = {
    polymorphic.contains(typeRestName)
  }
  
  /**
   * Parse a URI path to a List. Ensure it is non-empty.
   */
  private[api] def components(path: String) = { 
    val cs = path.trim
      .stripPrefix("/")
      .stripSuffix("/")
      .split("/").toList filter { _.trim != "" }
    
    if (cs.isEmpty) {
      throw new BadRequestException(s"Path must not beEmpty")
    } else cs
  }
  
  
  private[api] def lookupResource(typeName: String, resourceId: UUID) = {
    ResourceFactory.findById(resourceId) collect { 
      case res if isValidSubType(typeName, res.typeId) => res 
    }  
  }
  
  private[api] def isValidSubType(typeName: String, typeId: UUID): Boolean = {
    log.debug(s"isValidSubType(typeName = $typeName, typeId = $typeId)")
    typeName match {
      case "providers" => isSubTypeOf(ResourceIds.Provider, typeId)
      case "rules" => isSubTypeOf(ResourceIds.Rule, typeId)
      case _ => {
        log.error("isValidSubType -> MatchError for type: " + typeName)
        throw new RuntimeException(s"Type-test for base-type '${typeName}' not implemented.")
      }
    }  
  }

  protected[api] def findResource(info: Map[String,String]): Option[GestaltResourceInstance] = {
    info.size match {
      case 1 => findFqon(info)
      case 3 => find(info)
      case 5 => findChild(info)
      case _ => throw new RuntimeException("Invalid ResourceInfo map.")
    }
  }
  
  /**
   * Find an Org by FQON
   */
  protected[api] def findFqon(info: Map[String,String]) = {
    ResourceFactory.findByPropertyValue(ResourceIds.Org, Fqon, info(Fqon))
  }
  
  /**
   * Find a Resource by Type and ID
   */
  protected[api] def find(info: Map[String,String]) = {
    val targetId = UUID.fromString(info(TargetId))
    val targetTypeName = info(TargetType)
    
    if (targetTypeName == "resources") ResourceFactory.findById(targetId)
    else ResourceFactory.findById(typeOrElse(targetTypeName), targetId)
  }
  
  protected[api] def findFirstLevelList(info: Map[String,String]) = {
    val org = orgOrElse(info(Fqon))
    val targetTypeId = typeOrElse(info(TargetType))
    
    ResourceFactory.findAll(targetTypeId, org)
  }
  
  protected[api] def findSecondLevelList(info: Map[String,String]) = {
    val org = orgOrElse(info(Fqon))
    
    val parentId = UUID.fromString(info(ParentId))
    
    val parentType   = typeOrElse(info(ParentType))
    val targetTypeId = typeOrElse(info(TargetType))
    
    //
    // If the function takes an Account parameter, we can do the Authorization here and
    // return a filtered list.
    //
    
    ResourceFactory.findChildrenOfType(org, parentId, targetTypeId)
  }
  
  /**
   * Find a child Resource by Type and ID
   */
  protected[api] def findChild(info: Map[String,String]) = {
    
    val parentId = UUID.fromString(info(ParentId))
    val targetId = UUID.fromString(info(TargetId))
    
    val parentTypeId = typeOrElse(info(ParentType))
    val targetTypeId = typeOrElse(info(TargetType))
    
    ResourceFactory.findChildOfType(targetTypeId, parentId, targetId)
  }
  
  def getFqon(path: String) = {
    components(path)(0)
  }
  
  def isTopLevel(path: String) = {
    components(path).size == 2
  }
  
  /**
   * Parse a resource URI into a Map naming the path components.
   */
  def mapPathData(path: String): Map[String,String] = {
    log.debug(s"mapPathData($path)")
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
      case 3 => Map(Fqon -> cmps(0), TargetType -> cmps(1), TargetId -> validUUID(cmps(2)))
      case 5 => Map(Fqon -> cmps(0), ParentType -> cmps(1), ParentId -> validUUID(cmps(2)), TargetType -> cmps(3), TargetId -> validUUID(cmps(4)))
      case _ => throw illegal("Invalid path.")
    }
  }
  
  protected[api] def validUUID(id: String) = {
    if (parseUUID(id).isDefined) id
    else {
      throw new BadRequestException(s"'$id' is not a valid v4 UUID.")
    }
  }
  
  def mapListPathData(path: String): Map[String,String] = {
    
    val cmps = components(path)

    if (!(cmps.size > 1 && cmps.size % 2 == 0)) {
      throw illegal(s"Path does not identify a Resource. found: '/$path'")
    }
    cmps.size match {
      case 2 => Map(Fqon -> cmps(0), TargetType -> cmps(1))
      case 4 => Map(Fqon -> cmps(0), ParentType -> cmps(1), ParentId -> validUUID(cmps(2)), TargetType -> cmps(3))
      case _ => throw illegal("Invalid path.")
    }
  }
  
  protected[api] def typeOrElse(typeName: String) = {
    resourceUUID(typeName) getOrElse {
      throw new BadRequestException(s"Invalid Resource Type name: '${typeName}'")
    }
  }  
  
  protected[api] def orgOrElse(fqon: String) = {
    findByPropertyValue(ResourceIds.Org, "fqon", fqon) map { _.id } getOrElse {
      throw new BadRequestException(s"Invalid FQON: '$fqon'")
    }
  }
  
  protected[api] def illegal(message: String) = new IllegalArgumentException(message)
  
}