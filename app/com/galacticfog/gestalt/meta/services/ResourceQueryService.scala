package com.galacticfog.gestalt.meta.services



import play.api.{Logger => log}

import com.galacticfog.gestalt.data._

import com.galacticfog.gestalt.meta.api.sdk._

import com.galacticfog.gestalt.data.models._

import java.util.UUID

import scala.util.{Try, Success, Failure}

import play.api.libs.json._

import com.galacticfog.gestalt.meta.api._

import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException

/**
 * 
 * This object will become useful as we add the other System Resource Types
 * and more flexible query types.
 * 
 */

object ResourceQueryService {
  
  //import JsonImports._
  
  //val TEMP_API_PREFIX = "https://gf.com/api/v1.1"
  
  /**
   * Find all resources of the given type.
   */
    
//  def findAll(typeId: UUID) = Try {
//    ???
//  }
  
//  def findAllWithOrgId(org: UUID, typeId: UUID) = Try {
//    prettyLinks {  
//      ResourceFactory.findAll(org, typeId) map {
//        toResourceLink( _ )
//      }
//    }
//  }
  
  
  def findById(org: UUID, typeId: UUID, id: UUID) = Try {
    log.debug(s"ResourceQueryService::findById($org, $typeId)")
    Output.renderInstance {
      ResourceFactory.findById(typeId, id) getOrElse {
        throw new com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException("Resource Not Found")
      }
    }
  }
  
  def pretty(r: GestaltResourceInstance) = {
    Json.prettyPrint( Json.toJson( r ) )
  }  
  
  def prettyLink(r: ResourceLink) = {
    Json.prettyPrint( Json.toJson( r ) )
  }
  
  def prettyLinks(rs: Seq[ResourceLink]) = {
    Json.prettyPrint( Json.toJson( rs ) )
  }  
  
//  def toResourceLink( instance: GestaltResourceInstance ): ResourceLink = {
//    ResourceLink( instance.typeId, instance.id, Some(instance.name), Some(toHref(instance, TEMP_API_PREFIX)) )
//  }
//  
  def toHref(r: GestaltResourceInstance, host: String): String = {
    "%s/%s/%s/%s".format("http://{host}", "{org}", "{resource}", r.id.toString)
  }  

  
//  /**
//   * Find a resource of the given type by ID.
//   */
//  def findById(resourceTypeId: UUID, id: UUID) = Try {
//    ResourceFactory.assertValidResourceType( resourceTypeId )
//    render[GestaltResourceInstance]( getOrFail( resourceTypeId, id ) )
//  }
//  
//  
//  
//  private def getOrFail(resourceTypeId: UUID, id: UUID) = {
//    ResourceFactory.findById(resourceTypeId, id) getOrElse {
//      rnf(s"${ResourceType.name(resourceTypeId)} with ID '${id}' not found.")
//    }    
//  }
//  
//  /**
//   * TODO: All these reference-type functions will go away in next patch when
//   * references are promoted to full resources.
//   */
//  
//  
//  def findAllReferenceType(referenceTypeName: String) = Try {
//    assertValidReferenceType( referenceTypeName )
//    prettyLinks( ReferenceFactory.findAllLinks(referenceTypeName, TEMP_API_PREFIX) )
//  }
//
//  
//  def findReferenceTypeById(referenceTypeName: String, id: UUID) = Try {
//    assertValidReferenceType( referenceTypeName )
//    ReferenceFactory.findById(referenceTypeName, id) match {
//      case Some( ref ) => prettyRef( ref )
//      case None => rnf(s"${referenceTypeName} with ID ${id} not found.")
//    }
//  }
//  
//  
//  def findReferenceTypeByName(referenceTypeName: String, name: String) = Try {
//    assertValidReferenceType( referenceTypeName )
//    ???
//  }  
//  
//  
//  def getExpandedBlueprint(id: UUID) = Try {
//    BlueprintFactory.findById( id ) getOrElse {
//      rnf(s"Blueprint ID '${id.toString}' not found.")
//    }
//  }
//  
//  private def prettyLinks(rs: Seq[ResourceLink]): String = {
//    Json.prettyPrint( Json.toJson( rs ))
//  }  
//  
//
//  private implicit def ref2json(ref: ReferenceData) = Json.toJson( ref )
//  
//  private def prettyRefs(refs: Seq[ReferenceData]) = {
//    Json.prettyPrint( Json.toJson( refs ) )    
//  }
//  
//  private def prettyRef(ref: ReferenceData) = {
//    Json.prettyPrint( Json.toJson( ref ) )    
//  }
//  
//  private def assertValidReferenceType(typeName: String) = {
//    if (!References.exists(typeName)) {
//      throw new IllegalArgumentException(s"ResourceType '${typeName}' not found.")
//    }
//  }
  
}