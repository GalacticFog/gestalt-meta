package com.galacticfog.gestalt.meta.api.output



import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import java.util.UUID
import play.api.libs.json._
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
 

object OutputDatatypeHandlers {

  def default(property: GestaltTypeProperty, value: String) = JsString(value)

    
  def int(property: GestaltTypeProperty, value: String) = JsNumber(value.toInt)
  
  def intList(property: GestaltTypeProperty, value: String): JsArray = {
    JsArray(value.split(",") map { n => JsNumber(n.trim.toInt) })
  }
  
  def boolean(property: GestaltTypeProperty, value: String) = JsBoolean(value.toBoolean)

  def dateTime(property: GestaltTypeProperty, value: String) = JsString {
    ISODateTimeFormat.dateTime.print(new DateTime(value.trim))
  }
  
  def json(property: GestaltTypeProperty, value: String) = Json.parse( value )
  
  def renderUUID(property: GestaltTypeProperty, value: String) = JsString(value)
  /*
   * RESOURCE::UUID
   */
  def renderResourceUUID(property: GestaltTypeProperty, value: String) = JsString(value)  
  
  /**
   * Render resource::uuid::name
   */
  def resourceUUIDName(property: GestaltTypeProperty, value: String) = {
    val typeId = safeGetTypeId( property )
    val instanceId = UUID.fromString(value)
    val data = lookup(typeId)(typeId, instanceId) getOrElse {
      illegal(s"No resource of type '${typeId}' with ID '${instanceId} was found.")
    }
    JsString(data.asInstanceOf[GestaltResourceInstance].name)
  }
  
  def resourceUUIDLink(property: GestaltTypeProperty, value: String): JsValue = {
    /* Get resource referenced by instanceId */
    Json.toJson { 
      linkFromId( safeGetTypeId(property), UUID.fromString( value ) )
    }
  }
  
  /**
   * Render resource::uuid::link::list
   */
  def resourceUUIDLinkList(property: GestaltTypeProperty, value: String) = {
    println("Rendering UUID LinkList: " + value)
    val typeId = safeGetTypeId( property )
    
    // Convert value string to array of UUIDs
    val ids = value.split(",") map { v => v.trim }
    
    // Convert UUID array to array of ResourceLink
    val links = ids map { id => 
      Json.toJson( linkFromId( typeId, UUID.fromString( id ) ) ) 
    }
    JsArray( links )
  }
  
  

  
  
//  def renderReferenceUUID(property: GestaltResourceTypeProperty, value: String) = {
//    //val (tableName, instanceId) = toTuple2( s )
//    
//    val tableName = property.referenceValueType getOrElse {
//      illegal(s"ReferenceValueType not specified.")
//    }
//    val instanceId = UUID.fromString( value )
//    val table = SQLSyntax.createUnsafely( tableName )
//    
//    val data = sql"""
//      SELECT id FROM ${table}
//      WHERE id = ${instanceId}
//      """.map { rs =>
//        rs.any("id").asInstanceOf[UUID]
//      }.single.apply getOrElse {
//        illegal(s"'${tableName}' with ID '${instanceId}' not found.")
//      }
//      
//      // Return the UUID as a String
//      JsString( data.toString )
//  }
//  
//
//  
//  def renderReferenceUUIDProperty(property: GestaltResourceTypeProperty, value: String) = {
//    val tableName = property.referenceValueType getOrElse {
//      illegal(s"ReferenceValueType not specified.")
//    }
//    val instanceId = UUID.fromString( value )
//    val table = SQLSyntax.createUnsafely( tableName )
//    val data = sql"""
//      SELECT name FROM ${table}
//      WHERE id = ${instanceId}
//      """.map { rs =>
//        rs.any("name")
//      }.single.apply getOrElse {
//        illegal(s"'${tableName}' with ID '${instanceId}' not found.")
//      }
//      
//      // Return the UUID as a String
//      JsString( data.toString )
//  }  
//  
//  
//  def renderReferenceUUIDLink(property: GestaltResourceTypeProperty, value: String) = {    
//    val typeName = property.referenceValueType getOrElse {
//      illegal(s"ReferenceValueType not specified.")
//    } 
//    Json.toJson( refLinkFromId( typeName, UUID.fromString( value ) ) )
//  }  
  
  private def lookup(typeId: UUID): (UUID,UUID) => Option[Any] = {
    ReferenceFactory.isReferenceType( typeId ) match {
      case true  => ReferenceFactory.findById
      case false => ResourceFactory.findById
    }
  }
  
  /**
   * Get the resource_type_id of the resource a property is referring to.
   */
  private[output] def safeGetTypeId(p: GestaltTypeProperty) = p.refersTo getOrElse {
    illegal(s"Property value 'refersTo' must be specified - None found.")
  }
  
  /** Convert a resource type instance to a ResourceLink */
  private[output] def linkFromId(typeId: UUID, id: UUID) = {
    val target = ResourceFactory.findById(typeId, id) getOrElse {
      illegal(s"No resource of type '${ResourceType.name(typeId)}' with ID '${id} was found.")
    }
    ResourceLink(target.typeId, target.id, Some(target.name), Option(toHref(typeId, id)))
  }
  
  /** TODO: Provide a real implementation. */
  private[output] def toHref(typeId: UUID, id: UUID): String = {
    "http://dummy_href/%s".format(id.toString)
  }  
  
//  FOR ORG - need to build parent/child list dynamically from closure table
//  
//  NOW HOW do i handle different rendering strategies for different resource types?
  
  
  
  
}