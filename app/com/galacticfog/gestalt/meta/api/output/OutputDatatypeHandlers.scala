package com.galacticfog.gestalt.meta.api.output


import scala.math.BigDecimal.int2bigDecimal

import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

import com.galacticfog.gestalt.data.ReferenceFactory
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.ResourceType
import com.galacticfog.gestalt.data.illegal
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.sdk.ResourceLink
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.errors._

import play.api.libs.json.JsArray
import play.api.libs.json.JsBoolean
import play.api.libs.json.JsNumber
import play.api.libs.json.JsString
import play.api.libs.json.JsValue
import play.api.libs.json.Json

import java.util.UUID


object OutputDatatypeHandlers {
  
  def default(property: GestaltTypeProperty, value: String) = JsString(value)
  
  def int(property: GestaltTypeProperty, value: String) = JsNumber(value.toInt)
  

  
  def boolean(property: GestaltTypeProperty, value: String) = JsBoolean(value.toBoolean)
  
  def dateTime(property: GestaltTypeProperty, value: String) = JsString {
    ISODateTimeFormat.dateTime.print(new DateTime(value.trim))
  }
  
  def json(property: GestaltTypeProperty, value: String) = Json.parse( value )
  
  //def jsonList((property: GestaltTypeProperty, value: String) = Json.parse( value ))
  
  def renderUUID(property: GestaltTypeProperty, value: String) = JsString(value)
  
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
    if (ReferenceFactory.isReferenceType(typeId)) 
      JsString(data.asInstanceOf[GestaltReferenceData].name)
    else JsString(data.asInstanceOf[GestaltResourceInstance].name)
  }
  
  
  def resourceUUIDLink(property: GestaltTypeProperty, value: String): JsValue = {
    val baseUri = None //"CHANGEMEIN::resourceUUIDLink"
    /* Get resource referenced by instanceId */
    Json.toJson { 
      linkFromId( safeGetTypeId(property), UUID.fromString( value ), baseUri )
    }
  }
  
  
  /**
   * Render string::list
   */
  def stringList(property: GestaltTypeProperty, value: String) = {
    println(s"OutputDatatypeHandlers::stringList([property], $value)")
    // Remove surrounding square brackets if provided and split int JsArray
    //val normal = value.replaceAll("\\[", "").replaceAll("\\]", "").trim
    
    val items = normalArray(unquote(value)).split(",")
    JsArray { items.map( s => JsString( s.trim )) }
  }
  
  def intList(property: GestaltTypeProperty, value: String): JsArray = {
    //val normal = value.replaceAll("\\[", "").replaceAll("\\]", "").trim
    JsArray(normalArray(value).split(",") map { n => JsNumber(n.trim.toInt) })
  }
  
  def booleanList(property: GestaltTypeProperty, value: String): JsArray = {
    //val normal = value.replaceAll("\\[", "").replaceAll("\\]", "").trim
    JsArray(normalArray(value).split(",") map { b => JsBoolean(b.trim.toBoolean)})  
  }  
  
  def normalArray(value: String) = value.replaceAll("\\[", "").replaceAll("\\]", "").trim
  def unquote(s: String) = s.replaceAll("\"", "")
  
  
  /**
   * Render resource::uuid::link::list
   */
  def resourceUUIDLinkList(property: GestaltTypeProperty, value: String) = {
    val baseUri = None
    val typeId = safeGetTypeId( property )
    
    // Convert value string to array of UUIDs
    val ids = normalArray(unquote(value)).split(",") map { v => v.trim }
    
    // Convert UUID array to array of ResourceLink
    val links = ids map { id => 
      Json.toJson( linkFromId( typeId, UUID.fromString( id ), baseUri ) ) 
    }
    JsArray( links )
  }

//  def resourceUUIDList(property: GestaltTypeProperty, value: String) = {
//    val baseUri = None
//    val typeId = safeGetTypeId( property )
//    
//    // Convert value string to array of UUIDs
//    val ids = normalArray(value).split(",") map { v => v.trim }
//    
//    // Convert UUID array to array of ResourceLink
//    val links = ids map { id => 
//      Json.toJson( linkFromId( typeId, UUID.fromString( id ), baseUri ) ) 
//    }
//    JsArray( links )
//  }  
  
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
  private[output] def linkFromId(typeId: UUID, id: UUID, baseUri: Option[String] = None) = {
    
    import  com.galacticfog.gestalt.data.models.ResourceLike
    /*
     * 
     * ResourceType.name(typeId) is failing when the resource type is a reference-type.
     * 
     */

    val target = ResourceFactory.findById(typeId, id) getOrElse {
      throw new BadRequestException(s"No resource of type '${ResourceType.name(typeId)}' with ID '${id} was found.")
    }

    ResourceLink(target.typeId, target.id, Some(target.name), 
        href = Option(toHref(typeId, id, target.orgId, baseUri)))
  }
  
//  def linkFromReference(r: GestaltReferenceData) = {
//        ResourceLink(r.typeId, target.id, Some(target.name), 
//        href = Option(toHref(typeId, id, target.orgId, baseUri)))
//  }
  /** TODO: Provide a real implementation. */
//  private[output] def toHref(typeId: UUID, id: UUID): String = {
//    "http://dummy_href/%s".format(id.toString)
//  }  

  
}