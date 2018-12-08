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

  //
  // Simple Types
  //
  def renderDefault   (property: GestaltTypeProperty, value: String) = JsString(value)
  def renderFloat     (property: GestaltTypeProperty, value: String) = JsNumber(value.toDouble)
  def renderInt       (property: GestaltTypeProperty, value: String) = JsNumber(value.toInt)
  def renderBoolean   (property: GestaltTypeProperty, value: String) = JsBoolean(value.toBoolean)
  def renderJson      (property: GestaltTypeProperty, value: String) = {
    Json.parse( value )
  }
  def renderUUID      (property: GestaltTypeProperty, value: String) = JsString(value)
  def renderDateTime  (property: GestaltTypeProperty, value: String) = JsString {
    ISODateTimeFormat.dateTime.print(new DateTime(value.trim))
  }
  
  
  //
  // List Types
  //
  def renderStringList(property: GestaltTypeProperty, value: String) = {
    jsonarray(value)
  }
  
  def renderFloatList(property: GestaltTypeProperty, value: String): JsArray = {
    jsonarray(value)
  }
  
  def renderIntList(property: GestaltTypeProperty, value: String): JsArray = {
    jsonarray(value)
  }
  
  def renderBooleanList(property: GestaltTypeProperty, value: String): JsArray = {
    jsonarray(value)
  }
  
  def renderJsonList(property: GestaltTypeProperty, value: String): JsArray = {
    jsonarray(value)
  }
  

  //
  // REFERENCE types
  //
  
  /**
   * Render datatype resource::uuid
   */
  def renderResourceUUID(property: GestaltTypeProperty, value: String) = JsString(value)  
  
  
  /**
   * Render datatype resource::uuid::link
   */
  def resourceUUIDLink(property: GestaltTypeProperty, value: String): JsValue = {
    val baseUri = None //"CHANGEMEIN::resourceUUIDLink"
    /* Get resource referenced by instanceId */
    Json.toJson { 
      linkFromId( safeGetTypeId(property), UUID.fromString( value ), baseUri )
    }
  }
  
  /**
   * Render resource::uuid::link::list
   */
  def resourceUUIDLinkList(property: GestaltTypeProperty, value: String) = {
    if (value.isEmpty) Json.arr()
    else {
      val baseUri = None
      val typeId = safeGetTypeId(property)
      val ids = normalArray(unquote(value)).split(",") map { v => v.trim }
      val links = ids map { id =>
        Json.toJson( linkFromId( typeId, UUID.fromString( id ), baseUri ) ) 
      }
      JsArray( links )
    }
  }
  
  /**
   * Render resource::uuid::name
   */
  def resourceUUIDName(property: GestaltTypeProperty, value: String) = {
    val typeId = safeGetTypeId( property )
    val instanceId = UUID.fromString(value)
    
    val data = lookupFn(typeId)(typeId, instanceId) getOrElse {
      illegal(s"No resource of type '${typeId}' with ID '${instanceId} was found.")
    }

    println(data)
    if (ReferenceFactory.isReferenceType(typeId)) 
      JsString(data.asInstanceOf[GestaltReferenceData].name)
    else JsString(data.asInstanceOf[GestaltResourceInstance].name)
  }  
  
  
  /**
   * Get the 'find' function for a resource based on its type.
   */
  private def lookupFn(typeId: UUID): (UUID,UUID) => Option[Any] = {
    ReferenceFactory.isReferenceType( typeId ) match {
      case true  => ReferenceFactory.findById
      case false => ResourceFactory.findById
    }
  }
  
  /**
   * Get the resource_type_id of the resource a property is referring to.
   */
  private[output] def safeGetTypeId(p: GestaltTypeProperty) = p.refersTo getOrElse {
    throw new BadRequestException(s"Property value 'refersTo' must be specified - None found.")
  }
  
  /** 
   *  Convert a resource type instance to a ResourceLink 
   */
  private[output] def linkFromId(typeId: UUID, id: UUID, baseUri: Option[String] = None) = {
    
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
  
  private def jsonarray(value: String) = Json.parse(value).as[JsArray] 
  private def normalArray(value: String) = value.replaceAll("\\[", "").replaceAll("\\]", "").trim
  private def unquote(s: String) = s.replaceAll("\"", "")  
  
}