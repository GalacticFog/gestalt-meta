package com.galacticfog.gestalt.meta.api.output


import java.util.UUID

import scala.annotation.tailrec

import com.galacticfog.gestalt.data.DataType
import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.Properties
import com.galacticfog.gestalt.data.RequirementType
import com.galacticfog.gestalt.data.ResourceIds
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.data.TypeFactory
import com.galacticfog.gestalt.data.VisibilityType
import com.galacticfog.gestalt.data.illegal
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.models.GestaltResourceType
import com.galacticfog.gestalt.data.models.GestaltTypeProperty
import com.galacticfog.gestalt.data.models.ResourceOwnerLink
import com.galacticfog.gestalt.data.uuid

import OutputDatatypeHandlers.boolean
import OutputDatatypeHandlers.dateTime
import OutputDatatypeHandlers.default
import OutputDatatypeHandlers.int
import OutputDatatypeHandlers.intList
import OutputDatatypeHandlers.json
import OutputDatatypeHandlers.renderResourceUUID
import OutputDatatypeHandlers.resourceUUIDLink
import OutputDatatypeHandlers.resourceUUIDLinkList
import OutputDatatypeHandlers.resourceUUIDName
import play.api.libs.json.JsString
import play.api.libs.json.JsValue
import play.api.libs.json.Json


/*
 * 
 * TODO: Move these case classes into api.io
 * (or breakout into separate files in this package)
 * 
 */
case class GestaltResourceOutput(
  id: UUID,
  name: String,
  resource_type: JsValue,    // name
  resource_state: JsValue,   // name
  org: JsValue,               // link
  owner: JsValue,            // link 
  description: Option[String],
  created: JsValue,            //link
  modified: JsValue,           //link
  properties: Option[JsValue],
  variables: Option[JsValue],
  tags: Option[JsValue],
  auth: Option[JsValue])


case class GestaltResourceTypeOutput(
  id: UUID,
  name: String,
  extend: Option[JsValue], //link
  resource_type: JsValue,    // name
  resource_state: JsValue,   // name
  org: JsValue,               // link
  owner: JsValue,            // link 
  description: Option[String],
  created: JsValue,            //link
  modified: JsValue,           //link
  properties: Option[JsValue],
  variables: Option[JsValue],
  tags: Option[JsValue],
  auth: Option[JsValue],
  property_defs: Option[JsValue] = None)

  
case class GestaltTypePropertyOutput(
  id: UUID = uuid(),
  name: String,
  resource_type: JsValue, // name
  resource_state: JsValue, //name
  org: JsValue, //link
  owner: JsValue, //link
  description: Option[String],
  created: JsValue,
  modified: JsValue,
  
  applies_to: Option[JsValue],
  datatype: JsValue, //name
  default_value: Option[JsString],  
  is_sealed: Boolean,
  is_system: Boolean,
  requirement_type: JsValue, //name
  visibility_type: JsValue, //name
  refers_to: Option[JsValue], //link
  
  properties: Option[JsValue],
  variables: Option[JsValue],
  tags: Option[JsValue],
  auth: Option[JsValue]) 


object Output {

  def renderResourceTypeOutput(r: GestaltResourceType) = {
    val res = GestaltResourceTypeOutput(
      id = r.id,
      name = r.name,
      extend = jsonTypeName(r.extend),
      resource_type = jsonTypeName(Option(r.typeId)).get,
      resource_state = JsString(ResourceState.name(r.state)),

      org = jsonLink(ResourceIds.Org, r.orgId, None),
      owner = Json.toJson(r.owner),
      description = r.description,
      created = Json.toJson(r.created),
      modified = Json.toJson(r.modified),
      properties = jsonHstore(r.properties),
      variables = jsonHstore(r.variables),
      tags = jsonArray(r.tags),
      auth = jsonHstore(r.auth),
      property_defs = jsonTypePropertyLinks(r.id))
      
    //
    // TODO: .copy rendered properties!!!
    //
    Json.prettyPrint(Json.toJson(res))
  }
  
  
  def renderInstance(r: GestaltResourceInstance) = {
    val res = GestaltResourceOutput(
      id = r.id,
      org = jsonLink(ResourceIds.Org, r.orgId, None),
      resource_type = jsonTypeName(Option(r.typeId)).get,
      resource_state = JsString(ResourceState.name(r.state)),
      owner = Json.toJson(r.owner),
      name = r.name,
      description = r.description,
      created = Json.toJson(r.created),
      modified = Json.toJson(r.modified),
      properties = jsonHstore(r.properties),
      variables = jsonHstore(r.variables),
      tags = jsonArray(r.tags),
      auth = jsonHstore(r.auth) )
      
    //
    // TODO: .copy rendered properties!!!
    //
    val renderedProps = render(r)
    Json.prettyPrint(Json.toJson(res.copy(properties = renderedProps)))
  }
  
  
  def renderLink(r: GestaltResourceInstance) = toLink(r.typeId, r.id, Some(r.name))
  
  def renderLinks(rs: Seq[GestaltResourceInstance]) = {
    Json.prettyPrint(Json.toJson(rs map { r => toLink(r.typeId, r.id, Some(r.name)) }))
  }
  
  def renderTypeProperties(typeId: UUID) = {
    val ps = Properties.getTypeProperties(typeId) map { p => toPropertyOutput( p ) }
    Json.prettyPrint(Json.toJson( ps ))
  }

  
  def getTypePropertyLinks(typeId: UUID) = {
    Properties.getTypeProperties(typeId) map { p => toLink( p.typeId, p.id, Some(p.name) ) }
  }

  
  def renderTypePropertyOutput(p: GestaltTypeProperty) = {  
    val res = toPropertyOutput( p )
    //
    // TODO: .copy rendered properties!!!
    //
    Json.prettyPrint(Json.toJson(res))
  }

  
  def toPropertyOutput(p: GestaltTypeProperty) = {
    GestaltTypePropertyOutput(
      id = p.id,
      name = p.name,
      resource_type = jsonTypeName(Option(p.typeId)).get,
      resource_state = JsString(ResourceState.name(p.state)),
      org = jsonLink(ResourceIds.Org, p.orgId, None),
      owner = Json.toJson(p.owner),
      description = p.description,
      created = Json.toJson(p.created),
      modified = Json.toJson(p.modified),

      applies_to = jsonTypeName(Option(p.appliesTo)),
      datatype = JsString(DataType.name(p.datatype)), //name
      default_value = jsStringOpt(p.defaultValue),
      is_sealed = p.isSealed,
      is_system = p.isSystem,
      requirement_type = JsString(RequirementType.name(p.requirementType)),
      visibility_type = JsString(VisibilityType.name(p.visibilityType)),
      refers_to = jsonTypeName(p.refersTo),

      properties = jsonHstore(p.properties),
      variables = jsonHstore(p.variables),
      tags = jsonHstore(p.tags),
      auth = jsonHstore(p.auth))
  }

  
  def render(r: GestaltResourceInstance): Option[JsValue] = {
    
    val typeId   = r.typeId
    val template = Properties.getTypeProperties(typeId)
    
    /* Convert property list to Map[PropertyName, DataType */
    val pmap = Properties.getTypePropertyMap(typeId)

    /**
     * Return true if property is 'hidden' or 'optional' and its value is missing.
     */
    def skipRender(p: GestaltTypeProperty, props: Hstore) = {
      p.visibilityType == VisibilityType.id("hidden") || 
      p.requirementType == RequirementType.id("optional") && !props.contains(p.name)                               // optional
    }
    
    @tailrec
    def loop(propKeys: Seq[String], given: Hstore, acc: Map[String,JsValue]): Option[Map[String,JsValue]] = {
      propKeys match {
        case Nil => Option(acc)
        case h :: t => {
          println(s"render: parsing('$h'), type: ${DataType.name(pmap(h).datatype)}")
          /*
           * TODO: Here - you'll get "key not found 'h'" if key is not in type_properties.
           * Need to decide if we allow arbitrary values in properties - I vote No. Props
           * contains only what's defined in the schema. If you want arbitrary values use
           * variables.
           */
          if (skipRender(pmap(h), given)) loop(t, given, acc)
          else {
            val renderedValue = renderDataType(pmap( h ), given( h ))
            loop( t, given, acc + (h -> renderedValue) )
          }
        }
      }
    }

    val givenProperties =
      if (r.properties.isEmpty) None
      else {
        /*
         * TODO: This is a hack just to get something out the door.
         * Once the PropertyManager architecture is done, special cases
         * by resource-type will be handled in a much cleaner way. Here
         * we manually check for type Org and force in the hierarchy props
         * (parent/children) if there's a match. This is very temporary.
         */

        if (r.typeId != ResourceIds.Org) r.properties
        else OrgOutput.buildOrgProps(r.id)

      }

    Option {
      Json.toJson {
        if (givenProperties.isEmpty) None
        else loop(pmap.keys.toList, givenProperties.get, Map[String, JsValue]())
      }
    }
  }

//  def render(r: GestaltResourceInstance) = {
//    
//    val typeId   = r.typeId
//    val template = Properties.getTypeProperties(typeId)
//    
//    /* Convert property list to Map[PropertyName, DataType */
//    val pmap = Map( template map { p => (p.name, p) }: _* )
//
//    def loop(propKeys: Seq[String], props: Hstore, acc: Map[String,JsValue]): Option[Map[String,JsValue]] = {
//      propKeys match {
//        case Nil => Option(acc)
//        case h :: t => {
//          println(s"render: parsing('$h')")
//          /*
//           * TODO: Here - you'll get "key not found 'h'" if key is not in type_properties.
//           * that means that it's just an instance property (user-defined). We need to handle
//           * that - probably just format as string.
//           */
//          val renderedValue = renderDataType(pmap( h ), props( h ))
//          loop( t, props, acc + (h -> renderedValue) )
//        }
//      }
//    }
//    
//    val propertyDefNames = pmap.keys.toList
//    val givenProperties = r.properties.get
//    val renderedValues = Map[String,JsValue]()
//    
//    val renderedProperties = if (r.properties.isEmpty) None 
//          else loop(r.properties.get.keys.toList, r.properties.get, Map[String,JsValue]())
//
//  }  

  
  def renderDataType(property: GestaltTypeProperty, value: String): JsValue = {
    val typeName = DataType.name(property.datatype)
    if (typeRenderers.contains( typeName )) 
      typeRenderers( typeName )( property, value )
    else default( property, value )
  }  

  
  private val typeRenderers = Map[String, (GestaltTypeProperty,String) => JsValue](
      "resource::uuid"             -> renderResourceUUID,
      "resource::uuid::name"       -> resourceUUIDName,
      "resource::uuid::link"       -> resourceUUIDLink,
      "resource::uuid::link::list" -> resourceUUIDLinkList,
      
      /*
      "reference::uuid" -> renderReferenceUUID,
      "reference::uuid::link" -> renderReferenceUUIDLink,
      "reference::uuid::name" -> renderReferenceUUIDProperty,
      */
      
      "datetime"  -> dateTime,
      "json"      -> json,
      "int"       -> int,
      "int::list" -> intList,
      "boolean"   -> boolean
  )
  
  /*
   * TODO: Determine which of these json helpers are really necessary
   * and move up to package object.
   */
  
  def jsonTypeName(typeId: Option[UUID]): Option[JsValue] = {
    if (typeId.isEmpty) None
    else TypeFactory.findById(typeId.get) match {
      case Some(t) => Option(JsString(t.name))
      case None => illegal(s"Unknown type_id '${typeId}'")
    }
  }
  def jsonTypePropertyLinks(typeId: UUID) = {
    val ps = getTypePropertyLinks(typeId)
    if (ps.isEmpty) None else Some(Json.toJson(ps))
  }
  def jsStringOpt(s: Option[String]): Option[JsString] = {
    if (s.isDefined) Some(JsString(s.get)) else None
  }
  
  def jsonArray(ar: Option[List[String]]) = {
    if (ar.isEmpty) None else Some(Json.toJson(ar))
  }
  def jsonHstore(hs: Option[Hstore]) = {
    if (hs.isEmpty) None else Some(Json.toJson(hs))
  }
  def jsonOwnerLink(ro: ResourceOwnerLink) = {
    Json.toJson(ro)
  }
  def jsonLink(typeId: UUID, id: UUID, name: Option[String]) = {
    Json.toJson(toLink(typeId, id, name))
  }  

}