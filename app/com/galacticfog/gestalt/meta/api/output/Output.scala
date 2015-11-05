package com.galacticfog.gestalt.meta.api.output


import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api._

import java.util.UUID
import play.api.libs.json._

import OutputDatatypeHandlers._


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
  
  applies_to: JsValue,
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
      tags = jsonHstore(r.tags),
      auth = jsonHstore(r.auth),
      property_defs = jsonTypePropertyLinks(r.id))

    //
    // TODO: .copy rendered properties!!!
    //
    
    Json.prettyPrint(Json.toJson(res))
  }
  
  
  def renderInstance2(r: GestaltResourceInstance) = {
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
      tags = jsonHstore(r.tags),
      auth = jsonHstore(r.auth) )
      
    //
    // TODO: .copy rendered properties!!!
    //  
    Json.prettyPrint(Json.toJson(res))
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

      applies_to = jsonTypeName(Option(p.appliesTo)).get,
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
  def jsonHstore(hs: Option[Hstore]) = {
    if (hs.isEmpty) None else Some(Json.toJson(hs))
  }
  def jsonOwnerLink(ro: ResourceOwnerLink) = {
    Json.toJson(ro)
  }
  def jsonLink(typeId: UUID, id: UUID, name: Option[String]) = {
    Json.toJson(toLink(typeId, id, name))
  }
  
  def toLink(typeId: UUID, id: UUID, name: Option[String]) = {
    ResourceLink(typeId, id.toString, name, Some(toHref( typeId, id )))
  }
  
  def toHref(typeId: UUID, id: UUID) = {
    val typename = resourceRestName(typeId) getOrElse { "resources" }
    "/%s/%s".format(typename, id.toString)
  }    
  
  def renderBase(r: GestaltResourceInstance) = {
    val typeId = r.typeId
    //val template = 
  }
  

//  /**
//   * Translate Resource Type ID to REST name
//   */
//  def resourceRestName(typeId: UUID): Option[String] = {
//    typeId match {
//      case ResourceIds.Org             => Some("orgs")
//      case ResourceIds.User            => Some("users")
//      case ResourceIds.Workspace       => Some("workspaces")
//      case ResourceIds.Environment     => Some("environments")
//      case ResourceIds.ClusterTemplate => Some("clustertemplates")
//      case ResourceIds.NodeTemplate    => Some("nodetemplates")
//      case ResourceIds.MachineSpec     => Some("machinespecs")
//      case ResourceIds.Blueprint       => Some("blueprints")
//      case ResourceIds.Service         => Some("services")
//      case ResourceIds.Cluster         => Some("clusters")
//      case ResourceIds.Node            => Some("nodes")
//      case ResourceIds.Task            => Some("tasks")
//      
//      case ResourceIds.NodeType        => Some("nodetypes")
//      case ResourceIds.EnvironmentType => Some("environmenttypes")
//      case ResourceIds.DataType        => Some("datatypes")
//      case ResourceIds.RequirementType => Some("requirementtypes")
//      case ResourceIds.VisibilityType  => Some("visibilitytypes")
//      case ResourceIds.ResourceState   => Some("resourcestates")
//      case ResourceIds.ResourceType    => Some("resourcetypes")
//      case ResourceIds.TaskStatusType  => Some("taskstatustypes")
//      case _ => None
//    }
//  }    
  
  //
  // For hierarchical resource-types i need to inject parent/child props.
  //
  
  def render2(r: GestaltResourceInstance): Option[JsValue] = {
    
    val typeId   = r.typeId
    val template = Properties.getTypeProperties(typeId)
    
    /* Convert property list to Map[PropertyName, DataType */
    val pmap = Map( template map { p => (p.name, p) }: _* )

    println("PROPERTIES TEMPLATE:")
    for ((k,v) <- pmap) println("%-10s, type: %s".format(k, DataType.name(v.datatype)))
    println
    
    println("GIVEN PROPERTIES:")
    for ((k,v) <- r.properties.get) println("%-10s, type: %s".format(k, v))
    println
    
    def loop(propKeys: Seq[String], props: Hstore, acc: Map[String,JsValue]): Option[Map[String,JsValue]] = {
      propKeys match {
        case Nil => Option(acc)
        case h :: t => {
          println(s"render: parsing('$h'), type: ${DataType.name(pmap(h).datatype)}")
          /*
           * TODO: Here - you'll get "key not found 'h'" if key is not in type_properties.
           * that means that it's just an instance property (user-defined). We need to handle
           * that - probably just format as string.
           */
          val renderedValue = renderDataType(pmap( h ), props( h ))
          loop( t, props, acc + (h -> renderedValue) )
        }
      }
    }

    val propertyDefNames = pmap.keys.toList
    val givenProperties = r.properties.get
    val renderedValues = Map[String,JsValue]()

    //    val renderedProperties = if (r.properties.isEmpty) None 
    //          else loop(propertyDefNames, givenProperties, Map[String,JsValue]())

    if (r.properties.isEmpty) None
    else Some {
      Json.toJson {
        loop(propertyDefNames, givenProperties, Map[String, JsValue]())
      }
    }
    
    //Json.toJson(renderedProperties)          
          
          
//    println
//    println("RENDERED-PROPERTIES:")
//    println(Json.prettyPrint(Json.toJson(renderedProperties)))
//    println
    
//    Json.prettyPrint { 
//        Json.toJson(
//          toOutput( r, renderedProperties )  
//        )
//    }
    
  }    
  
  
  def render(r: GestaltResourceInstance) = {
    
    val typeId   = r.typeId
    val template = Properties.getTypeProperties(typeId)
    
    /* Convert property list to Map[PropertyName, DataType */
    val pmap = Map( template map { p => (p.name, p) }: _* )

    def loop(propKeys: Seq[String], props: Hstore, acc: Map[String,JsValue]): Option[Map[String,JsValue]] = {
      propKeys match {
        case Nil => Option(acc)
        case h :: t => {
          println(s"render: parsing('$h')")
          /*
           * TODO: Here - you'll get "key not found 'h'" if key is not in type_properties.
           * that means that it's just an instance property (user-defined). We need to handle
           * that - probably just format as string.
           */
          val renderedValue = renderDataType(pmap( h ), props( h ))
          loop( t, props, acc + (h -> renderedValue) )
        }
      }
    }
    
    val propertyDefNames = pmap.keys.toList
    val givenProperties = r.properties.get
    val renderedValues = Map[String,JsValue]()
    
    val renderedProperties = if (r.properties.isEmpty) None 
          else loop(r.properties.get.keys.toList, r.properties.get, Map[String,JsValue]())

//    Json.prettyPrint { 
//        Json.toJson(
//          toOutput( r, renderedProperties )  
//        )
//    }
    
  }  
  
  def renderDataType(property: GestaltTypeProperty, value: String): JsValue = {
    
    println("RENDERING PROPERTY => " + property.name)
    println(Json.prettyPrint(Json.toJson(property)))
    
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

}