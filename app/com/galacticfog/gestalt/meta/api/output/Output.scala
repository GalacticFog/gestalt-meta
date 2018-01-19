package com.galacticfog.gestalt.meta.api.output


import java.util.UUID

import scala.annotation.tailrec

import com.galacticfog.gestalt.meta.api.sdk._


import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.illegal

import com.galacticfog.gestalt.data.models.ResourceLike
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.models.GestaltResourceType
import com.galacticfog.gestalt.data.models.GestaltTypeProperty
//import com.galacticfog.gestalt.data.models.ResourceOwnerLink
import com.galacticfog.gestalt.data.uuid

import OutputDatatypeHandlers._

import play.api.libs.json.JsString
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import play.api.libs.json.Json

import scala.reflect.runtime.{ universe => ru }
import scala.reflect.runtime.currentMirror
import ru._

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import scala.language.implicitConversions

object OrgCache {
  // this never gets updated when new orgs are created: https://gitlab.com/galacticfog/gestalt-meta/issues/364#note_48945348
  val orgs = ResourceFactory.findAll(ResourceIds.Org)
  val fqons: Map[UUID, String] = orgs.map(o =>
    (o.id -> o.properties.get("fqon"))).toMap
    
  def getFqon(id: UUID): Option[String] = {
    fqons.get(id)
  }
  
  def getOrg(id: UUID) = {  
  } 
}


object Output {

  /*
   * TODO: Refactor renderLinks* to take ResourceLike args.
   */

  def renderInstance(r: GestaltResourceInstance, baseUri: Option[String] = None): JsValue = {

    val res = GestaltResourceOutput(
      id = r.id,
      org = {
        val props = OrgCache.getFqon(r.orgId) map { f => Map("fqon" -> f) }
        jsonLink(ResourceIds.Org, r.orgId, r.orgId, None, baseUri, props)
      },
      resource_type = jsonTypeName(Option(r.typeId)).get,
      resource_state = JsString(ResourceState.name(r.state)),
      owner = Json.toJson(r.owner),
      name = r.name,
      description = r.description,
      created = Json.toJson(r.created),
      modified = Json.toJson(r.modified),
      properties = None, // expanded below
      variables = jsonHstore(r.variables),
      tags = jsonArray(r.tags),
      auth = r.auth)       

    // this renders the properties
    val renderedProps = renderInstanceProperties(r.typeId, r.id, r.properties)
    Json.toJson(res.copy(
      properties = renderedProps orElse Some(Json.obj())
    ))
  }

  
  def compact(r: JsValue, metaUrl: Option[String] = None): JsValue = {
    import com.galacticfog.gestalt.patch._
    
    val patch = PatchDocument(
        PatchOp.Remove("/org"),
        PatchOp.Remove("/owner"),
        PatchOp.Remove("/created"),
        PatchOp.Remove("/modified"))
    
    patch.applyPatch(r.as[JsObject]).get
  }

  def compactInstance(r: GestaltResourceInstance, metaUrl: Option[String] = None): JsValue = {

    val res = GestaltResourceOutput(
      id = r.id,
      org = jsonLink(ResourceIds.Org, r.orgId, r.orgId, None, metaUrl),
      resource_type = jsonTypeName(Option(r.typeId)).get,
      resource_state = JsString(ResourceState.name(r.state)),
      owner = Json.toJson(r.owner),
      name = r.name,
      description = r.description,
      created = Json.toJson(r.created),
      modified = Json.toJson(r.modified),
      properties = None, // expanded below
      variables = jsonHstore(r.variables),
      tags = jsonArray(r.tags),
      auth = r.auth)       

    // this renders the properties
    val renderedProps = renderInstanceProperties(r.typeId, r.id, r.properties)
    val result = Json.toJson(res.copy(
      properties = renderedProps orElse Some(Json.obj())
    ))
    
    compact(result, metaUrl)
    
//    import com.galacticfog.gestalt.patch._
//    
//    val patch = PatchDocument(
//        PatchOp.Remove("/org"),
//        PatchOp.Remove("/owner"),
//        PatchOp.Remove("/created"),
//        PatchOp.Remove("/modified"))
//    patch.applyPatch(result.as[JsObject]).get
  }
  
  
  def renderCompact(r: GestaltResourceInstance, baseUri: Option[String] = None): JsValue = {
    val res = GestaltResourceOutput(
      id = r.id,
      org = jsonLink(ResourceIds.Org, r.orgId, r.orgId, None, baseUri),
      resource_type = jsonTypeName(Option(r.typeId)).get,
      resource_state = JsString(ResourceState.name(r.state)),
      owner = Json.toJson(r.owner),
      name = r.name,
      description = r.description,
      created = Json.toJson(r.created),
      modified = Json.toJson(r.modified),
      properties = None, // expanded below
      variables = jsonHstore(r.variables),
      tags = jsonArray(r.tags),
      auth = r.auth)       

    // this renders the properties
    val renderedProps = renderInstanceProperties(r.typeId, r.id, r.properties)
    val result = Json.toJson(res.copy(
      properties = renderedProps orElse Some(Json.obj())
    ))
    import com.galacticfog.gestalt.patch._
    
    val patch = PatchDocument(
        PatchOp.Remove("/org"),
        PatchOp.Remove("/owner"),
        PatchOp.Remove("/created"),
        PatchOp.Remove("/modified"))
    patch.applyPatch(result.as[JsObject]).get
  }  
  
  def renderResourceTypeOutput(r: GestaltResourceType, baseUri: Option[String] = None): JsValue = {
    val res = mkTypeOutput(r)    
    val props = res.properties.get.validate[Map[String,String]].get
    val renderedProps = renderTypeProperties(ResourceIds.ResourceType, Some(props))
    
    Json.toJson(res.copy(properties = renderedProps))
  }
  
  
  private def mkTypeOutput(r: GestaltResourceType, baseUri: Option[String] = None) = {
    GestaltResourceTypeOutput(
      id = r.id,
      name = r.name,
      extend = jsonTypeName(r.extend),
      resource_type = jsonTypeName(Option(r.typeId)).get,
      resource_state = JsString(ResourceState.name(r.state)),

      org = jsonLink(ResourceIds.Org, r.orgId, r.orgId, None, baseUri),
      owner = Json.toJson(r.owner),
      description = r.description,
      created = Json.toJson(r.created),
      modified = Json.toJson(r.modified),
      properties = jsonHstore(r.properties),
      variables = jsonHstore(r.variables),
      tags = jsonArray(r.tags),
      auth = jsonHstore(r.auth),
      property_defs = jsonTypePropertyLinks(r.id))      
  }

  def renderPropertyLinks(rs: Seq[GestaltTypeProperty], baseUri: Option[String] = None): JsValue = {
    Json.toJson(rs map { r => 
      toLink(r.typeId, r.id, name = Some(r.name), orgId = r.orgId, baseUri = baseUri) 
    })
  }
  
  def renderTypePropertyOutput(p: GestaltTypeProperty, baseUri: Option[String] = None): JsValue = { 
    val res = toPropertyOutput( p, baseUri )
    Json.toJson(res)
  }
  
  def renderLink(rs: ResourceLike, baseUri: Option[String] = None): JsValue = {
    Json.toJson(toLink(rs.typeId, rs.id, rs.orgId, Some(rs.name), baseUri))
  }
  
  def renderLinks(rs: Seq[ResourceLike], baseUri: Option[String] = None): JsValue = {
    Json.toJson(rs map { r => 
      toLink(r.typeId, r.id, name = Some(r.name), orgId = r.orgId, baseUri = baseUri) 
    })
  }
  
  /**
   * Convert a domain TypeProperty to an output TypeProperty.
   */
  protected[output] def toPropertyOutput(p: GestaltTypeProperty, baseUri: Option[String] = None) = {
    GestaltTypePropertyOutput(
      id = p.id,
      name = p.name,
      resource_type = jsonTypeName(Option(p.typeId)).get,
      resource_state = JsString(ResourceState.name(p.state)),
      org = jsonLink(ResourceIds.Org, p.orgId, p.orgId, None, baseUri),
      owner = Json.toJson(p.owner),
      description = p.description,
      created = Json.toJson(p.created),
      modified = Json.toJson(p.modified),

      applies_to = jsonTypeName(Option(p.appliesTo)),
      data_type = JsString(DataType.name(p.datatype)), //name
      default_value = jsStringOpt(p.defaultValue),
      is_sealed = p.isSealed,
      is_system = p.isSystem,
      requirement_type = JsString(RequirementType.name(p.requirementType)),
      visibility_type = JsString(VisibilityType.name(p.visibilityType)),
      refers_to = jsonTypeName(p.refersTo),
      
      properties = jsonHstore(p.properties),
      variables = jsonHstore(p.variables),
      tags = jsonArray(p.tags),
      auth = jsonHstore(p.auth))
  }
  
  /**
   * Takes a Resource's property Map (Hstore) and renders each property as appropriate
   * for its datatype.
   * 
   * @param typeId UUID of Type to gather properties for
   * @param instanceId UUID of Type instance
   * @return JsValue object containing all rendered property name/values.
   */
  def renderInstanceProperties(typeId: UUID, instanceId: UUID, properties: Option[Hstore]): Option[JsValue] = {
    /* Get a Map of the properties defined for the current ResourceType. */
    
    val templateProps = Properties.getTypePropertyMap(typeId)

    @tailrec
    def loop(propKeys: Seq[String], given: Hstore, acc: Map[String,JsValue]): Option[Map[String,JsValue]] = {
      propKeys match {
        case Nil    => Option(acc)
        case property :: tail => {

          /*
           * DEBUG-NOTE: You'll get "key not found 'property'" if key is not in templateProps.
           * That shouldn't happen as this is output (resource should been validated at create/update).
           * just a note to look here if you see that error pop up.
           */
          if (skipRender(templateProps(property), given)) loop(tail, given, acc)
          else {
            val renderedValue = renderDataType(templateProps( property ), given( property ))
            loop( tail, given, acc + (property -> renderedValue) )
          }
        }
      }
    }

    val givenProperties = collectInstanceProperties(typeId, instanceId, properties)
    givenProperties map {
      loop(templateProps.keys.toList, _, Map[String, JsValue]())
    } map {
      Json.toJson(_)
    }
  }
  
  def renderTypeProperties(typeId: UUID, properties: Option[Hstore]): Option[JsValue] = {
    /* Get a Map of the properties defined for the current ResourceType. */

    val givenProps = properties getOrElse Map()
    val templateProps = {
      val ps = Properties.getPrototype(typeId)
      val remove = ps.keySet.diff(givenProps.keySet)
      ps -- remove
    }

    @tailrec
    def loop(propKeys: Seq[String], given: Hstore, acc: Map[String,JsValue]): Option[Map[String,JsValue]] = {
      propKeys match {
        case Nil    => Option(acc)
        case property :: tail => {

          if (skipRender(templateProps(property), given)) {
            loop(tail, given, acc)
          }
          else if (templateProps.contains(property) && given.contains(property)) {
            val renderedValue = renderDataType(templateProps( property ), given( property ))
            loop( tail, given, acc + (property -> renderedValue) )            
          }
          else {
            loop(tail, given, acc)
          }
        }
      }
    }
    loop(templateProps.keys.toList, givenProps, Map[String, JsValue]()) map {
      Json.toJson(_)
    }
  }
  
  /*
   * TODO: This is a hack just to get something out the door.
   * Once the PropertyManager architecture is done, special cases
   * by resource-type will be handled in a much cleaner way. Here
   * we manually check for type Org and force in the hierarchy props
   * (parent/children) if there's a match. This is very temporary.
   */  
  private def collectInstanceProperties(typeId: UUID, instanceId: UUID, properties: Option[Hstore]) = {
    typeId match {
      case ResourceIds.Org => OrgOutput.buildOrgProps(instanceId)
      case _ => properties
    }
  }
  
  /*
   * Return true if property is 'hidden' or 'optional' and its value is missing.
   */
  private def skipRender(p: GestaltTypeProperty, props: Hstore) = {
    p.visibilityType == VisibilityType.id("hidden") ||
      p.requirementType == RequirementType.id("optional") && !props.contains(p.name) // optional
  }
  
  /**
   * Maps datatype names to rendering functions.
   */
  private def renderDataType(property: GestaltTypeProperty, value: String): JsValue = {
    val typeName = DataType.name(property.datatype)
    if (typeRenderers.contains( typeName )) 
      typeRenderers( typeName )( property, value )
    else renderDefault( property, value )
  }  
  
  type RenderFunction = (GestaltTypeProperty, String) => JsValue
  
  private val typeRenderers = Map[String, (GestaltTypeProperty,String) => JsValue](
      "resource::uuid"             -> renderResourceUUID,
      "resource::uuid::name"       -> resourceUUIDName,
      "resource::uuid::link"       -> resourceUUIDLink,
      
      "resource::uuid::list"       -> renderStringList,
      "resource::uuid::link::list" -> resourceUUIDLinkList,

      "int"       -> renderInt,
      "float"     -> renderFloat,
      "boolean"   -> renderBoolean,
      "datetime"  -> renderDateTime,
      "json"      -> renderJson,
      
      "json::list"    -> renderJsonList,
      "string::list"  -> renderStringList,
      "int::list"     -> renderIntList,
      "float::list"   -> renderFloatList,
      "boolean::list" -> renderBooleanList,
      "uuid::list"    -> renderStringList
  )
  
  /**
   * Get the TypeProprties of any resource-type and convert to list of ResourceLinks.
   */
  protected[output] def getTypePropertyLinks(typeId: UUID, baseUri: Option[String] = None) = {
    Properties.getTypeProperties(typeId) map { p => 
      toLink( p.typeId, p.id, name = Some(p.name), orgId = p.orgId, baseUri = baseUri ) 
    }
  }  
  
  /**
   * Given a resource type-id get the type-name as a JsString
   */
  protected[output] def jsonTypeName(typeId: Option[UUID]): Option[JsValue] = {
    if (typeId.isEmpty) None
    else {
      val tid = typeId.get
      TypeFactory.findById(tid).fold {
        ReferenceFactory.findById(tid, tid).fold {
          illegal(s"Unknown type_id '${tid}'")
        }{ ref => Option(JsString(ref.name)) }
      }{ tpe => Option(JsString(tpe.name)) }
    }
  }
  
  /**
   * Get list of resource TypeProperties, convert to rendered Seq[ResourceLink]
   */
  protected[output] def jsonTypePropertyLinks(typeId: UUID): Option[JsValue] = {
    val ps = getTypePropertyLinks(typeId)
    if (ps.isEmpty) None else Some(Json.toJson(ps))
  }
  
  /**
   * Create a ResourceLink converted to a JsValue.
   */
  protected[output] def jsonLink(typeId: UUID, id: UUID, org: UUID, name: Option[String], baseUri: Option[String] = None, props: Option[Map[String,String]] = None): JsValue = {
    Json.toJson(toLink(typeId, id, name = name, orgId = org, baseUri = baseUri, props = props))
  }  
  
  /**
   * Convert Option[String] to Option[JsString]
   */
  protected[output] implicit def jsStringOpt(s: Option[String]): Option[JsString] = {
    if (s.isDefined) Some(JsString(s.get)) else None
  }
  
  /**
   * Convert a List[String] to a JsArray
   */
  protected[output] implicit def jsonArray(ar: Option[List[String]]): Option[JsValue] = {
    if (ar.isEmpty) None else Some(Json.toJson(ar))
  }
  
  /**
   * Convert a Map[String,String] to a JsObject.
   */
  protected[output] implicit def jsonHstore(hs: Option[Hstore]): Option[JsValue] = {
    hs.map(Json.toJson(_))
  }

  
}

