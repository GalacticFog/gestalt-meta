package controllers


import java.util.UUID

import scala.{Either,Left,Right}
import scala.Right

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink

import play.api.libs.json.JsArray
import play.api.libs.json.Json


case class Entitlement(
    id: UUID,
    org: UUID,
    name: String,
    description: Option[String] = None,  
    properties: EntitlementProps,
    variables: Option[Hstore] = None,
    tags: Option[List[String]] = None)

    
object Entitlement {
  
  /**
   * Convert a GestaltResourceInstance to an Entitlement object.
   */
  def make(r: GestaltResourceInstance) = {
    Entitlement(
      id = r.id,
      org = r.orgId,
      name = r.name,
      description = r.description,
      properties = EntitlementProps.make(r),
      variables = r.variables,
      tags = r.tags
    )
  }
  
  /**
   * Convert an Entitlement object to a GestaltResourceInstance.
   */
  def toGestalt(creator: UUID, ent: Entitlement) = {
    
    GestaltResourceInstance(
      id = ent.id,
      typeId = ResourceIds.Entitlement,
      orgId = ent.org, // this needs to be org from URI
      owner = ResourceOwnerLink(ResourceIds.User, creator),
      name = ent.name,
      description = ent.description,
      properties = Option(EntitlementProps.toMap(ent.properties)) 
    ) 
  }  

  implicit lazy val entitlementPropsFormat = Json.format[EntitlementProps]
  implicit lazy val entitlementFormat = Json.format[Entitlement]
}


object EntitlementProps {
 /**
  * Convert EntitlementProps object to Map[String,String]
  */
  def toMap(props: EntitlementProps) = {
    val v = props.value map { v => Map("value" -> v) } getOrElse Map()
    val i = props.identities map { ids => 
      Map(
        "identities" -> Json.toJson(ids).as[JsArray].toString.replaceAll("\\\\", "")
      )
    } getOrElse Map()
    
    Map("action" -> props.action) ++ v ++ i
  }
  
  /**
   * Convert GestaltResourceInstance.properties to EntitlementProps object.
   */
  def make(r: GestaltResourceInstance): EntitlementProps = {
    val props = r.properties.get
    val action = props("action")
    val value = if (props.contains("value")) Some(props("value")) else None
    val identities = if (props.contains("identities")) Some(props("identities")) else None
    
    // TODO: Combine ops to regex for replaceAll
    def uuidsFromString(str: String) = str.
      stripPrefix("[").
      stripSuffix("]").
      replaceAll("\"", "").
      split(",").
      toSeq.
      map { UUID.fromString(_)
    }
    EntitlementProps(action, value, identities map { uuidsFromString(_) })
  }
}


case class EntitlementProps(
    action: String, 
    value: Option[String], 
    identities: Option[Seq[UUID]]) {

  /**
   * Validates that action name is valid for the resource type and that all identities given are valid.
   */
  def validate(): Either[String, Unit] = {
    if (identities.isEmpty) Right(())
    else {
      val given = identities.get
      val found = ResourceFactory.findAllIn(given) map { _.id }
      val notFound = given.diff(found)
      
      if (notFound.isEmpty) Right(())
      else Left(s"Invalid identities : [ ${notFound.mkString(",")} ]")
    }
  }
  
}

