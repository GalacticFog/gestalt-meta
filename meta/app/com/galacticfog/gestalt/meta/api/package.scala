package com.galacticfog.gestalt.meta

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.{DataType, PropertyFactory, TypeFactory, ResourceFactory}
import java.util.UUID

import com.galacticfog.gestalt.data
import com.galacticfog.gestalt.data.models.GestaltResourceType
import play.api.libs.json.Json

import scala.util.Try

package object api {

  def isProviderBackedResource(typeId: UUID): Boolean = getBackingProviderType(typeId).isDefined

  private[api] val GESTALT_CORE_TYPE_ID = UUID.fromString("10d0a758-e844-4496-8321-1ee7509381df")
  
  val rootid = Resource.findFqon("root").fold {
    throw new RuntimeException("Could not find Root Org. Unable to determine ID for entitlement setting.")
  }{ r => 
    r.id 
  }    

  def isGestalt(typeId: UUID): Boolean = {
    TypeFactory.findById(typeId).fold(false) { tpe =>
      isGestalt(Some(tpe))
    }
  }
  
  def isGestalt(tpe: GestaltResourceType): Boolean = {
    isGestalt(Some(tpe))
  }
  
  def isGestalt(tpe: Option[GestaltResourceType]): Boolean = {
    (for {
      x  <- tpe
      ps <- x.properties
      b  <- ps.get("is_gestalt").flatMap(g => Try(g.toBoolean).toOption)
    } yield b).getOrElse(false)
  }
  
  def findGestaltProvider(tpe: GestaltResourceType): Option[UUID] = {
    /*
     * Search /root for GestaltProvider type
     * Return the UUID
     */
    val rs = ResourceFactory.findChildrenOfType(GESTALT_CORE_TYPE_ID, rootid)
    rs.size match {
      case 0 => None
      case 1 => Some(rs.head.typeId)
      case _ => None
    }
  }
  
  import com.galacticfog.gestalt.data.models.GestaltResourceInstance
  
  def findGestaltProvider(): Option[GestaltResourceInstance] = {
    /*
     * Search /root for GestaltProvider type
     * Return the UUID
     */
    val rs = ResourceFactory.findChildrenOfType(GESTALT_CORE_TYPE_ID, rootid)
    rs.size match {
      case 0 => None
      case 1 => Some(rs.head)
      case _ => None
    }
  }  
  
  
  private[api] def findCustomProvider(tpe: GestaltResourceType): Option[UUID] = {
    for {
      // is the resourcetype marked as provider-backed?
      props <- tpe.properties
      isProviderBacked <- props.get("is_provider_backed") flatMap (s => Try{s.toBoolean}.toOption)
      if isProviderBacked
      // what is the refers to type for the provider property?
      pp <- PropertyFactory.findByName(tpe.id, "provider")
      if pp.datatype == DataType.id("resource::uuid::link")
      refersTo <- pp.refersTo
    } yield refersTo
  }
  
  
  def getBackingProviderType(resourceType: UUID): Option[UUID] = {
    TypeFactory.findById(resourceType).fold(Option.empty[UUID]) { tpe =>
//      if (!isGestalt(tpe)) {
//        findCustomProvider(tpe)
//      } else {
//        /*
//         * TODO: If type 'is-gestalt', it is an ERROR if provider not found!!!
//         */
//        findGestaltProvider(tpe)
//      }
      findCustomProvider(tpe).orElse(findGestaltProvider(tpe))
    }
  }
  
  
//  def getBackingProviderType(resourceType: UUID): Option[UUID] = {
//    for {
//      // is the resourcetype marked as provider-backed?
//      rt <- TypeFactory.findById(resourceType)
//      props <- rt.properties
//      isProviderBacked <- props.get("is_provider_backed") flatMap (s => Try{s.toBoolean}.toOption)
//      if isProviderBacked
//      // what is the refers to type for the provider property?
//      pp <- PropertyFactory.findByName(resourceType, "provider")
//      if pp.datatype == DataType.id("resource::uuid::link")
//      refersTo <- pp.refersTo
//    } yield refersTo
//  }

  /**
    * Translate resource REST name to Resource Type ID
    */
  def resourceUUID(resource: String): Option[UUID] = {
    val wellKnown = resource match {
      case "resources"        => Some(ResourceIds.Resource)
      case "orgs"             => Some(ResourceIds.Org)
      case "users"            => Some(ResourceIds.User)
      case "groups"           => Some(ResourceIds.Group)
      case "workspaces"       => Some(ResourceIds.Workspace)
      case "environments"     => Some(ResourceIds.Environment)
      case "containers"       => Some(ResourceIds.Container)
      case "jobs"             => Some(migrations.V33.JOB_TYPE_ID)
      case "lambdas"          => Some(ResourceIds.Lambda)
      case "secrets"          => Some(ResourceIds.Secret)
      case "apis"             => Some(ResourceIds.Api)
      case "apiendpoints"     => Some(ResourceIds.ApiEndpoint)
      case "domains"          => Some(ResourceIds.Domain)
      case "entitlements"     => Some(ResourceIds.Entitlement)
      case "policies"         => Some(ResourceIds.Policy)
      case "rules"            => Some(ResourceIds.Rule)
      case "providers"        => Some(ResourceIds.Provider)
      case "environmenttypes" => Some(ResourceIds.EnvironmentType)
      case "datatypes"        => Some(ResourceIds.DataType)
      case "requirementtypes" => Some(ResourceIds.RequirementType)
      case "visibilitytypes"  => Some(ResourceIds.VisibilityType)
      case "resourcestates"   => Some(ResourceIds.ResourceState)
      case "resourcetypes"    => Some(ResourceIds.ResourceType)
      case "licenses"         => Some(ResourceIds.License)
      case "datacontainers"   => Some(ResourceIds.DataContainer)
      case _                  => None
    }
    wellKnown orElse TypeFactory.listByApiPrefix(resource).headOption.map(_.id)
  }


  /**
    * Translate Resource Type ID to REST name
    */
  def resourceRestName(typeId: UUID): Option[String] = {
    val wellKnown = typeId match {
      case ResourceIds.Org             => Some("orgs")
      case ResourceIds.User            => Some("users")
      case ResourceIds.Group           => Some("groups")
      case ResourceIds.Workspace       => Some("workspaces")
      case ResourceIds.Environment     => Some("environments")
      case ResourceIds.Lambda          => Some("lambdas")
      case ResourceIds.Api             => Some("apis")
      case ResourceIds.ApiEndpoint     => Some("apiendpoints")
      case ResourceIds.Domain          => Some("domains")
      case ResourceIds.Container       => Some("containers")
      case ResourceIds.Entitlement     => Some("entitlements")
      case ResourceIds.EnvironmentType => Some("environmenttypes")
      case ResourceIds.DataType        => Some("datatypes")
      case ResourceIds.RequirementType => Some("requirementtypes")
      case ResourceIds.VisibilityType  => Some("visibilitytypes")
      case ResourceIds.ResourceState   => Some("resourcestates")
      case ResourceIds.ResourceType    => Some("resourcetypes")
      case ResourceIds.TypeProperty    => Some("typeproperties")
      case _ => None
    }
    wellKnown orElse {
      for {
        tpe <- data.TypeFactory.findById(typeId)
        props <- tpe.properties
        api <- props.get("api")
        api_json <- Try{Json.parse(api)}.toOption
        rest_name <- (api_json \ "rest_name").asOpt[String]
      } yield rest_name
    }
  }


}