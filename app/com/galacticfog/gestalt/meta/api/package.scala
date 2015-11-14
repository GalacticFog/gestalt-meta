package com.galacticfog.gestalt.meta

import com.galacticfog.gestalt.data.ResourceIds
import com.galacticfog.gestalt.data.models._
import _root_.play.api.libs.json._

import java.util.UUID

package object api {

  /**
   * Move to 'errors' sub-package
   */
  
  sealed abstract class ConfigStatus(content: String, ex: Option[Throwable])
  case class OK(content: String, ex: Option[Throwable] = None) extends ConfigStatus(content, ex)
  case class Wait(content: String, ex: Option[Throwable] = None) extends ConfigStatus(content, ex)
  case class Fail(content: String, ex: Option[Throwable] = None) extends ConfigStatus(content, ex)

  
  sealed abstract class ApiException(val code: Int, message: String) extends RuntimeException(message) {
    def toErrorString = Json.prettyPrint {
      Json.parse( s"""{ "code": ${code}, "message": "${message}" }""" ) 
    }
  }
  case class UnknownResourceException(message: String) extends ApiException(400, message)
  case class ResourceNotFoundException(message: String) extends ApiException(404, message)
  case class UnauthorizedException(message: String) extends ApiException(401, message)
  case class ForbiddenException(message: String) extends ApiException(403, message)
  
  
  def rte(message: String) = throw new RuntimeException(message)
  def rnf(message: String) = throw new ResourceNotFoundException(message)

  
  /**
   * Translate resource REST name to Resource Type ID
   */
  def resourceUUID(resource: String): Option[UUID] = {
    resource match {
      case "resources"        => Some(ResourceIds.Resource)
      case "orgs"             => Some(ResourceIds.Org)
      case "users"            => Some(ResourceIds.User)
      case "workspaces"       => Some(ResourceIds.Workspace)
      case "environments"     => Some(ResourceIds.Environment)
      case "clustertemplates" => Some(ResourceIds.ClusterTemplate)
      case "nodetemplates"    => Some(ResourceIds.NodeTemplate)
      case "machinespecs"     => Some(ResourceIds.MachineSpec)
      case "blueprints"       => Some(ResourceIds.Blueprint)
      case "deployments"      => Some(ResourceIds.Deployment)
      case "services"         => Some(ResourceIds.Service)
      case "clusters"         => Some(ResourceIds.Cluster)
      case "nodes"            => Some(ResourceIds.Node)
      case "tasks"            => Some(ResourceIds.Task)

      case "nodetypes"        => Some(ResourceIds.NodeType)
      case "environmenttypes" => Some(ResourceIds.EnvironmentType)
      case "datatypes"        => Some(ResourceIds.DataType)
      case "requirementtypes" => Some(ResourceIds.RequirementType)
      case "visibilitytypes"  => Some(ResourceIds.VisibilityType)
      case "resourcestates"   => Some(ResourceIds.ResourceState)
      case "resourcetypes"    => Some(ResourceIds.ResourceType)
      case "taskstatustypes"  => Some(ResourceIds.TaskStatusType)
      case _                  => None
    }
  }
  
  
  /**
   * Translate Resource Type ID to REST name
   */
  def resourceRestName(typeId: UUID): Option[String] = {
    typeId match {
      
      case ResourceIds.Org             => Some("orgs")
      case ResourceIds.User            => Some("users")
      case ResourceIds.Workspace       => Some("workspaces")
      case ResourceIds.Environment     => Some("environments")
      case ResourceIds.ClusterTemplate => Some("clustertemplates")
      case ResourceIds.NodeTemplate    => Some("nodetemplates")
      case ResourceIds.MachineSpec     => Some("machinespecs")
      case ResourceIds.Blueprint       => Some("blueprints")
      case ResourceIds.Service         => Some("services")
      case ResourceIds.Cluster         => Some("clusters")
      case ResourceIds.Node            => Some("nodes")
      case ResourceIds.Task            => Some("tasks")
      
      case ResourceIds.NodeType        => Some("nodetypes")
      case ResourceIds.EnvironmentType => Some("environmenttypes")
      case ResourceIds.DataType        => Some("datatypes")
      case ResourceIds.RequirementType => Some("requirementtypes")
      case ResourceIds.VisibilityType  => Some("visibilitytypes")
      case ResourceIds.ResourceState   => Some("resourcestates")
      case ResourceIds.ResourceType    => Some("resourcetypes")
      case ResourceIds.TypeProperty    => Some("typeproperties")
      case ResourceIds.TaskStatusType  => Some("taskstatustypes")
      
      case _ => None
    }
  }  
  
}