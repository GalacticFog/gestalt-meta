package com.galacticfog.gestalt.meta

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.data.models._
import _root_.play.api.libs.json._

import java.util.UUID

import _root_.play.api.mvc._
import _root_.play.api.mvc.Action
import _root_.play.api.mvc.Controller
import _root_.play.api.mvc.RequestHeader
import _root_.play.api.mvc.AnyContent
//import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.data.Hstore

package object api {

  /**
   * Move to 'errors' sub-package
   */
  
  sealed abstract class ConfigStatus(content: String, ex: Option[Throwable])
  case class OK(content: String, ex: Option[Throwable] = None) extends ConfigStatus(content, ex)
  case class Wait(content: String, ex: Option[Throwable] = None) extends ConfigStatus(content, ex)
  case class Fail(content: String, ex: Option[Throwable] = None) extends ConfigStatus(content, ex)

  /**
   * Translate resource REST name to Resource Type ID
   */
  def resourceUUID(resource: String): Option[UUID] = {
    resource match {
      
      case "resources"        => Some(ResourceIds.Resource)
      case "orgs"             => Some(ResourceIds.Org)
      case "users"            => Some(ResourceIds.User)
      case "groups"           => Some(ResourceIds.Group)
      case "workspaces"       => Some(ResourceIds.Workspace)
      case "environments"     => Some(ResourceIds.Environment)
      case "containers"       => Some(ResourceIds.Container)   
            
      case "lambdas"          => Some(ResourceIds.Lambda)
      case "apis"             => Some(ResourceIds.Api)
      case "apiendpoints"     => Some(ResourceIds.ApiEndpoint)
      case "domains"          => Some(ResourceIds.Domain)
   
      case "entitlements"     => Some(ResourceIds.Entitlement)
      case "policies"         => Some(ResourceIds.Policy)
      
      /*
       * - providers 2
       * - rules  2
       * - licenses
       */      
      
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
      case "actions"          => Some(ResourceIds.TypeAction)
      case "entitlements"     => Some(ResourceIds.Entitlement)
      case "licenses"         => Some(ResourceIds.License)
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
      case ResourceIds.Group           => Some("groups")
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
      
      case ResourceIds.Lambda          => Some("lambdas")
      case ResourceIds.Api             => Some("apis")
      case ResourceIds.ApiEndpoint     => Some("apiendpoints")
      case ResourceIds.Domain          => Some("domains")
      case ResourceIds.Container       => Some("containers")      
      case ResourceIds.Entitlement     => Some("entitlements")
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
  
  def stripSlash(s: String) = {
    
    lazy val err = throw new BadRequestException(s"Path must begin with '/', found: $s")
    
    (for {
      a <- Option { if (s.trim.startsWith("/")) s.drop(1) else err }
      b <- Option { if (a.endsWith("/")) a.dropRight(1) else a }
    } yield b).get
  }  
  
}