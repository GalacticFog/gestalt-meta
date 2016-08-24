package com.galacticfog.gestalt.meta.auth


import java.util.UUID
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.errors.BadRequestException


abstract class ResourceActions(
    val prefix: String, 
    additionalActions: Map[String,String] = Map()) {
  val Create = s"$prefix.create"
  val View   = s"$prefix.view"
  val Update = s"$prefix.update"
  val Delete = s"$prefix.delete"
  
  protected[auth] val actions = Map(
    "create" -> Create,
    "view" -> View,
    "update" -> Update,
    "delete" -> Delete) ++ (additionalActions map { case (k,v) => (k -> s"$prefix.$v") })
  
  def isValidAction(action: String) = {
    actions.keySet.contains(action)
  }
}

object Actions {
   
  val actionTypes = Map(
        ResourceIds.Org -> Org,
        ResourceIds.User -> User,
        ResourceIds.Group -> Group,
        ResourceIds.Workspace -> Workspace,
        ResourceIds.Environment -> Environment,
        ResourceIds.Container -> Container,
        ResourceIds.Lambda -> Lambda,
        ResourceIds.License -> License,
        ResourceIds.Provider -> Provider,
        ResourceIds.ApiGatewayProvider -> Provider,
        ResourceIds.MarathonProvider -> Provider,
        ResourceIds.Entitlement -> Entitlement,
        ResourceIds.Policy -> Policy,
        ResourceIds.Rule -> Rule,
        ResourceIds.RuleEvent -> Rule,
        ResourceIds.RuleLimit -> Rule,
        ResourceIds.Domain -> Domain,
        ResourceIds.ApiEndpoint  -> ApiEndpoint,
        ResourceIds.ResourceType -> ResourceType,
        ResourceIds.TypeProperty -> TypeProperty)  

  def typePrefix(typeId: UUID) = typeId match {
    case ResourceIds.Org => "org"
    case ResourceIds.User => "user"
    case ResourceIds.Group => "group"
    case ResourceIds.Workspace => "workspace"
    case ResourceIds.Environment => "environment"
    case ResourceIds.Lambda => "lambda"
    case ResourceIds.Container => "container"
    case ResourceIds.License => "license"
    case ResourceIds.Domain => "domain"
    case ResourceIds.ApiEndpoint => "apiendpoint"
    case ResourceIds.ResourceType => "resourcetype"
    case ResourceIds.TypeProperty => "typeproperties"
    case ResourceIds.ApiGatewayProvider => "provider"
    case ResourceIds.MarathonProvider => "provider"     
    case ResourceIds.RuleEvent => "rule"
    case ResourceIds.RuleLimit => "rule"
    case e => throw new IllegalArgumentException(s"Unknown action-prefix '$typeId'")
  }

  def actionName(typeId: UUID, action: String) = {
    actionTypes.get(typeId).fold {
      throw new RuntimeException(s"Unknown Action Object type '$typeId'.")
    }{ actionType =>
      if (actionType.isValidAction(action)) actionType.actions(action)
      else throw new BadRequestException(s"Invalid action '$action'.")
    }
  }
  
  object Org extends ResourceActions("org")
  object User extends ResourceActions("user")
  object Group extends ResourceActions("group")
  object Workspace extends ResourceActions("workspace")
  object Environment extends ResourceActions("environment")
  object License extends ResourceActions("license")
  object Provider extends ResourceActions("provider")
  object Entitlement extends ResourceActions("entitlement")
  object Policy extends ResourceActions("policy")
  object Rule extends ResourceActions("rule")
  object Domain extends ResourceActions("domain")
  object ApiEndpoint extends ResourceActions("apiendpoint")
  object ResourceType extends ResourceActions("resourcetype")
  object TypeProperty extends ResourceActions("typeproperty")
  
  object Container extends ResourceActions("container",
      additionalActions = Map("migrate" -> "migrate", "scale" -> "scale")) {
    val Migrate = s"${prefix}.migrate"
    val Scale   = s"${prefix}.scale"  
  }  
  
  object Lambda extends ResourceActions("lambda",
      additionalActions = Map("invoke" -> "invoke")) {
    val Invoke = s"${prefix}.invoke"  
  }
  
}
