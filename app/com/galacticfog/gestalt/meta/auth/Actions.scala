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
    "delete" -> Delete) ++ additionalActions
  
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
        ResourceIds.Entitlement -> Entitlement,
        ResourceIds.Policy -> Policy,
        ResourceIds.Rule -> Rule)  
  
  
  /*
   * TODO: Test this as a replacement for 'resourceAction()' below.
   */
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
  object Container extends ResourceActions("container")
  object License extends ResourceActions("license")
  
  object Provider extends ResourceActions("provider")
  object Entitlement extends ResourceActions("entitlement")
  object Policy extends ResourceActions("policy")
  object Rule extends ResourceActions("rule")
  
  object Lambda extends ResourceActions("lambda",
      additionalActions = Map("migrate" -> "migrate", "scale" -> "scale")) {
    val Migrate = s"${prefix}.migrate"
    val Scale   = s"${prefix}.scale"  
  }
  
}
