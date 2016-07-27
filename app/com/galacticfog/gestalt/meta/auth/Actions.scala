package com.galacticfog.gestalt.meta.auth


import java.util.UUID
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.errors.BadRequestException


abstract class ResourceActions(prefix: String) {
  val Create = s"$prefix.create"
  val View   = s"$prefix.view"
  val Update = s"$prefix.update"
  val Delete = s"$prefix.delete"
  
  protected[auth] val actions = Map(
    "create" -> Create,
    "view" -> View,
    "update" -> Update,
    "delete" -> Delete)
  
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
  
  
//  def resourceAction(typeId: UUID, action: String): String = {
//    (typeId match {
//      case ResourceIds.Org => Org
//      case ResourceIds.User => User
//      case ResourceIds.Group => Group
//      case ResourceIds.Workspace => Workspace
//      case ResourceIds.Environment => Environment
//      case ResourceIds.Container => Container
//      case ResourceIds.Lambda => Lambda
//      case ResourceIds.License => License
//      case ResourceIds.Provider => Provider
//      case ResourceIds.Entitlement => Entitlement
//      case ResourceIds.Policy => Policy
//      case ResourceIds.Rule => Rule
//      case e => {
//        throw new RuntimeException(s"Unknown Action Object type '$typeId'.")
//      }
//    }).actions(action)
//  }

  object Org extends ResourceActions("org")
  object User extends ResourceActions("user")
  object Group extends ResourceActions("group")
  object Workspace extends ResourceActions("workspace")
  object Environment extends ResourceActions("environment")
  object Lambda extends ResourceActions("lambda")
  object Container extends ResourceActions("container")
  object License extends ResourceActions("license")
  
  object Provider extends ResourceActions("provider")
  object Entitlement extends ResourceActions("entitlement")
  object Policy extends ResourceActions("policy")
  object Rule extends ResourceActions("rule")
}
