package com.galacticfog.gestalt.meta.auth


import java.util.UUID
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds


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
}

object Actions {
   
  def resourceAction(typeId: UUID, action: String): String = {
    (typeId match {
      case ResourceIds.Org => Org
      case ResourceIds.User => User
      case ResourceIds.Group => Group
      case ResourceIds.Workspace => Workspace
      case ResourceIds.Environment => Environment
      case ResourceIds.Container => Container
      case ResourceIds.Lambda => Lambda
      case ResourceIds.License => License
      case e => {
        throw new RuntimeException(s"Unknown Action Object type '$typeId'.")
      }
    }).actions(action)
  }

  object Org extends ResourceActions("org")
  object User extends ResourceActions("user")
  object Group extends ResourceActions("group")
  object Workspace extends ResourceActions("workspace")
  object Environment extends ResourceActions("environment")
  object Lambda extends ResourceActions("lambda")
  object Container extends ResourceActions("container")
  object License extends ResourceActions("license")
  
}
