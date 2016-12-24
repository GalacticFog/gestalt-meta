package com.galacticfog.gestalt.meta.auth

import com.galacticfog.gestalt.meta.api.sdk._

object ResourceEntitlement {

  import Entitlements.ACTIONS_CRUD

  val UserActions        = (ResourceIds.User         -> ACTIONS_CRUD)
  val GroupActions       = (ResourceIds.Group        -> ACTIONS_CRUD)
  val OrgActions         = (ResourceIds.Org          -> ACTIONS_CRUD)
  val WorkspaceActions   = (ResourceIds.Workspace    -> ACTIONS_CRUD)
  val EnvironmentActions = (ResourceIds.Environment  -> ACTIONS_CRUD)
  val PolicyActions      = (ResourceIds.Policy       -> ACTIONS_CRUD)  
  val ProviderActions    = (ResourceIds.Provider     -> ACTIONS_CRUD)
  val EntitlementActions = (ResourceIds.Entitlement  -> ACTIONS_CRUD)  
  val ResourceTypeActions = (ResourceIds.ResourceType -> ACTIONS_CRUD)
  val TypePropertyActions = (ResourceIds.TypeProperty -> ACTIONS_CRUD)
  val ContainerActions   = (ResourceIds.Container    -> (ACTIONS_CRUD ++ Seq("migrate", "scale")))
  val LambdaActions      = (ResourceIds.Lambda       -> (ACTIONS_CRUD ++ Seq("invoke")))   
  
}