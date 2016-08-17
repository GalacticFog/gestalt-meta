package com.galacticfog.gestalt.meta.auth

import java.util.UUID

import scala.util.{Try,Success,Failure}

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import controllers.util._
import play.api.{Logger => log}
import play.api.libs.json._
import play.api.mvc.Result
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.annotation.tailrec


object ResourceEntitlement extends Entitlements {

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