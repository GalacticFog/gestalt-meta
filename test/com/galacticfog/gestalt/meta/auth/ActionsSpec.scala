package com.galacticfog.gestalt.meta.auth

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Step
import play.api.libs.json._

import java.util.UUID

class ActionsSpec extends Specification {
  
  "actionName" should {
    
    "return the fully-qualified action name for the given resource-type and action" in {
      Actions.actionName(ResourceIds.Workspace, "create") === "workspace.create"
      Actions.actionName(ResourceIds.Environment, "view") === "environment.view"
      Actions.actionName(ResourceIds.Lambda, "update") === "lambda.update"
      Actions.actionName(ResourceIds.Container, "delete") === "container.delete"
    }
    
    "throw a RuntimeException when given a resource-type ID that is not mapped" in {
      Actions.actionName(uuid(), "create") must throwA[RuntimeException]
    }
    
    "throw a BadRequestException when given an action name that does not exist on the resource-type" in {
      Actions.actionName(ResourceIds.Org, "INVALID_ACTION") must throwA[BadRequestException]
    }
  }
  
}