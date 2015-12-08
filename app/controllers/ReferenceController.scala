package controllers

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.PropertyValidator
import com.galacticfog.gestalt.data.ResourceFactory

import com.galacticfog.gestalt.data.ResourceType
import com.galacticfog.gestalt.data.illegal
import com.galacticfog.gestalt.data.models.GestaltResourceInstance

import com.galacticfog.gestalt.data.uuid2string

import com.galacticfog.gestalt.meta.api.output.Output



import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.{GestaltResource => SecurityResource}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.galacticfog.gestalt.tasks.play.io.NonLoggingTaskEvents
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator

import controllers.util._
import play.api.{Logger => log}
import play.api.libs.json.JsError
import play.api.libs.json.JsValue
import play.api.libs.json.Json

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._

object ReferenceController extends GestaltFrameworkSecuredController[DummyAuthenticator] 
    with MetaController with NonLoggingTaskEvents {

  
  
  
}