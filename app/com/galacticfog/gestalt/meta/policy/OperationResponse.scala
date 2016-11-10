package com.galacticfog.gestalt.meta.policy

import java.util.UUID
import java.net.URL
import play.api.http.HttpVerbs
import play.api.libs.ws.WS
import play.api.Play.current

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.PropertyValidator
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.ResourceType
import com.galacticfog.gestalt.data.illegal
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.{ PatchOp, PatchDocument, PatchHandler }
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.{ GestaltResource => SecurityResource }
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController

import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import controllers.util.JsonUtil._
import controllers.util.db._

import play.api.Logger
import play.api.libs.json._
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._

import com.galacticfog.gestalt.meta.api._
import play.api.mvc.Result
import play.api.mvc.Action
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.security.api.json.JsonImports.linkFormat
import com.galacticfog.gestalt.meta.api.sdk.ApiResponse
import com.galacticfog.gestalt.meta.auth.Actions 
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
import com.galacticfog.gestalt.keymgr.GestaltLicense
import com.galacticfog.gestalt.keymgr.GestaltFeature

import com.galacticfog.gestalt.events._
import com.galacticfog.gestalt.meta.policy._

sealed trait OperationResponse[T] {
  def isContinue(): Boolean
  def isHalt(): Boolean
  def toTry(): Try[T]
}

case object Continue extends OperationResponse[Option[UUID]] {
  def isContinue(): Boolean = true
  def isHalt(): Boolean = false
  def toTry() = Try(None)
}

case object Accepted extends OperationResponse[Option[UUID]] {
  def isContinue(): Boolean = true
  def isHalt(): Boolean = false
  //
  // TODO: Change this to ResourceStates.Pending once implemented
  //
  def toTry() = Try(Option(ResourceState.id(ResourceStates.Active)))
}

case class Halt(reason: String) extends OperationResponse[Option[UUID]] {
  def isContinue(): Boolean = false
  def isHalt(): Boolean = true
  def toTry() = Failure(new ConflictException(reason))
}