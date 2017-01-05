package com.galacticfog.gestalt.meta.policy

import java.util.UUID

import scala.util.Failure
import scala.util.Try

import com.galacticfog.gestalt.data.ResourceState

import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.api.sdk.ResourceStates


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