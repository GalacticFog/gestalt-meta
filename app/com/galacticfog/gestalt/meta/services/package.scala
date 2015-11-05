//package com.galacticfog.gestalt.meta
//
////import scala.concurrent.ExecutionContext.Implicits.{global => newName}
//import _root_.play.api.libs.json.{Json => Js}
//
//package object services {
//
////  sealed abstract class ConfigStatus(content: String, ex: Option[Throwable])
////  case class Ok(content: String, ex: Option[Throwable] = None) extends ConfigStatus(content, ex)
////  case class Wait(content: String, ex: Option[Throwable] = None) extends ConfigStatus(content, ex)
////  case class Fail(content: String, ex: Option[Throwable] = None) extends ConfigStatus(content, ex)
////
////  class ResourceNotFoundException(message: String = "") extends Exception {
////    override def getMessage() = message
////  }
////  
////  case class UnauthorizedException(message: String = "") extends RuntimeException(message)
////  
////  def rte(message: String) = throw new RuntimeException(message)
////  def rnf(message: String) = throw new ResourceNotFoundException(message)  
//  
//}