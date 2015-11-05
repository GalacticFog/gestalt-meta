package com.galacticfog.gestalt.meta.services

import _root_.play.api.libs.json.{Json => Js}
  
  
case class GestaltRestError(code: Int, message: String, developerMessage: Option[String] = None)

//class ResourceNotFoundException(message: String) extends RuntimeException( message ) {
//  implicit lazy val restErrorFormat = Js.format[GestaltRestError]
//  def toError = GestaltRestError(404, message)
//  def toErrorString = Js.prettyPrint(Js.toJson( toError ))
//}
//
//
//
//object ServiceObjects {
//  implicit lazy val restErrorFormat = Js.format[GestaltRestError]
//  def rte(message: String) = throw new RuntimeException(message)
//  def rnf(message: String) = throw new ResourceNotFoundException(message)
//}