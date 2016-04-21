package com.galacticfog.gestalt.meta


import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data._
import java.util.UUID
import scala.util.{Try,Success,Failure}

import _root_.play.api.libs.json._
import _root_.play.api.{Logger => log}

package object policy {

  val Allow = true
  val Deny = false
  
  case class Predicate[+T](property: String, operator: String, value: T) {
    
    override def toString(): String = {
      val buf = new StringBuilder
      val tpe = value.getClass.getSimpleName
      buf append "Predicate[%s]('when %s %s %s')".format(tpe, property, operator, value.toString)
      buf toString
    }
    
  }

  def decide(target: ResourceLike, predicate: Predicate[Any], effect: Option[String] = None): Either[String,Unit] =  {
    val props = propertyHandler(target.typeId)
    val test = props.getValue(target, predicate.property).get

    log.debug(s"Testing: ${predicate.toString}")
    log.debug(s"Test-Value: " + test)
    
    if (props.compare(test, normalizedPredicate(predicate))) Right(Unit) else Left(predicate.toString)
  }
  
  
  /*
   * TODO: This is temporary. This gets us around the issue where we allow limit + 1 containers to be
   * created when the operator is '<='.
   */
  protected [policy] def normalizedPredicate(predicate: Predicate[_]) = {
    predicate operator match {
      case "<=" => predicate.copy(operator = "<")
      case ">=" => predicate.copy(operator = ">")
      case _ => predicate
    }
  }
  
  protected [policy] def propertyHandler(typeId: UUID): ResourceProperties = {
    typeId match {
      case ResourceIds.Environment => EnvironmentProperties
      case _ => 
        throw new IllegalArgumentException(s"No property handler for type '${typeId.toString}'.")
    }
  }

  protected [policy] def getEffect(effect: String) = {
    if (effect.isEmpty) false
    else {
      effect.toLowerCase match {
        case "allow" => true
        case "deny"  => false
        case _ => 
          throw new IllegalArgumentException(s"Invalid effect '$effect'")
      }
    }
  }    
  
}