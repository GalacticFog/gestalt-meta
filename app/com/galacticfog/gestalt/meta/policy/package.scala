package com.galacticfog.gestalt.meta


import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data._
import java.util.UUID
import scala.util.{Try,Success,Failure}

import _root_.play.api.libs.json._
import _root_.play.api.{Logger => log}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds


package object policy {

  val Allow = true
  val Deny = false
  
  val EmptyProperty = "NONE"
  val EmptyPropertySeq = "[\"NONE\"]"
  
  case class Predicate[+T](property: String, operator: String, value: T) {
    
    override def toString(): String = {
      val buf = new StringBuilder
      val tpe = value.getClass.getSimpleName
      buf append "Predicate[%s]('when %s %s %s')".format(tpe, property, operator, value.toString)
      buf toString
    }
    
  }
  
  import com.galacticfog.gestalt.meta.api.errors._
  
  def safeGetInputJson(json: JsValue): Try[GestaltResourceInput] = Try {
    log.debug(s"safeGetInputJson([json]")

    implicit def jsarray2str(arr: JsArray) = arr.toString

    json.validate[GestaltResourceInput].map {
      case resource: GestaltResourceInput => resource
    }.recoverTotal { e => 
      log.error("Error parsing request JSON: " + JsError.toFlatJson(e).toString)
      throw new BadRequestException(JsError.toFlatJson(e).toString)
    }
  }


  /* TODO:
   * Use the property name to decide whether tests are performed against the 'target' or the payload 
   * converted to a resource.
   */
  def resolveTestTarget(
      user: AuthAccountWithCreds,
      org: UUID, 
      target: ResourceLike, 
      payload: JsValue, 
      predicate: Predicate[Any]): ResourceLike = {
    
    val input = safeGetInputJson(payload).get
    
    /*
     * TODO: Obviously we can't test for all individual properties like this.
     * Need some component that can figure this out based on target.resourceType (or something).
     */
    
    if (predicate.property == "containers.count") target
    else {
      controllers.util.MetaController.inputWithDefaults(
          org = org, 
          input = input, 
          creator = user)
    }
  }
  
  def decide(
      user: AuthAccountWithCreds, 
      org: UUID,
      target: ResourceLike, 
      payload: JsValue, 
      predicate: Predicate[Any], 
      effect: Option[String] = None): Either[String,Unit] =  {
    
    
    val effectiveTarget = resolveTestTarget(user, org, target, payload, predicate)
    val props = propertyHandler(effectiveTarget.typeId)
    val test = props.getValue(effectiveTarget, predicate.property).get

    //if (test == EmptyProperty) throw new RuntimeException(s"Found EmptyProperty for => ${predicate}")
    
    log.debug(s"Testing: ${predicate.toString}")
    log.debug(s"Test-Value: " + test)
    
    //
    // TODO: To test containers.count, i need the environment. To test container.image, i need the container Json as a resource.
    //
    
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
      case ResourceIds.Container   => EnvironmentProperties
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