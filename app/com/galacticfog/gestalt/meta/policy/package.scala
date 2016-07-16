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
  
// THIS METHOD IS CURRENTLY UNUSED - BUT DO NOT DELETE YET! (sy)
//  def resolveTestTarget(
//      user: AuthAccountWithCreds,
//      org: UUID, 
//      target: ResourceLike, 
//      payload: JsValue, 
//      predicate: Predicate[Any]): ResourceLike = {
//    
//    val input = safeGetInputJson(payload).get
//    
//    /*
//     * TODO: Obviously we can't test for all individual properties like this.
//     * Need some component that can figure this out based on target.resourceType (or something).
//     */
//    
//    if (predicate.property == "containers.count") target
//    else {
//      controllers.util.MetaController.inputWithDefaults(
//          org = org, 
//          input = input, 
//          creator = user)
//    }
//  }

  
  def decide(
      user: AuthAccountWithCreds, 
      target: ResourceLike, 
      predicate: Predicate[Any], 
      effect: Option[String]): Either[String,Unit] =  {
    
    val effectiveTarget = target
    
    val props = propertyHandler(effectiveTarget.typeId)
    val test = props.getValue(effectiveTarget, predicate.property).get
    
    log.debug(s"Testing: ${predicate.toString}")
    log.debug(s"Test-Value: " + test)
    
    if (props.compare(test, normalizedPredicate(predicate))) Right(Unit) else Left(predicate.toString)
  }  
  
  
  def toPredicate(s: String) = {
    val j = Json.parse(s)
    val value = (j \ "value")
    Predicate[Any](
      property = (j \ "property").as[String],
      operator = (j \ "operator").as[String],
      value    = (j \ "value").as[JsValue]
    )
  }
  
  
  def DebugLogRules(parentId: UUID, rules: Seq[GestaltResourceInstance]) {
    if (log.isDebugEnabled) {
      if (rules.isEmpty) log.debug(s"No Policy Rules found for resource $parentId")
      else {
        log.debug(s"Policy Rules found for resource $parentId")
        rules foreach { r => 
          log.debug("%s - %s - %s".format(
              r.id, r.name, r.properties.get("actions")))
        }
      }
    }
  }
  
  
  def effectiveRules(parentId: UUID, ruleType: Option[UUID] = None, actions: Seq[String] = Seq()): Seq[GestaltResourceInstance] = {
    val rules = for {
      p <- ResourceFactory.findChildrenOfType(ResourceIds.Policy, parentId)
      r <- ResourceFactory.findChildrenOfSubType(ResourceIds.Rule, p.id)
    } yield r
    
    DebugLogRules(parentId, rules)
    
    def array(sa: String) = Json.parse(sa).validate[Seq[String]].get
    def matchAction(a: Seq[String], b: Seq[String]) = !(a intersect b).isEmpty
    def matchType(test: UUID) = ( test == (ruleType getOrElse test) )
    
    if (actions.isEmpty) rules else {
      rules filter { r =>
        matchType(r.typeId) &&
        matchAction(array(r.properties.get("actions")), actions)
      }
    }
  }  
  
  /*
   * TODO: This is temporary. This gets us around the issue where we allow limit + 1 containers to be
   * created when the operator is '<='.
   */
  protected [policy] def normalizedPredicate(predicate: Predicate[_]) = {
    if (predicate.property == "containers.count") {
      predicate operator match {
        case "<=" => predicate.copy(operator = "<")
        case ">=" => predicate.copy(operator = ">")
        case _ => predicate
      }
    } else predicate
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