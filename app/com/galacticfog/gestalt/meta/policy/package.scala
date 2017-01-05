package com.galacticfog.gestalt.meta


import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data._
import java.util.UUID
import scala.util.{Try,Success,Failure}

import _root_.play.api.libs.json._
import _root_.play.api.Logger
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.errors._
import controllers.util.JsonUtil
import controllers.util.RequestOptions
import scala.language.postfixOps
import com.galacticfog.gestalt.json.Js

package object policy {

  private val log = Logger(this.getClass)

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

  def propertyContainerCount(predicate: Predicate[Any], opts: RequestOptions, resourceJson: JsValue): JsValue = {
    log.debug(s"propertyContainerCount(policyOwner = ${opts.policyOwner}")
    val env = opts.policyOwner.get
    JsNumber(ResourceFactory.findChildrenOfType(ResourceIds.Container, env).size)
  }
  
  /**
   * Get container.properties.num_instances as a JsNumber.  This function is necessary because rules
   * may check container.properties.num_instances in events other than 'scale' (i.e. on
   * container creation).  This function determines the value to return in the order below
   * (first value found will be used):
   * 
   * 1.) RequestOptions.data -> scaleTo
   * 2.) container.properties.num_instances
   * 3.) 1
   */  
  def propertyContainerNumInstances(predicate: Predicate[Any], opts: RequestOptions, resourceJson: JsValue): JsValue = {

    opts.data.flatMap(_.get("scaleTo")).fold {
      val path = dot2slash("container.properties.num_instances")
      Js.find(resourceJson.as[JsObject], path).fold {
        JsNumber(1)
      }{ ni => JsNumber(ni.as[Int]) }
    }{ scaleTo => JsNumber(scaleTo.toInt) }
  }
  

  private def rte(message: String) = new RuntimeException(message)
  
  
  type PropertyLookup = (Predicate[Any],RequestOptions,JsValue) => JsValue
  
  private val propertyFunctions: Map[String, PropertyLookup] = Map(
    "containers.count" -> propertyContainerCount,
    "container.properties.num_instances" -> propertyContainerNumInstances
  )
  
  private[policy] def dot2slash(dotpath: String): String = {
    dotpath.split("""\.""").drop(1).mkString("/")
  }
  
  def decideJson[U](
    user: U,
    target: GestaltResourceInstance,
    rule: GestaltResourceInstance,
    effect: Option[String],
    opts: RequestOptions): Either[String, Unit] = {
    
    val json = Output.renderInstance(target)
    val predicate = toPredicate(rule.properties.get("eval_logic"))
    val path = dot2slash(predicate.property) //.split("""\.""").drop(1).mkString("/")
    
    log.debug(s"Property path: $path")
    log.debug(s"Predicate.Property : ${predicate.property}")
    log.debug(s"Testing: ${predicate.toString}")
    
    /*
     * Check if we have a lookup function for the property - execute it to
     * get the test-value if we do, otherwise try to get the value from the
     * target JSON.
     */
    
    val testValue = propertyFunctions.get(predicate.property).fold {
      Js.find(json.as[JsObject], path)
    }{ f =>
      log.debug(s"Found lookup function for property : '${predicate.property}'")
      Option(f(predicate, opts, json)) 
    }
    
    log.debug("Test-Value : " + testValue)
    testValue.fold(missingProperty(rule, predicate)) { test =>
      val p1 = normalizedPredicate(predicate)
      if (compareJson(test, p1)) Right(Unit) else Left(predicate.toString)      
    }

  }
  
  def isStrict(rule: GestaltResourceInstance) = {
    rule.properties.get.get("strict").map(_.toBoolean) getOrElse false
  }
  
  def missingProperty(rule: GestaltResourceInstance,predicate: Predicate[_]): Either[String,Unit] = {
    log.debug(s"Testing for missing property: ${predicate.property}")
    
    if (isStrict(rule)) {
      Left(s"Missing property ${predicate.property}. The rule is strict.")
    } else Right(Unit)
  }
  
  
  def isNegativeOp(op: String): Boolean = {
    op.startsWith("!") || op.startsWith("not")
  }
  
  
  def compareJson[T](test: JsValue, predicate: Predicate[T]) = {
    val value = toJsValue(predicate.value.toString)
    
    log.debug(s"TESTING : (${test.getClass.getSimpleName}, ${value.getClass.getSimpleName})")
    
    (test, value) match {
      case (JsString(_), JsString(_))   =>
        CompareSingle.compare(test, value, predicate.operator)
      case (JsBoolean(_), JsBoolean(_)) => 
        CompareSingle.compare(test, value, predicate.operator)
      case (JsNumber(_), JsNumber(_))   => 
        CompareSingleNumeric.compare(test.as[JsNumber], value.as[JsNumber], predicate.operator)
      case (JsArray(_), JsArray(_))     => 
        CompareArrayToArray.compare(test.as[JsArray], value.as[JsArray], predicate.operator)
      case (jsvalue, JsArray(_))        => 
        CompareSingleToArray.compare(value.as[JsArray], jsvalue, predicate.operator)
      case _ => throw new RuntimeException(s"Unhandled comparison. found: (${test.getClass.getSimpleName}, ${value.getClass.getSimpleName})")
    }
  }

  def toJsValue(s: String): JsValue = s.trim match {
    case a if isObjectString(s)  => Json.parse(s).as[JsObject]
    case c if isNumericString(s) => Json.parse(s).as[JsNumber]
    case d if isBooleanString(s) => Json.parse(s.toLowerCase).as[JsBoolean]
    case b if isArrayString(s)   => {
      Json.toJson(s.split(",") map { v => toJsValue(v.trim) })
    }
    case _ => JsString(s.replaceAll("\"", ""))
  }
  
  def isNumericString(s: String) = {
    val test = s.trim
    Try(test.toInt).isSuccess || Try(test.toDouble).isSuccess
  }

  def isBooleanString(s: String) = {
    val test = s.trim.toLowerCase
    Try(test.toBoolean).isSuccess
  }
  
  def isArrayString(s: String) = {
    val test = s.trim
    !isObjectString(s) && s.contains(",")
  }
  
  def isObjectString(s: String): Boolean = {
    val test = s.trim
    (test startsWith "{") && (test endsWith "}")
  }

  def toPredicate(s: String) = {
    val j = Json.parse(s)
    val value = (j \ "value")
    Predicate[Any](
      property = (j \ "property").as[String],
      operator = (j \ "operator").as[String],
      value = (j \ "value").as[JsValue])
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
    def matchType(test: UUID) = (test == (ruleType getOrElse test))

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
  protected[policy] def normalizedPredicate(predicate: Predicate[_]) = {
    if (Seq("containers.count", "container.scale").contains(predicate.property)) {
      predicate operator match {
        case "<=" => predicate.copy(operator = "<")
        case ">=" => predicate.copy(operator = ">")
        case _    => predicate
      }
    } else predicate
  }

  protected[policy] def propertyHandler(typeId: UUID): ResourceProperties = {
    typeId match {
      case ResourceIds.Environment => EnvironmentProperties
      case ResourceIds.Container   => EnvironmentProperties
      case ResourceIds.Workspace   => DefaultResourceProperties
      case _                       => DefaultResourceProperties
    }
  }

  protected[policy] def getEffect(effect: String) = {
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