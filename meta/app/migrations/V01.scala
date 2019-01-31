package migrations


import java.util.UUID

import scala.util.{Either, Left, Right}
import scala.util.{Try, Success, Failure}

import com.galacticfog.gestalt.data.CoVariant
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.session
import play.api.libs.json._

/**
 * Change `rule.properties.actions` to `rule.properties.match_actions`.
 * Update the type schema and all existing instances.
 */
class V1() extends MetaMigration() {

  private val acc = new MessageAccumulator()
  
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {
    
    perform(identity) match {
      case Success(_) => {
        Right(MigrationStatus(
            status = "SUCCESS",
            message = s"Upgrade successfully applied",
            succeeded = Some(acc.messages),
            None).toJson)
      }
      case Failure(e) => {
        Left(MigrationStatus(
            status = "FAILURE",
            message = s"There were errors performing this upgrade: ${e.getMessage}",
            succeeded = Some(acc.messages),
            errors = Some(Seq(e.getMessage))).toJson)
      }
    }
  }
  
  private[migrations] def perform(identity: UUID) = Try {
    /*
     * Get all the resource-types upfront.
     */
    val baseRule = TypeFactory.findById(ResourceIds.Rule) getOrElse {
      throw new RuntimeException(s"Type ${Resources.Rule} not found. Ensure database has been initialize.")
    }
    val limitRule = TypeFactory.findById(ResourceIds.RuleLimit) getOrElse {
      throw new RuntimeException(s"Type ${Resources.RuleLimit} not found. Ensure database has been initialize.")
    }
    val eventRule = TypeFactory.findById(ResourceIds.RuleEvent) getOrElse {
      throw new RuntimeException(s"Type ${Resources.RuleEvent} not found. Ensure database has been initialize.")
    }
    
    // These are the property names all rules should have in common (now).
    val commonPropNames = Seq("actions", "eval_logic", "filter", "defined_at", "parent")
    
    // These are the property names all rules should have in common going forward (rename: actions -> match_actions)
    val expectedPropNames = Seq("match_actions", "eval_logic", "filter", "defined_at", "parent").sorted
    
    /* ----------------------------------------------------------------------------------------------------------------
     * Both Limit and Event rule have identical versions of the common properties, so it doesn't matter which
     * one we use as the prototype.  Here we take the props from RuleLimit, changing the name of 'action' to 'match_actions'
     */
    val newProps = getPropertiesByName(limitRule.id, commonPropNames).map { p =>
      if (p.name == "actions") p.copy(name = "match_actions") else p
    }
  
    /* ----------------------------------------------------------------------------------------------------------------
     * Clone properties from sub-type to super-type
     * Properties are associated with a type via foreign-key value 'applies_to'. To 'clone' a property from
     * one type to another is a simple matter of copying the property and changing 'applies_to' to the ID of 
     * the new type (of course it's UUID also changes)
     */
    val cloneFailures = cloneProperties(identity, baseRule, newProps).collect { case Failure(e) => e.getMessage }
    if (cloneFailures.nonEmpty) {
      throw new GenericApiException(500, 
          s"There were error clonging properties to type '${Resources.Rule}'",
            Some(Json.toJson(cloneFailures)))
    }
    val propNamesAfter = PropertyFactory.findByType(baseRule.id).map(_.name).sorted
    verifyClonedProps(expectedPropNames, given = propNamesAfter)
    
    acc push s"Cloned properties ${bracketString(commonPropNames)} from ${Resources.RuleLimit} to ${Resources.Rule}"
    acc push s"Renamed ${Resources.Rule}.properties.actions to 'match_actions'"
    
    
    /* ----------------------------------------------------------------------------------------------------------------
     * Remove common properties from RuleLimit
     */
    removePropertiesFromType(limitRule.id, commonPropNames).get
    verifyPropsRemoved(limitRule.id, commonPropNames)
    acc push s"Removed properties ${bracketString(commonPropNames)} from ${Resources.RuleLimit}"
    
    
    /* ----------------------------------------------------------------------------------------------------------------
     * Remove common properties from RuleEvent
     */
    removePropertiesFromType(eventRule.id, commonPropNames).get
    verifyPropsRemoved(eventRule.id, commonPropNames)
    acc push s"Removed properties ${bracketString(commonPropNames)} from ${Resources.RuleEvent}"
    
    
    /* ----------------------------------------------------------------------------------------------------------------
     * Update ALL instances that are a sub-type of Rule with new action property name.
     * Lookup any instances that are sub-types of Rule and update the 'actions' property name to 'match_actions'
     */

    val allRules = ResourceFactory.findAllOfType(CoVariant(ResourceIds.Rule))
    val updated = updateInstancePropertyNames(allRules, Map("actions" -> "match_actions")).map { r =>
      val upd = ResourceFactory.update(r, identity).get
      acc push s"Updated ${ResourceLabel(upd.typeId)} [${upd.id}] with new property-mappings."      
    }
    
    acc push s"Instance updates complete. ${updated.size} instances updated."
    acc push "Migration complete"
  }

  
  private[migrations] def updateInstancePropertyNames(
      rss: Seq[GestaltResourceInstance], propMap: Map[String, String]): Seq[GestaltResourceInstance] = {
 
    rss.map { r =>
      val oldProps = r.properties.get
      val newProps = oldProps.map { case (k, v) =>
        val key = if (propMap.contains(k)) propMap(k) else k
        (key, v)
      }
      r.copy(properties = Some(newProps))
    }
  }
  
  /**
   * Ensure the list of named properties no longer exist on the given Resourcetype,
   */
  private[migrations] def verifyPropsRemoved(tpe: UUID, expectedGone: Seq[String]): Unit = {
    val props = PropertyFactory.findByType(tpe).collect { case p if expectedGone.contains(p.name) => p.name }
    assert(props.isEmpty) {
      s"Errors removing properties from ${Resources.RuleLimit}. The following properties were not removed: ${bracketString(props)}"
    } 
  }
  
  /**
   * Ensure the content of two string sequences match each other exactly.
   */
  private[migrations] def verifyClonedProps(expected: Seq[String], given: Seq[String]) = {
    assert(given == expected) {
      val expectedStr = bracketString(expected)
      val foundStr = bracketString(given)
      
      s"Something went wrong cloning properties to ${Resources.Rule}. expected: ${expectedStr}. found: ${foundStr}"
    }    
  }
  
  /**
   * Get the named properties (GestaltTypeProperty) from the given ResourceType
   */
  private[migrations] def getPropertiesByName(tpe: UUID, propNames: Seq[String]) = {
    val pnames = propNames.map(_.trim.toLowerCase)
    def takeit(name: String) = pnames.contains(name.trim.toLowerCase)
    
    PropertyFactory.findByType(tpe).collect { case p if takeit(p.name) => p } 
  }
  
  /**
   * Copy the list of given properties to the given ResourceType.
   * This affects persisted data - the cloned properties are cloned as children
   * of the given Type. 
   */
  private[migrations] def cloneProperties(identity: UUID, tpe: GestaltResourceType, props: Seq[GestaltTypeProperty]): Seq[Try[GestaltTypeProperty]] = {
    val existing = PropertyFactory.findByType(tpe.id).map(_.name.trim.toLowerCase)
    val given = props.map(_.name.trim.toLowerCase)
    val conflicts = existing.intersect(given)
    
    if (conflicts.nonEmpty) {
      throw new ConflictException(
          s"Cannot clone given properties to type - there are conflicts: ${bracketString(conflicts)}")
    }
    props.map { p =>
      PropertyFactory.create(identity)(p.copy(id = UUID.randomUUID(), appliesTo = tpe.id))
    }
  }
  
  /**
   * Delete the given list of properties from the given ResourceType
   */
  private[migrations] def removePropertiesFromType(tpe: UUID, propNames: Seq[String]): Try[Unit] = Try {
    val pnames = propNames.map(_.trim.toLowerCase)
    
    def killit(name: String) = pnames.contains(name.trim.toLowerCase)
    
    val deleteResults = TypeFactory.findById(tpe).fold {
      throw new ResourceNotFoundException(s"ResourceType '${tpe}' not found.")
    }{ _ =>
      PropertyFactory.findByType(tpe).collect { case p if killit(p.name) => 
        PropertyFactory.hardDeleteProperty(p.id)
      }
    }
    val failures = deleteResults.collect { case Failure(e) => JsString(e.getMessage) }
    if (failures.nonEmpty) 
      throw new GenericApiException(500, "Failure deleting TypeProperties:", Some(Json.toJson(failures)))
    else ()
  }
  
  private[migrations] def assert(predicate: => Boolean)(errorMessage: String): Unit = {
    if (!predicate) throw new RuntimeException(errorMessage)
  }
  
  private[migrations] def bracketString[A](seq: Seq[A]): String = {
    seq.mkString("[",",","]")
  }  
  
}
