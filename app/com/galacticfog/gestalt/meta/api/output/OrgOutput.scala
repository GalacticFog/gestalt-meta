package com.galacticfog.gestalt.meta.api.output

import play.api.libs.json._

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.util._
import org.joda.time.DateTime
import scalikejdbc._

import java.util.UUID

import com.galacticfog.gestalt.data.models._
import scalikejdbc._

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._

object OrgOutput {
  /**
   * Assemble an Org for output - injecting parent and children
   * properties as appropriate.
   */
  def buildOrg(id: UUID): Option[GestaltResourceInstance] = {
    val org = ResourceFactory.findById(ResourceIds.Org, id)
    
    if (org.isEmpty) None
    else {
      val relatives = ResourceFactory.getOrgWithRelatives(id)
      val props = buildOrgProps(id)
      if (props.isEmpty) org else Some(org.get.copy(properties = props))      
    }
  }

  /**
   * Injects parent and children Org properties into a given Org
   * with data from the instance closure table.
   */
  def buildOrgProps(orgId: UUID /*, os: Seq[(GestaltResourceInstance, Int)] */): Option[Hstore] = {

    val os = ResourceFactory.getOrgWithRelatives(orgId)
    
    def safeAddLink(org: Option[GestaltResourceInstance], links: Seq[ResourceLink]) = {
      if (org.isDefined) links :+ toLink( 
          ResourceIds.Org, org.get.id, name = Some(org.get.name), orgId = org.get.orgId ) else links
    }
    
    /*
     * Determine if the given org is a child of current. Return it if yes,
     * return None if not.
     */
    def testChildOrg(data: (GestaltResourceInstance, Int)) = {
      if (data._1.id != orgId && data._2 != -1) Some(data._1) else None
    }
    
    /*
     * The query returns a flat list of orgs - the parent org has a rel == -1
     * This code below recursively iterates over the orgs asserting if the ID is NOT 
     * the target org (the one we're building) and column::rel != -1, 
     * then it must be a child org, so add it to the list.
     */
    def loop(cs: Seq[ResourceLink], rs: Seq[(GestaltResourceInstance, Int)]): Seq[ResourceLink] = rs match {
      case Nil => cs
      case h :: t => {
        /* Add org as ResourceLink to list if it is a child */
        loop( safeAddLink( testChildOrg( h ), cs), t)
      }
    }
    
    /* Get parent org by selecting element where column::rel == -1 */
    val parent = os find { x => x._2 == -1 } map { _._1.id }//{ y => toLink( y._1 ) }

    /* Get the current org - the one we're building */
    val c = (os find { x => x._1.id == orgId }).get._1
    
    /* Recursively build child list */
    val children = loop(Seq(), os) map { _.id.toString }

    
    /* Add parent, children or both to map and return */
    //
    // TODO: This is gross - refactor upsertProp* to deal with
    // Option[Tuple]s and this can be a one-liner.
    //
    if (parent.isDefined && !children.isEmpty) {    
      Some( upsertProperties(c.properties, 
            "parent" -> parent.get.toString, 
            "children" -> children.mkString(",")) )
    } else if (parent.isDefined) 
        Some(upsertProperty(c.properties, "parent" -> parent.get.toString))
      else if (!children.isEmpty)
        Some(upsertProperty(c.properties, "children" -> children.mkString(",")))
      else c.properties
  }  
  
  
  
  private def upsertProperties(properties: Option[Hstore], nvp: Tuple2[String,String]*) = {
    def loop(ps: List[Tuple2[String,String]], acc: Hstore): Hstore = {
      ps match {
        case Nil => acc
        case h :: t => {
          loop(t, acc ++ upsertProperty(properties, h))
        }
      }
    }
    val m: Map[String,String] = if (properties.isEmpty) Map() else properties.get
    loop(nvp.toList, m)
  }
  
  /**
   * Add or update a property in an Hstore Map.
   */
  private def upsertProperty(properties: Option[Hstore], nvp: Tuple2[String,String]) = {
    if (properties.isDefined) {

      if(properties.contains(nvp._1)) {
        /* name/value pair exists in prop map - create new Map replacing value */
        (properties.get - nvp._1) ++ Map( nvp )
      } else {
        /* name/value pair does not exist in prop map - add it */
        properties.get ++ Map( nvp )
      }
    } else {
      /* prop map is empty - create new Map from name/value pair */
      Map( nvp )
    }
  }
}