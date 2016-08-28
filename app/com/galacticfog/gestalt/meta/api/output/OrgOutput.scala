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
   * Injects parent and children Org properties into a given Org
   * with data from the instance closure table.
   */  
  def buildOrgProps2(org: UUID): Option[Hstore] = {
    val os = ResourceFactory.findOrgParentAndChildren(org)
    val (parent, other) = os partition { _._1 == -1 }
    val (self, children) = other partition { _._1 == 0 }
    
    val pmap: Map[String,String] = if (parent.isEmpty) Map() else Map("parent" -> parent(0)._2.id)
    val cmap: Map[String,String] = if (children.isEmpty) Map() else {
      val cids = children.map( _._2.id.toString ).mkString(",")
      Map("children" -> cids)
    }
    Option( self(0)._2.properties.get ++ pmap ++ cmap )
  }
  
  def buildOrgProps(org: UUID): Option[Hstore] = { 
    val os = ResourceFactory.findOrgParentAndChildren(org)
    val m  = os groupBy  { _._1 } map { x => 
      (x._1 -> (x._2 map { _._2.id })) }
    
    Option {
      os(0)._2.properties.get ++ 
        (if (m.get(-1).isEmpty) Map() else Map("parent"   -> m(-1).mkString(",")) ) ++ 
        (if (m.get(1).isEmpty ) Map() else Map("children" -> m(1).mkString(",")) )
    }
  }
     
}