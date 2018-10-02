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
   * properties with data from the instance closure table.
   */  
  def buildOrgProps(org: UUID): Option[Hstore] = { 
    val os = ResourceFactory.findOrgParentAndChildren(org)
    val m  = os
      .groupBy { case(lvl,orgs) => lvl }
      .map { case(lvl,lvlset) =>
        lvl -> lvlset.map { _._2.id }
      }

    Option {
      os.find(_._1 == 0).get._2.properties.get ++
        (if (m.get(-1).isEmpty) Map() else Map("parent"   -> m(-1).mkString(",")) ) ++
        (if (m.get(1).isEmpty ) Map() else Map("children" -> m(1).mkString(",")) )
    }
  }
     
}