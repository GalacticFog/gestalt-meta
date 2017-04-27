package com.galacticfog.gestalt.meta.auth


import java.util.UUID

import scala.util.{Try,Success,Failure}

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import controllers.util._
import play.api.{Logger => log}
import play.api.libs.json._
import play.api.mvc.Result
import scala.concurrent.Future
//import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.annotation.tailrec
import scala.language.postfixOps
import com.galacticfog.gestalt.data.models.GestaltResourceType
import com.galacticfog.gestalt.data.bootstrap.{ActionInfo,LineageInfo}


trait ActionMethods {
  
  //
  // TODO: This type is unnecessary - use ActionInfo from gestalt-meta-repo
  //
  case class ActionSet(prefix: String, verbs: Seq[String])
  implicit lazy val actionSetFormat = Json.format[ActionSet]
  
  /**
   * Get a list of fully-qualified action names for the given type. The returned
   * Seq will include actions defined on any super-types of the given type.
   * 
   * @param typeId ID of the ResourceType to get actions for
   * @return Seq[String] of fully-qualified action names
   */
  def getSelfActions(typeId: UUID): Seq[String] = {
    
    TypeFactory.findById(typeId).fold {
      throw new IllegalArgumentException(s"ResourceType with ID '$typeId' not found.")
    }{ tpe =>
      
      def go(prefix: String, rss: Seq[UUID], acc: Set[String]): Set[String] = {
        rss match {
          case Nil => acc
          case rtype :: tail => {
            // Prepend prefix to all verbs found for current ResourceType.
            val actions = verbs(getType(rtype)) map { "%s.%s".format(prefix, _) }
            go(prefix, tail, actions.toSet ++ acc)
          }
        }
      }
      
      // List of all resource-type IDs in the inheritance hierarchy
      // including that of the target resource.
      val resourceSet = typeId +: TypeFactory.getAncestorIds(typeId)
      go(prefix(tpe), resourceSet, Set.empty).toSeq
    }   
  }
  
  def prefixFromResource(resource: GestaltResourceInstance): Option[String] = {
    for {
      tpe    <- TypeFactory.findById(resource.typeId)
      prefix <- Option(prefix(tpe))
    } yield prefix
  }
  
  def actionInfo(typeId: UUID): ActionInfo = {
    actionInfo(getType(typeId))
  }
  
  def actionInfo(tpe: GestaltResourceType): ActionInfo = {
    tpe.properties.get.get("actions").fold(
      throw new RuntimeException(
        s"Could not find ResourceType.properties.actions for type ${tpe.typeId}")
    )(JsonUtil.safeParse[ActionInfo]( _ ))
  }
  
  private[auth] def getLineageInfo(tpe: GestaltResourceType): Option[LineageInfo] = {
    tpe.properties.get.get("lineage") map { JsonUtil.safeParse[LineageInfo]( _ ) }
  }
  
  /** 
   * Get the action-prefix specified on the given ResourceType 
   */
  private[auth] def prefix(tpe: GestaltResourceType): String = {
    JsonUtil.safeParse[ActionSet](
      tpe.properties.get("actions")
    ).prefix
  }
  
  /**
   * Get list of action-verbs specified on the given ResourceType 
   */
  private[auth] def verbs(tpe: GestaltResourceType): Seq[String] = {
    (for {
      p <- tpe.properties
      a <- p.get("actions")
      s <- Option(JsonUtil.safeParse[ActionSet](a))
      r <- Option(s.verbs)
    } yield r) getOrElse Seq.empty
  }  
  
  /** 
   * Get the ResourceType indicated by typeId - exception if not found 
   */
  private[auth] def getType(typeId: UUID): GestaltResourceType = {
    TypeFactory.findById(typeId) getOrElse {
      throw new IllegalArgumentException(s"ResourceType with ID '$typeId' not found.")
    }
  }  
  

  /**
   * Get a list of all Entitlement actions that must be set on a new Resource.
   * This contains all self actions, and the actions of all child-types.
   */
  def getNewResourceActionSet(typeId: UUID): Set[String] = {
    
    val tpe = getType(typeId)
   
    // Get actions for the target resource.
    val selfActions = getSelfActions(typeId)
    
    // Get actions for all child-types
    val childActions = getChildActions(tpe)
    
    (selfActions ++ childActions).toSet
  }
  
  
  /**
   * Get a list of all actions specified on all child-types of the given type.
   * Includes actions inherited by super-types of the children.
   */
  def getChildActions(tpe: GestaltResourceType): Seq[String] = {
    def loop(types: Seq[UUID], acc: Seq[String]): Seq[String] = {
      types match {
        case Nil => acc
        case rtype :: tail => loop(tail, acc ++ getSelfActions(rtype))
      }
    }
    
    (for {
      info     <- getLineageInfo(tpe)
      children <- info.child_types
      actions  <- Option(loop(children, Seq.empty))
    } yield actions) getOrElse Seq.empty
  }
  
  def getChildActions(typeId: UUID): Seq[String] = {
    getChildActions(getType(typeId))
  }  
  
}