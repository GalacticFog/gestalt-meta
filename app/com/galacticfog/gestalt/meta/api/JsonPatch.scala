package com.galacticfog.gestalt.meta.api


import play.api.libs.json._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.util._
import com.galacticfog.gestalt.data.models._
import scala.util.{Try,Success,Failure}
import java.util.UUID

import com.galacticfog.gestalt.meta.api.output._

/* TODO: Move this method out of controllers.util */
import controllers.util.trimquotes
import play.api.{ Logger => log }

import com.galacticfog.gestalt.meta.api.errors._

import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
  

/*
 * TODO: Refactor.
 * 
 * There's no real need for PatchHandler to be a class - it can be a singleton:
 * 
 * PatchHandler.applyPatch(ops: Seq[PatchOp], res: GestaltResourceInstance)
 */

//case object ResourcePatch extends PatchHandle[UUID, Try[GestaltResourceInstance]] {
//  
//  def applyPatch(target: UUID, patch: PatchDocument) = Try {
//    
//    val res = safeGetResource(target)
//    
//    
//    ResourceFactory.update(res).get
//  }
//  
//  def safeGetResource(id: UUID) = ResourceFactory.findById(id) getOrElse {
//    throw new IllegalArgumentException(s"Resource not found '$id'.")
//  }
//  
//}


protected[api] object Attributes {
  val Id = "id"
  val Org = "org"
  val Name = "name"
  val Description = "description"
  val ResourceType = "resource_type"
  val ResourceState = "state"
  val Owner = "owner"
}

//trait AttributePatch[T <: BaseResource] {
//  
//  /* 
//   * TODO: Have this return an Option (None indicating no match) so i can compose this
//   * with another attribute setter for TypeProperty (which has a bunch of extra fields).
//   */
//  def setAttribute(p: PatchOp, r: GestaltResourceInstance) = stripSlash(p.path) match {
//    case Attributes.Org           => ??? // function to safely set Org
//    case Attributes.Owner         => ??? // function to safely set Owner
//    case Attributes.Name          => r.copy(name = p.value.toString())
//    case Attributes.ResourceState => r.copy(state = UUID.fromString(p.value.toString))
//    case Attributes.Description   => r.copy(description = Some(p.value.toString))
//    case _                        => throw new BadRequestException(s"Invalid path '${p.path}'")
//  }
//  
//}

case class PatchHandler(typeId: UUID, instanceId: UUID, doc: PatchDocument) {
  
  val AttributePaths = List("id", "org", "owner", "name", "description", "resource_type", "state")
  val PROPERTY_PREFIX = "/properties/"
  
  def applyPatch(identityType: UUID, identity: UUID) = Try {
    
    val res = ResourceFactory.findById(instanceId) getOrElse {
      throw new ResourceNotFoundException(s"Resource not found '$instanceId'.")
    }
    
    // Get new resource instance with altered properties.
    val ops = doc.op.toList

    assertValidAttributeOps(ops)
    
    val newres = updateAttributes(getAttributeOps(ops), updateProperties(getPropertyOps(ops), res))

    // Save instance with new data in the database
    ResourceFactory.update(newres, identity).get
  }

  /*
   * ops that DO NOT target properties, must be targeting attributes. Ensure
   * that anythin not targeting a property is a valid attribute name. Properties
   * will be validated by the resource factory when it attempts to actually
   * update the resource. 
   */
  def assertValidAttributeOps(ops: Seq[PatchOp]) = {
    val unknown = ops filter { o => !o.path.startsWith(PROPERTY_PREFIX) } filter { o => 
      !AttributePaths.contains(o.path.trim.drop(1)) 
    }
    if (!unknown.isEmpty) {
      throw new BadRequestException(errorString(unknown))
    }
  }
  
  def errorString(ops: Seq[PatchOp]) = {
    val bad = (ops.map(_.path).mkString(", "))
    s"Invalid path(s) : [$bad]. No changes made."
  }
  
  def getAttributeOps(ops: Seq[PatchOp]): Seq[PatchOp] = 
    ops filter { o => AttributePaths.contains( o.path.drop(1) ) }
  
  def getPropertyOps(ops: Seq[PatchOp]) = ops filter { _.path.startsWith(PROPERTY_PREFIX) }
  
  def unquote(s: String) = s.replaceAll(""""""", "")
  
  def setAttribute(op: PatchOp, r: GestaltResourceInstance) = {
    val p = strip(op.path)
    
    p match {
      case Attributes.Org           => r.copy(orgId = UUID.fromString(unquote(op.value.toString)))
      case Attributes.Owner         => r.copy(owner = ownerFromJson(op.value))
      case Attributes.Name          => r.copy(name = unquote(op.value.toString()))
      case Attributes.ResourceState => r.copy(state = UUID.fromString(unquote(op.value.toString)))
      case Attributes.Description   => r.copy(description = Some(unquote(op.value.toString)))
      
      case Attributes.Id            => throw new BadRequestException(s"Resource ID cannot be modified.")
      case Attributes.ResourceType  => throw new BadRequestException(s"Resource Type cannot be modified.")
      case _                        => throw new BadRequestException(s"Invalid path '${op.path}'")
    }
  }
  

  def ownerFromJson(js: JsValue) = {
    js.validate[ResourceOwnerLink].map {
      case link: ResourceOwnerLink => link
    }.recoverTotal { e =>
      log.error("Error parsing request JSON: " + JsError.toFlatJson(e).toString)
      throw new BadRequestException("Invalid Resource Owner JSON. " + JsError.toFlatJson(e).toString)
    }
  }
  
  /**
   * Replace the named property with the new value in the resource's properties collection.
   */
  def replaceProperty(pname: String, pvalue: JsValue, r: GestaltResourceInstance) = {
    if (!r.properties.isDefined || !r.properties.get.contains(pname)) {
      throw new BadRequestException(s"Property not defined '$pname'. No changes made.")
    }
    val props = r.properties.get
    val newprops = Some((props - pname) ++ Map(pname -> trimquotes(pvalue.toString)))
    r.copy(properties = newprops)
  } 
  
  def updateAttributes(ps: Seq[PatchOp], res: GestaltResourceInstance): GestaltResourceInstance = {
    /*
     * TODO: Enforce Rules
     * - remove may only be called on fields that are *truly* optional
     * - add may only be called on missing optional fields
     * - move and copy may NEVER be called
     */
    
    ps match {
      case Nil    => res
      case h :: t => updateAttributes(t, setAttribute(h, res))
    }
  }
  
  def updateProperties(ps: Seq[PatchOp], res: GestaltResourceInstance): GestaltResourceInstance = {
    ps match {
      case Nil    => res
      case h :: t => {
        val pname = getPropertyName(h.path).get
        log.debug("Updating property: " + pname)
        // TODO: invoke appropriate function by op-type - here we just replace.
        updateProperties(t, replaceProperty(pname, h.value, res))
      }
    }
  }

  def getPropertyName(s: String): Try[String] = Try {
    if (!s.trim.startsWith(PROPERTY_PREFIX)) {
      throw new BadRequestException(s"Invalid path to property '$s'")
    }
    else {
      val cs = s.trim.drop(1).split("/").toList
      cs.size match {
        case 2 => cs(1)
        case _ => throw new BadRequestException(s"Invalid path to property '$s'") 
      }
    }
  }

  // TODO: Check for and strip trailing slash
  def strip(s: String) = { 
    if (s.trim.startsWith("/")) s.trim.drop(1)
    else throw new BadRequestException(s"Path must begin with '/', found: $s")    
  } 
}

case class PatchOp(op: String, path: String, value: JsValue) {
  if (List("add", "remove", "move", "copy", "test").contains(op.toLowerCase)) {
    throw new RuntimeException(s"The '$op' op is not currently supported.")
  }
  else if (!List("replace").contains(op.toLowerCase)) {
    throw new BadRequestException(s"Illegal op value '$op'.")
  }
}

case class PatchDocument(op: PatchOp*) extends Iterable[PatchOp] {
  override def iterator = op.iterator    
}

object PatchDocument {
  
  def fromString(json: String) = {
    fromJsValue(Json.parse(json))
  }
  
  def fromJsValue(json: JsValue): PatchDocument = {    
    json.validate[Seq[PatchOp]] match {
      case s: JsSuccess[Seq[PatchOp]] => PatchDocument(s.get : _*)
      case e: JsError => {
        throw new RuntimeException(JsError.toFlatJson(e).toString)
      }
    }
  }
  
  /**
   * Create a new PatchDocument from an existing one by filtering for
   * only those PatchOps beginning with the given prefix.
   */
  def fromPathPrefix(prefix: String, doc: PatchDocument, rebase: Boolean = false) = {
    val pre = if (!prefix.trim.endsWith("/")) prefix.trim + "/" else prefix.trim
    
    println("fromPrefix::patch : " + doc)
    println("fromPrefix::pre:: " + pre)
    
    def rb(o: PatchOp) = { 
      if (rebase) o.copy(path = o.path.drop(pre.size-1)) else o 
    } 
    
    PatchDocument(doc.op collect { 
      case op: PatchOp if op.path.startsWith(pre) => rb( op ) } : _*)
  }

}  



