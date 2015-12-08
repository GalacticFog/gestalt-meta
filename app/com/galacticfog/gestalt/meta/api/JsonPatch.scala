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


case class PatchHandler(typeId: UUID, instanceId: UUID, doc: PatchDocument) {
  
  def applyPatch(identityType: UUID, identity: UUID) = Try {
    
    val res = ResourceFactory.findById(instanceId) getOrElse {
      throw new ResourceNotFoundException(s"Resource not found '$instanceId'.")
    }
    
    // Get new resource instance with altered properties.
    val newres = updateProperties(doc.op.toList, res)

    // Save instance with new data in the database
    ResourceFactory.update(newres, identity).get
  }
  
  def setAttribute(p: PatchOp, r: GestaltResourceInstance) = strip(p.path) match {
    case Attributes.Org           => ??? // function to safely set Org
    case Attributes.Owner         => ??? // function to safely set Owner
    case Attributes.Name          => r.copy(name = p.value.toString())
    case Attributes.ResourceState => r.copy(state = UUID.fromString(p.value.toString))
    case Attributes.Description   => r.copy(description = Some(p.value.toString))
    case _                        => throw new BadRequestException(s"Invalid path '${p.path}'")
  }
  
  def replaceProperty(pname: String, pvalue: JsValue, r: GestaltResourceInstance) = {
    if (!r.properties.isDefined || !r.properties.get.contains(pname)) {
      throw new BadRequestException(s"Property not defined '$pname'. No changes made.")
    }

    val props = r.properties.get
    val newprops = Some((props - pname) ++ Map(pname -> trimquotes(pvalue.toString)))
    r.copy(properties = newprops)
  } 
  
  def updateAttributes(ps: List[PatchOp], res: GestaltResourceInstance): GestaltResourceInstance = {
    ps match {
      case Nil    => res
      case h :: t => updateAttributes(t, setAttribute(h, res))
    }
  }
  
  def updateProperties(ps: List[PatchOp], res: GestaltResourceInstance): GestaltResourceInstance = {
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
    if (!s.trim.startsWith("/properties/")) {
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

  def strip(s: String) = { 
    if (s.trim.startsWith("/")) s.trim.drop(1)
    else throw new BadRequestException(s"Path must begin with '/', found: $s")
  }
  
  private object Attributes {
    val Id = "id"
    val Org = "org_id"
    val Name = "name"
    val Description = "description"
    val ResourceType = "resource_type_id"
    val ResourceState = "resource_state_id"
    val Owner = "owner"    
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



