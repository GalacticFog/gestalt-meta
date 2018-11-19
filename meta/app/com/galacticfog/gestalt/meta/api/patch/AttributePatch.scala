package com.galacticfog.gestalt.meta.api.patch


import scala.util.Try
import com.galacticfog.gestalt.data.models._
import java.util.UUID

import com.galacticfog.gestalt.patch._
import play.api.Logger


object AttributePatch {
  
  private[this] val log = Logger(this.getClass)
  
  def applyOps(r: ResourceLike, ops: Seq[PatchOp]): Try[GestaltResourceInstance] = {
    /*
     * Validate that all ops name valid attributes
     */
    def loop(ops: Seq[PatchOp], res: GestaltResourceInstance): GestaltResourceInstance = {
      ops match {
        case Nil => res
        case op :: tail => loop(tail, function(res, op))
      }
    }
    Try ( loop(ops.toList, r.asInstanceOf[GestaltResourceInstance]) )
  }
  
  
  private[api] def addAttribute(r: GestaltResourceInstance, op: PatchOp): GestaltResourceInstance = {
    log.debug(s"Entered addAttribute(_,op=$op)")
    toMatchPath(validateOp(PatchOps.Add, op, true).get) match {
      case Attributes.Description   => r.copy(description = Some(op.value.get.as[String]))
      case Attributes.Tags          => r.copy(tags = OpAddSeq(r.tags, op))
      case Attributes.Variables     => r.copy(variables = OpAddMap(r.variables, op))
      case _                        => throw new RuntimeException(s"Invalid path '${op.path}'")
    }
  }
  
  private[api] def OpAddSeq(seq: Option[Seq[String]], op: PatchOp): Option[List[String]] = {
    
    val q = seq getOrElse Seq()

    JsPointer.newPointer(op.path) match {
      case a @ JsArrayPointer(_, Indexed(n)) => {
        NonEmptyArray(q)
        Option(seqAddAtIndex(q, n, op.value.get.as[String]).toList)
      }
      case a @ JsArrayPointer(_, Appender(_)) => {
        NonEmptyArray(q)
        Option(seqAppend(q, op.value.get.as[String]).toList)
      }
      case v => {
        Option( op.value.get.validate[Seq[String]].get.toList )
      }
    }
  }
  
  
  private[api] def OpAddMap(map: Option[Map[String,String]], op: PatchOp): Option[Map[String,String]] = {
    
    val m = map getOrElse Map()
    
    JsPointer.newPointer(op.path) match {
      case a @ JsArrayPointer(_, Indexed(n)) => {
        throw new IllegalArgumentException(s"Invalid path: ${op.path}. Target is not an array.")
      }
      case a @ JsArrayPointer(_, Appender(_)) => {
        throw new IllegalArgumentException(s"Invalid path: ${op.path}. Target is not an array.")
      }
      case v => {
        Option( m ++ op.value.get.validate[Map[String,String]].get )
      }
    }
  }
  
  
  private[api] def removeAttribute(r: GestaltResourceInstance, op: PatchOp): GestaltResourceInstance = {

    toMatchPath(validateOp(PatchOps.Remove, op, true).get) match {
      case Attributes.Description   => r.copy(description = None)
      case Attributes.Tags          => r.copy(tags = OpRemoveSeq(r.tags, op))
      case Attributes.Variables     => r.copy(variables = OpRemoveMap(r.variables, op))
      case _                        => throw new RuntimeException(s"Invalid path '${op.path}'")
    }
  }
  
  private[api] def OpRemoveSeq(seq: Option[Seq[String]], op: PatchOp)/*(implicit readsT: Reads[T])*/: Option[List[String]] = {
    
    val q = seq getOrElse Seq()

    JsPointer.newPointer(op.path) match {
      case a @ JsArrayPointer(_, Indexed(n)) => {
        NonEmptyArray(q)
        Option(seqRemoveAtIndex(q, n).toList)
      }
      case a @ JsArrayPointer(_, Appender(_)) => {
        throw new IllegalArgumentException(s"Invalid path: ${op.path} : 'append' token to valid for op 'remove'")
      }
      case v => None
    }
  }  
  
  private[api] def OpRemoveMap(map: Option[Map[String,String]], op: PatchOp): Option[Map[String,String]] = {
    
    val m = map getOrElse {
      throw new IllegalArgumentException("Invalid path. '${op.path}' does not exist.")
    }

    JsPointer.newPointer(op.path) match {
      case a @ JsArrayPointer(_, Indexed(n)) => {
        throw new IllegalArgumentException(s"Invalid path: ${op.path}. Target is not an array.")
      }
      case a @ JsArrayPointer(_, Appender(_)) => {
        throw new IllegalArgumentException(s"Invalid path: ${op.path}. Target is not an array.")
      }
      case v => {
      
        val cs = JsPointer.toComponents(op.path)
        
        if (cs.size == 1) 
          None  
        else {
          // Remove a key from the map
          val result = removeKeysFromMap(m, Seq(cs.last))
          if (result.isEmpty) None else Option(result)
        }
      }
    }
  }  
  
  import scala.annotation.tailrec
  
  @tailrec
  private[api] def removeKeysFromMap(m: Map[String, String], keys: Seq[String]): Map[String,String] = {
    println("removeKeysFromMap => " + m)
    keys match {
      case Nil => m
      case h :: t => {
        if (!m.contains(h)) throw new IllegalArgumentException(s"Key '$h' not found")
        else removeKeysFromMap(m - h, t)
      }
    }
  }
  
  private[api] def replaceAttribute(r: GestaltResourceInstance, op: PatchOp): GestaltResourceInstance = {
    toMatchPath(validateOp(PatchOps.Replace, op, false).get) match {
      case Attributes.Org           => r.copy(orgId = UUID.fromString(op.value.get.as[String]))
//      case Attributes.Owner         => r.copy(owner = ownerFromJson(op.value))
      case Attributes.Name          => r.copy(name = op.value.get.as[String])
      case Attributes.ResourceState => r.copy(state = UUID.fromString(op.value.get.as[String]))
      case Attributes.Description   => r.copy(description = Some(op.value.get.as[String]))
      case Attributes.Tags          => r.copy(tags = OpReplaceSeq(r.tags, op))
      case Attributes.Variables     => r.copy(variables = OpReplaceMap(r.variables, op))
      case _                        => throw new RuntimeException(s"Invalid path '${op.path}'")
    }
  }  
  
  private[api] def OpReplaceSeq(seq: Option[Seq[String]], op: PatchOp)/*(implicit readsT: Reads[T])*/: Option[List[String]] = {
    
    val q = seq getOrElse Seq()
    
    JsPointer.newPointer(op.path) match {
      case a @ JsArrayPointer(_, Indexed(n)) => {
        NonEmptyArray(q)
        Option(seqReplaceAtIndex(q, n, op.value.get.as[String]).toList)
      }
      case a @ JsArrayPointer(_, Appender(_)) => {
        throw new IllegalArgumentException(s"Invalid path: ${op.path} : 'append' token to valid for op 'replace'")
      }
      case v => None
    }
  }
  
  
  private[api] def OpReplaceMap(map: Option[Map[String,String]], op: PatchOp): Option[Map[String,String]] = {
    
    val m = map getOrElse Map()
    
    JsPointer.newPointer(op.path) match {
      case a @ JsArrayPointer(_, Indexed(n)) => {
        throw new IllegalArgumentException(s"Invalid path: ${op.path}. Target is not an array.")
      }
      case a @ JsArrayPointer(_, Appender(_)) => {
        throw new IllegalArgumentException(s"Invalid path: ${op.path}. Target is not an array.")
      }
      case v => {

        val cs = JsPointer.toComponents(op.path)
        val newvalue = m ++ Map(cs.last -> op.value.get.as[String])
        
        Option {
          if (cs.size == 1) newvalue else m ++ newvalue
        }
        
      }
    }
  }  
  
  
  private[api] def NonEmptyArray(seq: Seq[_]) = {
    if (seq.isEmpty) throw new IllegalArgumentException(s"Cannot index an empty array.")
  }    
  
  private[api] def toMatchPath(op: PatchOp) = {
    /*
     * Ensure every path 'startsWith' a valid attribute,
     * then keep only the first component
     */
    
    "/" + JsPointer.toComponents(op.path).head
  }
  
  private[api] def validateOp(expectedOp: String, op: PatchOp, requireOptional: Boolean) = Try {
    
    if (expectedOp != op.op) 
      throw new RuntimeException(s"Unexpected Op type. found: ${op.op}, expected: $expectedOp")
    else {
      val at = Attributes.attrs( toMatchPath(op) )
      
      if (!at.editable) 
        throw new IllegalArgumentException(s"${op.path} is not editable.")
      else if (requireOptional && !at.optional) 
        throw new IllegalArgumentException(s"Cannot '${op.op}' non-optional field: ${op.path}")
      else op
    }
  }
  
  private[api] def function(r: GestaltResourceInstance, op: PatchOp) = op match {
    case PatchOp(PatchOps.Add, _, _) => addAttribute(r, op)
    case PatchOp(PatchOps.Remove, _, _) => removeAttribute(r, op)
    case PatchOp(PatchOps.Replace, _, _) => replaceAttribute(r, op)
    case other => throw new RuntimeException(s"Unsupported op: '${other.op}'")
  }

  
}