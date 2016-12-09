package com.galacticfog.gestalt.meta.api.patch


import play.api.libs.json._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.util._
import com.galacticfog.gestalt.data.models._
import scala.util.{Try,Success,Failure}
import java.util.UUID
import com.galacticfog.gestalt.meta.api.output._
import controllers.util.trimquotes
import play.api.{ Logger => log }
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import com.galacticfog.gestalt.patch._


object PatchInstance {
  
  /*
   * 1.) Partition ops into properties and attributes
   * 2.) Apply attribute ops
   * 3.) If there are property Ops, serialize resource and apply
   */
  def applyPatch(r: GestaltResourceInstance, patch: PatchDocument): Try[ResourceLike] = {
    for {
      (p,a) <- partitionOps(patch.ops)
      r1    <- AttributePatch.applyOps(r, a)
      r2    <- PropertyPatch.applyOps(r1, p)
    } yield r2
  }
  
  private[api] def partitionOps(ops: Seq[PatchOp]) = Try {
    
    val (properties, attributes) = ops partition { op =>
      op.path.startsWith("/properties")  
    }
    
    /*
     * diff contains ops that are not properties or attributes. This is an error.  
     */
    val diff = attributes.map(_.path).diff(Attributes.allowed)
    
    if (diff.isEmpty) (properties, attributes) else {
      val m = "Invalid PATCH-Op paths found: " + diff.mkString("[", ",", "]")
      throw new IllegalArgumentException(m)
    }
  }
  
}