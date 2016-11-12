package com.galacticfog.gestalt.meta.api.patch


import play.api.Logger
import com.galacticfog.gestalt.patch.PatchOp
import play.api.libs.json.JsString
import scala.language.implicitConversions


trait OpTransformer {  
  protected val log = Logger(this.getClass)
  protected val protectedProperties: Seq[String] = Seq()
  
  
  def transform(ops: Seq[PatchOp]): Seq[PatchOp]
  
  def isProtected(path: String) = protectedProperties exists { path.startsWith( _ ) }

  protected implicit def str2jsopt(s: String) = Option(JsString(s))
}


