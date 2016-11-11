package com.galacticfog.gestalt.meta.api.patch

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.illegal
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.security.api.{GestaltResource => SecurityResource}
import com.galacticfog.gestalt.security.api.json.JsonImports.linkFormat
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController

import controllers.util.JsonUtil._
import controllers.util.stringmap
import controllers.util.trace
import play.api.Logger
import play.api.Play.current

import com.galacticfog.gestalt.patch.PatchOp


object EnvOpTransformer extends OpTransformer {
  
  override val protectedProperties = Seq("/properties/workspace")
  
  
  def transform(ops: Seq[PatchOp]): Seq[PatchOp] = {
  
    ops map { op =>
      
      if (isProtected(op.path)) {
        throw new ConflictException(s"${op.path} may not be updated.")
      }
      
      // translate 'environment_type' from name to UUID.
      if (op.path == "/properties/environment_type") {
        
        val envtype = EnvironmentType.id(op.value.get.as[String])
        log.debug(s"Transforming environment_type from '${op.value}' to '${envtype}'")
        op.copy(value = envtype.toString)
        
      } else op
    }
  }
  
}