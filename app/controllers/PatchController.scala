package controllers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.ResourcePath
import com.galacticfog.gestalt.meta.auth.Authorization

import controllers.ResourceController.findResource
import controllers.util._

import com.galacticfog.gestalt.meta.api.patch._
import controllers.util.JsonUtil

import com.galacticfog.gestalt.patch._

import scala.util.{Try,Success,Failure}

import play.api.libs.json._

import java.util.UUID

import com.galacticfog.gestalt.data.EnvironmentType

import com.galacticfog.gestalt.meta.api.sdk._

object PatchController extends Authorization {
  
  
  type PatchTransform = (PatchDocument => PatchDocument)
  
  val transforms: Map[UUID,PatchTransform] = Map(
    ResourceIds.Environment -> transformEnvironmentPatch
  )
  

  
  private def transformEnvironmentPatch(patch: PatchDocument) = {
    
    def go(ops: Seq[PatchOp], acc: Seq[PatchOp]): Seq[PatchOp] = {
      ops match {
        case Nil => acc
        case h :: t => {
          /*
           * if field is /properties/environment_type, try to find the UUID of
           * the environment_type with the given name. Use it if found, 400 if not found.
           */
           val op = if (h.path.trim == "/properties/environment_type") {
             val eid = EnvironmentType.id(h.value.get.as[String])
             h.copy(value = Option(JsString(eid.toString)))
           } else h
           go(t, op +: acc)
        }
      }
    }
    
    PatchDocument(go(patch.ops, Seq.empty):_*)
    
  }
  
  def patchResource(fqon: String, path: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    Future {
      
      val rp = new ResourcePath(fqon, path)      
      if (rp.isList) {
        
        BadRequestResult(s"Path does not identify a resource. found:" + request.uri)
        
      } else {

        findResource(rp, request.identity).fold(NotFoundResult(request.uri)) { r =>
          
          val ops = JsonUtil.safeParse[Seq[PatchOp]](request.body)
          val patch = transforms.get(r.typeId).fold(PatchDocument(ops:_*)) {
            transform => transform (PatchDocument(ops:_*))
          }
          
          ResourcePatch.applyPatch(r, patch) match {
            case Failure(e) => HandleExceptions(e)
            case Success(r) => {
              
              /*
               * We've found the resource...
               * ------------------------------------------
               * 1. Patch it
               *   1a. Transform any literal inputs if needed.
               * 2. Update it
               *   2a. Update in external system if needed
               * 3. Return it
               */
              
              Ok(RenderSingle(r.asInstanceOf[GestaltResourceInstance]))
              
            }
          }
          
          
        }
            
      }
    }
  }
  
  
  
}

