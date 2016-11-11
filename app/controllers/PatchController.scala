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
import com.galacticfog.gestalt.meta.api.errors._

import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import com.galacticfog.gestalt.data.ResourceFactory.update

import com.galacticfog.gestalt.security.api.json.JsonImports.linkFormat
import scala.util.{Either,Right,Left}
import com.galacticfog.gestalt.security.api.{ResourceLink => SecurityLink}
import com.galacticfog.gestalt.security.api.GestaltSecurityClient

trait PatchController extends Authorization {
  
  protected[controllers] val secProvider = new SecurityProviderImpl(securityClient)
  
  /* 
   * Function to transform a PatchDocument
   */
  type PatchTransform = (PatchDocument => PatchDocument)
  
  /* 
   * Function to override the default Patch behavior
   */
  type PatchHandler   = (GestaltResourceInstance, PatchDocument, AuthAccountWithCreds) => Try[GestaltResourceInstance]
  
  
  private[controllers] val transforms: Map[UUID, PatchTransform] = Map(
    ResourceIds.Environment -> transformEnvironmentPatch)
  
  
  private[controllers] val handlers: Map[UUID, PatchHandler] = Map(
    ResourceIds.Group -> new GroupMethods(secProvider).groupPatch,
    ResourceIds.Lambda -> LambdaMethods.patchLambdaHandler)    
  
  
  def patchResource(fqon: String, path: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    log.debug(s"patchResource($fqon, $path)")
    Future {      
      val respath = new ResourcePath(fqon, path)
      val user = request.identity

      (for {
        
        r1 <- Patch(respath, request.body, user)
        r2 <- update(r1, user.account.id)
        
      } yield r2) match {
        case Failure(e) => HandleExceptions(e)
        case Success(r) => Ok(RenderSingle(r))
      }
    }
  }

  /**
   * This function finds and patches the requested resource - it does NOT persist the updated resource.
   */
  private[controllers] def Patch(path: ResourcePath, patchJs: JsValue, account: AuthAccountWithCreds) = Try {

    if (path.isList) {
      throw new BadRequestException(s"Path does not identify a resource. found:" + path.path)
    } else {
      findResource(path, account).fold {
        throw new ResourceNotFoundException(path.path)
      }{ r =>
        
        val ops   = JsonUtil.safeParse[Seq[PatchOp]](patchJs)
        val patch = transforms.get(r.typeId).fold(PatchDocument(ops: _*)) {
          transform => transform(PatchDocument(ops: _*))
        }
        val t = this.securityClient
        val handler = {
          if (handlers.get(r.typeId).isDefined) {
            log.debug(s"Found custom PATCH handler for type: ${r.typeId}")
            handlers(r.typeId)
          } else {
            log.debug("Using default PATCH handler for type: ${r.typeId")
            defaultResourcePatch _
          }
        }
        handler(r, patch, account)
        
        ResourcePatch.applyPatch(r, patch).get.asInstanceOf[GestaltResourceInstance] 
      }
    }
  }
  
  
  private[controllers] def defaultResourcePatch(
    resource: GestaltResourceInstance,
    patch: PatchDocument,
    user: AuthAccountWithCreds): Try[GestaltResourceInstance] = Try {

    ResourcePatch.applyPatch(resource, patch).get.asInstanceOf[GestaltResourceInstance]
  }
  
  
  /*
   * 
   * Patch Transformers
   * 
   */
  private def transformEnvironmentPatch(patch: PatchDocument) = {
    
    def go(ops: Seq[PatchOp], acc: Seq[PatchOp]): Seq[PatchOp] = {
      ops match {
        case Nil => acc
        case h :: t => {
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

}

