package controllers


import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.ResourcePath
import com.galacticfog.gestalt.meta.auth.Authorization
import controllers.util._
import com.galacticfog.gestalt.meta.api.patch._
import controllers.util.JsonUtil
import com.galacticfog.gestalt.patch._

import scala.util.{Failure, Success, Try}
import play.api.libs.json._
import java.util.UUID

import com.galacticfog.gestalt.data.EnvironmentType
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.galacticfog.gestalt.data.ResourceFactory.update

import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi
import javax.inject.Singleton
import com.galacticfog.gestalt.json.Js


@Singleton
class PatchController @Inject()( messagesApi: MessagesApi,
                                 env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
                                 groupMethods: GroupMethods,
                                 lambdaMethods: LambdaMethods,
                                 resourceController: ResourceController )
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {

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
    ResourceIds.Group -> groupMethods.groupPatch,
    ResourceIds.Lambda -> lambdaMethods.patchLambdaHandler,
    ResourceIds.Entitlement -> entitlementPatch)
  
  
  /**
   * Patch an Org by FQON
   * Implements route `PATCH /{fqon}`
   */
  def patchResourceFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    Future{
      orgFqon(fqon).fold(NotFoundResult(fqon))(org => applyPatch(org)) 
    }
  }
  
  /**
   * Patch a Resource by its URI (path)
   */
  def patchResource(fqon: String, path: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    Future {
      val respath = new ResourcePath(fqon, path)
      lookupResource(respath, request.identity) match {
        case Failure(e) => HandleExceptions(e)
        case Success(resource) => applyPatch(resource)
      }
    }
  }
  
  /**
   * Apply policy and authorization checks to PATCH operation. Orchestrates
   * application of the patch and persisting the updated resource to Meta DB.
   * 
   * @param target the Resource to be modified
   */
  private[controllers] def applyPatch(target: GestaltResourceInstance)(
      implicit request: SecuredRequest[JsValue]) = {
    
    val user = request.identity
    val action = actionInfo(target.typeId).prefix + ".update"
    val options = standardRequestOptions(user, target)
    val operations = standardRequestOperations(action)
    
    SafeRequest(operations, options) Protect { maybeState =>
      (for {
        
        r1 <- Patch(target)
        r2 <- update(r1, user.account.id)
        
      } yield r2) match {
        case Failure(e) => HandleExceptions(e)
        case Success(r) => Ok(RenderSingle(r))
      }
    }
  }

  private[this] def standardRequestOptions(
    user: AuthAccountWithCreds,
    resource: GestaltResourceInstance,
    data: Option[Map[String, String]] = None) = {

    RequestOptions(user,
      authTarget = Option(resource.id),
      policyOwner = Option(resource.id),
      policyTarget = Option(resource),
      data)
  }

  private[this] def standardRequestOperations(action: String) = {
    List(
      controllers.util.Authorize(action),
      controllers.util.EventsPre(action),
      controllers.util.PolicyCheck(action),
      controllers.util.EventsPost(action))
  }    
    
  /**
   * This function finds and patches the requested resource - it does NOT persist the updated resource.
   */
//  private[controllers] def Patch(path: ResourcePath, patchJs: JsValue, account: AuthAccountWithCreds) = {
//
//    if (path.isList) {
//      throw new BadRequestException(s"Path does not identify a resource. found:" + path.path)
//    } else {
//      resourceController.findResource(path, account).fold {
//        throw new ResourceNotFoundException(path.path)
//      }{ r =>
//        
//        val ops   = JsonUtil.safeParse[Seq[PatchOp]](patchJs)
//        val patch = transforms.get(r.typeId).fold(PatchDocument(ops: _*)) {
//          transform => transform(PatchDocument(ops: _*))
//        }
//
//        val handler = {
//          if (handlers.get(r.typeId).isDefined) {
//            log.debug(s"Found custom PATCH handler for type: ${r.typeId}")
//            handlers(r.typeId)
//          } else {
//            log.debug("Using default PATCH handler for type: ${r.typeId")
//            defaultResourcePatch _
//          }
//        }
//        handler(r, patch, account)
//      }
//    }
//  }
  
  private[controllers] def Patch(
      resource: GestaltResourceInstance,
      patchJson: JsValue,
      account: AuthAccountWithCreds): Try[GestaltResourceInstance] = {
    
    val ops   = JsonUtil.safeParse[Seq[PatchOp]](patchJson)
    
    val patch = transforms.get(resource.typeId).fold(PatchDocument(ops: _*)) {
      transform => transform(PatchDocument(ops: _*))
    }

    handlers.get(resource.typeId) match {
      case Some(customHandler) =>
        log.debug(s"Found custom PATCH handler for type: ${resource.typeId}")
        customHandler(resource, patch, account)
      case None =>
        log.debug(s"Using default PATCH handler for type: ${resource.typeId}")
        defaultResourcePatch(resource, patch, account)
    }
  }
  
  private[controllers] def Patch(resource: GestaltResourceInstance)(
      implicit request: SecuredRequest[JsValue]): Try[GestaltResourceInstance] = {
    Patch(resource, request.body, request.identity)
  }
  
  
  def lookupResource(path: ResourcePath, account: AuthAccountWithCreds) = Try {
    if (path.isList) {
      throw new BadRequestException(s"Path does not identify a resource. found:" + path.path)
    } else {
      resourceController.findResource(path, account) getOrElse {
        throw new ResourceNotFoundException(path.path)
      }
    }
  }
  
  private[controllers] def defaultResourcePatch(
    resource: GestaltResourceInstance,
    patch: PatchDocument,
    user: AuthAccountWithCreds): Try[GestaltResourceInstance] = Try {
    log.debug("entered defaultResourcePatch(_,_,_)")
    
    log.debug("Applying patch to resource...")
    val t = PatchInstance.applyPatch(resource, patch)
    log.debug("PATCHED RESOURCE : " + t)
    t.get.asInstanceOf[GestaltResourceInstance]
  }
  
  
  /*
   * 
   * Patch Transformers
   * 
   */
  
  /*
   * This function overrides the normal patch behavior for Entitlement - the need arises from a more
   * general issue where resources that contain reference UUIDs that are rendered as Links need 
   * intermediate transformation.
   * 
   * TODO: Generalize transformations for properties of type `resource::uuid::link`
   */
  def entitlementPatch(r: GestaltResourceInstance, patch: PatchDocument, auth: AuthAccountWithCreds): Try[GestaltResourceInstance] = {
    log.debug("entitlementPatch(...)")
    /*
     * convert r.properties.identities from ResourceLink to UUID - then proceed with Patch.
     */
    
    def hasIdentities = 
      patch.ops.exists(_.path.trim.startsWith("/properties/identities")) 
    
    val resource = {
      if (hasIdentities) {
        // transform identity values from ResourceLink to UUID
        val props = r.properties.get
        val identities = {
          props.get("identities") match {
            case None => Seq.empty
            case Some(stringids) => {
//              val jsids = JsonUtil.safeParse[Seq[JsObject]](stringids) 
//              jsids map { _ \ "id" }
              
              val jsids = Js.parse[Seq[JsObject]](Json.parse(stringids)).get
              jsids map { x => (x \ "id").get }

            }
          }
        }
        
        val newprops = props ++ Map("identities" -> Json.stringify(Json.toJson(identities)))
        r.copy(properties = Option(newprops))
      } else r
    }
    // apply patch to newres
    defaultResourcePatch(resource, patch, auth)
  }  
  
  /*
   * environment_type is input as a string (name) but stored as a UUID.
   * This function handles that conversion.
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

