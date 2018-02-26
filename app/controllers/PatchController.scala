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
import com.galacticfog.gestalt.meta.api.isProviderBackedResource

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
import play.api.mvc.{RequestHeader, Result}
import com.galacticfog.gestalt.meta.api.output._

@Singleton
class PatchController @Inject()( 
     messagesApi: MessagesApi,
     env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
     groupMethods: GroupMethods,
     gatewayMethods: GatewayMethods,
     lambdaMethods: LambdaMethods,
     containerService: ContainerService,
     genericResourceMethods: GenericResourceMethods,
     resourceController: ResourceController )
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  /*
   * Function to transform a PatchDocument
   */
  type PatchTransform = (PatchDocument => PatchDocument)
  
  /* 
   * Function to override the default Patch behavior
   */
  type PatchHandler   = (GestaltResourceInstance, PatchDocument, AuthAccountWithCreds, RequestHeader) => Future[GestaltResourceInstance]
  
  private[controllers] val transforms: Map[UUID, PatchTransform] = Map(
    ResourceIds.Environment -> transformEnvironmentPatch)
  
  /*
   * These functions will be called instead of the default patch handler
   * for the corresponding resource-types.
   */
  private[controllers] val handlers: Map[UUID, PatchHandler] = Map(
    ResourceIds.Group       -> groupMethods.groupPatch,
    ResourceIds.User        -> groupMethods.userPatch,
    ResourceIds.Lambda      -> lambdaMethods.patchLambdaHandler,
    ResourceIds.ApiEndpoint -> gatewayMethods.patchEndpointHandler,
    ResourceIds.Entitlement -> entitlementPatch,
    ResourceIds.Container   -> containerService.patchContainer
  )
  
  
  /**
   * Patch an Org by FQON
   * Implements route `PATCH /{fqon}`
   */
  def patchResourceFqon(fqon: String) = AsyncAudited(fqon) { implicit request =>
    orgFqon(fqon).fold(
      Future.successful(NotFoundResult(fqon))
    )(
      org => applyPatch(org)
    )
  }
  
  def patchProperty(fqon: String, id: UUID) = AsyncAudited(fqon) { implicit request =>
    log.debug(s"patchProperty($fqon, $id)")
    PropertyFactory.findById(id).fold {
      Future.successful(NotFoundResult(s"Property with ID $id not found"))
    }{ prop =>
      val ops = JsonUtil.safeParse[Seq[PatchOp]](request.body)
      
      PatchProperty.applyPatch(prop, PatchDocument(ops:_*)) match {
        case Failure(e) => HandleExceptionsAsync(e)
        case Success(u) => {

          val p1 = Js.find(u.as[JsObject], "/properties").get.as[JsObject]
          
          val updated = for {
            jsonProps <- Js.parse[Map[String,JsValue]](p1)
            stringProps  = jsonProps.map { case (k,v) => (k, v.toString) }
            resource <- Js.parse[GestaltTypeProperty] {
              u.as[JsObject] ++ Json.obj("properties" -> Json.toJson(stringProps))
            }
            d <- {
              val result = PropertyFactory.update(resource, request.identity.account.id)
              result
            }
          } yield d

          updated match {
            case Failure(e) => HandleExceptionsAsync(e)
            case Success(up) => Future.successful(Accepted(Output.renderTypePropertyOutput(up, Some(META_URL))))
          }
        }
      }
    }
  }
    
  def patchType(fqon: String, id: UUID) = AsyncAudited(fqon) { implicit request =>
    TypeFactory.findById(id).fold {
      
      Future.successful(NotFoundResult(id))
      
    }{ tpe =>
      val ops = JsonUtil.safeParse[Seq[PatchOp]](request.body)

      PatchType.applyPatch(tpe, PatchDocument(ops:_*)) match {
        case Failure(e) => HandleExceptionsAsync(e)
        case Success(u) => {

          val p1 = Js.find(u.as[JsObject], "/properties").get.as[JsObject]
          val p2 = Js.parse[Map[String,JsValue]](p1).get.map { 
            case (k,v) => (k, v.toString) 
          }

          val updated = for {
            a <- Js.parse[Map[String,JsValue]](p1)
            b  = a.map { case (k,v) => (k, v.toString) }
            c <- Js.parse[GestaltResourceType] {
              u.as[JsObject] ++ Json.obj("properties" -> Json.toJson(p2))
            }
            up <- TypeFactory.update(c, request.identity.account.id)
            out <- TypeMethods.renderType(up, request.queryString, META_URL)
          } yield out

          updated match {
            case Failure(e) => HandleExceptionsAsync(e)
            case Success(t) => Future.successful(Accepted(t))
          }
          
        }
      }
    }
  }
  
  /**
   * Patch a Resource by its URI (path)
   */
  def patchResource(fqon: String, path: String) = AsyncAudited(fqon) { implicit request =>
    val f = for {
      respath <- Future(new ResourcePath(fqon, path))
      r       <- Future.fromTry(lookupResource(respath, request.identity))
      pr      <- applyPatch(r)
    } yield pr
    f recover {
      case e: Throwable => HandleExceptions(e)
    }
  }
  
  /**
   * Apply policy and authorization checks to PATCH operation. Orchestrates
   * application of the patch and persisting the updated resource to Meta DB.
   * 
   * @param target the Resource to be modified
   */
  private[controllers] def applyPatch( target: GestaltResourceInstance )
                                     ( implicit request: SecuredRequest[JsValue] ): Future[Result] = {

    val user = request.identity
    val action = actionInfo(target.typeId).prefix + ".update"
    val options = standardRequestOptions(user, target)
    val operations = standardRequestOperations(action)
    
    SafeRequest(operations, options) ProtectAsync { maybeState =>
      val result = for {
        patched <- Patch(target)
        updated <- Future.fromTry {
          log.info("Updating resource in Meta...")
          update(patched, user.account.id)
        }
      } yield Ok(RenderSingle(updated))
      result recover {
        case e: Throwable => HandleExceptions(e)
      }
    }
  }

  private[controllers] def standardRequestOptions(
    user: AuthAccountWithCreds,
    resource: GestaltResourceInstance,
    data: Option[Map[String, String]] = None) = {

    // cgbaker: may need a more general solution for this, but for resources parented under an environment,
    // this should locate the policies in the environment, which is what we want
    val parentId = ResourceFactory.findParent(resource.id) map (_.id)
    RequestOptions(user,
      authTarget = Option(resource.id),
      policyOwner = parentId orElse Option(resource.id),
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

  private[controllers] def Patch(resource: GestaltResourceInstance,
                                 patchJson: JsValue,
                                 account: AuthAccountWithCreds,
                                 rh: RequestHeader): Future[GestaltResourceInstance] = {

    val ops = JsonUtil.safeParse[Seq[PatchOp]](patchJson)

    val patch = transforms.get(resource.typeId).fold(PatchDocument(ops: _*)) {
      transform => transform(PatchDocument(ops: _*))
    }

    handlers.get(resource.typeId) match {
      case Some(customHandler) =>
        log.debug(s"Found custom PATCH handler for type: ${resource.typeId}")
        customHandler(resource, patch, account, rh)
      case None if isProviderBackedResource(resource.typeId) =>
        log.debug(s"Using provider-backed PATCH handler for type: ${resource.typeId}")
        providerBackedResourcePatch(resource, patch, account, rh)
      // case None if isSomeSortOfProvider => patchProviderSpecial(resource, patch, account)
      case None =>
        log.debug(s"Using default PATCH handler for type: ${resource.typeId}")
        defaultResourcePatch(resource, patch, account)
    }
  }
  
  private[controllers] def Patch(resource: GestaltResourceInstance)(
      implicit request: SecuredRequest[JsValue]): Future[GestaltResourceInstance] = {
    Patch(resource, request.body, request.identity, request)
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

  private[controllers] def providerBackedResourcePatch( resource: GestaltResourceInstance,
                                                        patch: PatchDocument,
                                                        user: AuthAccountWithCreds,
                                                        rh: RequestHeader ): Future[GestaltResourceInstance] = {
    log.debug("entered providerBackedResourcePatch(_,_,_)")
    for {
      patched <- Future.fromTry {
        log.debug("Applying patch to resource...")
        PatchInstance.applyPatch(resource, patch) map {_.asInstanceOf[GestaltResourceInstance]}
      }
      org <- Future.fromTry(Try{
        ResourceFactory.findById(ResourceIds.Org, patched.orgId).getOrElse(
          throw new InternalErrorException(s"could not locate org '${patched.orgId}' for resource '${patched.id}'")
        )
      })
      updated <- {
        log.debug("Invoking provider patch...")
        genericResourceMethods.updateProviderBackedResource(
          org = org,
          identity = user,
          updatedResource = patched,
          actionVerb = rh.getQueryString("action").getOrElse("update")
        )(rh)
      }
      _ = {
        log.debug("PATCHED RESOURCE : " + updated)
      }
    } yield updated
  }

  private[controllers] def defaultResourcePatch(
    resource: GestaltResourceInstance,
    patch: PatchDocument,
    user: AuthAccountWithCreds): Future[GestaltResourceInstance] = Future {
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
  def entitlementPatch( r: GestaltResourceInstance,
                        patch: PatchDocument,
                        auth: AuthAccountWithCreds,
                        request: RequestHeader ): Future[GestaltResourceInstance] = {
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

