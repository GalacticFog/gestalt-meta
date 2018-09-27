package controllers

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltFrameworkSecurity}
import com.google.inject.Inject
import controllers.util._
import javax.inject.Singleton
import play.api.i18n.MessagesApi
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.RequestHeader
import services._
import skuber.Namespace

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}


@Singleton
class DeleteController @Inject()(
    messagesApi: MessagesApi,
    sec: GestaltFrameworkSecurity,
    security: Security,
    providerManager: ProviderManager,
    gatewayMethods: GatewayMethods,
    lambdaMethods: LambdaMethods,
    skuberFactory: SkuberFactory,
    genericResourceMethods: GenericResourceMethods
 ) extends SecureController(messagesApi = messagesApi, sec = sec) with Authorization {
   
  /*
   * Each of the types named by the keys in this map have representations both in
   * Meta and in some external system. When delete is called on any of these types
   * the given function is called to delete the resource (and whatever else) in the
   * external system before the resource is deleted from Meta.
   */

  private[this] def wrapUnauthedHandler(f: (GestaltResourceInstance) => Try[Unit])(r: GestaltResourceInstance, a: AuthAccountWithCreds) = f(r)

  private[controllers] val manager = new HardDeleteInstanceManager[AuthAccountWithCreds](
    external = Map(
      ResourceIds.Org         -> deleteExternalOrg,
      ResourceIds.User        -> deleteExternalUser,
      ResourceIds.Group       -> deleteExternalGroup,
      ResourceIds.Container   -> wrapUnauthedHandler(ContainerService.deleteContainerHandler(providerManager,_)),
      ResourceIds.Secret      -> wrapUnauthedHandler(ContainerService.deleteSecretHandler(providerManager,_)),
      migrations.V13.VOLUME_TYPE_ID -> wrapUnauthedHandler(ContainerService.deleteVolumeHandler(providerManager,_)),
      ResourceIds.Api         -> wrapUnauthedHandler(gatewayMethods.deleteApiHandler),
      ResourceIds.ApiEndpoint -> wrapUnauthedHandler(gatewayMethods.deleteEndpointHandler),
      ResourceIds.Lambda      -> wrapUnauthedHandler(lambdaMethods.deleteLambdaHandler)
    )
  )

  private def deleteOps(typeId: UUID) = {
    val action = actionInfo(typeId).prefix + ".delete"
    List(
      controllers.util.Authorize(action),
      controllers.util.PolicyCheck(action),
      controllers.util.EventsPre(action),
      controllers.util.EventsPost(action))
  }
  
  def requestOps(
      user: AuthAccountWithCreds, 
      policyOwner: UUID, 
      target: GestaltResourceInstance,
      targetParent: GestaltResourceInstance): RequestOptions = {
    
    RequestOptions(user, 
        authTarget = Option(policyOwner), 
        policyOwner = Option(policyOwner), 
        policyTarget = Option(target),
        data = Some(Map("parentId" -> targetParent.id.toString)))
  }


  def deleteGenericProviderBackedResource( fqon: String, resource: GestaltResourceInstance, identity: AuthAccountWithCreds )
                                         ( implicit request: RequestHeader ): Future[Unit] = {
    
    log.debug(s"deleteGenericProviderBackedResource($fqon, ${resource.name}, _)")
    
    val owner      = findResourceParent(resource.id)
    val operations = deleteOps(resource.typeId)
    val options    = requestOps(identity, resource.id, resource, owner)

    log.debug(s"Policy Owner : " + owner.id)

    SafeRequest (operations, options) ProtectAsync { _ =>
      for {
        _ <- genericResourceMethods.deleteProviderBackedResource(
          org = orgFqon(fqon).get,
          identity = identity,
          resource = resource,
          actionVerb = request.getQueryString("action").getOrElse("delete")
        )
        metaDelete <- Future.fromTry{
          DeleteHandler.handle(resource, identity)
        }
      } yield metaDelete
    }
  }


  def deleteResource(resource: GestaltResourceInstance, identity: AuthAccountWithCreds)(implicit request: RequestHeader): Future[Unit] = {
    
    log.debug(s"deleteResource(${resource.name}, _)")
    
    val owner      = findResourceParent(resource.id)
    val operations = deleteOps(resource.typeId)
    val options    = requestOps(identity, resource.id, resource, owner)

    log.debug(s"Policy Owner : " + owner.id)

    SafeRequest (operations, options) ProtectAsync { _ =>

      Future.fromTry {
        /*
         * TODO: The type testing and branching here is a hack to get around limitations
         * in the general delete handler mechanism.
         * 
         * See: https://gitlab.com/galacticfog/gestalt-meta/issues/319
         */
        val test = if (resource.typeId == ResourceIds.Environment) {
            deleteEnvironmentSpecial(resource, identity)
        } else {
          Success(())
        }
        
        test match {
          case Failure(e) => throw e
          case Success(_) => DeleteHandler.handle(resource, identity)
        }
      }
    }
  }

  def hardDeleteTypeProperty(fqon: String, id: UUID) = Audited(fqon) { implicit request =>
    log.debug(s"hardDeleteTypeProperty($fqon, $id)")
    
    PropertyFactory.findById(id).map { p =>
      val force = QueryString.singleBoolean(request.queryString, "force")
      val deleter = new HardDeletePropertyType[AuthAccountWithCreds]
      
      deleter.delete(p, request.identity, force, manager) match {
        case Failure(e) => HandleExceptions(e)
        case Success(_) => NoContent
      }
    } getOrElse NotFoundResult(s"Property with ID $id not found")
  }
  
  def hardDeleteResourceType(fqon: String, typeId: UUID) = Audited(fqon) { implicit request =>
    
    log.debug(s"hardDeleteResourceType($fqon, $typeId)")
    
    TypeFactory.findById(typeId).fold {
      NotFoundResult(s"Type with ID $typeId not found") 
    }{ tpe =>
 
      // TODO: Test for 'is-gestalt'

      val forceParam = QueryString.singleBoolean(request.queryString, "force")
      val deleter = new HardDeleteResourceType[AuthAccountWithCreds]
      val caller = request.identity.account.id
      
      val result = for {
        // Delete resource-type in meta
        x <- deleter.delete(tpe, request.identity, forceParam, manager)
        
        // Cleanup entitlements and parent lineage
        y <- teardownResourceType(caller, tpe)
      } yield y
      
      result match {
        case Failure(e) => HandleExceptions(e)
        case Success(_) => NoContent
      } 
    }
  }
  
  
  private[controllers] def teardownResourceType(caller: UUID, tpe: GestaltResourceType): Try[Unit] = Try {

    val parentTypes = TypeMethods.getParentTypes(tpe)
    
    log.debug(s"Unlinking deleted type (${tpe.typeId}) from parents...")
    val unlinkErrors = TypeMethods.divorceParents(caller, tpe.id, parentTypes).filter {
      _.isFailure
    }
    
    log.debug("Testing for unlink errors...")
    if (unlinkErrors.nonEmpty) {
      throw new RuntimeException("There were errors unlinking parent-types.")
    }
    TypeMethods.destroyTypeEntitlements(tpe).get
  }
  
  def hardDeleteResource(fqon: String, path: String) = AsyncAuditedAny(fqon) { implicit request =>

    val p = if (path.trim.isEmpty) fqon else "%s/%s".format(fqon, path)

    Resource.fromPath( p ) map {
      resource =>
        val resp = if (isProviderBackedResource(resource.typeId)) {
          log.debug(s"resource '${resource.id}' of type '${sdk.ResourceLabel(resource.typeId)}' is provider-backed")
          deleteGenericProviderBackedResource(fqon, resource, request.identity)
        } else {
          deleteResource(resource, request.identity)
        }
        resp.map (_ => NoContent)
            .recover {case e => HandleExceptions(e)}
    } getOrElse Future.successful(NotFoundResult(request.uri))
  }
  
  def deleteExternalOrg[A <: ResourceLike](res: A, account: AuthAccountWithCreds) = {
    security.deleteOrg(res.id, account) map ( _ => () )
  }
  
  def deleteExternalUser[A <: ResourceLike](res: A, account: AuthAccountWithCreds) = {
    val result = security.deleteAccount(res.id, account)
    result map ( _ => () )
  }

  def deleteExternalGroup[A <: ResourceLike](res: A, account: AuthAccountWithCreds) = {
    security.deleteGroup(res.id, account) map ( _ => () )
  }


  def deleteEnvironmentSpecial(res: GestaltResourceInstance, account: AuthAccountWithCreds) = Try {
    log.info("Checking for in-scope Kube providers to clean up namespaces...")
    
    val namespace = res.id.toString
    val kubeProviders = ResourceFactory.findAncestorsOfSubType(ResourceIds.KubeProvider, res.id)
    log.info(s"Found [${kubeProviders.size}] Kube Providers in Scope...")
    
    val deletes = kubeProviders.map { k =>
      log.info(s"Deleting namespace '$namespace' from Kube Provider '${k.name}'...")

      try{
        skuberFactory.initializeKube(k.id, namespace) flatMap { kube =>
          val deleted = kube.delete[Namespace](namespace).recoverWith {
            case e: skuber.api.client.K8SException => {
              /*
               * There are a few cases where kube may throw an error when attempting to
               * delete a namespace. This guard checks for thos known cases and ignores
               * the failure if possible.
               */
              if (ignorableNamespaceError(namespace, e.status))
                Future.successful(())
              else throw e
            }
          }
          deleted.onComplete(_ => kube.close)
          deleted
        }
      } catch {
        case e: Throwable => {
          log.warn(s"unable to initialize kube with id = '${k.id}' with cause '${e.getCause()}'...")
          Future.successful(())

        }
      }
    }

    Await.result(Future.sequence(deletes), 10.seconds)
    ()
  }
  
  /**
   * Examine the API status returned from attempting to delete a Kubernetes namespace - if
   * an error is found, determine if it is safe to ignore the error.
   * 
   * Many systems return a 200 when trying to delete an entity that does not exist. Kubernetes does NOT
   * behave this way, returning a 404 instead. Our process aligns kube namespaces with meta environments.
   * When a meta environment is deleted, the corresponding kube namespace is also deleted. This function 
   * handles the case where the corresponding namespaces does not exist in kube (or was already cleaned up),
   * by ignoring the 404 received on namepsace delete.
   * 
   * Kubernetes returns a 409 when you attempt to delete a namespace that has associated runtime entitiles
   * (specifically pods/containers). The error returned indicates that this is the case, but states that
   * the system is "attempting to purge", and the namespace will be deleted when complete. I believe this is
   * a bug - the return should be 'Accepted' since that's what happened.  I've confirmed that waiting before repeating
   * the delete succeeds.  This function simply looks for the 409 and a mention of "purge" in the reason and
   * ignores the error if satisfied.
   */
  private def ignorableNamespaceError(namespace: UUID, status: skuber.api.client.Status): Boolean = {
    val code = status.code
    
    if (code.nonEmpty) {
      if (code.get == 404) {
        log.warn(s"Namespace '$namespace' not found in Kubernetes. Skipping.")
        true
      }
      else if (code.get == 409 && status.message.contains("be purged")) {
        log.warn(s"Conflict deleting namespace '$namespace'. Ignoring.")
        true
      }
      else {
        log.error(s"Recieved code '${code.get}' on namespace delete.")
        false
      }
    } else false
  }
  

  trait RequestHandler[A,B] {
    def handle(resource: A, account: AuthAccountWithCreds)(implicit request: RequestHeader): B
  }

  /*
   * TODO: convert to class and construct with Map of DeleteFunctions.
   */
  
  object DeleteHandler extends RequestHandler[ResourceLike,Try[Unit]] {
    
    def handle(res: ResourceLike, account: AuthAccountWithCreds)(implicit request: RequestHeader): Try[Unit] = {
      manager.delete(
        res.asInstanceOf[GestaltResourceInstance], 
        account, 
        QueryString.singleBoolean(request.queryString, "force"),
        skipExternals(res, request.queryString) ) map { Success(_) }
    }
    
    def canDelete(typeId: UUID, resourceId: UUID): Boolean = {
      manager.canDelete(typeId, resourceId)
    }
  }

  /**
   * Get the type ID for any resources that should have their external delete function skipped.
   * Currently only used for CaaS Providers - if 'deleteContainers' is false (or missing), we
   * need to skip the delete from the cloud provider.
   */
  protected[controllers] def skipExternals(res: ResourceLike, qs: Map[String, Seq[String]]) = {
    if (Seq(ResourceIds.DcosProvider, ResourceIds.KubeProvider, ResourceIds.DockerProvider, migrations.V14.ECS_PROVIDER_TYPE_ID).contains(res.typeId) &&
        !QueryString.singleBoolean(qs, "deleteContainers")) {
      
      log.debug("Delete CaaS Provider: 'deleteContainers' is FALSE.")
      log.debug("Containers WILL NOT be deleted from backend cloud provider.")
      
      Seq(ResourceIds.Container)
    } else Seq()
  }

  def findResourceParent(child: UUID) = {
    ResourceFactory.findParent(child) getOrElse {
      /* TODO:
       * The only way this should happen is when trying to delete the root Org.
       * Not sure we should allow that.
       */
      throw new ResourceNotFoundException("Could not find parent resource.")
    }
  }
  
  def hardDeleteResourceType2(typeId: UUID) = {
    TypeFactory.hardDeleteType(typeId) match {
      case Success(_) => NoContent
      case Failure(e) => HandleExceptions(e)
    }
  }
  
}

