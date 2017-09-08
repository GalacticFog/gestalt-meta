package controllers


import java.net.URL
import java.util.UUID

import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.ResourceFactory.findById
import com.galacticfog.gestalt.data.models._

import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import controllers.util._
import controllers.util.db.EnvConfig
import play.api.libs.json._
import play.api.mvc.RequestHeader

//import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.Await
import com.galacticfog.gestalt.meta.auth.Authorization
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi

import scala.language.postfixOps
import javax.inject.Singleton

import com.galacticfog.gestalt.meta.providers.ProviderManager

import services.KubernetesService
import play.api.libs.ws.WSClient
import com.galacticfog.gestalt.json.Js
import services._


@Singleton
class DeleteController @Inject()(
    messagesApi: MessagesApi,
    env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
    security: Security,
    providerManager: ProviderManager,
    providerMethods: ProviderMethods,
    gatewayMethods: GatewayMethods,
    lambdaMethods: LambdaMethods,
    ws: WSClient,
    skuberFactory: SkuberFactory
 ) extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
 
  // TODO: change to dynamic, provide a ContainerService impl, off-load deleteExternalContainer contents to the ContainerService
  
  /*
   * Each of the types named by the keys in this map have representations both in
   * Meta and in some external system. When delete is called on any of these types
   * the given function is called to delete the resource (and whatever else) in the
   * external system before the resource is deleted from Meta.
   */
  private[controllers] val manager = new HardDeleteInstanceManager[AuthAccountWithCreds](
    external = Map(
      ResourceIds.Org         -> deleteExternalOrg,
      ResourceIds.User        -> deleteExternalUser,
      ResourceIds.Group       -> deleteExternalGroup,
      ResourceIds.Container   -> deleteExternalContainer,
      ResourceIds.Api         -> gatewayMethods.deleteApiHandler,
      ResourceIds.ApiEndpoint -> gatewayMethods.deleteEndpointHandler,
      ResourceIds.Lambda      -> lambdaMethods.deleteLambdaHandler
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

  def deleteResource(resource: GestaltResourceInstance, identity: AuthAccountWithCreds)(implicit request: RequestHeader): Future[Unit] = {
    val owner      = findResourceParent(resource.id)
    val operations = deleteOps(resource.typeId)
    val options    = requestOps(identity, resource.id, resource, owner)

    log.debug(s"Policy Owner : " + owner.id)

    SafeRequest (operations, options) ProtectAsync { _ =>
      
      Future.fromTry {
        /*
         * TODO: This is a bit of a hack. The delete manager/handler system
         * needs an overhaul. The issue with Environment is we need to get
         * any child kube-providers *before* children are deleted. Currently
         * the only hook occurs *after*. See:
         * https://gitlab.com/galacticfog/gestalt-meta/issues/319
         */
        if (resource.typeId == ResourceIds.Environment) {
          deleteEnvironmentSpecial(resource, identity)
        }
        DeleteHandler.handle(resource, identity)
      }
      
    }
  }

  def hardDeleteResource(fqon: String, path: String) = AsyncAuditedAny(fqon) { implicit request =>
    
    val p = if (path.trim.isEmpty) fqon else "%s/%s".format(fqon, path)
    
    Resource.fromPath( p ) map {
      deleteResource(_, request.identity)
        .map (_ => NoContent)
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
  
  def deleteExternalContainer(res: GestaltResourceInstance, account: AuthAccountWithCreds) = {
    val provider = containerProvider(res)
    
    providerManager.getProviderImpl(provider.typeId) map { service =>
      Await.result(service.destroy(res), 5 seconds)
    }
  }
  
  import skuber.Namespace
  def deleteEnvironmentSpecial(res: GestaltResourceInstance, account: AuthAccountWithCreds) = Try {
    log.info("Checking for in-scope Kube providers to clean up namespaces...")
    
    val namespace = res.id.toString
    val kubeProviders = ResourceFactory.findAncestorsOfSubType(ResourceIds.KubeProvider, res.id)
    log.info(s"Found [${kubeProviders.size}] Kube Providers in Scope...")
    
    val deletes = kubeProviders.map { k =>
      log.info(s"Deleting namespace '$namespace' from Kube Provider '${k.name}'...")
      skuberFactory.initializeKube(k.id, namespace) flatMap { kube =>
        val deleted = kube.delete[Namespace](namespace)
        deleted.onComplete(_ => kube.close)
        deleted
      }
    }
    Await.result(Future.sequence(deletes), 10.seconds)
    ()
  }
  
  private def containerProvider(container: GestaltResourceInstance): GestaltResourceInstance = {
    val providerId = ContainerService.containerProviderId(container)

    ResourceFactory.findById(providerId) getOrElse {
      throw new ResourceNotFoundException(
        s"Provider with ID '$providerId' not found. Container '${container.id}' is corrupt.")
    }
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
        singleParamBoolean(request.queryString, "force"),
        skipExternals(res, request.queryString)) map { Success(_) }
    }
  }

  /**
   * Get the type ID for any resources that should have their external delete function skipped.
   * Currently only used for MarathonProvider - if 'deleteContainers' is false (or missing), we
   * need to skip the delete from Marathon.
   */
  protected[controllers] def skipExternals(res: ResourceLike, qs: Map[String, Seq[String]]) = {
    if (Seq(ResourceIds.DcosProvider, ResourceIds.KubeProvider, ResourceIds.DockerProvider).contains(res.typeId) &&
        !singleParamBoolean(qs, "deleteContainers")) {
      
      log.debug("Delete Marathon Provider: 'deleteContainers' is FALSE.")
      log.debug("Containers WILL NOT be deleted from Marathon.")
      
      Seq(ResourceIds.Container)
    } else Seq()
  }

  /*
   * TODO: Move this function... 
   * Make part of common controller so all controllers can use.
   * Possibly refactor into a QueryString object that makes dealing with the
   * nested structure a bit easier.
   * 
   */
  def singleParamBoolean(qs: Map[String,Seq[String]], param: String) = {
    if (!qs.contains(param)) false
    else {
      val bp = qs(param)
      Try {
        bp.mkString.toBoolean
      } match {
        case Success(b) => b == true
        case Failure(_) => throw new BadRequestException(s"Value of '$param' parameter must be true or false. found: $bp")
      }
    }
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
  
  def hardDeleteResourceType(typeId: UUID) = {
    TypeFactory.hardDeleteType(typeId) match {
      case Success(_) => NoContent
      case Failure(e) => HandleExceptions(e)
    }
  }
  
}

