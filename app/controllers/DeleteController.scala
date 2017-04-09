package controllers


import java.net.URL
import java.util.UUID

import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.ResourceFactory.findById
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.laser.Laser
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import controllers.util._
import controllers.util.db.EnvConfig
import play.api.libs.json._
import play.api.mvc.RequestHeader

import scala.concurrent.ExecutionContext.Implicits.global
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

@Singleton
class DeleteController @Inject()( messagesApi: MessagesApi,
                                  env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
                                  security: Security,
                                  providerManager: ProviderManager )
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {

  /*
   * Each of the types named by the keys in this map have representations both in
   * Meta and in some external system. When delete is called on any of these types
   * the given function is called to delete the resource (and whatever else) in the
   * external system before the resource is deleted from Meta.
   */
  private[controllers] val manager = new HardDeleteInstanceManager[AuthAccountWithCreds](
      external = Map(
        ResourceIds.Org -> deleteExternalOrg,
        ResourceIds.User -> deleteExternalUser,
        ResourceIds.Group -> deleteExternalGroup,
        ResourceIds.Container -> deleteExternalContainer,
        ResourceIds.Api -> deleteExternalApi,
        ResourceIds.ApiEndpoint -> deleteExternalEndpoint
      /*
        ResourceIds.Lambda -> deleteExternalLambda,
        ,
        ,
        ResourceIds.ApiGatewayProvider -> deleteExternalApiGateway
        */
      ))

  
  private def deleteOps(typeId: UUID) = {
    //val action = Actions.actionName(typeId, "delete")
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
      target: GestaltResourceInstance): RequestOptions = {
    
    RequestOptions(user, 
        authTarget = Option(policyOwner), 
        policyOwner = Option(policyOwner), 
        policyTarget = Option(target))    
  }

  def deleteResource(resource: GestaltResourceInstance, identity: AuthAccountWithCreds)(implicit request: RequestHeader): Future[Unit] = {
    val owner      = findResourceParent(resource.id)
    val operations = deleteOps(resource.typeId)
    val options    = requestOps(identity, resource.id, resource)

    log.debug(s"Policy Owner : " + owner.id)

    SafeRequest (operations, options) ProtectAsync { _ =>
      Future.fromTry(DeleteHandler.handle(resource, identity))
    }
  }

  def hardDeleteResource(fqon: String, path: String) = Authenticate(fqon).async { implicit request =>
    
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
    //security.deleteAccount(res.id, account) map ( _ => () )
    val result = security.deleteAccount(res.id, account)
    
    //log.debug("Security.deleteAccount() result : " + result)
    
    result map ( _ => () )
  }  

  def deleteExternalGroup[A <: ResourceLike](res: A, account: AuthAccountWithCreds) = {
    security.deleteGroup(res.id, account) map ( _ => () )
  }
  
  /* *************************************************
   * TEMPORARY
   * 
   */
  
  import com.galacticfog.gestalt.json.Js

  def deleteExternalContainer(res: GestaltResourceInstance, account: AuthAccountWithCreds) = {
    val provider = containerProvider(res)
    
    providerManager.getProviderImpl(provider.typeId) map { service =>
      Await.result(service.destroyContainer(res), 5 seconds)
    }
  }

  import services._

  private def providerIdProperty(ps: Map[String, String]): Option[UUID] = {
    Js.find(Json.parse(ps("provider")).as[JsObject], "/id") map { id =>
      UUID.fromString(id.as[String])
    }
  }  
  
  private def containerProvider(container: GestaltResourceInstance): GestaltResourceInstance = {
    val providerId = providerIdProperty(container.properties.get) getOrElse {
      throw new ResourceNotFoundException(
        s"Could not parse provider ID from container '${container.id}'")
    }
    
    ResourceFactory.findById(providerId) getOrElse {
      throw new ResourceNotFoundException(
        s"Provider with ID '$providerId' not found. Container '${container.id}' is corrupt.")
    }    
  }  
  /* ************** END TEMPORARY **************** */
  
  def deleteExternalLambda[A <: ResourceLike](res: A, account: AuthAccountWithCreds) = {
    laser.deleteLambda(res.id) map ( _ => () )
  }

  import controllers.util.GatewayMethods
  def deleteExternalApi[A <: ResourceLike](res: A, account: AuthAccountWithCreds): Try[Unit] = Try {
    
    val provider = (for {
      ps  <- res.properties
      pr  <- ps.get("provider")
      pid <- Js.find(Json.parse(pr).as[JsObject], "/id")
      prv <- findById(ResourceIds.GatewayManager, UUID.fromString(pid.as[String]))
    } yield prv) getOrElse {
      
      throw new RuntimeException("Could not parse GatewayManager ID from API.")
    }

    val client = GatewayMethods.configureWebClient(provider)
    val fdelete = client.delete(s"/apis/${res.id.toString}") map { result =>
      log.info("Deleting API from GatewayManager...")
      val response = client.unwrapResponse(result, Seq(200))
      log.debug("Response from GatewayManager: " + response.output)
    } recover {
      case e: Throwable => {
        log.error(s"Error deleting API from Gateway Manager: " + e.getMessage)
        throw e
      }
    }
    ()
    //laser.deleteApi(res.id) map ( _ => () )
  }
  
  def deleteExternalEndpoint[A <: ResourceLike](res: A, account: AuthAccountWithCreds) = Try {

    val api = ResourceFactory.findParent(res.id) getOrElse {
      throw new ResourceNotFoundException(s"Could not find parent API for endpoint: '${res.id}'")
    }

    val client = (for {
      provider <- GatewayMethods.findGatewayProvider(api)
      client = GatewayMethods.configureWebClient(provider)
    } yield client) getOrElse {
      throw new RuntimeException("Could not parse GatewayManager ID from API.")
    }
    
    val uri = s"/apis/${api.id.toString}/endpoints/${res.id.toString}"
    val fdelete = client.delete(uri) map { result =>
      log.info("Deleting Endpoint from GatewayManager...")
      val response = client.unwrapResponse(result, Seq(200))
      log.debug("Response from GatewayManager: " + response.output)
    } recover {
      case e: Throwable => {
        log.error(s"Error deleting Endpoint from Gateway Manager: " + e.getMessage)
        throw e
      }
    }
    ()
  }
  
  def deleteExternalApiGateway[A <: ResourceLike](res: A, account: AuthAccountWithCreds) = {
    val externalId = res.properties.get("external_id")
    laser.deleteGateway(externalId) map ( _ => () )
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
    if (Seq(ResourceIds.DcosProvider, ResourceIds.KubeProvider).contains(res.typeId) &&
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

  /*
   * TODO: This seems off - why are we using the environment configuration for
   * lambda and gateway? This information should now come from the provider we
   * are trying to delete. I think this works by accident - meaning, we're only
   * working with a single provider at a time, so the env config is always "correct".
   */
  lazy val gatewayConfig = HostConfig.make(new URL(EnvConfig.gatewayUrl))
  lazy val lambdaConfig  = HostConfig.make(new URL(EnvConfig.lambdaUrl))
  lazy val laser = new Laser(
    gatewayConfig, lambdaConfig, 
    Option(EnvConfig.securityKey), 
    Option(EnvConfig.securitySecret))
  
  
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

