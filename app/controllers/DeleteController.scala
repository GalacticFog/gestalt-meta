package controllers


import java.net.URL
import java.util.UUID

import scala.{Either,Left,Right}
import scala.math.BigDecimal.int2bigDecimal
import scala.util.{Try,Success,Failure}

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.laser.Laser
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.output.toLink
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.security.api.errors.SecurityRESTException
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import controllers.util._
import controllers.util.db.EnvConfig

import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper

import play.api.{Logger => log}

  import play.api.mvc.Result
  import play.api.mvc.RequestHeader
  import play.api.mvc.{Security => PlaySecurity}
  import play.api.libs.ws.WS
  import play.api.Play.current
  import com.galacticfog.gestalt.laser.MarathonClient
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future
  import scala.concurrent.duration._
  import scala.concurrent.Await


object DeleteController extends Authorization {
 
  private val manager = new HardDeleteInstanceManager[AuthAccountWithCreds](
      external = Map(
        ResourceIds.Org -> deleteExternalOrg,
        ResourceIds.User -> deleteExternalUser,
        ResourceIds.Group -> deleteExternalGroup,
        ResourceIds.Container -> deleteExternalContainer,
        ResourceIds.Lambda -> deleteExternalLambda,
        ResourceIds.Api -> deleteExternalApi,
        ResourceIds.ApiEndpoint -> deleteExternalEndpoint))
  
  
  def resourceFromPath(p: String): Option[GestaltResourceInstance] = {
    val cmps = { p.trim
        .stripPrefix("/")
        .stripSuffix("/")
        .split("/").toList filter { _.trim != "" }
    }
    
    val typeName = cmps size match {
      case 0 => throw new BadRequestException(s"Invalid path. found: '$p'")
      case 1 => cmps(0)
      case _ => cmps(cmps.size - 2)
    }
    
    val resourceId = UUID.fromString(cmps.last)
    
    if (typeName == "providers") {
      ResourceFactory.findById(UUID.fromString(cmps.last)) match {
        case None => throw new ResourceNotFoundException(s"Provider with ID '${cmps.last}' not found.")
        case Some(provider) => {
          log.debug(s"Looking up ${ResourceLabel(provider.typeId)}, ${resourceId}")
          ResourceFactory.findById(provider.typeId, resourceId)
        }
      }
    } else Resource.findByPath(p)
    
  }
  
  def hardDeleteTest(fqon: String, path: String) = Authenticate(fqon) { implicit request =>
    
    val p = if (path.trim.isEmpty) fqon else "%s/%s".format(fqon, path)
    
    resourceFromPath( p ).fold {
      NotFoundResult(request.uri)
    } { resource =>
      
      DeleteHandler.handle(resource, request.identity) match {
        case Failure(e) => HandleExceptions(e)
        case Success(a) => NoContent
      }
      //DefaultRequestRouter.route( resource )
    }
    
  }

  def deleteExternalOrg[A <: ResourceLike](res: A, account: AuthAccountWithCreds) = {
    Security.deleteOrg(res.id, account) map ( _ => () )
  }
  
  def deleteExternalUser[A <: ResourceLike](res: A, account: AuthAccountWithCreds) = {
    Security.deleteAccount(res.id, account) map ( _ => () )
  }  

  def deleteExternalGroup[A <: ResourceLike](res: A, account: AuthAccountWithCreds) = {
    Security.deleteGroup(res.id, account) map ( _ => () )
  }
  
  def deleteExternalContainer[A <: ResourceLike](res: A, account: AuthAccountWithCreds) = Try {
    val result = Await.result(deleteMarathonApp(res), 5 seconds)
  }
  
  def deleteExternalLambda[A <: ResourceLike](res: A, account: AuthAccountWithCreds) = {
    laser.deleteLambda(res.id) map ( _ => () )
  }

  
  def deleteExternalApi[A <: ResourceLike](res: A, account: AuthAccountWithCreds) = {
    laser.deleteApi(res.id) map ( _ => () )
  }
  
  def deleteExternalEndpoint[A <: ResourceLike](res: A, account: AuthAccountWithCreds) = {
    val api = UUID.fromString(res.properties.get("api"))
    laser.deleteEndpoint(api, res.id) map ( _ => () )
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
    if (res.typeId == ResourceIds.MarathonProvider &&
        !singleParamBoolean(qs, "deleteContainers")) {
      
      log.debug("Delete Marathon Provider: 'deleteContainers' is FALSE.")
      log.debug("Containers WILL NOT be deleted from Marathon.")
      
      Seq(ResourceIds.Container)
    } else Seq()
  }
  

  
  protected [controllers] def deleteMarathonApp[A <: ResourceLike](container: A): Future[JsValue] = {
    val providerId = Json.parse(container.properties.get("provider")) \ "id"
    val provider = ResourceFactory.findById(UUID.fromString(providerId.as[String])) getOrElse {
      throw new RuntimeException("Could not find Provider : " + providerId)
    }
    val externalId = container.properties.get("external_id")
    
    marathonClient(provider).deleteApplication(externalId)
  }
  
  protected [controllers] def marathonClient(provider: GestaltResourceInstance): MarathonClient = {
    val providerUrl = (Json.parse(provider.properties.get("config")) \ "url").as[String]
    log.debug("Marathon URL: " + providerUrl)
    MarathonClient(WS.client, providerUrl)
  }
  
  


//  def deleteEnvironmentFqon(fqon: String, environment: UUID) = Authenticate(fqon) { implicit request =>
//    orgFqon(fqon) match {
//      case Some(org) => {
//        HardDeleteEnvironment.delete(environment, true) match {
//          case Success(_) => NoContent
//          case Failure(e) => HandleRepositoryExceptions(e)
//        }
//      }
//      case None => OrgNotFound(fqon)
//    }
//  }
//  
//  def deleteWorkspaceFqon(fqon: String, workspace: UUID) = Authenticate(fqon) { implicit request =>
//
//    Authorize(fqid(fqon), Actions.Workspace.Delete, request.identity) {
//      HardDeleteWorkspace.delete(workspace, true) match {
//        case Success(_) => NoContent
//        case Failure(e) => HandleRepositoryExceptions(e)
//      }
//    }
//  }
  
  
  def removeEndpointImplementation(endpoint: UUID) = {
    ???
  }
  
  
  def deleteLambda(org: UUID, lambda: UUID) = Authenticate(org) { implicit request =>
    /*
     * 1.) Get list of associated endpoints.
     * 2.) Update each endpoint (removing, implementation property)
     * 3.) Delete the lambda
     */
//    ResourceFactory.findEndpointsByLambda(lambda) foreach { p =>
//      val pjson = Json.toJson(p)
//    }
    ???  
  }
  
  
//  def deleteLevel1Resource(org: UUID, restName1: String, id1: UUID) = Authenticate(org) { implicit request =>
//    trace(s"deleteLevel1Resource($org, $restName1, $id1)")
//    if (!handlers.contains(restName1)) NotFoundResult(request.path)
//    else {
//      lookupfn(restName1)(id1) match {
//        case None => NotFoundResult("not found.")
//        case Some(_) => {
//          val force = getForceParam(request.queryString)
//          handlers(restName1).delete(id1, force) match {
//            case Success(_) => NoContent
//            case Failure(e) => HandleRepositoryExceptions(e)
//          }
//          
//        }
//      }
//    }
//  }
  
  import scala.util.{Try,Success,Failure}
  
  def getForceParam(qs: Map[String,Seq[String]]): Boolean = {
    if (!qs.contains("force")) false
    else {
      val fp = qs("force")
      Try {
        fp.mkString.toBoolean
      } match {
        case Success(b) => b == true
        case Failure(_) => throw new BadRequestException(s"Value of 'force' parameter must be true or false. found: $fp")
      }
    }
  }
  
  
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
  
  def deleteLevel2Resource(org: UUID, restName1: String, id1: UUID, restName2: String, id2: UUID) = Authenticate(org) { implicit request =>
    trace(s"deleteLevel1Resource($org, $restName1, $id1, $restName2, $id2)")
    Ok(s"DELETING $restName2 : ${id2.toString}")  
  }
  
  
  // =========================================================
  
  
  def deleteProviderFqon(fqon: String, parentType: String, parentId: UUID) = Authenticate(fqon) { implicit request =>
    ???  
  }
  
  
  def deleteProviderOrgFqon(fqon: String, provider: UUID) = Authenticate(fqon) { implicit request =>
    val org = fqid(fqon)
    println("Calling delete Common...")
    deleteProviderCommon(org, org, provider)
  }  
  
  
  def deleteProviderCommon(org: UUID, parentId: UUID, provider: UUID) = {
    val types = ResourceFactory.findTypesWithVariance(CoVariant(ResourceIds.Provider)) map { _.id }
    
    ResourceFactory.findById(provider) match {
      
      case None    => NotFoundResult(s"Provider with ID '$provider' not found.")
      case Some(p) => {
        
        // TODO: Ensure the resource we found is actually a child of 'parent'
        
        // We found a resource with the ID, now check if it's actually a provider...
        if (!types.contains(p.typeId)) NotFoundResult(s"Provider with ID '$provider' not found.")
        else {
          
          // Check if it's a gateway provider
          if (p.typeId == ResourceIds.ApiGatewayProvider) {
            
            val laserProviderId = p.properties.get("external_id")

            laser.deleteProvider(laserProviderId) match {
              case Failure(e) => HandleExceptions(e)
              case Success(_) => {
                
                // Now delete the provider from Meta
                ResourceFactory.hardDeleteResource(provider) match {
                  case Failure(e) => HandleExceptions(e)
                  case Success(_) => NoContent
                }
              }
            }
          } else {
            ResourceFactory.hardDeleteResource(provider) match {
              case Failure(e) => HandleExceptions(e)  
              case Success(_) => NoContent
            }
          }
          
        }
      }
    }    
  }  
  
  def hardDeleteWorkspaceProvider(org: UUID, workspace: UUID, id: UUID) = Authenticate(org) { implicit request =>
    hardDeleteMetaResource(id, ResourceIds.ApiGatewayProvider)
  }
  
  def hardDeleteEnvironmentProvider(org: UUID, environment: UUID, id: UUID) = Authenticate(org) { implicit request =>
    hardDeleteMetaResource(id, ResourceIds.MarathonProvider)  
  }
  
  def hardDeleteEnvironmentProviderFqon(fqon: String, environment: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    // TODO: Ensure the given provider is a chid of this Environment.
    deleteProviderCommon(fqid(fqon), environment, id)
  }
  
  def hardDeleteWorkspaceProviderFqon(fqon: String, workspace: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    // TODO: Ensure the given provider is a child of this Workspace.
    deleteProviderCommon(fqid(fqon), workspace, id)
  }
  
  def hardDeleteChildFqon(fqon: String, childType: UUID, childId: UUID, parentType: UUID, parentId: Some[UUID]) = Authenticate(fqon) { implicit request =>
    val org = fqid(fqon)
    hardDeleteChild(org, childType, childId, ResourceIds.Org, org)
  }
  
  
  def hardDeleteResourceFqon(fqon: String, resourceId: UUID) = Authenticate(fqon) { implicit request =>
    ResourceFactory.findById(resourceId) match {
      case None => NotFoundResult(request.uri)
      case Some(r) => {
        if (r.orgId != fqid(fqon)) NotFoundResult(s"Resource ID ${resourceId} not found in Org '$fqon'.")
        else ResourceFactory.hardDeleteResource(resourceId) match {
          case Success(_) => NoContent
          case Failure(e) => HandleRepositoryExceptions(e)
        }
      }
    }
  }
  
  def hardDeleteResourceByNameFqon(fqon: String, restName: String, resourceId: UUID) = Authenticate(fqon) { implicit request =>
    extractByName(fqon, restName) match {
      case Left(result) => result
      case Right((org, typeId)) => ResourceFactory.findById(typeId, resourceId) match {
        case None => NotFoundResult(request.uri)
        case Some(res) => ResourceFactory.hardDeleteResource(resourceId) match {
          case Success(_) => NoContent
          case Failure(e) => HandleRepositoryExceptions(e)
        }
      }
    }
  }
  
  
  def hardDeleteChild(org: UUID, childType: UUID, childId: UUID, parentType: UUID, parentId: UUID) = {
    
    def notfound(label: String, id: UUID) = "%s with ID %s not found.".format(label, id)
    
    ResourceFactory.findById(parentType, parentId) match {
      case None => NotFoundResult(notfound(ResourceLabel(parentType), parentId))
      case Some(res) => ResourceFactory.hardDeleteResource(childType, childId) match {
        case Success(_) => NoContent
        case Failure(e) => HandleRepositoryExceptions(e)
      } 
    }
  }
  

  def hardDeleteLambda(org: UUID, id: UUID) = Authenticate(org) { implicit request =>
    hardDeleteLambdaCommon(org, id)
  }
  
  def hardDeleteLambdaCommon(org: UUID, id: UUID)(implicit request: SecuredRequest[_]) = {
    val associatedRules = 
      ResourceFactory.findAllByPropertyValue(ResourceIds.RuleEvent, "lambda", id.toString)
    
    if (associatedRules.isEmpty) {
      log.debug("Deleting Lambda...")
        hardDeleteMetaResource(id, ResourceIds.Lambda) 
      } else {
    
      log.warn("Conflict - Attempting to delete lambda with associated rules.")
      val msg = "There are Event Rules associated with this Lambda. Cannot delete."
      val response = Json.obj("code" -> 409, "message" -> msg, 
          "conflicts" -> Json.toJson(
              associatedRules map { toLink(_, META_URL) }).as[JsObject])
      Conflict(response)
    }
  }
  
  def canDeleteLambda(org: UUID, id: UUID)(implicit request: SecuredRequest[_]): Either[JsValue,Unit] = {
    val associatedRules = 
      ResourceFactory.findAllByPropertyValue(ResourceIds.RuleEvent, "lambda", id.toString)
    
    if (associatedRules.isEmpty) Right(Unit) else {
      log.warn("Conflict - Attempting to delete lambda with associated rules.")
      Left {      
        val msg = "There are Event Rules associated with this Lambda. Cannot delete."
        val conflicts = Json.toJson(associatedRules map { toLink(_, META_URL) })
        Json.obj("code" -> JsNumber(409), "message" -> JsString(msg), 
            "conflicts" -> conflicts)
      }
      
    }    
  }
  
  lazy val gatewayConfig = HostConfig.make(new URL(EnvConfig.gatewayUrl))
  lazy val lambdaConfig  = HostConfig.make(new URL(EnvConfig.lambdaUrl))
  lazy val laser = new Laser(
    gatewayConfig, lambdaConfig, 
    Option(EnvConfig.securityKey), 
    Option(EnvConfig.securitySecret))
  
  
  def deleteLaserApi(id: UUID) = ???
  def deleteLaserEndpoint(apiId: UUID, id: UUID) = ???
  def deleteLaserLambda(id: UUID) = ???
  
  
//  def hardDeleteApiFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
//    orgFqon(fqon) match {
//      case None => OrgNotFound(fqon)
//      case Some(org) => {
//        ResourceFactory.findById(ResourceIds.Api, id) match {
//          case None => NotFoundResult(request.uri)
//          case Some(api) => {
//            laser.deleteApi(id.toString) match {
//              case Failure(e) => HandleRepositoryExceptions(e)
//              case Success(_) => HardDeleteApi.delete(id, true) match {
//                case Success(_) => NoContent
//                case Failure(e) => HandleRepositoryExceptions(e)
//              }
//            }
//          }
//        }
//      }
//    }
//  }
  
  
//  def deleteApiCommon(id: UUID) = Try {
//    ResourceFactory.findById(ResourceIds.ApiEndpoint, id) match {
//      case None => throw new ResourceNotFoundException(s"API with ID $id not found.")
//      case Some(ep) => {
//        val apiId = ep.properties.get("api")
//        
//        // 2.) Delete endpoint in laser
//        laser.deleteEndpoint(apiId, id) match {
//          case Failure(e) => throw e
//          case Success(_) => {
//            // 3.) Delete endpoint in meta
//            HardDeleteEndpoint.delete(id, true).get
//          }
//        }
//      }
//    }
//  }
  

  
//  def hardDeleteEndpointFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
//    trace(s"hardDeleteEndpointFqon($fqon, $id)")
//
//    orgFqon(fqon) match {
//      case None => OrgNotFound(fqon)
//      case Some(org) => {
//        println(s"Looking up endpoint $id")
//        // 1.) Lookup given endpoint
//        ResourceFactory.findById(ResourceIds.ApiEndpoint, id) match {
//          case None => NotFoundResult(request.uri)
//          case Some(ep) => {
//            
//            val apiId = ep.properties.get("api")
//            println(s"FOUND ENDPOINT: API-ID => $apiId")
//            // 2.) Delete endpoint in laser
//            laser.deleteEndpoint(apiId, id) match {
//              case Failure(e) => HandleRepositoryExceptions(e)
//              case Success(_) => {
//                // 3.) Delete endpoint in meta
//                HardDeleteEndpoint.delete(id, true) match {
//                  case Success(_) => NoContent
//                  case Failure(e) => {
//                    log.error(e.getMessage)
//                    HandleRepositoryExceptions(e)
//                  }
//                }                
//              }
//            }
//          }
//        }
//      }
//    }
//  }
  
//  def hardDeleteLambdaFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
//    
//    canDeleteLambda(fqid(fqon), id) match {
//      case Left(json) => Conflict(json)
//      case Right(_)   => {
//        ResourceFactory.findById(ResourceIds.Lambda, id) match {
//          case None => NotFoundResult(s"Lambda with ID '$id' not found.")
//          case Some(lambda) => {
//            
//            ResourceFactory.getLaserLambdaIds(id) foreach { lid => 
//              laser.deleteLambda(lid).get
//            }
//            
//            ResourceFactory.findAllApisByLambda(id) foreach { aid =>
//              laser.deleteApi(aid).get  
//            }
//            
//            HardDeleteLambda.delete(id, true) match {
//              case Success(_) => NoContent
//              case Failure(e) => HandleExceptions(e)
//            }
//          }
//        }
//      }
//    }
//  }  
  
  
  def hardDeleteWorkspaceDomain(org: UUID, workspace: UUID, id: UUID) = Authenticate(org) { implicit request =>
    hardDeleteMetaResource(id, ResourceIds.Domain)
  }
  
  
  def hardDeleteWorkspaceDomainFqon(fqon: String, workspace: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => hardDeleteMetaResource(id, ResourceIds.Domain)
      case None => OrgNotFound(fqon)
    }
  }
  
//  /**
//   * Permanently delete an Org from Security and Meta
//   */
//  def hardDeleteOrg(org: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
//    trace(s"hardeDeleteOrg($org)")
//    hardDeleteSecure(org, request.identity, Security.deleteOrg)    
//  }
//  
//  /**
//   * 
//   */
//  def hardDeleteOrgFqon(fqon: String) = Authenticate(fqon) { implicit request =>
//    orgFqon(fqon) match {
//      case Some(org) => hardDeleteSecure(org.id, request.identity, Security.deleteOrg)
//      case None      => OrgNotFound(fqon)
//    }    
//  }
//  
//  def hardDeleteGroup(fqon: String, group: UUID) = Authenticate(fqon) { implicit request =>
//    hardDeleteSecure(group, request.identity, Security.deleteGroup)
//  }
//  
//  /**
//   * Permanently delete a User/Account from Security and Meta
//   */
//  def hardDeleteUser(org: UUID, id: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
//    hardDeleteSecure(id, request.identity, Security.deleteAccount)
//  }
//  
//  /**
//   * 
//   */
//  def hardDeleteUserFqon(fqon: String, id: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
//    orgFqon(fqon) match {
//      case Some(org) => hardDeleteSecure(id, request.identity, Security.deleteAccount)
//      case None      => OrgNotFound(fqon)
//    }
//  }
  
  /**
   * Permanently delete a ResourceType along with any associated TypeProperties
   */
   def hardDeleteResourceTypeFqon(fqon: String, id: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
     orgFqon(fqon) match {
       case Some(org) => hardDeleteResourceType(id)
       case None      => OrgNotFound(fqon)
     }
   }
   
   def hardDeleteResourceType(typeId: UUID) = {
     TypeFactory.hardDeleteType(typeId) match {
       case Success(_) => NoContent
       case Failure(e) => e match {
         case iae : IllegalArgumentException => BadRequestResult(iae.getMessage)
         case x => GenericErrorResult(500, x.getMessage)
       }
     }
   }
   
  /**
   * Permanently delete a Resource from Meta and Security
   */   
  def hardDeleteSecure(id: UUID, auth: AuthAccountWithCreds, fn: SecurityDelete) = {
    
    hardDeleteSynchronized(id, auth, fn) match {
      case Success(_) => NoContent
      case Failure(e) => {
        log.error(s"hardDeleteSecure: ERROR: " + e.getMessage)
        e match {
          case s: SecurityRESTException     => HandleExceptions(s)
          case r: ResourceNotFoundException => NotFoundResult(r.getMessage)
          case _ => GenericErrorResult(500, e.getMessage)
        }
      }
    }
  }
  
  

  def hardDeleteSecurityResource[A <: ResourceLike](res: A, auth: AuthAccountWithCreds, fn: SecurityDelete) = {
    
    hardDeleteSynchronized(res.id, auth, fn) map { _ => Unit } 
//    match {
//      case Success(_) => Success(Unit)
//      case Failure(e) => HandleExceptions(e) 
//        {
//        log.error(s"hardDeleteSecure: ERROR: " + e.getMessage)
//        
//        e match {
//          case s: SecurityRESTException     => HandleExceptions(s)
//          case r: ResourceNotFoundException => NotFoundResult(r.getMessage)
//          case _ => GenericErrorResult(500, e.getMessage)
//        }
//      }
    //}
  }  
  
  def hardDeleteSecure[A <: ResourceLike](res: A, auth: AuthAccountWithCreds, fn: SecurityDelete) = {
    
    hardDeleteSynchronized(res.id, auth, fn) match {
      case Success(_) => NoContent
      case Failure(e) => {
        log.error(s"hardDeleteSecure: ERROR: " + e.getMessage)
        e match {
          case s: SecurityRESTException     => HandleExceptions(s)
          case r: ResourceNotFoundException => NotFoundResult(r.getMessage)
          case _ => GenericErrorResult(500, e.getMessage)
        }
      }
    }
  }  
  

  
  
  // TODO: Simpler since resources aren't synced with security.
  private def hardDeleteMetaResource(id: UUID, typeId: UUID) = {
    ResourceFactory.hardDeleteResource(typeId, id) match {
      case Success(_) => NoContent
      case Failure(e) => {
        log.error(s"hardDeleteMetaResource($id, $typeId)")
        HandleRepositoryExceptions(e)
      }
    }
  }
    
}

