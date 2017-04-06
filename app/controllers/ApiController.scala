package controllers

import java.net.URL
import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Either, Left, Right}
import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.parseUUID
import com.galacticfog.gestalt.data.session
import com.galacticfog.gestalt.data.string2uuid
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.meta.api.errors._

import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.output.toLink
import com.galacticfog.gestalt.meta.api.sdk._

import controllers.util._
import controllers.util.JsonUtil._
import controllers.util.db.EnvConfig
import play.api.Logger
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import com.galacticfog.gestalt.meta.auth.Authorization

import scala.util.Either
import com.galacticfog.gestalt.keymgr.GestaltFeature

import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi
import com.galacticfog.gestalt.json.Js


import javax.inject.Singleton
import com.galacticfog.gestalt.meta.providers._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.patch._
import controllers.util.GatewayMethods._
import play.api.libs.ws.WSClient


@Singleton
class ApiController @Inject()(
    ws: WSClient,
    messagesApi: MessagesApi,
    env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
      extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  /*
   *  This is the name of the provider variable needed to get the host address.
   */
  private[this] val hostVariable = "HTTP_API_VHOST_0"
  
  
  def postApi(fqon: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    ResourceFactory.findById(ResourceIds.Environment, parent).fold {
      Future(ResourceNotFound(ResourceIds.Environment, parent))
    }{ _ =>
      val (payload, provider, location) = validateNewApi(request.body)
      log.debug(s"GatewayManager: ${provider.id}, ${provider.name}, Location: $location")  

      val lapi = toGatewayApi(payload.as[JsObject], location)
            	
      val caller = request.identity
      val client = ProviderMethods.configureWebClient(provider, hostVariable, Some(ws))
      val org = fqid(fqon)
      
      /*
       * Create API resource in Meta - if successful, create in GatewayManager.
       * If the create subsequently fails in GatewayManager, we set the status
       * of the already created Meta resource to 'FAILED'.
       */

      CreateResource(fqid(fqon), payload, caller, ResourceIds.Api, parent) match {
        case Failure(e) => HandleExceptionsAsync(e)
        case Success(resource) => {
          log.debug("Creating API in GatewayManager...")
          client.post("/apis", Option(Json.toJson(lapi))) map { result =>
            
            if (Seq(200, 201).contains(result.status)) {
              log.info("Successfully created API in GatewayManager.")
              setNewEntitlements(org, resource.id, caller, Some(parent))
              Created(RenderSingle(resource))
            } else {
              log.error("Error creating API in GatewayManager.")
              updateFailedBackendCreate(caller, resource, ApiError(result.status, result.body).throwable)
            }
          } recover {
            case e: Throwable => {
              log.error(s"Error creating API in GatewayManager.")
              updateFailedBackendCreate(caller, resource, e)
            }
          }
        }
      }
    }
  }
  
  def putApi(fqon: String, api: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    ???
  }
  
  def postApiEndpoint(fqon: String, api: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>

    ResourceFactory.findById(ResourceIds.Api, api).fold {
      Future(ResourceNotFound(ResourceIds.Api, api)) 
    }{ a =>
      
      val caller = request.identity
      val provider = findGatewayProvider(a).get // <-- can generate in 'for' below
      
      log.info("Creating Endpoint in Meta...")
      
      val org = fqid(fqon)
      val metaCreate = for {
        p <- validateNewEndpoint(request.body, api)
        l <- toGatewayEndpoint(p, api)
        r <- CreateResource(org, p, caller, ResourceIds.ApiEndpoint, api) 
      } yield (r, l)
      
      metaCreate match {
        case Failure(e) => {
          log.error("Failed creating Endpoint in Meta")
          HandleExceptionsAsync(e)
        }
        case Success((metaep, laserep)) => {
          log.info("Endpoint created in Meta.")
          setNewEntitlements(org, metaep.id, caller, Some(api))
          
          log.info("Creating Endpoint in GatewayManager...")
          val uri = "/apis/%s/endpoints".format(api.toString)
          val client = ProviderMethods.configureWebClient(provider, hostVariable, Some(ws))
          
          client.post(uri, Option(Json.toJson(laserep))) map { result =>
            if (Seq(200, 201).contains(result.status)) {
              log.info("Successfully created Endpoint in GatewayManager.")
              setNewEntitlements(org, metaep.id, caller, Some(api))
              Created(RenderSingle(metaep))
            } else {
              log.error("Error creating Endpoint in GatewayManager.")
              updateFailedBackendCreate(caller, metaep, ApiError(result.status, result.body).throwable)
            }
          } recover {
            case e: Throwable => {
              log.error(s"Error creating Endpoint in Gateway Manager.")
              updateFailedBackendCreate(caller, metaep, e)
            }
            
          }
        }
      }

    }
  }

  
  private[controllers] def validateNewApi(js: JsValue): (JsValue, GestaltResourceInstance, UUID) = {

    // provider.id property exists
    val pid = Js.find(js.as[JsObject], "/properties/provider/id") getOrElse {
      unprocessable("Missing required property [properties.provider.id]")
    }
    
    val location = Js.find(js.as[JsObject], "/properties/provider/locations") flatMap { locs =>
      locs.as[JsArray].value.headOption
    } getOrElse {
      unprocessable("Missing required property [properties.provider.locations]")
    }
    
    // 'provider.id' is valid UUID
    val uid = parseUUID(pid.as[String]) getOrElse {
      unprocessable(s"Invalid [provider.id] (not a valid UUID). found: '${pid}'")
    }

    val locationuid = parseUUID(location.as[String]) getOrElse {
      unprocessable(s"Invalid [provider.location] (not a valid UUID). found: '$location'")
    }
    // If payload doesn't specify /id, inject one.
    val finaljson = Js.find(js.as[JsObject], "/id").fold {
      js.as[JsObject] ++ Json.obj("id" -> UUID.randomUUID.toString)
    }{ json => json.as[JsObject] }
    
    // 'provider.id' is an existing KONG provider.
    ResourceFactory.findById(ResourceIds.GatewayManager, uid).fold {
      unprocessable(s"GatewayManager provider with ID '$pid' not found")
    }{
      gateway => (finaljson, gateway, locationuid)
    }
  }

  private[controllers] def validateNewEndpoint(js: JsValue, api: UUID): Try[JsValue] = Try {
    val json = js.as[JsObject]
    val id = Js.find(json, "/id") map (_.toString) getOrElse UUID.randomUUID.toString

    val path = Js.find(json, "/properties/resource") map (_.toString) getOrElse {
      unprocessable("Invalid payload: [apiendpoint.properties.resource] is missing.")
    }

    val patch = PatchDocument(
        PatchOp.Replace("/id", JsString(id.toString)),
        PatchOp.Add("/properties/parent", JsString(api.toString)))
        
    patch.applyPatch(json).get
  }

  private[this] def standardRequestOptions(
    user: AuthAccountWithCreds,
    parent: UUID,
    resource: GestaltResourceInstance,
    data: Option[Map[String, String]] = None) = {

    RequestOptions(user,
      authTarget = Option(parent),
      policyOwner = Option(parent),
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
  

}


