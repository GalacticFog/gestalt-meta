package controllers

import java.util.UUID

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.parseUUID
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.errors._

import controllers.util._
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.data.string2uuid

import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi
import com.galacticfog.gestalt.json.Js


import javax.inject.Singleton
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.patch._
import play.api.libs.ws.WSClient


@Singleton
class ApiController @Inject()(
    ws: WSClient,
    messagesApi: MessagesApi,
    gatewayMethods: GatewayMethods,
    env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
    providerMethods: ProviderMethods,
    db: play.api.db.Database )
      extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  import gatewayMethods.unprocessable
  
  def postResourceOpt(fqon: String, typ: Option[String], parent: UUID) = AsyncAudited(fqon) { implicit request =>
    val org = fqid(fqon)
    val typeid = {
      (typ.map(UUID.fromString(_)) orElse resolveTypeFromPayload(request.body)) getOrElse {
        throw new UnprocessableEntityException(s"Missing [resource_type].")
      }
    }
    val (payload, provider, location) = validateNewApi(request.body)
    newResourceResultAsync(org, typeid, parent, payload) { resource =>
      createApi(org, payload, parent, provider, location)
    }
  }
  
  def createApi(org: UUID, payload: JsValue, parent: UUID, provider: GestaltResourceInstance, location: UUID)(
      implicit request: SecuredRequest[JsValue]) = {
    
    ResourceFactory.findById(ResourceIds.Environment, parent).fold {
      Future(ResourceNotFound(ResourceIds.Environment, parent))
    }{ _ =>
      
      //val (payload, provider, location) = validateNewApi(json)
      log.debug(s"GatewayManager: ${provider.id}, ${provider.name}, Location: $location")
      
      val lapi = gatewayMethods.toGatewayApi(payload.as[JsObject], location)
      val client = providerMethods.configureWebClient(provider, Some(ws))
      val caller = request.identity
      /*
       * Create API resource in Meta - if successful, create in GatewayManager.
       * If the create subsequently fails in GatewayManager, we set the status
       * of the already created Meta resource to 'FAILED'.
       */
      
      CreateWithEntitlements(org, caller, payload, ResourceIds.Api, Some(parent)) match {
        case Failure(e) => HandleExceptionsAsync(e)
        case Success(resource) => {
          log.debug("Creating API in GatewayManager...")
          client.post("/apis", Option(Json.toJson(lapi))) map { result =>
            
            if (Seq(200, 201).contains(result.status)) {
              log.info("Successfully created API in GatewayManager.")
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

  def postApiEndpoint(fqon: String, api: UUID) = AsyncAudited(fqon) { implicit request =>

    ResourceFactory.findById(ResourceIds.Api, api).fold {
      Future(ResourceNotFound(ResourceIds.Api, api))
    }{ a =>
      
      val caller = request.identity
      val gatewayProvider = gatewayMethods.findGatewayProvider(a) getOrElse {
        throw new UnprocessableEntityException(s"Cannot find gateway provider for API '${a.id} (${a.name})'")
      }
      log.info("Creating Endpoint in Meta...")

      val endpointId = Js.find(request.body.as[JsObject], "/id").fold(UUID.randomUUID.toString)(_.as[String])
      
      val org = fqid(fqon)
      val setup = for {
        apiProvider  <- Try{Json.parse(a.properties.get("provider")).as[JsObject]}
        endpointJson <- validateNewEndpoint(request.body, a)
        gmEndpoint   <- gatewayMethods.toGatewayEndpoint(endpointJson, api)
        updatedJson  <- addUpstreamUrlAndProvider(endpointJson, gmEndpoint.upstreamUrl, apiProvider)
      } yield (
        (updatedJson ++ Json.obj("id" -> endpointId)),
        gmEndpoint.copy(id = Some(UUID.fromString(endpointId)))
      )
      
      val uri = "/apis/%s/endpoints".format(api.toString)
      val client = providerMethods.configureWebClient(gatewayProvider, Some(ws))
      
      setup match {
        case Failure(e) => HandleExceptionsAsync(e)
        case Success((payload, gatewayEndpoint)) => {
          for {
            r <- newDefaultResourceResult(org, ResourceIds.ApiEndpoint, api, payload)
            metaEndpoint = ResourceFactory.findById(UUID.fromString(endpointId)).get
            result <- {
              log.info("Endpoint created in Meta.")
              log.info("Creating Endpoint in GatewayManager...")
              client.post(uri, Option(Json.toJson(gatewayEndpoint)))
            }
            response = {
              if (Seq(200, 201, 202).contains(result.status)) {
                log.info("Successfully created Endpoint in GatewayManager.")
                Created(RenderSingle(metaEndpoint))
              } else {
                log.error("Error creating Endpoint in GatewayManager.")
                updateFailedBackendCreate(caller, metaEndpoint, ApiError(result.status, result.body).throwable)
              }
            }
          } yield response
        }
      }
    }
  }
  
  
  private[controllers] def addUpstreamUrlAndProvider(json: JsValue, upstreamUrl: String, provider: JsObject): Try[JsObject] = {
    val propAdder = (__ \ 'properties).json.update(
      __.read[JsObject].map{ o =>
        o ++ Json.obj( "upstream_url" -> upstreamUrl ) ++ Json.obj( "provider" -> provider )
      }
    )
    json.transform(propAdder) match {
      case JsSuccess(o,_) => Success(o)
      case JsError(_) => Failure(
          new RuntimeException("unable to add 'upstream_url' and 'provider' to ApiEndpoint resource"))
    }
  }
  
//  def postApiEndpoint(fqon: String, api: UUID) = AsyncAudited(fqon) { implicit request =>
//
//    ResourceFactory.findById(ResourceIds.Api, api).fold {
//      Future(ResourceNotFound(ResourceIds.Api, api))
//    }{ a =>
//
//      val caller = request.identity
//      val gatewayProvider = gatewayMethods.findGatewayProvider(a) getOrElse {
//        throw new UnprocessableEntityException(s"Cannot find gateway provider for API '${a.id} (${a.name})'")
//      }
//
//      log.info("Creating Endpoint in Meta...")
//
//      def addUpstreamUrlAndProvider(json: JsValue, upstreamUrl: String, provider: JsObject): Try[JsObject] = {
//        val propAdder = (__ \ 'properties).json.update(
//          __.read[JsObject].map{ o =>
//            o ++ Json.obj( "upstream_url" -> upstreamUrl ) ++ Json.obj( "provider" -> provider )
//          }
//        )
//        json.transform(propAdder) match {
//          case JsSuccess(o,_) => Success(o)
//          case JsError(_) => Failure(
//              new RuntimeException("unable to add 'upstream_url' and 'provider' to ApiEndpoint resource"))
//        }
//      }
//
//      val org = fqid(fqon)
//      val metaCreate = for {
//        apiProvider <- Try{Json.parse(a.properties.get("provider")).as[JsObject]}
//        p  <- validateNewEndpoint(request.body, a)
//        ep <- gatewayMethods.toGatewayEndpoint(p, api)
//        pWithAddlProps <- addUpstreamUrlAndProvider(p, ep.upstreamUrl, apiProvider)
//        r <- CreateWithEntitlements(org, caller, pWithAddlProps, ResourceIds.ApiEndpoint, Some(api))
//      } yield (r, ep)
//
//      
//      val rr = newDefaultResourceResult(org, ResourceIds.ApiEndpoint, api, ???)
//      
//      
//      metaCreate match {
//        case Failure(e) => {
//          log.error("Failed creating Endpoint in Meta",e)
//          HandleExceptionsAsync(e)
//        }
//        case Success((metaEndpoint, gmEndpoint)) => {
//          log.info("Endpoint created in Meta.")
//          //setNewResourceEntitlements(org, metaEndpoint.id, caller, Some(api))
//
//          log.info("Creating Endpoint in GatewayManager...")
//          val uri = "/apis/%s/endpoints".format(api.toString)
//          val client = providerMethods.configureWebClient(gatewayProvider, Some(ws))
//
//          client.post(uri, Option(Json.toJson(gmEndpoint))) map { result =>
//            if (Seq(200, 201).contains(result.status)) {
//              log.info("Successfully created Endpoint in GatewayManager.")
//              Created(RenderSingle(metaEndpoint))
//            } else {
//              log.error("Error creating Endpoint in GatewayManager.")
//              updateFailedBackendCreate(caller, metaEndpoint, ApiError(result.status, result.body).throwable)
//            }
//          } recover {
//            case e: Throwable => {
//              log.error(s"Error creating Endpoint in Gateway Manager.")
//              updateFailedBackendCreate(caller, metaEndpoint, e)
//            }
//            
//          }
//        }
//      }
//
//    }
//  }

  
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

  private[controllers] def validateNewEndpoint(js: JsValue, api: GestaltResourceInstance): Try[JsValue] = Try {
    val json = js.as[JsObject]
    val id = Js.find(json, "/id") map (_.toString) getOrElse UUID.randomUUID.toString
    
    val path = Js.find(json, "/properties/resource") map (_.toString) getOrElse {
      unprocessable("Invalid payload: [apiendpoint.properties.resource] is missing.")
    }
    
    val location = {
      api.properties.fold {
        unprocessable(s"Parent API [${api.id}] does not have properties. This resource is invalid.")
      }{ ps =>
        ps.get("provider").fold {
          unprocessable(s"Parent API [${api.id}] is missing proprty [provider]. This resource is invalid.")
        }{ pstr =>
          val provider = Json.parse(pstr).as[JsObject]
          Js.find(provider, "/locations").fold {
            unprocessable(s"Parent API [${api.id}] is missing property [provider.locations]. This resource is invalid.")
          }{ locationsjson =>
            val locs = locationsjson.as[JsArray]
            locs.value.headOption.fold {
              unprocessable(s"Parent API [provider.locations] property is empty. This resource is invalid.")
            }{ 
              locjs => locjs.as[String]
            }
          }
        }
      }
    }
    
    val patch = PatchDocument(
        PatchOp.Replace("/id", JsString(id.toString)),
        PatchOp.Add("/properties/parent", JsString(api.id.toString)),
        PatchOp.Add("/properties/location_id", JsString(location)))
        
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


