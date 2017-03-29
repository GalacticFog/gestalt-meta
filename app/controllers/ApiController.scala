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


/*
 * 
 * TODO:
 * 
 * -| More refactoring - cleanup and generalize function to translate meta gateway providers
 * -| to laser gateway providers. Need to ensure that properties.external_id is used in any
 * -| call to laser that needs a provider ID.
 * 
 */
import javax.inject.Singleton
import com.galacticfog.gestalt.meta.providers._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.ResourceState

@Singleton
class ApiController @Inject()(
    messagesApi: MessagesApi,
    env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
      extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  case class EndpointImpl(`type`: String, id: String, function: String)
  object EndpointImpl {
    implicit lazy val endpointImplFormat = Json.format[EndpointImpl]  
  }
  
  def configureWebClient(provider: GestaltResourceInstance): JsonWebClient = {

    val privatevars = for {
      env <- ProviderEnv.fromResource(provider)
      prv <- env.privatev
    } yield prv

    val config = privatevars map { vs =>
      val url = "http://example.com"
      val key = ""
      val secret = ""
//      val url = vs.get("PROVIDER_URL") getOrElse {
//        throw new UnprocessableEntityException("Missing 'PROVIDER_URL' variable.") 
//      }
//      val key = vs.get("AUTH_KEY") getOrElse {
//        throw new UnprocessableEntityException("Missing 'AUTH_KEY' variable.") 
//      }
//      val secret = vs.get("AUTH_SECRET") getOrElse {
//        throw new UnprocessableEntityException("Missing 'AUTH_SECRET' variable.") 
//      }
      HostConfig.make(new URL(url), creds = Some(BasicCredential(key, secret)))
      //HostConfig.make(new URL(url), creds = Some(BearerCredential("76762798-07bb-42c4-ba3f-429f297a7335")))
    } getOrElse {
      throw new UnprocessableEntityException("Could not parse [properties.config.env] from provider")
    }
    new JsonWebClient(config)
  }
  
  def get[T](client: JsonWebClient, resource: String, expected: Seq[Int])
      (implicit fmt: Format[T]): Option[T] = {
    
    JsonWebClient.apiResponse(client.get(resource), expected = expected) match {
      case Failure(err) => throw err
      case Success(res) => res.output match {
        case Some(out) => out.validate[T] match {
          case s: JsSuccess[T] => Some(s.get)
          case e: JsError => 
            throw new RuntimeException(Js.errorString(e))
        }
        case None => None
      }
    }    
  }
  
  
  def postApi(fqon: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    ResourceFactory.findById(ResourceIds.Environment, parent).fold {
      Future(ResourceNotFound(ResourceIds.Environment, parent))
    }{ _ =>
      val (payload, provider, location) = validateNewApi(request.body)
      log.debug(s"GatewayManager: ${provider.id}, ${provider.name}, Location: $location")  
      
      val apiname = Js.find(payload.as[JsObject], "/name") match {
        case None => unprocessable("Could not find /name property in payload")
        case Some(n) => n.as[String]
      }
      
      // Extract 'id' from payload, use to create backend system API payload
      val id = Js.find(payload.as[JsObject], "/id").get.as[String]
      val lapi = LaserApi(Some(UUID.fromString(id)), apiname, 
            provider = Some(Json.obj(
            	"id" -> provider.id.toString, 
            	"location" -> location)))
      
      val caller = request.identity
      val client = configureWebClient(provider)
      val org = fqid(fqon)
      
      /*
       * Create API resource in Meta - if successful, create in GatewayManager.
       * If the create subsequently fails in GatewayManager, we set the status
       * of the already created Meta resource to 'FAILED'.
       */
      CreateResource(org, payload, caller, ResourceIds.Api, parent) match {
        case Failure(e) => HandleExceptionsAsync(e)
        case Success(r) => {
          setNewEntitlements(org, r.id, caller, Some(parent))
          Future(Created(RenderSingle(r)))
        }
      }
      
//      CreateResource(fqid(fqon), payload, caller, ResourceIds.Api, parent) match {
//        case Failure(e) => HandleExceptionsAsync(e)
//        case Success(r) => {
//          client.post("/apis", Option(Json.toJson(lapi))) map { result =>
//            log.debug("Creating API in GatewayManager...")
//            val response = client.unwrapResponse(result, Seq(200))
//            log.debug("Response from GatewayManager: " + response.output)
//            Created(RenderSingle(r))
//          } recover {
//            case e: Throwable => {
//              log.error(s"Error creating API in Gateway Manager: " + e.getMessage)
//              log.error("Setting Meta API state to 'FAILED'")
//
//              val failstate = ResourceState.id(ResourceStates.Failed)
//              ResourceFactory.update(r.copy(state = failstate), caller.account.id)
//              HandleExceptions(e)
//            }
//          }
//        }
//      }
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
      // provider.id property exists
      import com.galacticfog.gestalt.meta.api.output.gestaltResourceInstanceFormat
      
      val prvder = Json.parse(a.properties.get("provider")).as[JsObject]

      val pid = Js.find(/*Json.toJson(a).as[JsObject]*/ prvder, "/id").fold {
        unprocessable("Missing required property [properties.provider.id]")
      }{ js => UUID.fromString(js.as[String]) }

//      val pid = ResourceFactory.findById(ResourceIds.Api, api).fold {
//        unprocessable(s"API with ID '$api' not found.")
//      }{ a => 
//Js.find(Json.toJson(a).as[JsObject], "/properties/provider/id").fold {
////        unprocessable("Missing required property [properties.provider.id]")
////      }{ js => UUID.fromString(js.as[String]) }        
//      }
      
      log.debug("Parsed provider ID from API for endpoint: " + pid)

      val provider = ResourceFactory.findById(ResourceIds.GatewayManager, pid).fold {
        unprocessable(s"GatewayManager provider with ID '$pid' not found")
      }{ gateway => gateway }
      
      val client = configureWebClient(provider)
      
      log.debug("Creating Endpoint in Meta...")
      
      val org = fqid(fqon)
      val metaCreate = for {
        p <- validateNewEndpoint(request.body, api)
        l <- toLaserEndpoint(p, api)
        r <- CreateResource(org, p, caller, ResourceIds.ApiEndpoint, api) 
      } yield (r, l)
      
      metaCreate match {
        case Failure(e) => HandleExceptionsAsync(e)
        case Success((r, _)) => {
          setNewEntitlements(org, r.id, request.identity, Some(api))
          Future(Created(RenderSingle(r)))
        }
      }
      
//      metaCreate match {
//        case Failure(e) => {
//          log.error("Failed creating Endpoint in Meta")
//          HandleExceptionsAsync(e)
//        }
//        case Success((metaep, laserep)) => {
//          log.debug("Endoint created in Meta")
//          log.debug("Creating Endpoint in GatewayManager...")
//          client.post("/endpoints", Option(Json.toJson(laserep))) map { result =>
//            val response = client.unwrapResponse(result, Seq(200, 201))
//            log.debug("Response from GatewayManager: " + response.output)
//            Created(RenderSingle(metaep))
//          } recover {
//            case e: Throwable => {
//              log.error(s"Error creating API in Gateway Manager: " + e.getMessage)
//              log.error("Setting Meta API state to 'FAILED'")
//
//              val failstate = ResourceState.id(ResourceStates.Failed)
//              ResourceFactory.update(metaep.copy(state = failstate), caller.account.id)
//              HandleExceptions(e)
//            }
//          }
//        }
//      }

    }
  }
  
  def postApiEndpointEnv(fqon: String, env: UUID, api: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    /*
     * TODO: Ensure environment and api exists.
     */
    ResourceFactory.findById(ResourceIds.Api, api).fold {
      Future(ResourceNotFound(ResourceIds.Api, api)) 
    }{ a =>
      createResourceCommon(fqid(fqon), api, ResourceIds.ApiEndpoint, request.body)
    }
  }  
  
  private[controllers] def postApiEndpointCommon(fqon: String, api: UUID)(implicit request: SecuredRequest[JsValue]) = {
    
  }
  
  private[controllers] def validateNewApi(js: JsValue): (JsValue, GestaltResourceInstance, String) = {

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
      unprocessable(s"Invalid [provider.id] (not a valid UUID). found '${pid}'")
    }

    // If payload doesn't specify /id, inject one.
    val finaljson = Js.find(js.as[JsObject], "/id").fold {
      js.as[JsObject] ++ Json.obj("id" -> UUID.randomUUID.toString)
    }{ json => json.as[JsObject] }
    
    // 'provider.id' is an existing gateway-manager provider.
    ResourceFactory.findById(ResourceIds.GatewayManager, uid).fold {
      unprocessable(s"GatewayManager provider with ID '$pid' not found")
    }{
      gateway => (finaljson, gateway, location.as[String])
    }
  }
    
/*
{
  "name":"ApiEndpoint-2",
  
  "properties":{
    "resource": "/path",
    "http_method":"GET",
    "auth_type":{
      "type":"None"
    },
    "upstream_url": "https://lambda.lambda.io:5432/lambdas/{uuid}/invoke"
}

case class EndpointDao(
  id : Option[String],
  apiId : String,
  upstreamUrl : String,
  path : Option[String],
  domain : Option[String],
  endpointInfo : Option[JsValue],
  url : Option[String] = None,
  payload : Option[String]
)
*/  
      import com.galacticfog.gestalt.patch._
  
  private[controllers] def validateNewEndpoint(js: JsValue, api: UUID): Try[JsValue] = Try {
    val json = js.as[JsObject]
    val id = Js.find(json, "/id") map (_.toString) getOrElse UUID.randomUUID.toString

    val path = Js.find(json, "/properties/resource") map (_.toString) getOrElse {
      unprocessable("Invalid payload: [apiendpoint.properties.resource] is missing.")
    }
    
//    val upstreamUrl = Js.find(json, "/properties/upstream_url").fold {
//      unprocessable("Invalid payload: [apiendpoint.properties.upstream_url] is missing.")
//    }{ url => url.toString }
//
//    val implid = Js.find(json, "/properties/implementation_id") map (i => UUID.fromString(i.as[String])) getOrElse {
//      unprocessable("Invalid payload: [apiendpoint.properties.implementation_id] is missing.")
//    }

    val patch = PatchDocument(
        PatchOp.Replace("/id", JsString(id.toString)),
        PatchOp.Add("/properties/parent", JsString(api.toString)))
        
    patch.applyPatch(json).get
  }
  
  
  def toLaserEndpoint(js: JsValue, api: UUID): Try[LaserEndpoint] = Try {
    val json = js.as[JsObject]
    val id = Js.find(json, "/id").get.toString
    val upstreamUrl = Js.find(json, "/properties/upstream_url") getOrElse JsString("") //.get.toString
    val path = Js.find(json, "/properties/resource").get.toString
    
    LaserEndpoint(Some(id), 
        apiId = api.toString,
        upstreamUrl = upstreamUrl.toString, 
        path = path)
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
  
  private[this] def unprocessable(message: String) =
    throw new UnprocessableEntityException(message)  
}


