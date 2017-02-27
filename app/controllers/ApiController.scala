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

@Singleton
class ApiController @Inject()(
    messagesApi: MessagesApi,
    env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
      extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  case class EndpointImpl(`type`: String, id: String, function: String)
  object EndpointImpl {
    implicit lazy val endpointImplFormat = Json.format[EndpointImpl]  
  }
  
  def postApi(fqon: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    ResourceFactory.findById(ResourceIds.Environment, parent).fold {
      Future(ResourceNotFound(ResourceIds.Environment, parent))
    }{ _ =>
      val payload = validateNewApi(request.body)
      createResourceCommon(fqid(fqon), parent, ResourceIds.Api, payload)
    }
  }
  
  private[controllers] def validateNewApi(js: JsValue): JsValue = {

    // provider.id property exists
    val pid = Js.find(js.as[JsObject], "/properties/provider/id") getOrElse {
      unprocessable("Missing required property [properties.provider.id]")
    }
    
    // 'provider.id' is valid UUID
    val uid = parseUUID(pid.as[String]) getOrElse {
      unprocessable(s"Invalid [provider.id] (not a valid UUID). found '${pid}'")
    }
    
    // 'provider.id' is an existing gateway-manager provider.
    ResourceFactory.findById(ResourceIds.GatewayManager, uid).fold {
      throw new ResourceNotFoundException(s"GatewayManager provider with ID '$pid' not found")
    }{
      _ => js
    }
  }
  
  def putApi(fqon: String, api: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    ???
  }
  
  
  def postApiEndpoint(fqon: String, api: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    /*
     * TODO: Ensure API exists
     */
    ResourceFactory.findById(ResourceIds.Api, api).fold {
      Future(ResourceNotFound(ResourceIds.Api, api)) 
    }{ a =>
      /*
       * TODO: validate endpoint payload
       */
      val payload = validateNewEndpoint(request.body)
      
      /*
       * Create in Meta
       * Create in gestalt-api-gateway
       * return
       */
      createResourceCommon(fqid(fqon), api, ResourceIds.ApiEndpoint, payload)
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
  
  private[controllers] def validateNewEndpoint(js: JsValue): JsValue = {

    val implobj = Js.find(js.as[JsObject], "/properties/implementation") getOrElse {
      unprocessable("Required value [properties.implementation] is missing.")
    }

    Js.parse[EndpointImpl](implobj) match {
      case Failure(e) => unprocessable(s"Malformed [implementation] JSON. found: '${implobj}'")
      case Success(impl) => {

        // 'type' == lambda
        if (impl.`type`.toLowerCase != "lambda")
          unprocessable(s"Unsupported [implementation.type]. found: '${impl.`type`}'")

        // 'id' is valid UUID
        val implid = parseUUID(impl.id) getOrElse {
          unprocessable(s"Invalid [implementation.id] (not a valid UUID). found '${impl.id}'")
        }
        
        // 'id' points to existing lambda
        ResourceFactory.findById(ResourceIds.Lambda, implid).fold {
          throw new ResourceNotFoundException(s"Lambda with ID '${implid}' not found.")
        }{ 
          lmb => js
        }
      }
    }
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


