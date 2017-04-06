package controllers


import java.net.URL

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Either, Left, Right}
import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
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
import play.api.libs.ws.WSClient

@Singleton
class LambdaController @Inject()(
    ws: WSClient,
    messagesApi: MessagesApi,
    env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
      extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  /*
   * This is the provider variable containing the provider host address.
   */
  private[this] val hostVariable = "SERVICE_VHOST_0"
  
  
  def postLambdaFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    val org = orgFqon(fqon).get
    createLambdaCommon(org.id, org)
  }
  
  def postLambda(fqon: String, parentType: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    validateLambdaParent(parentType, parent).fold {
      Future(ResourceNotFound(UUID.fromString(parentType), parent)) 
    }{ p =>
      createLambdaCommon(fqid(fqon), p)
    }  
  }
  
  private[controllers] def validateLambdaParent(tpe: String, id: UUID) = {
    /*
     * TODO: We can get valid parent types from the {LambdaResourceType}.properties.lineage.parent_types
     */
    val typeId = UUID.fromString(tpe)
    ResourceFactory.findById(typeId, id)
  }
  
  protected[controllers] def createLambdaCommon(org: UUID, parent: GestaltResourceInstance)
      (implicit request: SecuredRequest[JsValue]): Future[play.api.mvc.Result] = {
    
    safeGetInputJson(request.body, Some(ResourceIds.Lambda)) match {
      case Failure(e)     => HandleExceptionsAsync(e)
      case Success(input) => {
        
        val lambdaId: UUID = input.id.getOrElse(UUID.randomUUID)
        
        // Set ID for the Lambda.
        val newjson = injectParentLink(
            request.body.as[JsObject] ++ Json.obj("id" -> lambdaId.toString), parent)
        
        val pinfo = getProviderInfo(newjson)
        val provider = ResourceFactory.findById(ResourceIds.LambdaProvider, pinfo.id) getOrElse {
          unprocessable(s"Lambda Provider with ID '${pinfo.id}' not found.")
        }
        
        val caller = request.identity
        val client = ProviderMethods.configureWebClient(provider, hostVariable, Some(ws))
        
        val metaCreate = for {
          metalambda <- createResourceInstance(org, newjson, Some(ResourceIds.Lambda), Some(parent.id))
          laserlambda = toLaserLambda(input.copy(id = Some(lambdaId)), provider.id.toString, "")
        } yield (metalambda, laserlambda)
        
        metaCreate match {
          case Failure(e) => {
            log.error("Failed to create Lambda in Meta: " + e.getMessage)
            HandleExceptionsAsync(e)
          }
          case Success((meta,laser)) => {
            log.debug("Creating API in GatewayManager...")            
            client.post("/lambdas", Option(Json.toJson(laser))) map { result =>

              if (Seq(200, 201).contains(result.status)) {
                log.info("Successfully created Lambda in backend system.")
                setNewEntitlements(org, meta.id, caller, Some(parent.id))
                Created(RenderSingle(meta))
              } else {
                log.error("Error creating Lambda in backend system.")
                updateFailedBackendCreate(caller, meta, ApiError(result.status, result.body).throwable)
              }              
            } recover {
              case e: Throwable => {
                log.error(s"Error creating Lambda in backend system.")
               updateFailedBackendCreate(caller, meta, e)
              }
            }
          }
        }
        
      }
    }
  }
  
  
  def toLaserLambda(lambda: GestaltResourceInput, providerId: String, location: String) = {
    
    log.debug("toLaserLambda(...)")
    
    val props = lambda.properties.get
    
    val handler = props("handler").as[String]
    val isPublic = if (props.contains("public")) props("public").as[Boolean] else false
    val compressed = if (props.contains("compressed")) props("compressed").as[Boolean] else false
    val artifactUri = if (props.contains("package_url")) Some(props("package_url").as[String]) else None
    
    LaserLambda(
      id          = Some(lambda.id.get.toString), 
      eventFilter = Some(UUID.randomUUID.toString),
      public      = isPublic,
      provider    = Some(Json.parse(s"""{ "id": "${providerId.toString}", "location": "$location", "href": "/foo/bar" }""")),
      
      LaserArtifactDescription(
          artifactUri = artifactUri,
          description = if (props.contains("description")) props("description").asOpt[String] else None,
          handler     = handler,
          memorySize  = props("memory").as[Int],
          cpus        = props("cpus").as[Double],
          publish     = false,     // <- currently not used
          role        = "none",    // <- currently not used
          runtime     = props("runtime").as[String],
          timeoutSecs = props("timeout").as[Int],
          compressed  = compressed,
          code        = if (props.contains("code")) props("code").asOpt[String] else None,
          headers     = if( props.contains("headers")) props("headers").as[Map[String,String]] else Map.empty
    ))
  }  
  
  def injectParentLink(json: JsObject, parent: GestaltResourceInstance) = {
    val parentLink = toLink(parent, None)
    json ++ Json.obj("properties" -> 
      replaceJsonPropValue(json, "parent", Json.toJson(parentLink)))
  }  
  
  case class LambdaProviderInfo(id: String, locations: Seq[String])
  object LambdaProviderInfo {
    implicit lazy val lambdaProviderInfoFormat = Json.format[LambdaProviderInfo]
  }
  
  def getProviderInfo(lambdaJson: JsValue): LambdaProviderInfo = {
    Js.find(lambdaJson.as[JsObject], "/properties/provider/id").fold {
      unprocessable("Could not find value for [lambda.properties.provider.id]")
    }{ pid =>
      parseUUID(pid.as[String].trim).fold {
        unprocessable(s"[lambda.properties.provider.id] is not a valid UUID. found: '$pid'")
      }{ uid =>
        ResourceFactory.findById(UUID.fromString(uid)).fold {
          unprocessable(s"[lambda.properties.provider.id] '$uid' not found.")
        }{ p =>
          val pjson = Js.find(lambdaJson.as[JsObject], "/properties/provider").get
          Js.parse[LambdaProviderInfo](pjson.as[JsObject]) match {
            case Success(result) => result
            case Failure(e) =>
              unprocessable(s"Could not parse [lambda.properties.provider]. found: ${e.getMessage}'")
          }
        }
      }
    }
  }
  
  
  private[this] def unprocessable(message: String) =
    throw new UnprocessableEntityException(message)  
  
}