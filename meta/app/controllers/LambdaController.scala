package controllers


import java.net.URL
import java.util.UUID

import play.api.libs.concurrent.Execution.Implicits.defaultContext

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
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltFrameworkSecurity, GestaltFrameworkSecurityEnvironment, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi
import com.galacticfog.gestalt.json.Js
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import javax.inject.Singleton
import play.api.libs.ws.WSClient

case class LambdaProviderInfo(id: String, locations: Seq[String])
object LambdaProviderInfo {
  implicit lazy val lambdaProviderInfoFormat = Json.format[LambdaProviderInfo]
}

@Singleton
class LambdaController @Inject()(
    ws: WSClient,
    messagesApi: MessagesApi,
    providerMethods: ProviderMethods,
    resourceController: ResourceController,
    sec: GestaltFrameworkSecurity)
      extends SecureController(messagesApi = messagesApi, sec = sec) with Authorization with JsonInput {
  
  /*
   * This is the provider variable containing the provider host address.
   */
  
  def postLambdaFqon(fqon: String) = AsyncAudited(fqon) { implicit request =>
    val org = orgFqon(fqon).get
    createLambdaCommon(org.id, org)
  }
  
  def postLambda(fqon: String, parentType: String, parent: UUID) = AsyncAudited(fqon) { implicit request =>
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
  



  
  /*
   * TODO: Overload this method - take payload JSON directly instead of from request.body
   */
  protected[controllers] def createLambdaCommon(org: UUID, parent: GestaltResourceInstance)
      (implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]): Future[play.api.mvc.Result] = {
    
    toInput(request.body) match {
      
      case Failure(e)     => HandleExceptionsAsync(e)
      case Success(input) => {
        
        assertValidTypeId(input, Some(ResourceIds.Lambda))
            
        val lambdaId: UUID = input.id.getOrElse(UUID.randomUUID)
        
        // Set ID for the Lambda.
        val newjson = injectParentLink(
            request.body.as[JsObject] ++ Json.obj("id" -> lambdaId.toString), parent)
        
        val pinfo = getProviderInfo(newjson)
        val provider = ResourceFactory.findById(ResourceIds.LambdaProvider, pinfo.id) getOrElse {
          unprocessable(s"Lambda Provider with ID '${pinfo.id}' not found.")
        }
        
        val caller = request.identity
        val client = providerMethods.configureWebClient(provider, Some(ws))
        
        for {
          metaLambda <- newDefaultResource(org, ResourceIds.Lambda, parent.id, newjson)
          laser <- Future.fromTry(toLaserLambda(metaLambda, provider.id))
          result <- client.post("/lambdas", Option(Json.toJson(laser)))
          response = {
            if (Seq(200, 201, 202).contains(result.status)) {
              log.info("Successfully created Lambda in backend system.")
              Created(RenderSingle(resourceController.transformResource(metaLambda).get))
            } else { 
              log.error("Error creating Lambda in backend system.")
              updateFailedBackendCreate(caller, metaLambda, ApiError(result.status, result.body).throwable)
            }
          }
        } yield response

      }
    }
  }
  
  
  def injectParentLink(json: JsObject, parent: GestaltResourceInstance) = {
    val parentLink = toLink(parent, None)
    json ++ Json.obj("properties" -> 
      replaceJsonPropValue(json, "parent", Json.toJson(parentLink)))
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