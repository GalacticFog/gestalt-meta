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


@Singleton
class LambdaController @Inject()(
    messagesApi: MessagesApi,
    env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
      extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  
  def postLambdaFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    Future {
      val org = orgFqon(fqon).get
      createLambdaCommon(org.id, org)
    }
  }
  
  def postLambda(fqon: String, parentType: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    
    Future {
      validateLambdaParent(parentType, parent).fold {
        ResourceNotFound(UUID.fromString(parentType), parent) 
      }{ p =>
        createLambdaCommon(fqid(fqon), p)
      }  
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
      (implicit request: SecuredRequest[JsValue]) = {
    
    safeGetInputJson(request.body, Some(ResourceIds.Lambda)) match {
      case Failure(e)     => BadRequestResult(e.getMessage)
      case Success(input) => {
        
        val lambdaId: UUID = input.id.getOrElse(UUID.randomUUID)
        
        // Set ID for the Lambda.
        val newjson = injectParentLink(
            request.body.as[JsObject] ++ Json.obj("id" -> lambdaId.toString), parent)
        
        val ps = getProviderInfo(newjson)
        /*
         * TODO: This function needs a lot of help - currently lambdas will be created in laser
         * but creating the API will fail if the request is bad (say the location name is bad).
         * I can either verify location-names first, or is there any reason not to create the
         * APIs first?
         */
        
        val resource = 
          createResourceInstance(org, newjson, Some(ResourceIds.Lambda), Some(parent.id))
        
        resource match {
          case Failure(err) => HandleExceptions(err)
          case Success(res) => {
            setNewEntitlements(org, res.id, request.identity, Some(parent.id))
            Created(Output.renderInstance(res, META_URL))
          }
        }
      }
    }
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