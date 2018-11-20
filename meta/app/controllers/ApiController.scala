package controllers

import java.util.UUID

import play.api.libs.concurrent.Execution.Implicits.defaultContext

// import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors._
import controllers.util._
// import play.api.libs.json._
// import play.api.libs.json.Json.toJsFieldJsValueWrapper
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltFrameworkSecurity}
import com.google.inject.Inject
import play.api.i18n.MessagesApi
// import com.galacticfog.gestalt.json.Js
import javax.inject.Singleton
import com.galacticfog.gestalt.meta.api.sdk._
// import com.galacticfog.gestalt.patch._
import play.api.libs.ws.WSClient


@Singleton
class ApiController @Inject()(
    ws: WSClient,
    messagesApi: MessagesApi,
    gatewayMethods: GatewayMethods,
    resourceController: ResourceController,
    sec: GestaltFrameworkSecurity,
    providerMethods: ProviderMethods,
    db: play.api.db.Database )
      extends SecureController(messagesApi = messagesApi, sec = sec) with Authorization {
  
  // import GatewayMethods.unprocessable
  
  def postResourceOpt(fqon: String, typ: Option[String], parent: UUID) = AsyncAudited(fqon) { implicit request =>
    val org = fqid(fqon)
    val parentRes = ResourceFactory.findById(ResourceIds.Environment, parent) getOrElse {
      throw new UnprocessableEntityException(s"No Environment with id ${parent}.")
    }
    gatewayMethods.createApi(org, parent, request.body, request.identity) map { apiResource =>
      Created(RenderSingle(resourceController.transformResource(apiResource).get))
    } recoverWith { case throwable =>
      HandleExceptionsAsync(throwable)
    }
  }

  def postApiEndpoint(fqon: String, apiId: UUID) = AsyncAudited(fqon) { implicit request =>
    val api = ResourceFactory.findById(ResourceIds.Api, apiId) getOrElse {
      throw new UnprocessableEntityException(s"No Api with id ${apiId}.")
    }
    gatewayMethods.createEndpoint(fqid(fqon), api, request.body, request.identity) map { endpointResource =>
      Created(RenderSingle(resourceController.transformResource(endpointResource).get))
    } recoverWith { case throwable =>
      HandleExceptionsAsync(throwable)
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
  
}


