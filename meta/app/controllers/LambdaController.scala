package controllers


import java.util.UUID
import scala.concurrent.Future

import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data.TypeFactory
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.output.toLink
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.util.EitherFromJsResult._
import com.galacticfog.gestalt.util.FutureFromTryST._
import controllers.util._
import cats.syntax.either._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.auth.Authorization

import com.galacticfog.gestalt.security.play.silhouette.{GestaltFrameworkSecurity, GestaltFrameworkSecurityEnvironment}
import com.google.inject.Inject
import play.api.i18n.MessagesApi
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import javax.inject.Singleton
import play.api.libs.ws.WSClient    

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

  case class ProviderPropertiesProvider(
    id: UUID,
    locations: Seq[String]
  )
  case class ProviderProperties(
    provider: ProviderPropertiesProvider,
    parent: JsValue
  )

  implicit val providerPropertiesProviderFormat = Json.format[ProviderPropertiesProvider]
  implicit val providerPropertiesFormat = Json.format[ProviderProperties]
  
  /*
   * TODO: Overload this method - take payload JSON directly instead of from request.body
   */
  protected[controllers] def createLambdaCommon(org: UUID, parent: GestaltResourceInstance)
      (implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]): Future[play.api.mvc.Result] = {

    val eitherFR: Either[String,Future[play.api.mvc.Result]] = for(
      gri <- eitherFromJsResult(request.body.validate[GestaltResourceInput]);
      typeId = gri.resource_type.getOrElse(ResourceIds.Lambda);
      _ <- Either.fromOption(TypeFactory.findById(typeId), Errors.TYPE_NOT_FOUND(typeId));
      rawProperties <- Either.fromOption(gri.properties, "Provider properties not set");
      properties0 <- eitherFromJsResult(JsObject(rawProperties).validate[ProviderProperties]);
      parentLink = Json.toJson(toLink(parent, None));
      properties = properties0.copy(parent=parentLink);
      lambdaId = gri.id.getOrElse(UUID.randomUUID);
      payload = gri.copy(
        id=Some(lambdaId),
        properties=Some(Json.toJson(properties).as[Map[String,JsValue]])
      );
      lambdaProvider <- Either.fromOption(ResourceFactory.findById(ResourceIds.LambdaProvider, properties.provider.id),
       s"Lambda Provider with ID '${properties.provider.id}' not found.")
    ) yield {
      val client = providerMethods.configureWebClient(lambdaProvider, Some(ws))
      for(
        metaLambda <- newDefaultResource(org, ResourceIds.Lambda, parent.id, Json.toJson(payload));
        laser <- Future.fromTryST(toLaserLambda(metaLambda, lambdaProvider.id));
        result <- client.post("/lambdas", Option(Json.toJson(laser)))
      ) yield {
        if(Seq(200, 201, 202).contains(result.status)) {
          log.info("Successfully created Lambda in backend system.")
          Created(RenderSingle(resourceController.transformResource(metaLambda).get))
        }else {
          log.error("Error creating Lambda in backend system.")
          updateFailedBackendCreate(request.identity, metaLambda, ApiError(result.status, result.body).throwable)
        }
      }
    }

    eitherFR valueOr { errorMessage =>
      HandleExceptionsAsync(new RuntimeException(errorMessage))
    }
  }
}