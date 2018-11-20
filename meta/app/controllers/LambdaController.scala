package controllers


import java.util.UUID
import scala.concurrent.Future

import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import controllers.util._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.meta.api.errors.BadRequestException

import com.galacticfog.gestalt.security.play.silhouette.{GestaltFrameworkSecurity, GestaltFrameworkSecurityEnvironment}
import com.google.inject.Inject
import play.api.i18n.MessagesApi
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import javax.inject.Singleton

@Singleton
class LambdaController @Inject()(
    messagesApi: MessagesApi,
    resourceController: ResourceController,
    lambdaMethods: LambdaMethods,
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

    val action = request.getQueryString("action").getOrElse("create")
    val actionResult = action match {
      case "create" => lambdaMethods.createLambdaCommon(org, parent, request.body, request.identity)
      case "import" => lambdaMethods.importLambdaCommon(org, parent, request.body, request.identity)
      case _ => Future.failed(new BadRequestException(s"Unsupported action: $action"))
    }

    actionResult map { metaLambda =>
      Created(RenderSingle(resourceController.transformResource(metaLambda).get))
    } recoverWith { case throwable =>
      HandleExceptionsAsync(throwable)
    }
  }
}