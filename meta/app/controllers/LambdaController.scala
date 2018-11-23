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
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
// import com.galacticfog.gestalt.meta.api.errors.{ConflictException,BadRequestException}
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.util.Either._
// import com.galacticfog.gestalt.util.FutureFromTryST._
import cats.syntax.either._

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
      extends SecureController(messagesApi = messagesApi, sec = sec) with Authorization {
  
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
      case "create" => lambdaMethods.createLambda(org, parent, request.body, request.identity)
      case "import" => lambdaMethods.importLambda(org, parent, request.body, request.identity)
      case _ => Future.failed(new BadRequestException(s"Unsupported action: $action"))
    }

    actionResult map { metaLambda =>
      Created(RenderSingle(resourceController.transformResource(metaLambda).get))
    } recoverWith { case throwable =>
      HandleExceptionsAsync(throwable)
    }
  }

  def migrateLambda(fqon: String, lid: UUID) = AsyncAuditedAny(fqon) { implicit request =>
    val operations = List(
      controllers.util.Authorize("lambda.migrate"),
      PolicyCheck("lambda.migrate"),
      EventsPre("lambda.migrate")
    )

    val metaUrl = System.getenv().getOrDefault("META_POLICY_CALLBACK_URL", META_URL)

    for(
      providerId <- Either.fromOption(request.getQueryString("provider"), "Provider id must be supplied").liftTo[Future];
      targetProvider <- Either.fromOption(ResourceFactory.findById(UUID.fromString(providerId)),
       s"Target provider not found with id ${providerId}").liftTo[Future];
      resource <- Either.fromOption(ResourceFactory.findById(ResourceIds.Lambda, lid),
       s"Lambda not found with id ${lid}").liftTo[Future];
      // no suitable field:
      // _ <- if(resource.properties.get("status") == Some("MIGRATING")) {
      //   Future.failed(new ConflictException(s"Lambda '${resource.id}' is already migrating. No changes made."))
      // }else {
      //   Future.successful(())
      // };
      env <- Either.fromOption(ResourceFactory.findParent(ResourceIds.Environment, resource.id),
       s"Parent environment for ${resource.id} not found").liftTo[Future];
      _ <- Either.fromOption(EventMethods.findEffectiveEventRules(env.id, Some("lambda.migrate")),
       "No migration policy found.").liftTo[Future];
      options = RequestOptions(
        request.identity,
        authTarget = Option(env.id),
        policyOwner = Option(env.id),
        policyTarget = Option(resource),
        data = Option(Map(
          "fqon" -> fqon,
          "meta_url" -> metaUrl,
          "environment_id" -> s"${env.id}",
          "provider_id" -> s"${targetProvider.id}"
        ))
      );
      _ <- ComposableSafeRequest.Protect(operations, options)//;
      // updatedResource <- Future.fromTryST(ResourceFactory.update(
      //   resource.copy(properties=Some(resource.properties.getOrElse(Map()) ++ Map("status" -> "MIGRATING"))),
      //   request.identity.account.id
      // ))
    ) yield Accepted(RenderSingle(resource))
  }
}