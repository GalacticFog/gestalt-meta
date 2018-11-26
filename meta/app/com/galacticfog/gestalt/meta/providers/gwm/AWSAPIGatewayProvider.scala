package com.galacticfog.gestalt.meta.providers.gwm

import scala.concurrent.Future
import scala.util.Try
import com.google.inject.Inject
import play.api.libs.ws.WSClient
import play.api.Logger
import play.api.libs.json._
import cats.syntax.either._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.patch.PatchDocument
import com.galacticfog.gestalt.meta.providers.faas.{AWSLambdaProperties,LambdaSpec}
import com.galacticfog.gestalt.util.Either._
import com.galacticfog.gestalt.util.ResourceSerde
import controllers.util.ProviderMethods

class AWSAPIGatewayProvider @Inject()(ws: WSClient, providerMethods: ProviderMethods) extends GwmProviderImplementation[Future] {

  private[this] val log = Logger(this.getClass)
  
  import play.api.libs.concurrent.Execution.Implicits.defaultContext
  import GwmSpec.Implicits._
  import LambdaSpec.Implicits._

  def getPublicUrl(resource: GestaltResourceInstance): Option[String] = {
    val res = for(
      endpointProperties <- ResourceSerde.deserialize[ApiEndpointProperties](resource);
      provider <- Either.fromOption(ResourceFactory.findById(migrations.V20.AWS_LAMBDA_PROVIDER_TYPE_ID, endpointProperties.provider.id),
       s"Provider not found ${endpointProperties.provider.id}");
      config <- eitherFromTry(Try(Json.parse(provider.properties.get("config"))));
      awsRegion <- Either.fromOption((config \ "env" \ "public" \ "AWS_REGION").asOpt[String],
       "AWS_REGION env var not defined on provider");
      restApiId <- Either.fromOption(endpointProperties.container_port_name,
       s"container_port_name (restApiId) field is missing on ${resource.id}")
    ) yield s"https://${restApiId}.execute-api.${awsRegion}.amazonaws.com/default/path"

    res.left.foreach { errorMessage =>
      log.warn(s"Failed to get public url: ${errorMessage}")
    }

    res.toOption
  }
  
  def createApi(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[GestaltResourceInstance] = {
    log.debug("AWSAPIGateway.createApi is a noop")
    Future.successful(resource)
  }
  def deleteApi(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[Unit] = {
    log.debug("AWSAPIGateway.deleteApi is a noop")
    Future.successful(())
  }
  
  def createEndpoint(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[GestaltResourceInstance] = {
    val client = providerMethods.configureWebClient(provider, Some(ws))
    for(
      endpointProperties <- ResourceSerde.deserialize[ApiEndpointProperties](resource).liftTo[Future];
      _ <- if(endpointProperties.implementation_type == "lambda") {
        Future.successful(())
      }else {
       Future.failed(new RuntimeException(s"Implementation type `${endpointProperties.implementation_type}` not supported with AWS API Gateway"))
      };
      lambdaResource <- Either.fromOption(ResourceFactory.findById(ResourceIds.Lambda, endpointProperties.implementation_id),
       s"Lambda with id ${endpointProperties.implementation_id} not found").liftTo[Future];
      lambdaProperties <- ResourceSerde.deserialize[AWSLambdaProperties](lambdaResource).liftTo[Future];
      awsFunctionId <- Either.fromOption(lambdaProperties.aws_function_id,
       s"aws_function_id is unset for Lambda ${lambdaResource.id}").liftTo[Future];
      response <- client.post("/endpoint/create", Some(Json.obj(
        "name" -> s"${resource.id}",
        "functionArn" -> awsFunctionId
      )));
      _ <- if(200 >= response.status && response.status < 300) {
        Future.successful(())
      }else {
        Future.failed(ApiError(response.status, response.body).throwable)
      };
      restApiId = response.json.as[String];
      _ = log.debug(s"restApiId: ${restApiId}");
      updatedEndpointProperties = endpointProperties.copy(container_port_name=Some(restApiId));
      updatedResource <- ResourceSerde.serialize[ApiEndpointProperties](resource, updatedEndpointProperties).liftTo[Future]
    ) yield updatedResource
  }
  def updateEndpoint(provider: GestaltResourceInstance, resource: GestaltResourceInstance, patch: PatchDocument): Future[GestaltResourceInstance] = {
    log.error("AWSAPIGateway.updateEndpoint is not supported")
    Future.failed(???)
  }
  def deleteEndpoint(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[Unit] = {
    val client = providerMethods.configureWebClient(provider, Some(ws))
    for(
      endpointProperties <- ResourceSerde.deserialize[ApiEndpointProperties](resource).liftTo[Future];
      _ <- if(endpointProperties.implementation_type == "lambda") {
        Future.successful(())
      }else {
       Future.failed(new RuntimeException(s"Implementation type `${endpointProperties.implementation_type}` not supported with AWS API Gateway"))
      };
      restApiId <- Either.fromOption(endpointProperties.container_port_name,
       s"container_port_name (restApiId) is unset for Endpoint ${resource.id}").liftTo[Future];
      response <- client.delete(s"/endpoint/${restApiId}");
      _ <- if(200 >= response.status && response.status < 300) {
        Future.successful(())
      }else {
        Future.failed(ApiError(response.status, response.body).throwable)
      }
    ) yield ()
  }
}