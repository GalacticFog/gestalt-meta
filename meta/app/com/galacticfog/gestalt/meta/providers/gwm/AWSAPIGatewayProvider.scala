package com.galacticfog.gestalt.meta.providers.gwm

import java.util.UUID
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

  case class LinkedProvider(name: String, `type`: String, id: UUID)
  case class AWSAPIGatewayProviderProperties(linked_providers: Seq[LinkedProvider])
  
  implicit val formatLinkedProvider = Json.format[LinkedProvider]
  implicit val formatAWSAPIGatewayProviderProperties = Json.format[AWSAPIGatewayProviderProperties]

  private def getLinkedAwsProvider(provider: GestaltResourceInstance): Either[String,GestaltResourceInstance] = {
    for(
      awsProviderProperties <- ResourceSerde.deserialize[AWSAPIGatewayProviderProperties](provider);
      linkedProvider <- Either.fromOption(awsProviderProperties.linked_providers.find(_.name == "AWS"),
       "AWS missing in linked_providers");
      awsProvider <- Either.fromOption(ResourceFactory.findById(migrations.V20.AWS_LAMBDA_PROVIDER_TYPE_ID, linkedProvider.id),
        s"AWS Lambda Provider with id ${linkedProvider.id} not found")
    ) yield awsProvider
  }

  def getPublicUrl(resource: GestaltResourceInstance): Option[String] = {
    val res = for(
      endpointProperties <- ResourceSerde.deserialize[ApiEndpointProperties](resource);
      provider <- Either.fromOption(ResourceFactory.findById(migrations.V24.AWS_API_GATEWAY_PROVIDER_TYPE_ID, endpointProperties.provider.id),
       s"Provider not found ${endpointProperties.provider.id}");
      awsProvider <- getLinkedAwsProvider(provider);
      config <- eitherFromTry(Try(Json.parse(awsProvider.properties.get("config"))));
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
    for(
      endpointProperties <- ResourceSerde.deserialize[ApiEndpointProperties](resource).liftTo[Future];
      awsProvider <- getLinkedAwsProvider(provider).liftTo[Future];
      client = providerMethods.configureWebClient(awsProvider, Some(ws));
      payload <- if(endpointProperties.implementation_type == "lambda") {
        for(
          lambdaResource <- Either.fromOption(ResourceFactory.findById(ResourceIds.Lambda, UUID.fromString(endpointProperties.implementation_id)),
           s"Lambda with id ${endpointProperties.implementation_id} not found").liftTo[Future];
          lambdaProperties <- ResourceSerde.deserialize[AWSLambdaProperties](lambdaResource).liftTo[Future];
          awsFunctionId <- Either.fromOption(lambdaProperties.aws_function_id,
           s"aws_function_id is unset for Lambda ${lambdaResource.id}").liftTo[Future]
        ) yield Json.obj(
          "name" -> s"${resource.id}",
          "aws" -> Json.obj(
            "functionArn" -> awsFunctionId
          )//,
          // "usersWhitelist" -> Json.arr("arn:aws:iam::326625211408:user/test-user")
        )
      }else if(endpointProperties.implementation_type == "link") {
        Future.successful(Json.obj(
          "name" -> s"${resource.id}",
          "publicHttp" -> Json.obj(
            "url" -> endpointProperties.implementation_id
          )
        ))
      }else {
        Future.failed(new RuntimeException(s"implementation_type ${endpointProperties.implementation_type} is not supported"))
      };
      payloadWithWhitelist = endpointProperties.whitelist.fold(payload) { whitelist =>
        payload ++ Json.obj("usersWhitelist" -> new JsArray(whitelist.toVector.map(JsString(_))))
      };
      response <- client.post("/endpoint/create", Some(payloadWithWhitelist));
      _ <- if(200 >= response.status && response.status < 300) {
        Future.successful(())
      }else {
        Future.failed(throw ApiError(response.status, response.body).throwable)
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
    for(
      endpointProperties <- ResourceSerde.deserialize[ApiEndpointProperties](resource).liftTo[Future];
      awsProvider <- getLinkedAwsProvider(provider).liftTo[Future];
      client = providerMethods.configureWebClient(awsProvider, Some(ws));
      restApiId <- Either.fromOption(endpointProperties.container_port_name,
       s"container_port_name (restApiId) is unset for Endpoint ${resource.id}").liftTo[Future];
      response <- client.delete(s"/endpoint/${restApiId}");
      _ <- if(200 >= response.status && response.status < 300) {
        Future.successful(())
      }else {
        Future.failed(throw ApiError(response.status, response.body).throwable)
      }
    ) yield ()
  }
}