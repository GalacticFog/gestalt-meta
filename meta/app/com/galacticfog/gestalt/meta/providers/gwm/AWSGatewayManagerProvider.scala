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
import com.galacticfog.gestalt.util.Error
import com.galacticfog.gestalt.util.EitherWithErrors._
import com.galacticfog.gestalt.util.ResourceSerde
import controllers.util.ProviderMethods

class AWSGatewayManagerProvider @Inject()(ws: WSClient, providerMethods: ProviderMethods) extends GwmProviderImplementation[Future] {

  private[this] val log = Logger(this.getClass)
  
  import play.api.libs.concurrent.Execution.Implicits.defaultContext
  import GwmSpec.Implicits._
  import LambdaSpec.Implicits._

  case class LinkedProvider(name: String, `type`: String, id: UUID)
  case class AWSAPIGatewayProviderProperties(linked_providers: Seq[LinkedProvider])
  
  implicit val formatLinkedProvider = Json.format[LinkedProvider]
  implicit val formatAWSAPIGatewayProviderProperties = Json.format[AWSAPIGatewayProviderProperties]

  case class PublicEnvs(
    AWS_APIGATEWAY_LOGLEVEL: Option[String],
    AWS_APIGATEWAY_LOGREQUESTRESPONSE: Option[String],
    AWS_APIGATEWAY_ACCESSLOGGROUP: Option[String],
    AWS_APIGATEWAY_ACCESSLOGFORMAT: Option[String]
  )
  case class Envs(public: PublicEnvs)
  case class ProviderConfig(env: Envs)
  
  implicit val formatPublicEnvs = Json.format[PublicEnvs]
  implicit val formatEnvs = Json.format[Envs]
  implicit val formatProviderConfig = Json.format[ProviderConfig]

  private def getLinkedAwsProvider(provider: GestaltResourceInstance): EitherError[GestaltResourceInstance] = {
    for(
      awsProviderProperties <- ResourceSerde.deserialize[AWSAPIGatewayProviderProperties](provider);
      linkedProvider <- Either.fromOption(awsProviderProperties.linked_providers.find(_.name == "AWS"),
       Error.Default("AWS missing in linked_providers"));
      awsProvider <- Either.fromOption(ResourceFactory.findById(migrations.V20.AWS_LAMBDA_PROVIDER_TYPE_ID, linkedProvider.id),
        Error.NotFound(s"AWS Lambda Provider with id ${linkedProvider.id} not found"))
    ) yield awsProvider
  }

  def getPublicUrl(resource: GestaltResourceInstance): Option[String] = {
    val res = for(
      endpointProperties <- ResourceSerde.deserialize[ApiEndpointProperties](resource);
      provider <- Either.fromOption(ResourceFactory.findById(migrations.V24.AWS_API_GATEWAY_PROVIDER_TYPE_ID, endpointProperties.provider.id),
       Error.NotFound(s"Provider not found ${endpointProperties.provider.id}"));
      awsProvider <- getLinkedAwsProvider(provider);
      config <- eitherFromTry(Try(Json.parse(awsProvider.properties.get("config"))));
      awsRegion <- Either.fromOption((config \ "env" \ "public" \ "AWS_REGION").asOpt[String],
       Error.Default("AWS_REGION env var not defined on provider"));
      restApiId <- Either.fromOption(endpointProperties.container_port_name,
       Error.Default(s"container_port_name (restApiId) field is missing on ${resource.id}"))
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
      rawProviderConfig <- eitherFromTry(Try(Json.parse(provider.properties.get("config")))).liftTo[Future];
      providerConfig <- eitherFromJsResult(rawProviderConfig.validate[ProviderConfig]).liftTo[Future];
      _ = log.debug(s"providerConfig: $providerConfig");
      payload <- if(endpointProperties.implementation_type == "lambda") {
        for(
          lambdaResource <- Either.fromOption(ResourceFactory.findById(ResourceIds.Lambda, UUID.fromString(endpointProperties.implementation_id)),
           Error.NotFound(s"Lambda with id ${endpointProperties.implementation_id} not found")).liftTo[Future];
          lambdaProperties <- ResourceSerde.deserialize[AWSLambdaProperties](lambdaResource).liftTo[Future];
          awsFunctionId <- Either.fromOption(lambdaProperties.aws_function_id,
           Error.Default(s"aws_function_id is unset for Lambda ${lambdaResource.id}")).liftTo[Future]
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
      payloadWithErrorLog <- providerConfig.env.public.AWS_APIGATEWAY_LOGLEVEL.fold(Future.successful(payloadWithWhitelist)) { loglevel =>
        val logRequestResponse = providerConfig.env.public.AWS_APIGATEWAY_LOGREQUESTRESPONSE.getOrElse("FALSE")
        for(
          _ <- if(Seq("TRUE", "FALSE").contains(logRequestResponse)) {
            Future.successful(())
          }else {
            Future.failed(throw new RuntimeException("AWS_APIGATEWAY_LOGREQUESTRESPONSE must be one of TRUE or FALSE"))
          };
          _ <- if(Seq("INFO", "ERROR").contains(loglevel)) {
            Future.successful(())
          }else {
            Future.failed(throw new RuntimeException("AWS_APIGATEWAY_LOGLEVEL must be one of ERROR or INFO"))
          }
        ) yield {
          payloadWithWhitelist ++ Json.obj("errorLog" -> Json.obj(
            "level" -> loglevel,
            "logRequestResponseData" -> JsBoolean(if(logRequestResponse == "TRUE") { true }else { false })
          ))
        }
      };
      payloadWithAccessLog = providerConfig.env.public.AWS_APIGATEWAY_ACCESSLOGGROUP.fold(payloadWithErrorLog) { accessLogGroup =>
        val accessLogFormat = providerConfig.env.public.AWS_APIGATEWAY_ACCESSLOGFORMAT.getOrElse("""$context.identity.sourceIp $context.identity.caller $context.identity.user [$context.requestTime] "$context.httpMethod $context.resourcePath $context.protocol" $context.status $context.responseLength $context.requestId""")
        payloadWithErrorLog ++ Json.obj("accessLog" -> Json.obj(
          "group" -> accessLogGroup,
          "format" -> accessLogFormat
        ))
      };
      response <- client.post("/endpoint/create", Some(payloadWithAccessLog));
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
       Error.Default(s"container_port_name (restApiId) is unset for Endpoint ${resource.id}")).liftTo[Future];
      response <- client.delete(s"/endpoint/${restApiId}");
      _ <- if(200 >= response.status && response.status < 300) {
        Future.successful(())
      }else {
        Future.failed(throw ApiError(response.status, response.body).throwable)
      }
    ) yield ()
  }
}