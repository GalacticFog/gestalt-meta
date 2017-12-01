package com.galacticfog.gestalt.meta.genericactions

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.api.sdk.GestaltResourceInput
import com.google.inject.Inject
import controllers.util.{ContainerService, JsonInput}
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.libs.ws.{WSClient, WSResponse}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.http.HeaderNames.CONTENT_TYPE

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

trait GenericProviderManager {
  def getProvider(provider: GestaltResourceInstance): Try[GenericProvider]
}

trait GenericProvider {
  type InvocationResponse = Either[GestaltResourceInstance,(Option[Int],Option[String],Option[String])]

  def invokeAction(context: GenericActionInvocation): Future[InvocationResponse]
}

case class HttpGenericProvider(client: WSClient, url: String) extends GenericProvider with JsonInput {

  private[this] def processResponse(resource: Option[GestaltResourceInstance], response: WSResponse): InvocationResponse = {
    val maybeResource = for {
      json <- Try{response.json}.toOption
      // mandatory
      name <- (json \ "name").asOpt[String]
      properties <- (json \ "properties").asOpt[Map[String,JsValue]]
      // optional
      description = (json \ "description").asOpt[String] orElse resource.flatMap(_.description)
      stateId <- (json \ "resource_state").asOpt[String].map(state => ResourceState.id(state)) orElse resource.map(_.state)
      // from original
      orgId  <- resource.map(_.orgId)
      id     <- resource.map(_.id)
      typeId <- resource.map(_.typeId)
      owner  <- resource.map(_.owner)
    } yield GestaltResourceInstance(
      id = id,
      typeId = typeId,
      state = stateId,
      orgId = orgId,
      owner = owner,
      name = name,
      description = description,
      properties = controllers.util.stringmap(Some(properties)),
      variables = resource.flatMap(_.variables),
      tags = resource.flatMap(_.tags),
      auth = resource.flatMap(_.auth)
    )
    lazy val notResource = Right(
      Some(response.status), response.header(CONTENT_TYPE), Try{response.bodyAsBytes}.toOption.map(bts => new String(bts))
    )
    maybeResource.map(Left(_)) getOrElse notResource
  }

  override def invokeAction(context: GenericActionInvocation): Future[InvocationResponse] = {
    client.url(url).post(context.toJson()).map(processResponse(context.resource, _))
  }

}

class DefaultGenericProviderManager @Inject()( wsclient: WSClient ) extends GenericProviderManager {
  override def getProvider(provider: GestaltResourceInstance): Try[GenericProvider] = {
    ContainerService.getProviderConfig(provider) flatMap (c => (c \ "endpoint").asOpt[JsObject]) match {
      case None => Failure(ConflictException(
        s"Could not instantiate GenericProvider because provider ${provider.id} was missing '.properties.config.endpoint'"
      ))
      case Some(endpointConfig) =>
        parseEndpointType(endpointConfig) match {
          case Some(gp) => Success(gp)
          case None     => Failure(ConflictException(
            s"Provider ${provider.id} endpoint configuration did not match supported types."
          ))
        }
    }
  }

  private[this] def parseEndpointType(endpoint: JsObject): Option[GenericProvider] = {
    (for {
      http <- (endpoint \ "http").asOpt[JsObject]
      url  <- (http \ "url").asOpt[String]
    } yield HttpGenericProvider(wsclient, url))
  }

}
