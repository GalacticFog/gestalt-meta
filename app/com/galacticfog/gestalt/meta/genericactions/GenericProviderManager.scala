package com.galacticfog.gestalt.meta.genericactions

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.google.inject.Inject
import controllers.util.ContainerService
import play.api.libs.json.{JsObject, Json}
import play.api.libs.ws.{WSClient, WSResponse}
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

trait GenericProviderManager {
  def getProvider(provider: GestaltResourceInstance): Try[GenericProvider]
}

trait GenericProvider {
  type InvocationResponse = Either[GestaltResourceInstance,(Option[Int],Option[String],Option[String])]

  def invokeAction(context: GenericActionInvocation): Future[InvocationResponse]
}

case class HttpGenericProvider(client: WSClient, url: String) extends GenericProvider {

  private[this] def processResponse(response: WSResponse): InvocationResponse = {
    ???
  }

  override def invokeAction(context: GenericActionInvocation): Future[InvocationResponse] = {
    client.url(url).post(context.toJson()).map(processResponse(_))
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
