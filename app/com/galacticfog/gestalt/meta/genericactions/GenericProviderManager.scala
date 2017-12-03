package com.galacticfog.gestalt.meta.genericactions

import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.google.inject.Inject
import controllers.util.{ContainerService, JsonInput}
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.libs.ws.{WSClient, WSResponse}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.http.HeaderNames.CONTENT_TYPE

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

trait GenericProviderManager {
  case class EndpointProperties(default: Boolean, actions: Seq[String])
  implicit val endpointPropsReads: Reads[EndpointProperties] = (
    (__ \ "default").readNullable[Boolean].map(_ getOrElse false) and
      (__ \ "actions").readNullable[Seq[String]].map(_ getOrElse Seq.empty)
  )(EndpointProperties.apply _)

  def getProvider(provider: GestaltResourceInstance, action: String): Try[Option[GenericProvider]]
}

trait GenericProvider {
  type InvocationResponse = Either[GestaltResourceInstance,(Option[Int],Option[String],Option[String])]

  def invokeAction(invocation: GenericActionInvocation): Future[InvocationResponse]
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

  override def invokeAction(invocation: GenericActionInvocation): Future[InvocationResponse] = {
    client.url(url).post(invocation.toJson()).map(processResponse(invocation.resource, _))
  }

}

class DefaultGenericProviderManager @Inject()( wsclient: WSClient ) extends GenericProviderManager {
  override def getProvider(provider: GestaltResourceInstance, action: String): Try[Option[GenericProvider]] = {
    val endpoints: Seq[JsObject] = ContainerService.getProviderConfig(provider) flatMap (c => (c \ "endpoints").asOpt[Seq[JsObject]]) getOrElse Seq.empty
    endpoints find {
      _.asOpt[EndpointProperties] match {
        case Some(EndpointProperties(isDefault, actions)) =>
          isDefault || actions.contains(action)
        case _ =>
          false
      }
    } match {
      case None => Success(None)
      case Some(ep) => parseEndpointType(ep, s"Provider ${provider.id} endpoint configuration did not match supported types.") map (Some(_))
    }
  }

  private[this] def parseEndpointType(endpoint: JsObject, msg: => String): Try[GenericProvider] = {
    // only support a single endpoint type right now, pretty simple
    val url = (for {
      http <- (endpoint \ "http").asOpt[JsObject]
      url  <- (http \ "url").asOpt[String]
    } yield HttpGenericProvider(wsclient, url))
    url map (Success(_)) getOrElse Failure(ConflictException(msg))
  }

}
