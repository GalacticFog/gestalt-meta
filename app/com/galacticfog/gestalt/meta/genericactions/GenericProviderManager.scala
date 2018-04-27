package com.galacticfog.gestalt.meta.genericactions

import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.{BadRequestException, ConflictException}
import com.galacticfog.gestalt.meta.genericactions.GenericProvider.{InvocationResponse, RawInvocationResponse}
import com.google.inject.Inject
import controllers.util.{ContainerService, JsonInput}
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.libs.ws.{WSClient, WSResponse}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.http.HeaderNames.{AUTHORIZATION, CONTENT_TYPE}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

trait GenericProviderManager {
  case class EndpointProperties(default: Boolean, actions: Seq[String])
  implicit val endpointPropsReads: Reads[EndpointProperties] = (
    (__ \ "default").readNullable[Boolean].map(_ getOrElse false) and
      (__ \ "actions").readNullable[Seq[String]].map(_ getOrElse Seq.empty)
  )(EndpointProperties.apply _)

  def getProvider(provider: GestaltResourceInstance, action: String, callerAuth: String): Try[Option[GenericProvider]]
}

trait GenericProvider {
  import GenericProvider._

  def invokeAction(invocation: GenericActionInvocation): Future[InvocationResponse]
}

object GenericProvider {
  case class RawInvocationResponse(statusCode: Option[Int], contentType: Option[String], contentString: Option[String])
  type InvocationResponse = Either[GestaltResourceInstance,RawInvocationResponse]
}

case class HttpGenericProvider(client: WSClient,
                               url: String,
                               authHeader: Option[String] = None) extends GenericProvider with JsonInput {

  private[this] def processResponse(resource: Option[GestaltResourceInstance], response: WSResponse): Try[InvocationResponse] = {

    val maybeError = for {
      json <- Try{response.json}.toOption
      failure <- (json \ "actionFailed").asOpt[String]
    } yield Failure(new BadRequestException(failure))
    
    lazy val maybeResource = for {
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
    } yield Success(Left(GestaltResourceInstance(
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
    )))
    
    lazy val notResource = Success(Right(
      RawInvocationResponse(
          statusCode = Some(response.status), 
          contentType = response.header(CONTENT_TYPE), 
          contentString = Try{response.bodyAsBytes}.toOption.map(bts => new String(bts)))
    ))
    
    response.status match {
      case s if Range(200,299).contains(s) =>
        val r = maybeError orElse maybeResource getOrElse notResource
        log.debug(s"response $s from provider endpoint: $r")
        r
      case e =>
        log.debug(s"response $e from provider endpoint, returning failure")
        Failure(BadRequestException(s"endpoint invocation return response code ${e}"))
    }
  }
  
  override def invokeAction(invocation: GenericActionInvocation): Future[InvocationResponse] = {
    println(s"***** invokeAction(...${invocation.action}...)")
    val request = authHeader.foldLeft( client.url(url) ) {
      case (req, header) => {
        log.debug("Adding header to external request config: " + AUTHORIZATION + " : " + header)
        req.withHeaders(AUTHORIZATION -> header)
      }
    }
    
    request.post(invocation.toJson()).flatMap { resp => 
      
      Future.fromTry(processResponse(invocation.resource, resp))
    }.recover {
      case e: Throwable => {
        log.error("Failed calling external action endpoint : " + e.getMessage)
        throw new RuntimeException("Failed calling external action endpoint : " + e.getMessage)
      }
    }
  }
  
}

class DefaultGenericProviderManager @Inject()( wsclient: WSClient ) extends GenericProviderManager {
  
  override def getProvider(provider: GestaltResourceInstance, action: String, callerAuth: String): Try[Option[GenericProvider]] = {
    
    // Parse config.endpoints from provider resource properties.

    val endpoints: Seq[JsObject] = ContainerService.getProviderConfig(provider).flatMap {
      c => (c \ "endpoints").asOpt[Seq[JsObject]] 
    }.getOrElse(Seq.empty)

     // Find an endpoint that matches the given 'action' (or endpoint labeled 'default')

    val e1 = endpoints.find {
      _.asOpt[EndpointProperties] match {
        case Some(EndpointProperties(isDefault, actions)) =>
          isDefault || actions.contains(action)
        case _ => false
      }
    }
    
    e1 match {
      case None => Success(None)
      case Some(ep) => {
        // Ensure the endpoint is of a supported type
        parseEndpointType(ep, callerAuth,
            msg = s"Provider ${provider.id} endpoint configuration did not match supported types.").map(Some(_))
      }
    }
  }
  
  private[this] def parseEndpointType(endpoint: JsObject, callerAuth: String, msg: => String): Try[GenericProvider] = {
    // only support a single endpoint type right now, pretty simple
    val url = (for {
      http <- (endpoint \ "http").asOpt[JsObject]
      url  <- (http \ "url").asOpt[String]
      authHeader = ((http \ "authentication").asOpt[String] orElse Some(callerAuth))
    } yield HttpGenericProvider(wsclient, url, authHeader = authHeader))
    url map (Success(_)) getOrElse Failure(ConflictException(msg))
  }

}
