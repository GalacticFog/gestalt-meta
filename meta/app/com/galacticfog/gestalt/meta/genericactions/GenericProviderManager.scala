package com.galacticfog.gestalt.meta.genericactions

import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.{BadRequestException, ConflictException}
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.genericactions.GenericProvider.{InvocationResponse, RawInvocationResponse}
import com.google.inject.Inject
import controllers.util.{ContainerService, JsonInput}
import org.clapper.scalasti.ST
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.libs.ws.{WSClient, WSResponse}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.http.HeaderNames.{AUTHORIZATION, CONTENT_TYPE}

import scala.concurrent.Future
import scala.util.parsing.json.JSON
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
                               method: String,
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
          contentString = Try{response.bodyAsBytes}.toOption.map(_.utf8String))
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
  
  import controllers.util.QueryString
  
  override def invokeAction(invocation: GenericActionInvocation): Future[InvocationResponse] = {
    val params = QueryString.asFlatSeq(invocation.queryParams)

    def resourceToMap(r: GestaltResourceInstance) = {
      def convertObj(j: JsObject) = {
        j.fields.map({
          case (k,v) => k -> convert(v)
        }).toMap
      }
      def convert(j: JsValue): Any = {
        j match {
          case JsArray(a) => a.map(convert)
          case j: JsObject => convertObj(j)
          case JsString(s) => s
          case JsBoolean(b) => b
          case JsNull => "null"
          case JsNumber(n) => n
        }
      }
      convertObj(Output.renderInstanceInput(r).as[JsObject])
    }

    val context = Seq(
      "org" -> Some(invocation.context.org),
      "workspace" -> invocation.context.workspace,
      "environment" -> invocation.context.environment
    ).collect({
      case (lbl,Some(r)) => lbl -> resourceToMap(r)
    }).toMap
    
    val st = invocation.resource.foldLeft(
      ST(url)
        .add("metaAddress", invocation.metaAddress)
        .addMappedAggregate("queryParams", invocation.queryParams.collect({
          case (p, s) if s.nonEmpty => p -> s.head
        }))
        .addMappedAggregate("provider", resourceToMap(invocation.provider))
        .addMappedAggregate("context", context)
    ) {
      case (st, resource) => st.addMappedAggregate(
        "resource", resourceToMap(resource)
      )
    }

    log.debug(st.attributes.toString)

    val resp = for {
      expandedUrl <- Future.fromTry(st.render())
      _ = println(s"*****template url: ${url} expanded url: ${expandedUrl}")
      request = authHeader.foldLeft(
          client.url(expandedUrl)
            .withHeaders(params:_*)
            .withMethod(method)
            .withBody(invocation.toJson()) 
          ) {
        case (req, header) => req.withHeaders(AUTHORIZATION -> header)
      }
      rawResp   <- request.execute()
      processed <- Future.fromTry(processResponse(invocation.resource, rawResp))

    } yield processed

    resp recover {
      case e: Throwable => {
        log.error("Failed calling external action endpoint : " + e.getMessage)
        throw new RuntimeException("Failed calling external action endpoint : " + e.getMessage)
      }
    }
  }  
}


class DefaultGenericProviderManager @Inject()( wsclient: WSClient ) extends GenericProviderManager {
  
  import com.galacticfog.gestalt.json.Js
  
  override def getProvider(provider: GestaltResourceInstance, action: String, callerAuth: String) = {
    val config = ContainerService.getProviderConfig(provider).getOrElse {
      throw new RuntimeException(s"'/properties/config' not found on Provider '${provider.id}'")
    }
    
    val cfg = Js.parse[GestaltFunctionConfig](config.as[JsObject])(formatGestaltFunctionConfig).get
    val found = for {
      e <- cfg.endpoints
      a <- e.actions
      if a.name == action
    } yield (e, a)
    
    val data = found.size match {
      case 1 => {
        parseEndpointType(found.head._1,found.head._2, Some(callerAuth)).map(Some(_))
      }
      case 0 => Success(None)
      case _ => Failure(new RuntimeException(
          s"Ambiguous endpoint for action '${action}'. found: ${found.size}, expected: 1"))
    }
    data
  }
  
  def parseEndpointType(ep: GestaltEndpoint, action: GestaltFunction, callerAuth: Option[String]) = {
    val method = action.verbString()
    val protocol = ep.kind
    val url = ep.url
    
    // Try to use the auth string in the endpoint, or else try the caller creds if given.
    val effectiveAuth = ep.authentication orElse callerAuth
    
    println(s"${protocol}, ${url}, ${method}")
    
    val http = Option(HttpGenericProvider(wsclient, url, method, authHeader = effectiveAuth))
    http match {
      case Some(p) =>
        Success(p)
      case None =>
        Failure(ConflictException(s"Provider endpoint configuration did not match supported types."))
    }    
  }

}
