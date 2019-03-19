package com.galacticfog.gestalt.meta.genericactions

import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.data.{ResourceFactory, ResourceState}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.{BadRequestException, ConflictException}
import com.galacticfog.gestalt.meta.genericactions.GenericProvider.{InvocationResponse, RawInvocationResponse}
import com.google.inject.Inject
import controllers.util.{ContainerService, JsonInput}
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.libs.ws.{WSClient, WSResponse, DefaultWSProxyServer}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.http.HeaderNames.{AUTHORIZATION, CONTENT_TYPE}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import java.net.URL
import java.util.UUID
import com.galacticfog.gestalt.data.parseUUID

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

  private[genericactions] val DEFAULT_LAMBDA_PROTOCOL = "http"
  
  def invokeAction(invocation: GenericActionInvocation): Future[InvocationResponse]
   
  
  /**
   * 
   */
  private[genericactions] def findEndpointUrl(
      config: GestaltFunctionConfig, invocation: GenericActionInvocation): Try[String] = {

    val action = invocation.action
    /*
     * First test if endpoint.url is a valid URL - if it is, use it.
     * If not, check that it's a valid template/token - if it is
     * look for endpoint.implementation.
     */
    config.getEndpointByActionName(action) match {
      case None => 
        throw new RuntimeException(
          s"Endpoint for action '${action}' not found in Provider config.")        
      case Some(ep) => {
        if (Try(new URL(ep.url)).isSuccess) 
          Success(ep.url)
        else {
          templateFunction(ep)(invocation).flatMap(makeEndpointUrl(ep.url, _))
        }
      }
    }
  }  
  
  /**
   * 
   */
  private[genericactions] def isUrlTemplate(s: String): Boolean = {
    val tokens = Seq("<lambda.address>")
    tokens.exists(s.startsWith(_))
  }
  
  private[genericactions] def templateFunction(ep: GestaltEndpoint):
    (GenericActionInvocation) => Try[String] = {
    
    if (ep.url.startsWith("<lambda.address>"))
      buildLambdaUrl(ep) _
    else if (ep.url.startsWith("<provider.address>"))
      buildProviderUrl(ep) _
    else 
      _ => Failure(new RuntimeException(s"URL is not recognized as a template: found: '${ep.url}'"))
  }
  
  /**
   * Forms a complete URL to a lambda using a template URL (<lambda.addtess>) and
   * a Lambda service address.
   */
  private[genericactions] def makeEndpointUrl(templateUrl: String, lambdaAddress: String): Try[String] = {
    /*
     * This is wrapped in a Try because there will be additional validation done on
     * the inputs.
     */
    val tail = templateUrl.dropWhile(_ != '/').stripPrefix("/").stripSuffix("/")
    Try(s"${lambdaAddress}/${tail}")
  }
  
  private[genericactions] def buildProviderUrl(endpoint: GestaltEndpoint)(invocation: GenericActionInvocation): Try[String] = {
    addressFromVariables(invocation.provider)
  }
  
  private[genericactions] def buildLambdaUrl(endpoint: GestaltEndpoint)(invocation: GenericActionInvocation): Try[String] = {
    
    val action = invocation.action
    endpoint.implementation.fold {
      println(s"No implementation block found for action '${action}'")
      val failure: Try[String] = Failure(
        new RuntimeException("Invalid endpoint URL. Given value is a template, but 'implementation' is missing."))
      failure
    }{ implementation =>
      println(s"Found implementation block for action '${action}'")
      // Use implementation to lookup/build URL
      for {
        lambda <- Try(ResourceFactory.findById(implementation.id).getOrElse {
                throw new RuntimeException(s"Lambda with ID '${implementation.id}' not found.")
              })
        providerId <- getProviderFromResource(lambda)
        provider <- Try(ResourceFactory.findById(providerId).getOrElse {
                throw new RuntimeException(s"Lambda Provider with ID '${providerId}' not found.")
              })
        providerAddress <- addressFromVariables(provider)
      } yield providerAddress
    }
  }

  /**
   * Parse provider ID from resource properties. This handles the discrepancy we have
   * where some resources user Provider: String and others Provider: JSON
   */
  private[genericactions] def getProviderFromResource(res: GestaltResourceInstance): Try[UUID] = Try {
    
    val ps = res.properties.getOrElse {
      throw new RuntimeException("Resource has no properties.")
    }
    val provider = ps.get("provider").getOrElse {
      throw new RuntimeException("Property 'provider' not found on resource.")
    }
    // Try to parse provider as an object.
    val pidValue = Try(Json.parse(provider)) match {
      case Success(p) => (p \ "id").as[String]
      case Failure(e) => provider
    }
    
    UUID.fromString {
      parseUUID(pidValue).getOrElse {
        throw new RuntimeException(s"Provider ID is not a valid UUID. found: '${pidValue}'")
      }
    }
  }  
  
  /**
   * Construct lambda service address from LambdaProvider public env vars.
   */
  private[genericactions] def addressFromVariables(provider: GestaltResourceInstance): Try[String] = Try {
    (for {
      props <- provider.properties
      config <- Try{Json.parse(props("config")).as[JsObject]}.toOption
      protocol <- Js.find(config, "/env/public/SERVICE_PROTOCOL").flatMap(_.asOpt[String])
        .orElse(Js.find(config, "/env/public/SERVICE_PROTOCOL_OVERRIDE").flatMap(_.asOpt[String]))
        .orElse(Some(DEFAULT_LAMBDA_PROTOCOL))
      host <- Js.find(config, "/env/public/SERVICE_HOST_OVERRIDE").flatMap(_.asOpt[String])
        .orElse(Js.find(config, "/env/public/SERVICE_HOST").flatMap(_.asOpt[String]))
      port <- Js.find(config, "/env/public/SERVICE_PORT_OVERRIDE").flatMap(_.asOpt[String])
          .orElse(Js.find(config, "/env/public/SERVICE_PORT").flatMap(_.asOpt[String]))
    } yield s"$protocol://$host:$port").getOrElse {
      throw new RuntimeException("Could not find Lambda Provider URL in properties/config/env/public")
    }    
  }
}

object GenericProvider {
  case class RawInvocationResponse(statusCode: Option[Int], contentType: Option[String], contentString: Option[String])
  type InvocationResponse = Either[GestaltResourceInstance,RawInvocationResponse]
}

case class HttpGenericProvider(client: WSClient,
                               url: String,
                               method: String,
                               authHeader: Option[String] = None,
                               proxy: Option[DefaultWSProxyServer] = None) extends GenericProvider with JsonInput {

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
  
  import scala.concurrent.duration._
  import controllers.util.QueryString
  
  val FUNCTION_REQUEST_TIMEOUT = 20000.millis
  
  override def invokeAction(invocation: GenericActionInvocation): Future[InvocationResponse] = {
    
    val params = QueryString.asFlatSeq(invocation.queryParams)
    val providerConfig = getFunctionConfig(invocation.provider)
    
    val maybeUrl = for {
      config <- getFunctionConfig(invocation.provider)
      url <- findEndpointUrl(config, invocation)
    } yield url
    
    println("URL lookup result : " + maybeUrl)
    val resp = for {
      endpointUrl <- Future.fromTry(maybeUrl)
      _ = println("Using URL : " + endpointUrl)
      request = client.url(endpointUrl)
        .withRequestTimeout(FUNCTION_REQUEST_TIMEOUT)
        .withHeaders(params:_*)
        .withMethod(method)
        .withBody(invocation.toJson())

      requestWithHeader = authHeader.fold(request) { header =>
        request.withHeaders(AUTHORIZATION -> header)
      }
      requestWithProxy = proxy.fold(requestWithHeader) { proxy =>
        requestWithHeader.withProxyServer(proxy)
      }
      _ = log.warn("Request: " + invocation.toJson().toString())
      rawResp <- requestWithProxy.execute()
      processed <- Future.fromTry(processResponse(invocation.resource, rawResp))

    } yield processed    

    resp recover {
      case e: Throwable => {
        e.printStackTrace()
        log.error("Failed calling external action endpoint : " + e.getMessage)
        throw new RuntimeException("Failed calling external action endpoint : " + e.getMessage)
      }
    }
  }
}


class DefaultGenericProviderManager @Inject()( wsclient: WSClient ) extends GenericProviderManager {
  
  import com.galacticfog.gestalt.json.Js
  
  
  /*
   * TODO: For temporary compatibility...
   * - Attempt to parse to GestaltFunctionConfig (new) - if that fails, fall back to EndpointProperties (old)
   * - Configure provider with parseEndpointTypeV1 and V2 functions.
   * In this build - for quick integration purposes - we're only checking for the new config format...but we
   * can support both versions if we need to.
   */  
  override def getProvider(provider: GestaltResourceInstance, action: String, callerAuth: String) = {
    
    val config = ContainerService.getProviderConfig(provider).getOrElse {
      throw new RuntimeException(s"'/properties/config' not found on Provider '${provider.id}'")
    }
        
    val cfg = Js.parse[GestaltFunctionConfig](config.as[JsObject])(formatGestaltFunctionConfig).get
    val found: Seq[(GestaltEndpoint, GestaltFunction)] = for {
      e <- cfg.endpoints
      a <- e.actions
      if a.name == action
    } yield (e, a)
    
    val data = found.size match {
      case 1 => {
        parseEndpointTypeV2(found.head._1,found.head._2, Some(callerAuth)).map(Some(_))
      }
      case 0 => Success(None)
      case _ => Failure(new RuntimeException(
          s"Ambiguous endpoint for action '${action}'. found: ${found.size} action configs, expected: 1"))
    }
    data
  }
  
  private[this] def parseEndpointType(endpoint: JsObject, provider: GestaltResourceInstance, callerAuth: String): Try[GenericProvider] = {
    // only support a single endpoint type right now, pretty simple
    val http = for {
      http <- (endpoint \ "http").asOpt[JsObject]
      method = (http \ "method").asOpt[String].getOrElse("POST")
      url  <- (http \ "url").asOpt[String]
      authHeader = ((http \ "authentication").asOpt[String] orElse Some(callerAuth))
      proxyHost = (http \ "proxyHost").asOpt[String]
      proxyPort = (http \ "proxyPort").asOpt[Int]
      proxy = for(
        host <- proxyHost;
        port <- proxyPort
      ) yield new DefaultWSProxyServer(host, port)
    } yield HttpGenericProvider(wsclient, url, method, authHeader = authHeader, proxy = proxy)


    http match {
      case Some(p) =>
        Success(p)
      case None =>
        Failure(ConflictException(s"Provider endpoint configuration did not match supported types."))
    }    
  }
  
  /*
   * This parses the 'new' (latest) provider config format.
   */
  def parseEndpointTypeV2(ep: GestaltEndpoint, action: GestaltFunction, callerAuth: Option[String]): Try[GenericProvider] = {
    val method = action.verbString()
    val protocol = ep.kind
    val url = ep.url
    
    // Try to use the auth string in the endpoint, or else try the caller creds if given.
    val effectiveAuth = ep.authentication orElse callerAuth
    
    val http = Option(HttpGenericProvider(wsclient, url, method, authHeader = effectiveAuth))
    http match {
      case Some(p) =>
        Success(p)
      case None =>
        Failure(ConflictException(s"Provider endpoint configuration did not match supported types."))
    }    
  }

}
