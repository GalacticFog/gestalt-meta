package controllers.util

import play.api.Logger
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.providers._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk._
import java.net.URL

import javax.inject.Inject
import play.api.libs.ws._
import com.galacticfog.gestalt.data.ResourceFactory
import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.Materializer
import play.api.libs.json.{JsDefined, JsObject, JsUndefined, Json}

import cats.syntax.either._


case class ActionInvokeEvent(
  lambdaId     : String,                          // ONLY required parameter, the lambda to invoke
  payload      : Option[String] = None,           // any data that needs to be passed to the lambda
  executionId  : Option[String] = None,           // you can specify the execution ID if you want to retrieve results
  creds        : Option[String] = None,           // any credentials you care to pass to the lambda
  user         : Option[String] = None,           // any user (identity) you want to pass to the function
  params       : Map[String,Seq[String]] = Map(), // query params you want to pass along (currently useful for specifying ?entryPoint=overridedMethod)
  responseEventName : Option[String] = None,      // the response Event Name for the user response
  responseTopic     : Option[String] = None)      // the response topic for the user response

object ActionInvokeEvent {
  implicit lazy val actionInvokeEventFormat = Json.format[ActionInvokeEvent]
}
/*
 * The Response Event looks like this and is published to the exchange using 
 * the provided topic and event name if they're specified
 */
case class ActionResponseEvent(eventName : String, data : String)
  
class ProviderMethods @Inject()()(implicit actorSystem: ActorSystem, mat: Materializer) {
  
  private[this] val log = Logger(this.getClass)
  
  val DEFAULT_PROTOCOL = "http"

  def getHostConfig(provider: GestaltResourceInstance): Either[String,HostConfig] = {
    for(
      publicVars <- Either.fromOption(ProviderEnv.fromResource(provider).flatMap(_.public),
       "Could not parse [properties.config.env] from provider");
      isOverride = publicVars.get("SERVICE_HOST_OVERRIDE").isDefined;
      _ = if (log.isDebugEnabled && isOverride) {
        log.debug("Found OVERRIDES for host configuration")
      };
      hostVar = if(isOverride) "SERVICE_HOST_OVERRIDE" else "SERVICE_HOST";
      portVar = if(isOverride) "SERVICE_PORT_OVERRIDE" else "SERVICE_PORT";
      host <- Either.fromOption(publicVars.get(hostVar), s"Could not find host address variable for provider with ID '${provider.id}'");
      port <- Either.fromOption(publicVars.get(portVar), s"Could not find port variable for provider with ID '${provider.id}'");
      key <- Either.fromOption(publicVars.get("GESTALT_SECURITY_KEY"), "Missing 'GESTALT_SECURITY_KEY' variable.");
      secret <- Either.fromOption(publicVars.get("GESTALT_SECURITY_SECRET"), "Missing 'GESTALT_SECURITY_SECRET' variable.");
      protocol = if (isOverride) { publicVars.get("SERVICE_PROTOCOL_OVERRIDE") getOrElse DEFAULT_PROTOCOL }else { DEFAULT_PROTOCOL }
    ) yield {
      val url = "%s://%s:%s".format(protocol, host, port)
      HostConfig.make(new URL(url), creds = Some(BasicCredential(key, secret)))
    }
  }
  
  /**
   * Configure a JSON Web Client for communicating with Provider Services.
   * @param provider the provider resource you want a web client configured for
   * @param client the underlying web client to configure
   */  
  def configureWebClient(provider: GestaltResourceInstance, client: Option[WSClient] = None): JsonClient = {
    val config = getHostConfig(provider) match {
      case Right(v) => v
      case Left(errorMessage) => throw new UnprocessableEntityException(errorMessage)
    }
    
    log.debug("Configuring API Web Client with URL : " + config.url)
    val wsclient = client orElse Some(JsonClient.newClient())
    new JsonClient(config, wsclient)    
  }
  
}

object ProviderMethods {
  
  private val log = Logger(this.getClass())

  def isCaaSProvider(testType: UUID) : Boolean = {
    ResourceFactory.isSubTypeOf(testType, ResourceIds.CaasProvider)
  }

  def maskCredentials(provider: GestaltResourceInstance) : GestaltResourceInstance = {
    val propertiesToMask = Seq("access_key", "secret_key")

    val maybeMasked = for {
      properties <- provider.properties
      config <- Json.parse(properties("config")).asOpt[JsObject]
    } yield {
      val maskedConfig = propertiesToMask.foldLeft(config) { (acc, property) => {
        config \ property match {
          case _: JsDefined => acc.deepMerge(Json.obj(
            property -> ""
          ))
          case _: JsUndefined => acc
        }
      }}
      provider.copy(properties = Some(properties + ("config" -> Json.stringify(maskedConfig))))
    }
    maybeMasked.getOrElse(provider)
  }

}

