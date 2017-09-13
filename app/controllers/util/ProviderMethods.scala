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
import play.api.libs.json.Json

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
  
  
class ProviderMethods @Inject() () {
  
  private[this] val log = Logger(this.getClass)
  
  val DEFAULT_PROTOCOL = "http"
  
  /**
   * Configure a JSON Web Client for communicating with Provider Services.
   * @param provider the provider resource you want a web client configured for
   * @param client the underlying web client to configure
   */  
  def configureWebClient(provider: GestaltResourceInstance, client: Option[WSClient] = None): JsonClient = {

    val publicvars = {
      for {
        env <- ProviderEnv.fromResource(provider)
        pub <- env.public
      } yield pub
    }
    
    val config = publicvars map { vs =>
    
      val isOverride = vs.get("SERVICE_HOST_OVERRIDE").isDefined
      
      if (log.isDebugEnabled && isOverride) {
        log.debug("Found OVERRIDES for host configuration")
      }
      
      val hostVar = if(isOverride) "SERVICE_HOST_OVERRIDE" else "SERVICE_HOST"
      val portVar = if(isOverride) "SERVICE_PORT_OVERRIDE" else "SERVICE_PORT"
      
      val host = vs.get(hostVar) getOrElse {
        throw new UnprocessableEntityException(
          s"Could not find host address variable for provider with ID '${provider.id}'")
      }
      
      val port = vs.get(portVar) getOrElse {
        throw new UnprocessableEntityException(
          s"Could not find port variable for provider with ID '${provider.id}'")
      }
      
      val key = vs.get("GESTALT_SECURITY_KEY") getOrElse {
        throw new UnprocessableEntityException("Missing 'GESTALT_SECURITY_KEY' variable.") 
      }
      
      val secret = vs.get("GESTALT_SECURITY_SECRET") getOrElse {
        throw new UnprocessableEntityException("Missing 'GESTALT_SECURITY_SECRET' variable.")
      }    
      
      val protocol = {
        if (isOverride) 
          vs.get("SERVICE_PROTOCOL_OVERRIDE") getOrElse DEFAULT_PROTOCOL
        else DEFAULT_PROTOCOL
      }
      val url = "%s://%s:%s".format(protocol, host, port)
      HostConfig.make(new URL(url), creds = Some(BasicCredential(key, secret)))
      
    } getOrElse {
      throw new UnprocessableEntityException("Could not parse [properties.config.env] from provider")
    }
    
    log.debug("Configuring API Web Client with URL : " + config.url)
    val wsclient = client orElse Some(JsonClient.newClient())
    new JsonClient(config, wsclient)    
  }
  
}


import com.galacticfog.gestalt.meta.api.output.Output
import play.api.libs.json.Json
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds


object ProviderMethods {
  private val log = Logger(this.getClass())
  
  def isActionProvider(testType: UUID): Boolean = {
    ResourceFactory.isSubTypeOf(testType, ResourceIds.ActionProvider)
  }
 
  def injectProviderActions(res: GestaltResourceInstance): GestaltResourceInstance = {
    log.debug("injectProviderActions(_) : Looking up Provider Actions")
    val actions = ResourceFactory.findChildrenOfType(ResourceIds.ProviderAction, res.id)
    val actjson = Output.renderLinks(actions, None)
    val props = res.properties map { ps =>
      ps ++ Map("provider_actions" -> Json.stringify(actjson))
    }
    res.copy(properties = props)    
  }

  def buildActionUi(actionId: UUID, resourceId: UUID, user: AuthAccountWithCreds) = {
    /*
     * 1 - lookup action
     * 2 - extract 'ui template'
     * 3 - 
     */
//    ResourceFactory.findById(ResourceIds.ProviderAction, actionId).foldLeft {
//      ???
//    }{ act =>
//      val spec = ProviderActionSpec.fromResource(act)
//      ???
//    }
  }
  
}