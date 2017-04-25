package controllers.util

import play.api.Logger
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.providers._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk._
import java.net.URL
import play.api.libs.ws._

object ProviderMethods {
  
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