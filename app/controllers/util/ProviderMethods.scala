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
  
  /**
   * 
   * @param provider the provider resource you want a web client configured for
   * @param hostVar the name of the provider variable containing the hostname
   * @param client the underlying web client to configure
   */
  def configureWebClient(
      provider: GestaltResourceInstance, 
      hostVar: String, 
      client: Option[WSClient] = None): JsonClient = {
    
    val wsclient = client orElse Some(JsonClient.newClient()) 
    val privatevars = for {
      env <- ProviderEnv.fromResource(provider)
      prv <- env.public
    } yield prv
    
    /*
     * This needs to change - need a retry strategy that acknowledges public and private addresses.
     */
    val config = privatevars map { vs =>
      
      val DEFAULT_PROTOCOL = "https"
      val url = {
        val host = vs.get(hostVar) getOrElse {
          throw new UnprocessableEntityException(s"Missing '${hostVar}' variable.")
        }
        val port = "443"
        "%s://%s:%s".format(DEFAULT_PROTOCOL, host, port)
      }
      val key = vs.get("GESTALT_SECURITY_KEY") getOrElse {
        throw new UnprocessableEntityException("Missing 'GESTALT_SECURITY_KEY' variable.") 
      }
      val secret = vs.get("GESTALT_SECURITY_SECRET") getOrElse {
        throw new UnprocessableEntityException("Missing 'GESTALT_SECURITY_SECRET' variable.")
      }
      
      HostConfig.make(new URL(url), creds = Some(BasicCredential(key, secret)))
      
    } getOrElse {
      throw new UnprocessableEntityException("Could not parse [properties.config.env] from provider")
    }
    log.debug("Configuring API Web Client with URL : " + config.url)
    new JsonClient(config, wsclient)
  }
  
}