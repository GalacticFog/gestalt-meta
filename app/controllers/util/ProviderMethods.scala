package controllers.util

import play.api.Logger
import play.api.libs.json._

import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.json.Js
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import java.util.UUID
import javax.inject.{Inject, Singleton}

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.providers._
import com.galacticfog.gestalt.data.CoVariant
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.ResourceLabel
import com.galacticfog.gestalt.json._
import com.galacticfog.gestalt.meta.api.errors._
import services._
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink

import services._

import controllers.util.JsonInput
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.patch._
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.ContainerSpec.PortMapping

import com.galacticfog.gestalt.meta.api._
import java.net.URL
import play.api.libs.ws.ning.{NingWSRequest, NingAsyncHttpClientConfigBuilder, NingWSClient}
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