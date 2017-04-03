package controllers.util

import java.net.URL
import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Either, Left, Right}
import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.parseUUID
import com.galacticfog.gestalt.data.session
import com.galacticfog.gestalt.data.string2uuid
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.output.toLink
import com.galacticfog.gestalt.meta.api.sdk._

import controllers.util._
import controllers.util.JsonUtil._
import controllers.util.db.EnvConfig
import play.api.Logger
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import com.galacticfog.gestalt.meta.auth.Authorization

import scala.util.Either
import com.galacticfog.gestalt.keymgr.GestaltFeature

import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi
import com.galacticfog.gestalt.json.Js

import javax.inject.Singleton
import com.galacticfog.gestalt.meta.providers._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.ResourceState

object GatewayMethods {
  
  private[this] val log = Logger(this.getClass)
  
  
  def toGatewayApi(api: JsObject, location: UUID) = {
    val name = Js.find(api.as[JsObject], "/name") match {
      case None => unprocessable("Could not find /name property in payload")
      case Some(n) => n.as[String]
    }
    
    // Extract 'id' from payload, use to create backend system API payload
    val id = Js.find(api.as[JsObject], "/id").get.as[String]
    LaserApi(Some(UUID.fromString(id)), name, 
          provider = Some(Json.obj(
          	"id"       -> location.toString, 
          	"location" -> location.toString)))
  }
  
  def toGatewayEndpoint(js: JsValue, api: UUID): Try[LaserEndpoint] = Try {
    val json = js.as[JsObject]
    val id = Js.find(json, "/id").get.as[String]
    val upstreamUrl = Js.find(json, "/properties/upstream_url") getOrElse JsString("") //.get.toString
    val path = Js.find(json, "/properties/resource").get.as[String]
    
    LaserEndpoint(Some(id), 
        apiId = api.toString,
        upstreamUrl = upstreamUrl.as[String], 
        path = path)
  }  
  
  def findGatewayProvider(api: GestaltResourceInstance): Try[GestaltResourceInstance] = Try {
      val prvder = Json.parse(api.properties.get("provider")).as[JsObject]

      val pid = Js.find(prvder, "/id").fold {
        unprocessable("Missing required property [properties.provider.id]")
      }{ js => UUID.fromString(js.as[String]) }

      log.debug("Parsed provider ID from API for endpoint: " + pid)

      ResourceFactory.findById(ResourceIds.GatewayManager, pid).fold {
        unprocessable(s"GatewayManager provider with ID '$pid' not found")
      }{ gateway => gateway }    
  }
  
  def configureWebClient(provider: GestaltResourceInstance): JsonWebClient = {

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
        val host = vs.get("HTTP_API_VHOST_0") getOrElse {
          throw new UnprocessableEntityException("Missing 'HTTP_API_VHOST_0' variable.")
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
    new JsonWebClient(config)
  }  
  
//  def get[T](client: JsonWebClient, resource: String, expected: Seq[Int])
//      (implicit fmt: Format[T]): Option[T] = {
//    
//    JsonWebClient.apiResponse(client.get(resource), expected = expected) match {
//      case Failure(err) => throw err
//      case Success(res) => res.output match {
//        case Some(out) => out.validate[T] match {
//          case s: JsSuccess[T] => Some(s.get)
//          case e: JsError => 
//            throw new RuntimeException(Js.errorString(e))
//        }
//        case None => None
//      }
//    }    
//  }  
  
  private[controllers] def unprocessable(message: String) =
    throw new UnprocessableEntityException(message)    
  
}