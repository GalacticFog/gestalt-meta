package services

import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.security.{Key, KeyFactory, PrivateKey, PublicKey}
import java.util.UUID

import akka.actor.{Actor, ActorLogging, Props}
import com.google.inject.{Inject, Singleton}
import io.jsonwebtoken.{Jwts, SignatureAlgorithm}
import org.apache.commons.codec.binary.Base64
import play.api.libs.json.Json
import play.api.libs.ws.WSClient

import scala.util.{Failure, Success, Try}
import collection.JavaConverters._
import scala.collection.mutable
import akka.pattern.ask
import play.api.Logger

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps

@Singleton
class DCOSAuthTokenActor @Inject() (client: WSClient) extends Actor {

  val log = Logger(this.getClass)

  import services.DCOSAuthTokenActor._

  // TODO: once we start using cached tokens, how do we handle that the underlying credentials may change?

  // TODO: if we automatically refresh tokens, we'll need to keep the url and the secret

  // TODO: now start using this
  val providerTokens: mutable.Map[UUID, String] = mutable.Map.empty

  val requestChild = context.actorOf(Props(classOf[DCOSAuthTokenRequestActor], client))

  // use the error kernel pattern to help protect our state from being wiped...
  // if we fail, it's not the end of the world, because the tokens will be regenerated
  override def receive: Receive = {
    case r: DCOSAuthTokenActor.DCOSAuthTokenRequest =>
      import context.dispatcher
      val s = sender()
      val f = requestChild.ask(r)(30 seconds)
      f.onComplete {
        case Success(response) =>
          if (response.isInstanceOf[DCOSAuthTokenResponse]) {
            providerTokens += (r.providerId -> response.asInstanceOf[DCOSAuthTokenResponse].authToken)
          }
          s ! response
        case Failure(t) =>
          s ! DCOSAuthTokenError(t.getMessage)
      }
    case e =>
      log.error(s"DCOSAuthTokenActor: unhandled message type: $e")
  }

}

object DCOSAuthTokenActor {

  final val name = "dcos-auth-token-actor"

  case class DCOSAuthTokenRequest( providerId: UUID,
                                   serviceAccountId : String,
                                   privateKey : String,
                                   dcosUrl : String )

  case class DCOSAuthTokenResponse( authToken: String )

  case class DCOSAuthTokenError( message: String )

  def strToPrivateKey(pkcs8: String): PrivateKey = {
    val encoded = Base64.decodeBase64(
      pkcs8.replace("-----BEGIN PRIVATE KEY-----\n", "")
           .replace("-----END PRIVATE KEY-----", "")
    )
    val kf = KeyFactory.getInstance("RSA");
    val keySpec = new PKCS8EncodedKeySpec(encoded);
    kf.generatePrivate(keySpec)
  }

  def strToPublicKey(pkcs8: String): PublicKey = {
    val encoded = Base64.decodeBase64(
      pkcs8.replace("-----BEGIN PUBLIC KEY-----\n", "")
        .replace("-----END PUBLIC KEY-----", "")
    )
    val kf = KeyFactory.getInstance("RSA");
    val keySpec = new X509EncodedKeySpec(encoded)
    kf.generatePublic(keySpec)
  }

}

class DCOSAuthTokenRequestActor(client: WSClient) extends Actor with ActorLogging {
  override def receive: Receive = {
    case r: DCOSAuthTokenActor.DCOSAuthTokenRequest =>
      import context.dispatcher
      val s = sender()
      val f = for {
        jwt <- Future.fromTry(Try{
          Jwts.builder()
            .setExpiration(java.util.Date.from(java.time.Instant.now().plusSeconds(60*5)))
            .setClaims(Map[String,AnyRef](
              "uid" -> r.serviceAccountId
            ).asJava)
            .signWith(SignatureAlgorithm.RS256, DCOSAuthTokenActor.strToPrivateKey(r.privateKey))
            .compact()
        })
        url = r.dcosUrl.stripPrefix("/") + "/acs/api/v1/auth/login"
        payload = Json.obj(
          "uid" -> r.serviceAccountId,
          "token" -> jwt
        )
        _ = log.debug("sending " + Json.prettyPrint(payload) + " to " + url)
        resp <- client.url(url).post(payload)
      } yield resp
      f.onComplete(_ match {
        case Success(resp) if resp.status == 200 =>
          (resp.json \ "token").asOpt[String] match {
            case Some(tok) => s ! DCOSAuthTokenActor.DCOSAuthTokenResponse(tok)
            case None      => s ! DCOSAuthTokenActor.DCOSAuthTokenError("acs responded with 200 but response did not have parsable token")
          }
        case Success(resp) if resp.status != 200 =>
          s ! DCOSAuthTokenActor.DCOSAuthTokenError(resp.body)
        case Failure(t) =>
          s ! DCOSAuthTokenActor.DCOSAuthTokenError(t.getMessage)
      })
  }
}
