package services

import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.security.{Key, KeyFactory}

import akka.actor.Actor
import com.google.inject.{Inject, Singleton}
import io.jsonwebtoken.{Jwts, SignatureAlgorithm}
import org.apache.commons.codec.binary.Base64
import play.api.libs.json.Json
import play.api.libs.ws.WSClient

import scala.util.{Failure, Success}
import collection.JavaConverters._

@Singleton
class DCOSAuthTokenActor @Inject() (client: WSClient) extends Actor {
  import services.DCOSAuthTokenActor._

  override def receive: Receive = {
    case r: DCOSAuthTokenActor.DCOSAuthTokenRequest =>
      import context.dispatcher
      val s = sender()
      val jwt = Jwts.builder()
        .setExpiration(java.util.Date.from(java.time.Instant.now().plusSeconds(60*5)))
        .setClaims(Map[String,AnyRef](
          "exp" -> (System.currentTimeMillis() / 1000 + 5*60).toString,
          "uid" -> r.serviceAccountId
        ).asJava)
        .signWith(SignatureAlgorithm.RS256, strToPrivateKey(r.privateKey))
        .compact()
      val f = client
        .url(r.dcosUrl.stripPrefix("/") + "/acs/api/v1/auth/login")
        .post(Json.obj(
          "uid" -> r.serviceAccountId,
          "token" -> jwt
        ))
      f.onComplete(_ match {
        case Success(resp) if resp.status == 200 =>
          (resp.json \ "token").asOpt[String] match {
            case Some(tok) => s ! DCOSAuthTokenResponse(tok)
            case None      => s ! DCOSAuthTokenError("acs responded with 200 but response did not have parsable token")
          }
        case Success(resp) if resp.status != 200 =>
          s ! DCOSAuthTokenError(resp.body)
        case Failure(t) =>
          s ! DCOSAuthTokenError(t.getMessage)
      })
  }

}

object DCOSAuthTokenActor {

  final val name = "dcos-auth-token-actor"

  case class DCOSAuthTokenRequest( serviceAccountId : String,
                                   privateKey : String,
                                   dcosUrl : String )

  case class DCOSAuthTokenResponse( authToken: String )

  case class DCOSAuthTokenError( message: String )

  def strToPrivateKey(pkcs8: String): Key = {
    val encoded = Base64.decodeBase64(
      pkcs8.replace("-----BEGIN PRIVATE KEY-----\n", "")
           .replace("-----END PRIVATE KEY-----", "")
    )
    val kf = KeyFactory.getInstance("RSA");
    val keySpec = new PKCS8EncodedKeySpec(encoded);
    kf.generatePrivate(keySpec)
  }

  def strToPublicKey(pkcs8: String): Key = {
    val encoded = Base64.decodeBase64(
      pkcs8.replace("-----BEGIN PUBLIC KEY-----\n", "")
        .replace("-----END PUBLIC KEY-----", "")
    )
    val kf = KeyFactory.getInstance("RSA");
    val keySpec = new X509EncodedKeySpec(encoded)
    kf.generatePublic(keySpec)
  }

}
