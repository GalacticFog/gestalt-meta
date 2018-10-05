package com.galacticfog.gestalt.container.dcos

import java.security.spec.PKCS8EncodedKeySpec
import java.security.{KeyFactory, PrivateKey}

import io.jsonwebtoken.{Jwts, SignatureAlgorithm}
import org.apache.commons.codec.binary.Base64
import play.api.Logger
import play.api.libs.json.Json
import play.api.libs.ws.WSClient

import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class ACSTokenClient(client: WSClient) {

  val log = Logger(this.getClass)

  import ACSTokenClient._

  def getToken(r: DCOSAuthTokenRequest)(implicit ec: ExecutionContext): Future[String] = {
    val claims: Map[String, AnyRef] = Map(
      "uid" -> r.serviceAccountId,
      "exp" -> float2Float(System.currentTimeMillis()/1000 + 5*60)
    )
    for {
      jwt <- Future.fromTry(Try{
        Jwts.builder()
          .setClaims(claims.asJava)
          .signWith(SignatureAlgorithm.RS256, strToPrivateKey(r.privateKey))
          .compact()
      })
      url = r.dcosUrl.stripPrefix("/") + "/acs/api/v1/auth/login"
      payload = Json.obj(
        "uid" -> r.serviceAccountId,
        "token" -> jwt
      )
      _ = log.warn("sending " + Json.prettyPrint(payload) + " to " + url)
      resp <- client.url(url).post(payload)
      json <- if (resp.status == 200) Future.successful(resp.json) else Future.failed(new RuntimeException(resp.body))
      token <- (json \ "token").asOpt[String] match {
        case Some(tok) => Future.successful(tok)
        case None      => Future.failed(new RuntimeException("acs responded with 200 but response did not have parsable token"))
      }
    } yield token
  }

}

object ACSTokenClient {

  case class DCOSAuthTokenRequest( serviceAccountId : String,
                                   privateKey : String,
                                   dcosUrl : String )

  def strToPrivateKey(pkcs8: String): PrivateKey = {
    val encoded = Base64.decodeBase64(
      pkcs8.replace("-----BEGIN PRIVATE KEY-----\n", "")
        .replace("-----END PRIVATE KEY-----", "")
    )
    val kf = KeyFactory.getInstance("RSA")
    val keySpec = new PKCS8EncodedKeySpec(encoded)
    kf.generatePrivate(keySpec)
  }

}

