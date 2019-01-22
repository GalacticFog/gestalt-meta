package com.galacticfog.gestalt.integrations.kubernetes

import java.time.Instant
import java.util.UUID

import akka.actor.{Actor, Props}
import akka.pattern.ask
import com.google.inject.{Inject, Singleton}
import play.api.{Configuration, Logger}
import KubeTokenActor.{KubeAuthTokenError, KubeAuthTokenRequest, KubeAuthTokenResponse}

import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

@Singleton
class KubeTokenActor @Inject() (config: Configuration) extends Actor {

  val log = Logger(this.getClass)

  import KubeTokenActor._

  val providerTokens: mutable.Map[UUID, CachedToken] = mutable.Map.empty

  val requestChild = context.actorOf(Props(classOf[KubeExternalExecActor]))

  val expiryInSeconds = config.getInt("skuberFactory.tokenExpiryInSeconds").getOrElse[Int](10 * 60)
  val timeoutInSeconds = config.getLong("skuberFactory.execAuthTimeoutInSeconds").getOrElse(30l)
  implicit val timeout = akka.util.Timeout(timeoutInSeconds .seconds)

  // use the error kernel pattern to help protect our state from being wiped...
  // if we fail, it's not the end of the world, because the tokens will be regenerated
  override def receive: Receive = {
    case r: KubeAuthTokenRequest =>
      import context.dispatcher
      val s = sender()
      providerTokens.get(r.providerId) match {
        case Some(CachedToken(accessToken, configHash, expiry)) if configHash == r.configHash && Instant.now().compareTo(expiry) < 0 =>
          s ! KubeAuthTokenResponse(accessToken)
        case _ =>
          val f = requestChild ? r
          f.onComplete {
            case Success(response: KubeAuthTokenResponse) =>
              log.info(s"Caching token for ${r.providerId} with config hash ${r.configHash}")
              providerTokens += ( r.providerId -> CachedToken(response.authToken,r.configHash,Instant.now().plusSeconds(expiryInSeconds)))
              s ! response
            case Success(r) =>
              s ! KubeAuthTokenError(s"got unexpected response from child actor: ${r}")
            case Failure(t) =>
              s ! KubeAuthTokenError(t.getMessage)
          }
      }
    case e =>
      log.error(s"KubeTokenActor: unhandled message type: $e")
  }

}

object KubeTokenActor {

  case class CachedToken(accessToken: String, configHash: Int, expiry: Instant)

  final val name = "kube-token-actor"

  case class KubeAuthTokenRequest(providerId: UUID,
                                  configHash: Int,
                                  tokenAuth: skuber.api.client.AccessTokenAuth )
  case class KubeAuthTokenResponse(authToken: String )
  case class KubeAuthTokenError(message: String )
}

class KubeExternalExecActor() extends Actor {
  val log = Logger(classOf[KubeTokenActor ])

  override def receive: Receive = {
    case r: KubeAuthTokenRequest =>
      val s = sender()
      Try(r.tokenAuth.accessToken) match {
        case Success(token) => s ! KubeAuthTokenResponse(token)
        case Failure(err)   => s ! KubeAuthTokenError(err.getMessage)
      }
  }
}
