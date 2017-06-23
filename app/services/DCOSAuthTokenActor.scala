package services

import akka.actor.Actor
import com.google.inject.{Inject, Singleton}

@Singleton
class DCOSAuthTokenActor @Inject() () extends Actor {

  override def receive: Receive = {
    case r: DCOSAuthTokenActor.DCOSAuthTokenRequest => ???
  }

}

object DCOSAuthTokenActor {

  final val name = "dcos-auth-token-actor"

  case class DCOSAuthTokenRequest( serviceAccountId : String,
                                   privateKey : String,
                                   dcosUrl : String )

  case class DCOSAuthTokenResponse( authToken: String )

  case class DCOSAuthTokenError( message: String )

}
