package actors

import akka.actor.Actor
import com.google.inject.{Inject, Singleton}

@Singleton
class SystemConfigActor @Inject() extends Actor {

  override def receive: Receive = ???

}

object SystemConfigActor {
  final val name = "meta-system-config-actor"

  case class SetKey(key: String, value: String)
  case class GetKey(key: String)
  case object GetAllConfig
}
