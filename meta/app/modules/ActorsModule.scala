package modules

import com.google.inject.AbstractModule
import play.api.libs.concurrent.AkkaGuiceSupport

import services.ECSActor

class ActorsModule extends AbstractModule with AkkaGuiceSupport {
  override def configure(): Unit = {
    bindActor[ECSActor]("ecs-actor")
  }
}