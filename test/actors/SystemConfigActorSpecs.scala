package actors

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.testkit.ImplicitSender

import scala.concurrent.duration._
import com.galacticfog.gestalt.meta.test.{MetaRepositoryOps, ResourceScope, WithDb}
import org.specs2.matcher.JsonMatchers
import org.specs2.specification.{BeforeAfterEach, BeforeAll}
import play.api.inject.BindingKey
import play.api.libs.concurrent.AkkaGuiceSupport
import play.api.test.PlaySpecification

class SystemConfigActorSpecs extends PlaySpecification with MetaRepositoryOps {

  implicit val askTimeout: akka.util.Timeout = 5 seconds

  abstract class TestScope extends WithDb(containerApp(
  )) {
    lazy val configActor = app.actorSystem.actorOf(Props(new SystemConfigActor))
  }

  "SystemConfigActor" should {

    "return system config entries" in new TestScope {

      configActor ! SystemConfigActor.SetKey("test-key", "test-value")
      await((configActor ? SystemConfigActor.GetKey("test-key")).mapTo[Option[String]]) must beSome("test-value")
      await((configActor ? SystemConfigActor.GetAllConfig).mapTo[Map[String,String]]) must havePair(
        "test-key" -> "test-value"
      )
    }

  }

}
