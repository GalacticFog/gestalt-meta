package actors

import akka.actor.Props
import akka.pattern.ask
import com.galacticfog.gestalt.meta.test.{MetaRepositoryOps, WithDb}
import play.api.test.PlaySpecification

import scala.concurrent.duration._

class SystemConfigActorSpecs extends PlaySpecification with MetaRepositoryOps {

  implicit val askTimeout: akka.util.Timeout = 5 seconds

  abstract class TestScope extends WithDb(containerApp(
  )) {
    lazy val configActor = app.actorSystem.actorOf(Props(new SystemConfigActor))
  }

  "SystemConfigActor" should {

    "return system config entries" in new TestScope {

      await((configActor ? SystemConfigActor.GetKey("nonexistent-key")).mapTo[Option[String]]) must beNone
      await((configActor ? SystemConfigActor.SetKeys( ("test-key" -> "test-value-a") )).mapTo[Boolean]) must beTrue
      await((configActor ? SystemConfigActor.GetKey("test-key-a")).mapTo[Option[String]]) must beSome("test-value-a")
      await((configActor ? SystemConfigActor.SetKeys( ("test-key-b" -> "test-value-b") )).mapTo[Boolean]) must beTrue
      await((configActor ? SystemConfigActor.GetAllConfig).mapTo[Map[String,String]]) must havePair(
        "test-key-a" -> "test-value-a",
        "test-key-b" -> "test-value-b"
      )
      await((configActor ? SystemConfigActor.SetKeys( ("test-key-a" -> "updated-value-a") )).mapTo[Boolean]) must beTrue
      await((configActor ? SystemConfigActor.GetAllConfig).mapTo[Map[String,String]]) must havePair(
        "test-key-a" -> "updated-value-a",
        "test-key-b" -> "test-value-b"
      )
    }

  }

}
