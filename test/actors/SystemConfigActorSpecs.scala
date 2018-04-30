package actors

import akka.actor.Props
import akka.pattern.ask
import com.galacticfog.gestalt.meta.test.{MetaRepositoryOps, WithDb}
import controllers.SecurityResources
import org.specs2.specification.BeforeAll
import play.api.test.PlaySpecification

import scala.concurrent.duration._
import scala.util.Success

class SystemConfigActorSpecs extends PlaySpecification with MetaRepositoryOps {

  implicit val askTimeout: akka.util.Timeout = 5 seconds

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  abstract class TestScope extends WithDb(containerApp(
  )) {
    lazy val configActor = app.actorSystem.actorOf(Props(new SystemConfigActor))
    lazy val Success(testUser) = Ents.createNewMetaUser(user, dummyRootOrgId, user.account,
      Some(Map(
        "firstName" -> user.account.firstName,
        "lastName" -> user.account.lastName,
        "email" -> user.account.email.getOrElse(""),
        "phoneNumber" -> user.account.phoneNumber.getOrElse("")
      )),
      user.account.description
    )
  }

  "SystemConfigActor" should {

    "return system config entries" in new TestScope {

      await((configActor ? SystemConfigActor.GetKey("nonexistent-key")).mapTo[Option[String]]) must beNone
      await((configActor ? SystemConfigActor.SetKeys( testUser.id, ("test-key-a" -> "test-value-a") )).mapTo[Boolean]) must beTrue
      await((configActor ? SystemConfigActor.GetKey("test-key-a")).mapTo[Option[String]]) must beSome("test-value-a")
      await((configActor ? SystemConfigActor.SetKeys( testUser.id, ("test-key-b" -> "test-value-b") )).mapTo[Boolean]) must beTrue
      await((configActor ? SystemConfigActor.GetAllConfig).mapTo[Map[String,String]]) must havePairs(
        "test-key-a" -> "test-value-a",
        "test-key-b" -> "test-value-b"
      )
      await((configActor ? SystemConfigActor.SetKeys( testUser.id, ("test-key-a" -> "updated-value-a") )).mapTo[Boolean]) must beTrue
      await((configActor ? SystemConfigActor.GetAllConfig).mapTo[Map[String,String]]) must havePairs(
        "test-key-a" -> "updated-value-a",
        "test-key-b" -> "test-value-b"
      )
    }

  }

}
