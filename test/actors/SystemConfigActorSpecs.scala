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

  override def beforeAll(): Unit = {
    pristineDatabase()
    val Success(_) = Ents.createNewMetaUser(user, dummyRootOrgId, user.account,
      Some(Map(
        "firstName" -> user.account.firstName,
        "lastName" -> user.account.lastName,
        "email" -> user.account.email.getOrElse(""),
        "phoneNumber" -> user.account.phoneNumber.getOrElse("")
      )),
      user.account.description
    )
  }


  abstract class TestScope extends WithDb(containerApp()) {
    lazy val configActor = app.actorSystem.actorOf(Props(new SystemConfigActor))
    lazy val testUserId = user.account.id
  }

  "SystemConfigActor" should {

    "handle missing config on startup" in new TestScope {
      await((configActor ? SystemConfigActor.GetAllConfig).mapTo[Map[String,String]]) must beEmpty
    }

    "set and return individual keys appropriately" in new TestScope {
      await((configActor ? SystemConfigActor.GetKey("solo-key-a")).mapTo[Option[String]]) must beNone
      await((configActor ? SystemConfigActor.SetKey(testUserId, "solo-key-a", Some("value-a"))).mapTo[Option[String]]) must beNone
      await((configActor ? SystemConfigActor.GetKey("solo-key-a")).mapTo[Option[String]]) must beSome("value-a")
      await((configActor ? SystemConfigActor.GetAllConfig).mapTo[Map[String,String]]) must havePair(
        "solo-key-a" -> "value-a"
      )
    }

    "handle multiple add/remove in a transaction" in new TestScope {
      await((configActor ? SystemConfigActor.SetKeys(testUserId, Map(
        "test-key-a" -> Some("test-value-a")
      ))).mapTo[Map[String,Option[String]]]) must_== Map(
        "test-key-a" -> None
      )
      await((configActor ? SystemConfigActor.GetKey("test-key-a")).mapTo[Option[String]]) must beSome("test-value-a")
      await((configActor ? SystemConfigActor.SetKeys(testUserId, Map(
        "test-key-b" -> Some("test-value-b")
      ))).mapTo[Map[String,Option[String]]]) must_== Map(
        "test-key-b" -> None
      )
      await((configActor ? SystemConfigActor.GetAllConfig).mapTo[Map[String,String]]) must havePairs(
        "test-key-a" -> "test-value-a",
        "test-key-b" -> "test-value-b"
      )
      await((configActor ? SystemConfigActor.SetKeys(testUserId, Map(
        "test-key-a" -> None,
        "test-key-b" -> Some("new-test-value-b"),
        "test-key-c" -> Some("test-value-c")
      ))).mapTo[Map[String,Option[String]]]) must_== Map(
        "test-key-a" -> Some("test-value-a"),
        "test-key-b" -> Some("test-value-b"),
        "test-key-c" -> None
      )
      await((configActor ? SystemConfigActor.GetAllConfig).mapTo[Map[String,String]]) must havePairs(
        "test-key-b" -> "new-test-value-b",
        "test-key-c" -> "test-value-c"
      ) and not haveKey("test-key-a")
    }

    "test-and-set for non-existent items" in new TestScope {
      await((configActor ? SystemConfigActor.TestAndSet(
        testUserId, "nonexistent-test-and-set",
        {(o: Option[String]) => o.contains("will not pass")},
        {(o: Option[String]) => Some("does will not")}
      )).mapTo[SystemConfigActor.TestAndSetResult]) must_== (false, None)
      await((configActor ? SystemConfigActor.GetKey("nonexistent-test-and-set")).mapTo[Option[String]]) must beNone
    }

    "test-and-set for bad predicate functions" in new TestScope {
      await((configActor ? SystemConfigActor.TestAndSet(
        testUserId, "nonexistent-test-and-set",
        {(o: Option[String]) => throw new RuntimeException("who's bad?")},
        {(o: Option[String]) => Some("does will not")}
      )).mapTo[SystemConfigActor.TestAndSetResult]) must_== (false, None)
      await((configActor ? SystemConfigActor.GetKey("nonexistent-test-and-set")).mapTo[Option[String]]) must beNone
    }

    "test-and-set for bad value functions" in new TestScope {
      await((configActor ? SystemConfigActor.TestAndSet(
        testUserId, "nonexistent-test-and-set",
        {(o: Option[String]) => true},
        {(o: Option[String]) => throw new RuntimeException("who's bad?")}
      )).mapTo[SystemConfigActor.TestAndSetResult]) must_== (false, None)
      await((configActor ? SystemConfigActor.GetKey("nonexistent-test-and-set")).mapTo[Option[String]]) must beNone
    }

    "test-and-set for extant items" in new TestScope {

    }

  }

}
