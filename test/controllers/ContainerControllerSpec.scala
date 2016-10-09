package controllers


import java.util.UUID
import com.galacticfog.gestalt.data.bootstrap.Bootstrap
import controllers.util.db.ConnectionManager
import org.specs2.mutable._
import org.specs2.specification._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import play.api.test._


class ContainerControllerSpec extends PlaySpecification with ResourceScope with BeforeAll {
  val rootOrgId = UUID.randomUUID()

  override def beforeAll(): Unit = pristineDatabase()

  "findMigrationRule" should {
    
    "find any container.migrate.* rules that are within the given scope" in new WithApplication {
      val org = newOrg(id = dummyRootOrgId)
      org must beSuccessfulTry
      
      val data = newDummyEnvironment(dummyRootOrgId)
      
      ContainerController.findMigrationRule(data("environment")).isEmpty must beTrue
      
      val (_, rule) = createEventRule(data("environment"), data("lambda"), "container.migrate.pre")
      ResourceFactory.findById(ResourceIds.RuleEvent, rule) must beSome
      
      val event = ContainerController.findMigrationRule(data("environment"))
      
      event.isEmpty must beFalse
    }
  }
}