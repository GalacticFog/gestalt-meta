package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.events
import com.galacticfog.gestalt.events.AmqpClient
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceOwnerLink}
import com.galacticfog.gestalt.meta.policy.{EventArgs, EventMessage}
import com.galacticfog.gestalt.meta.test.ResourceScope
import org.joda.time.DateTime
import org.specs2.matcher._
import org.specs2.mock.Mockito
import org.specs2.specification.BeforeAll
import play.api.libs.json.JsValue
import play.api.test.PlaySpecification
import com.galacticfog.gestalt.data._

class EventsSpec extends PlaySpecification with JsonMatchers with ResourceScope with Mockito with BeforeAll {

  lazy val rootOrgId = UUID.randomUUID()
  //lazy val adminUserId = UUID.randomUUID()
  lazy val owner = ResourceOwnerLink(ResourceIds.User, adminUserId)

  stopOnFail
  sequential

  override def beforeAll(): Unit = pristineDatabase()

  lazy val mockAmqpClient = mock[AmqpClient]
  lazy val eventMethods = new EventMethodsTrait {
    def eventsClient(): AmqpClient = mockAmqpClient
  }

  "EventMessage" should {
    "serialize to the expected payload" in {
      val resource = newInstance(typeId = ResourceIds.Container, name = "some-container")
      val eventMessage = EventMessage(
        id = uuid,
        identity = uuid,
        timestamp = DateTime.now(),
        event = "resource.action",
        action = "resource.action.when",
        args = EventArgs(
          rule = newInstance(typeId = ResourceIds.RuleEvent, name = "some-rule"),
          payload = Some(resource)
        )
      )
      // TODO: can't get the matching working on this JsObject, so just check a single field
      eventMessage.toJson.toString must /("args") /("payload") /("resource") /("name" -> resource.name)
    }
  }

  "event payload" should {

    "include optional data" in {
      val eventMessage = EventMessage(
        id = uuid,
        identity = uuid,
        timestamp = DateTime.now(),
        event = "resource.action",
        action = "resource.action.when",
        args = EventArgs(
          rule = newInstance(typeId = ResourceIds.RuleEvent, name = "some-rule"),
          payload = Some(newInstance(typeId = ResourceIds.Container, name = "some-container"))
        )
      )
      val metaUrl = "http://meta.test.com"
      val providerId = uuid.toString
      val requestOpts = RequestOptions(
        user = dummyAuthAccountWithCreds(),
        authTarget = None,
        policyOwner = None,
        policyTarget = None,
        data = Some(Map(
          "meta_url" -> metaUrl,
          "provider_id" -> providerId,
          "arbitrary" -> "value"
        ))
      )
      val hasMetaUrl = /("args") /("payload") /("meta_url" -> metaUrl)
      val hasProviderId = /("args") /("payload") /("provider_id" -> providerId)
      eventMethods.publishEvent(event = eventMessage, opts = requestOpts)
      there was one(mockAmqpClient).publish(any[events.AmqpEndpoint], new Matcher[JsValue] {
        override def apply[S <: JsValue](t: Expectable[S]): MatchResult[S] = {
          val v = t.value
          result(((v \ "args" \ "payload" \ "meta_url").asOpt[String].contains(metaUrl), "contains meta_url", "does not contain meta_url"), t) and
            result(((v \ "args" \ "payload" \ "provider_id").asOpt[String].contains(providerId), "contains provider_id", "does not contain provider_id"), t) and
            result(((v \ "args" \ "payload" \ "arbitrary").asOpt[String].contains("value"), "contains arbitrary value", "does not contain arbitrary value"), t) and
            result(((v \ "args" \ "rule" \ "name").asOpt[String].contains("some-rule"), "contains rule", "does not contain rule"), t)
        }
      })
    }
  }//.pendingUntilFixed("catch up with refactor")

}