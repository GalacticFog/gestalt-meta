package controllers.util


import com.galacticfog.gestalt.meta.test._
import play.api.test.PlaySpecification
import scala.util.{Success, Try}
import com.galacticfog.gestalt.meta.policy.EventMessage

class EventMethodsSpec extends PlaySpecification with MetaRepositoryOps {
  
  
  //def findEffectiveEventRules(parentId: UUID, event: Option[String] = None)
  
  "findEffectiveEventRules" should {

//    "find all event rules up the tree corresponding to the given event" >> {
//      val (wrk, env) = createWorkspaceEnvironment(dummyRootOrgId)
//
//      val testLambda = newDummyLambda(env)
//      testLambda must beSuccessfulTry
//
//      val action = "resource.verb.post"
//      val (orgPolicy, orgRule) = createEventRule(dummyRootOrgId, testLambda.get.id, action)
//      val (wrkPolicy, wrkRule) = createEventRule(wrk, testLambda.get.id, action)
//      val (envPolicy, envRule) = createEventRule(env, testLambda.get.id, action)
//
//      ResourceFactory.findById(ResourceIds.RuleEvent, orgRule) must beSome
//      ResourceFactory.findById(ResourceIds.RuleEvent, wrkRule) must beSome
//      ResourceFactory.findById(ResourceIds.RuleEvent, envRule) must beSome
//
//
//      val em = new EventMethods{}
//
//      val effective = em.findEffectiveEventRules2(env, Option(action))
//
//      println("****EFF.SIZE : " + effective.size)
//      failure
//    }

    "find event rule in ancestor environment" >> {
      val (wrk, env) = createWorkspaceEnvironment(dummyRootOrgId)

      val Success(testApi) = createApi("test-api", parent = Some(env))
      val Success(testEndpoint) = createApiEndpoint(testApi.id, name = "-dummy", properties = Some(Map(
        "resource" -> "/dummy"
      )))
      val testLambda = newDummyLambda(env)
      testLambda must beSuccessfulTry

      val action = "apiendpoint.update.post"
      val (envPolicy, envRule) = createEventRule(env, testLambda.get.id, action)

      val pub = scala.collection.mutable.ArrayBuffer.empty[EventMessage]
      val em = new EventMethodsTrait {
        override def publishEvent(event: EventMessage, opts: RequestOptions): Try[Unit] = {
          pub += event
          Success(())
        }

        def eventsClient() = null
      }

      val effective = em.publishEvent(
        actionName = "apiendpoint.update",
        eventType = "post",
        opts = RequestOptions(
          user = user,
          authTarget = None,
          policyTarget = Some(testEndpoint),
          policyOwner = None,
          data = Some(Map(
            "parentId" -> testApi.id.toString
          ))
        )
      )

      effective must beSuccessfulTry
      pub must haveSize(1)
    }

  }
  
}