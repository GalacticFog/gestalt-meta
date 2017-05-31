package controllers


import controllers.util.{GatewayMethods, GestaltProviderMocking, LambdaMethods}
import org.specs2.mock.Mockito
import org.specs2.specification._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import play.api.test._
import org.specs2.matcher.JsonMatchers
import com.galacticfog.gestalt.patch._
import com.galacticfog.gestalt.meta.api.ResourcePath
import com.galacticfog.gestalt.meta.api.patch.PatchInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import play.api.inject.bind
import org.mockito.Matchers.{eq => meq}

import scala.concurrent.Future
import scala.util.{Success, Try}

class PatchControllerSpec extends PlaySpecification with GestaltProviderMocking with JsonMatchers with ResourceScope with BeforeAll with Mockito {

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

  sequential

  abstract class FakeSecurity extends WithDb(containerApp(
    additionalBindings = Seq(
      bind(classOf[LambdaMethods]).toInstance(mockLambdaMethods),
      bind(classOf[GatewayMethods]).toInstance(mockGatewayMethods)
    )
  ))

  trait TestApplication extends FakeSecurity {
    val Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")

    Ents.setNewEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

    val Success(testKubeProvider) = createInstance(ResourceIds.KubeProvider, "test-kube-provider", properties = Some(Map()))
    val Success(testLambdaProvider) = createInstance(ResourceIds.LambdaProvider, "test-lambda-provider", properties = Some(Map(
      "config" ->
        """{
          |  "env": {
          |     "public": {
          |       "SERVICE_HOST": "laser.service",
          |       "SERVICE_PORT": "1111"
          |     }
          |  }
          |}""".stripMargin
    )))
    val Success(testLambda) = createInstance(
      ResourceIds.Lambda,
      "test-lambda",
      properties = Some(Map(
        "public" -> "true",
        "cpus" -> "0.1",
        "memory" -> "128",
        "code_type" -> "inline",
        "timeout" -> "30",
        "handler" -> "blah;blah",
        "runtime" -> "custom",
        "provider" -> Json.obj(
          "name" -> testLambdaProvider.name,
          "id" -> testLambdaProvider.id.toString
        ).toString
      )),
      parent = Some(testEnv.id)
    )
    val Success(testEndpoint) = createInstance(
      ResourceIds.ApiEndpoint,
      "test-endpoint",
      properties = Some(Map(
        "resource" -> "/the/path"
      )),
      parent = Some(testEnv.id)
    )
    val Success(testContainer) = createInstance(
      ResourceIds.Container,
      "test-container",
      parent = Some(testEnv.id),
      properties = Some(Map(
        "container_type" -> "DOCKER",
        "image" -> "nginx",
        "provider" -> Output.renderInstance(testKubeProvider).toString,
        "cpus" -> "1.0",
        "memory" -> "1024",
        "disk" -> "0",
        "num_instances" -> "1",
        "force_pull" -> "true",
        "port_mappings" -> "[]",
        "network" -> "default",
        "external_id" -> s"/${testEnv.id}/test-container"
      ))
    )
  }

  "Patch" should {

    "Environment Variables" should {

      "add a new env object when it does not exist" in new TestApplication {
        
        val orgName = "patchtest1"
        val org = newOrg(id = dummyRootOrgId, name = orgName)
        org must beSuccessfulTry
        

        // Create new environment and ensure /properties/env is None
        //
        val data = newDummyEnvironment(org = org.get.id)
        val envRes = ResourceFactory.findById(data("environment")).get
        envRes.properties.get.get("env") must beNone

        // Create PATCH JSON
        //
        val var1 = "DATABASE" -> "gestalt-test"
        val var2 = "USERNAME" -> "user1"
        val var3 = "PASSWORD" -> "secret"

        val patchJs = Json.toJson(Seq(
            PatchOp.Add("/properties/env", Json.obj(
                var1._1 -> var1._2, 
                var2._1 -> var2._2, 
                var3._1 -> var3._2 )))
        )
        
        // Create dummy user account
        //
        val account = dummyAuthAccountWithCreds(userInfo = Map("id" -> adminUserId))

        val path = new ResourcePath(s"/${orgName}/environments/${envRes.id}")

        val rc = app.injector.instanceOf[ResourceController]
        val pc = app.injector.instanceOf[PatchController]
        
        val updated = pc.Patch(envRes, patchJs, account)
        updated must beSuccessfulTry
        
        val envJs = Output.renderInstance(updated.get)

        envJs.toString must /("properties") /("env") /(var1._1 -> var1._2)
        
      }

      "add a new key to the map when env already exists" in {
        failure
      }.pendingUntilFixed("write these tests")

      "replace the entire env object with a new value" in {
        failure
      }.pendingUntilFixed("write these tests")

      "replace the value of a single key" in {
        failure
      }.pendingUntilFixed("write these tests")

      "remove a single key from the env object" in {
        failure
      }.pendingUntilFixed("write these tests")

      "remove the entire env object" in {
        failure
      }.pendingUntilFixed("write these tests")
    }
  }

  "toPatch" should {

    "return a PatchDocument from valid patch JSON" in {
      failure
    }.pendingUntilFixed("write these tests")

    "fail with a BadRequestException if the JSON is invalid" in {
      failure
    }.pendingUntilFixed("write these tests")

  }

  "PatchController" should {

    "use LambdaMethods for external lambda patch" in new TestApplication {

      mockLambdaMethods.patchLambdaHandler(any,any,any) returns Try(testLambda)

      val patchDoc = PatchDocument()

      val request = fakeAuthRequest(PATCH,
        s"/root/environments/${testEnv.id}/lambdas/${testLambda.id}", testCreds
      ).withBody(patchDoc.toJson)

      val Some(result) = route(request)

      status(result) must equalTo(OK)

      there was one(mockLambdaMethods).patchLambdaHandler(
        r = argThat(
          (r: GestaltResourceInstance) => r.id == testLambda.id
        ),
        patch = meq(patchDoc),
        user = any
      )
    }

    "use GatewayMethods for external apiendpoint patch" in new TestApplication {

      mockGatewayMethods.patchEndpointHandler(any, any, any) returns Try(testEndpoint)

      val patchDoc = PatchDocument()

      val request = fakeAuthRequest(PATCH,
        s"/root/environments/${testEnv.id}/apiendpoints/${testEndpoint.id}", testCreds
      ).withBody(patchDoc.toJson)

      val Some(result) = route(request)

      status(result) must equalTo(OK)

      there was one(mockGatewayMethods).patchEndpointHandler(
        r = argThat(
          (r: GestaltResourceInstance) => r.id == testEndpoint.id
        ),
        patch = meq(patchDoc),
        user = any)
    }

    "use ContainerService for external container patch" in new TestApplication {

      mockContainerService.patchContainer(any, any, any) answers {
        (a: Any) =>
          val arr = a.asInstanceOf[Array[Object]]
          val r = arr(0).asInstanceOf[GestaltResourceInstance]
          val pd = arr(1).asInstanceOf[PatchDocument]
          PatchInstance.applyPatch(r, pd).map(_.asInstanceOf[GestaltResourceInstance])
      }
      mockContainerService.getEnvironmentContainer("root", testEnv.id, testContainer.id) returns Future.successful(Some(testContainer -> Seq.empty))

      val patchDoc = PatchDocument(PatchOp.Replace("/properties/image", "nginx:upgrade"))

      val request = fakeAuthRequest(PATCH,
        s"/root/environments/${testEnv.id}/containers/${testContainer.id}", testCreds
      ).withBody(patchDoc.toJson)

      val Some(result) = route(request)

      status(result) must equalTo(OK)
      (contentAsJson(result) \ "properties" \ "image").as[String] must_== "nginx:upgrade"

      there was one(mockContainerService).patchContainer(
        container = argThat(
          (r: GestaltResourceInstance) => r.id == testContainer.id
        ),
        patch = meq(patchDoc),
        user = any
      )
    }

  }
  
}

