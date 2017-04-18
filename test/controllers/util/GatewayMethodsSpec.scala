package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.laser.LaserEndpoint
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import controllers.{ContainerController, DeleteController, SecurityResources}
import org.joda.time.DateTime
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.matcher.{JsonMatchers, Matcher}
import org.specs2.specification.{BeforeAll, Scope}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.PlaySpecification

import scala.concurrent.Future
import scala.util.Success

class GatewayMethodsSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll with JsonMatchers {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    pristineDatabase()
    val Success(createdUser) = Ents.createNewMetaUser(user, dummyRootOrgId, user.account,
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

  abstract class FakeCaaSScope extends Scope {
    var Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    Entitlements.setNewEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

    val injector =
      new GuiceApplicationBuilder()
        .disable[modules.ProdSecurityModule]
        .disable[modules.MetaDefaultSkuber]
        .disable[modules.MetaDefaultServices]
        .disable[modules.HealthModule]
        .bindings(
          bind(classOf[GestaltSecurityConfig]).toInstance(mock[GestaltSecurityConfig])
        )
        .injector
  }
  
  trait TestApplication extends FakeCaaSScope {
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
    val Success(testLambda) = createInstance(ResourceIds.Lambda, "test-lambda", properties = Some(Map(
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
    )))
    val Success(testContainer) = createInstance(ResourceIds.Container, "test-container", properties = Some(Map(
      "cpus" -> "0.1",
      "memory" -> "128",
      "image" -> "nginx",
      "container_type" -> "DOCKER",
      "port_mappings" -> Json.toJson(Seq(
        ContainerSpec.PortMapping("tcp").copy(
          name = Some("web"),
          expose_endpoint = Some(true),
          service_address = Some(ContainerSpec.ServiceAddress(
            host = "my-nginx.service-address",
            port = 80,
            protocol = Some("tcp")
          ))
        ),
        ContainerSpec.PortMapping("tcp").copy(
          name = Some("not-exposed")
        )
      )).toString,
      "provider" -> Json.obj(
        "id" -> UUID.randomUUID().toString,
        "name" -> "nonexistent-provider-does-not-matter"
      ).toString
    )))
  }

  "GatewayMethods" should {

    "properly create upstream_url for synchronous lambda-bound endpoints" in new TestApplication {
      val endpointId = UUID.randomUUID()
      val apiId = UUID.randomUUID()
      GatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> endpointId.toString,
        "properties" -> Json.obj(
          "implementation_type" -> "lambda",
          "synchronous" -> true,
          "implementation_id" -> testLambda.id.toString,
          "resource" -> "/some-path"
        )
      ), apiId) must beSuccessfulTry(
        (e: LaserEndpoint) => (e.upstreamUrl must_== s"http://laser.service:1111/lambdas/${testLambda.id}/invokeSync") and
          (e.id must beSome(endpointId.toString)) and
          (e.apiId must_== apiId.toString)
      )
    }

    "properly create upstream_url for asynchronous lambda-bound endpoints" in new TestApplication {
      val endpointId = UUID.randomUUID()
      val apiId = UUID.randomUUID()
      GatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> endpointId.toString,
        "properties" -> Json.obj(
          "implementation_type" -> "lambda",
          "synchronous" -> false,
          "implementation_id" -> testLambda.id.toString,
          "resource" -> "/some-path"
        )
      ), apiId) must beSuccessfulTry(
        (e: LaserEndpoint) => (e.upstreamUrl must_== s"http://laser.service:1111/lambdas/${testLambda.id}/invoke") and
          (e.id must beSome(endpointId.toString)) and
          (e.apiId must_== apiId.toString)
      )
    }

    "properly infer implementation_type from implementation_id for backwards compatibility for lambda-bound endpoints and overwrite upstream_url" in new TestApplication {
      val endpointId = UUID.randomUUID()
      val apiId = UUID.randomUUID()
      GatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> endpointId.toString,
        "properties" -> Json.obj(
          "implementation_id" -> testLambda.id.toString,
          "resource" -> "/some-path",
          "upstream_url" -> "please-overwrite-me"
        )
      ), apiId) must beSuccessfulTry(
        (e: LaserEndpoint) => (e.upstreamUrl must_== s"http://laser.service:1111/lambdas/${testLambda.id}/invokeSync") and
          (e.id must beSome(endpointId.toString)) and
          (e.apiId must_== apiId.toString)
      )
    }

    "properly create upstream_url for container-bound endpoints" in new TestApplication {
      val endpointId = UUID.randomUUID()
      val apiId = UUID.randomUUID()
      GatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> endpointId.toString,
        "properties" -> Json.obj(
          "implementation_type" -> "container",
          "implementation_id" -> testContainer.id.toString,
          "container_port_name" -> "web",
          "resource" -> "/some-path"
        )
      ), apiId) must beSuccessfulTry(
        (e: LaserEndpoint) => (e.upstreamUrl must_== s"http://my-nginx.service-address:80") and
          (e.id must beSome(endpointId.toString)) and
          (e.apiId must_== apiId.toString)
      )
    }

    "BadRequest for container-bound endpoints with un-exposed port mapping" in new TestApplication {
      GatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> UUID.randomUUID().toString,
        "properties" -> Json.obj(
          "implementation_type" -> "container",
          "implementation_id" -> testContainer.id.toString,
          "container_port_name" -> "not-exposed",
          "resource" -> "/some-path"
        )
      ), UUID.randomUUID()) must beFailedTry(
        (e: Throwable) => (e must beAnInstanceOf[BadRequestException]) and (e.getMessage must contain("port mapping with the specified name was not exposed or did not contain service address"))
      )
    }

    "BadRequest for container-bound endpoints with missing port mapping" in new TestApplication {
      GatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> UUID.randomUUID().toString,
        "properties" -> Json.obj(
          "implementation_type" -> "container",
          "implementation_id" -> testContainer.id.toString,
          "container_port_name" -> "missing",
          "resource" -> "/some-path"
        )
      ), UUID.randomUUID()) must beFailedTry(
        (e: Throwable) => (e must beAnInstanceOf[BadRequestException]) and (e.getMessage must contain("no port mapping with the specified name"))
      )
    }

    "BadRequest for missing lambda on lambda-bound endpoints" in new TestApplication {
      GatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> UUID.randomUUID().toString,
        "properties" -> Json.obj(
          "implementation_type" -> "lambda",
          "synchronous" -> true,
          "implementation_id" -> UUID.randomUUID().toString,
          "resource" -> "/some-path"
        )
      ), UUID.randomUUID()) must beFailedTry(
        (e: Throwable) => (e must beAnInstanceOf[BadRequestException]) and (e.getMessage must contain("no lambda with id matching"))
      )
    }

    "BadRequest for missing container on container-bound endpoints" in new TestApplication {
      GatewayMethods.toGatewayEndpoint(Json.obj(
        "id" -> UUID.randomUUID().toString,
        "properties" -> Json.obj(
          "implementation_type" -> "container",
          "implementation_id" -> UUID.randomUUID().toString,
          "container_port_name" -> "web",
          "resource" -> "/some-path"
        )
      ), UUID.randomUUID()) must beFailedTry(
        (e: Throwable) => (e must beAnInstanceOf[BadRequestException]) and (e.getMessage must contain("no container with id matching"))
      )
    }
  }

}