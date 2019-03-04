package com.galacticfog.gestalt.meta.providers.gwm

// import java.util.UUID
// import scala.util.{Try,Success}
import scala.util.Success
// import scala.concurrent.Await
// import scala.concurrent.duration._
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.inject.bind
import play.api.test.PlaySpecification
// import play.api.libs.json._
// import play.api.mvc.Results._
// import play.api.mvc._
import org.specs2.specification.{BeforeAll, Scope}
import org.specs2.matcher.JsonMatchers
// import mockws.MockWS
// import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
// import com.galacticfog.gestalt.meta.api.ContainerSpec
// import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
// import com.galacticfog.gestalt.meta.api.patch.PatchInstance
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
// import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import com.galacticfog.gestalt.meta.test._
import controllers.util.{GestaltSecurityMocking,ProviderMethods}
import controllers.SecurityResources

class IstioGatewayManagerProviderSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll
 with DbShutdown with JsonMatchers {

  object Ents extends AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    pristineDatabase()
    val Success(createdUser) = Ents.createNewMetaUser(user, dummyRootOrgId, rootOwnerLink(), user.account,
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

  trait TestApplication extends Scope {
    val mockGestaltSecurityConfig = mock[GestaltSecurityConfig]
    val appBuilder = new GuiceApplicationBuilder()
      .disable[modules.ProdSecurityModule]
      .disable[modules.MetaDefaultSkuber]
      .disable[modules.MetaDefaultServices]
      .disable[modules.HealthModule]
      .bindings(
        bind(classOf[GestaltSecurityConfig]).toInstance(mockGestaltSecurityConfig)//,
        // bind(classOf[ProviderMethods]).toInstance(mockProviderMethods)
      )
    val injector = appBuilder.injector()
    setInjector(injector)

    val providerMethods = injector.instanceOf[ProviderMethods]

    var Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    Entitlements.setNewResourceEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))
    
    // val gatewayMethods = application.injector.instanceOf[GatewayMethods]
    // val mockProviderMethods = application.injector.instanceOf[ProviderMethods]

    // val patchController = application.injector.instanceOf[PatchController]
    // val apiController = application.injector.instanceOf[ApiController]
    
    // val Success(testLambdaProvider) = createInstance(ResourceIds.LambdaProvider, "test-lambda-provider", properties = Some(Map(
    //   "config" ->
    //     """{
    //       |  "env": {
    //       |     "public": {
    //       |       "SERVICE_HOST": "laser.service",
    //       |       "SERVICE_PORT": "1111"
    //       |     }
    //       |  }
    //       |}""".stripMargin
    // )))
    // val Success(testGatewayProvider) = createInstance(ResourceIds.GatewayManager, "test-gateway-provider", properties = Some(Map(
    //   "config" ->
    //     """{
    //       |  "env": {
    //       |     "public": {
    //       |       "SERVICE_HOST": "gateway.service",
    //       |       "SERVICE_PORT": "6473",
    //       |       "GESTALT_SECURITY_KEY": "",
    //       |       "GESTALT_SECURITY_SECRET": ""
    //       |     }
    //       |  }
    //       |}""".stripMargin
    // )))
    // val Success(testKongProvider) = createInstance(ResourceIds.KongGateway, "test-kong-provider", properties = Some(Map(
    //   "config" -> Json.obj(
    //     "env" -> Json.obj("public" -> Json.obj("PUBLIC_URL_VHOST_0" -> "public url host")),
    //     "external_protocol" -> "http"
    //   ).toString
    // )))
    // val Success(testLambda) = createInstance(ResourceIds.Lambda, "test-lambda", properties = Some(Map(
    //   "public" -> "true",
    //   "cpus" -> "0.1",
    //   "memory" -> "128",
    //   "code_type" -> "inline",
    //   "timeout" -> "30",
    //   "handler" -> "blah;blah",
    //   "runtime" -> "custom",
    //   "provider" -> Json.obj(
    //     "name" -> testLambdaProvider.name,
    //     "id" -> testLambdaProvider.id.toString
    //   ).toString
    // )))
    // val Success(testLambda2) = createInstance(ResourceIds.Lambda, "test-lambda", properties = Some(Map(
    //   "public" -> "true",
    //   "cpus" -> "0.1",
    //   "memory" -> "128",
    //   "code_type" -> "inline",
    //   "timeout" -> "30",
    //   "handler" -> "blah;blah",
    //   "runtime" -> "custom",
    //   "provider" -> Json.obj(
    //     "name" -> testLambdaProvider.name,
    //     "id" -> testLambdaProvider.id.toString
    //   ).toString
    // )))
    // val Success(testContainer) = createInstance(ResourceIds.Container, "test-container", properties = Some(Map(
    //   "cpus" -> "0.1",
    //   "memory" -> "128",
    //   "image" -> "nginx",
    //   "container_type" -> "DOCKER",
    //   "port_mappings" -> Json.toJson(Seq(
    //     ContainerSpec.PortMapping("tcp").copy(
    //       name = Some("web"),
    //       expose_endpoint = Some(true),
    //       service_address = Some(ContainerSpec.ServiceAddress(
    //         host = "my-nginx.service-address",
    //         port = 80,
    //         protocol = Some("tcp")
    //       ))
    //     ),
    //     ContainerSpec.PortMapping("tcp").copy(
    //       name = Some("not-exposed")
    //     )
    //   )).toString,
    //   "provider" -> Json.obj(
    //     "id" -> UUID.randomUUID().toString,
    //     "name" -> "nonexistent-provider-does-not-matter"
    //   ).toString
    // )))

    // val Success(testApi) = createInstance(ResourceIds.Api, "test-api", properties = Some(Map(
    //   "provider" -> Json.obj(
    //     "id" -> testGatewayProvider.id.toString,
    //     "locations" -> Json.arr(testKongProvider.id.toString)
    //   ).toString
    // )))
    // Ents.setNewResourceEntitlements(dummyRootOrgId, testApi.id, user, Some(testEnv.id))

    // val Success(testEndpoint) = createInstance(ResourceIds.ApiEndpoint, "test-endpoint", properties = Some(Map(
    //   "resource" -> "/original/path",
    //   "upstream_url" -> "http://original-upstream-url-is-irrelevant:1234/blah/blah/blah",
    //   "methods" -> Json.toJson(Seq("GET")).toString,
    //   "implementation_type" -> "lambda",
    //   "implementation_id" -> testLambda.id.toString,
    //   "location_id" -> testKongProvider.id.toString,
    //   "parent" -> testApi.id.toString,
    //   "provider" -> Json.obj(
    //     "id" -> testGatewayProvider.id.toString,
    //     "locations" -> Json.arr(testKongProvider.id.toString)
    //   ).toString
    // )), parent=Some(testApi.id))
    // Ents.setNewResourceEntitlements(dummyRootOrgId, testEndpoint.id, user, Some(testApi.id))

    // def newEndpoint(props: Map[String,String]) = {
    //   val Success(testEndpoint) = createInstance(ResourceIds.ApiEndpoint, "test-endpoint", properties = Some(Map(
    //     "parent" -> testApi.id.toString,
    //     "provider" -> Json.obj(
    //       "id" -> testGatewayProvider.id.toString,
    //       "locations" -> Json.arr(testKongProvider.id.toString)
    //     ).toString
    //   ) ++ props), parent=Some(testApi.id))
    //   Ents.setNewResourceEntitlements(dummyRootOrgId, testEndpoint.id, user, Some(testApi.id))
    //   testEndpoint
    // }
  }

  "IstioGatewayManagerProvider" should {
    // import com.galacticfog.gestalt.util.EitherWithErrors._
    // import cats.instances.either._
    // import play.api.libs.concurrent.Execution.Implicits.defaultContext
    "test" in {
      // val istioProvider = new IstioGatewayManagerProvider[scala.concurrent.Future]()
      val istioProvider = new IstioGatewayManagerProvider()
      
      1 must_== 1
    }
  }
}