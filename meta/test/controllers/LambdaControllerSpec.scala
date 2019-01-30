package controllers

// import java.util.UUID

// import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.genericactions.GenericProviderManager
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test._
// import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.fakes.FakeGestaltSecurityModule
import controllers.util._
import modules._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.execute.{AsResult, Result}
import org.specs2.matcher.JsonMatchers
import play.api.inject.bind
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.libs.json._
import play.api.test.PlaySpecification
import services._

// import scala.util.{Try,Success}
import scala.util.Success

import com.galacticfog.gestalt.meta.api.sdk.GestaltConfigurationManager
import com.galacticfog.gestalt.data.PostgresConfigManager



@RunWith(classOf[JUnitRunner])
class LambdaControllerSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {
  // import play.api.libs.concurrent.Execution.Implicits.defaultContext

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    pristineDatabase()
    val Success(_) = Ents.createNewMetaUser(user, dummyRootOrgId, rootOwnerLink(), user.account,
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

  def appWithMocks() = {
    val defaultDisabled = Seq(
      classOf[MetaDefaultDocker],
      classOf[MetaDefaultDCOS],
      classOf[MetaDefaultSkuber],
      classOf[ProdSecurityModule],
      classOf[MetaDefaultServices],
      classOf[HealthModule]
    )

    val sc: Seq[GuiceableModule] = Seq(
      FakeGestaltSecurityModule(fakeSecurityEnvironment()),
      new SystemConfigModule,
      bind[SecureController].toInstance(mockSecureController),
      bind[SecurityClientProvider].toInstance(mock[SecurityClientProvider]),
      bind[SecurityKeyInit].toInstance(mock[SecurityKeyInit]),
      bind[MetaHealth].toInstance(mock[MetaHealth]),
      bind[MetaServiceStatus].toInstance(mock[MetaServiceStatus]),
      bind[UpgraderService].toInstance(mock[UpgraderService]),
      bind[ContainerService].toInstance(mock[ContainerService]),
      bind[ProviderManager].toInstance(mock[ProviderManager]),
      bind[MarathonClientFactory].toInstance(mock[MarathonClientFactory]),
      bind[kubernetes.SkuberFactory].toInstance(mock[kubernetes.SkuberFactory]),
      bind[DockerClientFactory].toInstance(mock[DockerClientFactory]),
      bind[GenericProviderManager].toInstance(mock[GenericProviderManager]),
      bind[GenericResourceMethods].toInstance(mock[GenericResourceMethods]),
      bind(classOf[GestaltConfigurationManager]).toInstance(PostgresConfigManager)
    )

    new GuiceApplicationBuilder()
      .disable(defaultDisabled: _*)
      .bindings(sc: _*)
      .build
  }

  abstract class TestLambdaController extends WithDb(appWithMocks()) {
    lazy val (testWork,testEnv) = {
      val Success((tW,tE)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
      Ents.setNewResourceEntitlements(dummyRootOrgId, tE.id, user, Some(tW.id))
      (tW,tE)
    }

    lazy val Success(laserProvider) = createInstance(ResourceIds.LambdaProvider, "test-laser-provider", properties = Some(Map(
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

    lazy val Success(awslProvider) = createInstance(migrations.V20.AWS_LAMBDA_PROVIDER_TYPE_ID, "test-awsl-provider", properties = Some(Map(
      "config" ->
        """{
          |  "env": {
          |     "public": {
          |       "AWS_API_KEY": "",
          |       "AWS_SECRET_KEY": "",
          |       "AWS_REGION": "us-east-2",
          |       "SERVICE_HOST": "gateway.service",
          |       "SERVICE_PORT": "6473",
          |       "GESTALT_SECURITY_KEY": "",
          |       "GESTALT_SECURITY_SECRET": ""
          |     }
          |  }
          |}""".stripMargin
    )))
    // lazy val (envPolicy2, envRule2) = createEventRule(testEnv.id, awslProvider.id, "lambda.migrate")

    override def around[T: AsResult](t: => T): Result = super.around {
      scalikejdbc.config.DBs.setupAll()
      t
    }
  }

  "LambdaController" should {
    
    "migrateLambda laser -> aws lambda" in new TestLambdaController {

      val Success(createdResource) = createInstance(ResourceIds.Lambda, "test-lambdas",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "provider" -> Json.obj(
            "id" -> s"${laserProvider.id}",
            "locations" -> Json.arr()
          ).toString,
          "public" -> "false",
          "cpus" -> "0",
          "code_type" -> "n/a",
          "timeout" -> "0",
          "handler" -> "n/a",
          "runtime" -> "n/a",
          "memory" -> "0"
        ))
      )
      Ents.setNewResourceEntitlements(dummyRootOrgId, createdResource.id, user, Some(testEnv.id))

      val (envPolicy, envRule) = createEventRule(testEnv.id, createdResource.id, "lambda.migrate.pre")

      val request = fakeAuthRequest(POST, s"/root/lambdas/${createdResource.id}/migrate?provider=${awslProvider.id}", testCreds)
      val Some(result) = route(app, request)

      (contentAsJson(result) \ "properties").as[JsValue] must beEqualTo(Json.obj(
        "provider" -> Json.obj("id" -> s"${laserProvider.id}","locations" -> Json.arr()),
        "public" -> false,
        "cpus" -> 0,
        "code_type" -> "n/a",
        "timeout" -> 0,
        "handler" -> "n/a",
        "runtime" -> "n/a",
        "memory" -> 0
      ))
    }

  }

}