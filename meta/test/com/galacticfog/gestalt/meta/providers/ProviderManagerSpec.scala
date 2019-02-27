package com.galacticfog.gestalt.meta.providers

import com.galacticfog.gestalt.meta.api.sdk.GestaltConfigurationManager
import com.galacticfog.gestalt.data.PostgresConfigManager
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.fakes.FakeGestaltSecurityModule
import com.galacticfog.gestalt.meta.test.{ MetaRepositoryOps, WithDb }
import com.galacticfog.gestalt.meta.genericactions.GenericProviderManager
import controllers.util.GestaltSecurityMocking
import modules._
import services._
import controllers.util._
import org.junit.runner.RunWith
import play.api.inject.bind
import play.api.inject.guice.{ GuiceApplicationBuilder, GuiceableModule }
import org.specs2.matcher.JsonMatchers
import org.specs2.runner.JUnitRunner
import play.api.libs.json.Json.JsValueWrapper
import play.api.test.PlaySpecification
import scala.util.Success
import services.kubernetes.SkuberFactory

@RunWith(classOf[JUnitRunner])
class ProviderManagerSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {
  override def beforeAll(): Unit = pristineDatabase()

  //   // override def before: Unit = scalikejdbc.config.DBs.setupAll()

  //   // override def after: Unit = scalikejdbc.config.DBs.closeAll()

  sequential

  def appWithMocks() = {
    val defaultDisabled = Seq(
      classOf[MetaDefaultDocker],
      classOf[MetaDefaultDCOS],
      classOf[MetaDefaultSkuber],
      classOf[ProdSecurityModule],
      classOf[MetaDefaultServices],
      classOf[HealthModule])

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
      bind[SkuberFactory].toInstance(mock[SkuberFactory]),
      bind[DockerClientFactory].toInstance(mock[DockerClientFactory]),
      bind[GenericProviderManager].toInstance(mock[GenericProviderManager]),
      bind[GenericResourceMethods].toInstance(mock[GenericResourceMethods]),
      bind(classOf[GestaltConfigurationManager]).toInstance(PostgresConfigManager))

    new GuiceApplicationBuilder()
      .disable(defaultDisabled: _*)
      .bindings(sc: _*)
      .build
  }

  abstract class MockScope(providerConfig: Seq[(String, JsValueWrapper)] = Seq.empty) extends WithDb(appWithMocks()) {
    lazy val testAuthResponse = GestaltSecurityMocking.dummyAuthResponseWithCreds()
    lazy val testCreds = testAuthResponse.creds
    lazy val user = AuthAccountWithCreds(testAuthResponse.account, Seq.empty, Seq.empty, testCreds, dummyRootOrgId)

    lazy val providerManager = app.injector.instanceOf[ProviderManager]

    lazy val Success(testProvider) = createDockerProvider(testEnv.id)

    lazy val (testWork, testEnv) = {
      val (tw, te) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment").get
      Entitlements.setNewResourceEntitlements(dummyRootOrgId, te.id, user, Some(tw.id))
      (tw, te)
    }
  }

  "ProviderManager" should {

    "get provider implementation" in new MockScope {
      val Some(provider) = ResourceFactory.findById(testProvider.id)
      providerManager.getProviderImpl(provider.typeId) must beEqualTo(null)
    }
  }

}
 