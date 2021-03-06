package controllers.util

import com.galacticfog.gestalt.meta.genericactions.GenericProviderManager
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.play.silhouette.fakes.FakeGestaltSecurityModule
import modules._
import org.specs2.mock.Mockito
import play.api.inject._
import play.api.inject.guice.GuiceableModule.{fromGuiceModule, fromPlayBinding}
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.test.PlaySpecification
import services.{DockerClientFactory, MarathonClientFactory}
import services.kubernetes.SkuberFactory

import com.galacticfog.gestalt.meta.api.sdk.GestaltConfigurationManager
import com.galacticfog.gestalt.data.PostgresConfigManager



trait GestaltProviderMocking extends PlaySpecification with GestaltSecurityMocking with Mockito with ResourceScope {

  lazy val mockContainerService = mock[ContainerService]
  lazy val mockProviderManager  = mock[ProviderManager]
  lazy val mockSecureController = mock[SecureController]
  lazy val mockLambdaMethods    = mock[LambdaMethods]
  lazy val mockGatewayMethods   = mock[GatewayMethods]
  lazy val mockSecurity         = mock[Security]

  /**
   * Get a Play Application configured with Guice. All Meta and Security modules are
   * disabled by default and the GestaltSecurityEnvironment is bound to a fake.
   */
  def application( disabled: Seq[Class[_]] = Seq.empty,
                   additionalBindings: Seq[GuiceableModule] = Seq.empty ): play.api.Application = {

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
      bind[GenericResourceMethods].to[GenericResourceMethodsImpl]
    )

    new GuiceApplicationBuilder()
      .disable((disabled ++ defaultDisabled): _*)
      .bindings((sc ++ additionalBindings): _*)
      .build
  }

  /**
   * Return a Play Application with ContainerService bound to a mock[ContainerService]
   */
  def containerApp(additionalBindings: Seq[GuiceableModule] = Seq.empty): play.api.Application = {
    val bindings: Seq[GuiceableModule] = Seq(
      bind[ContainerService].toInstance(mockContainerService),
      bind[ProviderManager].toInstance(mockProviderManager),
      bind[SkuberFactory].toInstance(mock[SkuberFactory]),
      bind[UpgraderService].toInstance(mock[UpgraderService]),
      bind[DockerClientFactory].toInstance(mock[DockerClientFactory]),
      bind[MarathonClientFactory].toInstance(mock[MarathonClientFactory]),
      bind[GenericProviderManager].toInstance(mock[GenericProviderManager]),
      bind(classOf[GestaltConfigurationManager]).toInstance(PostgresConfigManager)
    )
    application(additionalBindings = (bindings ++ additionalBindings)) 
  }
}
