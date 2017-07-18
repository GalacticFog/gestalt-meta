package controllers.util

import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.play.silhouette.fakes.FakeGestaltSecurityModule
import modules._
import org.specs2.mock.Mockito
import play.api.inject._
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.inject.guice.GuiceableModule.{fromGuiceModule, fromPlayBinding}
import play.api.test.PlaySpecification
import services.{DockerClientFactory, MarathonClientFactory, SkuberFactory}

trait GestaltProviderMocking extends PlaySpecification with GestaltSecurityMocking with Mockito with ResourceScope {

  lazy val mockContainerService = mock[ContainerService]
  lazy val mockProviderManager  = mock[ProviderManager]
  lazy val mockSecureController = mock[SecureController]
  lazy val mockLambdaMethods    = mock[LambdaMethods]
  lazy val mockGatewayMethods   = mock[GatewayMethods]

  /**
   * Get a Play Application configured with Guice. All Meta and Security modules are
   * disabled by default and the GestaltSecurityEnvironment is bound to a fake.
   */
  def application( disabled: Seq[Class[_]] = Seq.empty,
                   additionalBindings: Seq[GuiceableModule] = Seq.empty ): play.api.Application = {

    val defaultDisabled = Seq(
      classOf[MetaDefaultDCOS],
      classOf[MetaDefaultSkuber],
      classOf[ProdSecurityModule],
      classOf[MetaDefaultServices],
      classOf[MetaDefaultDocker],
      classOf[HealthModule]
    )

    val sc: Seq[GuiceableModule] = Seq(
      FakeGestaltSecurityModule(fakeSecurityEnvironment()),
      bind(classOf[SecureController]).toInstance(mockSecureController),
      bind(classOf[SecurityClientProvider]).toInstance(mock[SecurityClientProvider]),
      bind(classOf[SecurityKeyInit]).toInstance(mock[SecurityKeyInit]),
      bind(classOf[MetaHealth]).toInstance(mock[MetaHealth]),
      bind(classOf[MetaServiceStatus]).toInstance(mock[MetaServiceStatus])
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
      bind(classOf[ContainerService]).toInstance(mockContainerService),
      bind(classOf[ProviderManager]).toInstance(mockProviderManager),
      bind(classOf[SkuberFactory]).toInstance(mock[SkuberFactory]),
      bind(classOf[DockerClientFactory]).toInstance(mock[DockerClientFactory]),
      bind(classOf[MarathonClientFactory]).toInstance(mock[MarathonClientFactory])
    )
    application(additionalBindings = (bindings ++ additionalBindings)) 
  }
}
