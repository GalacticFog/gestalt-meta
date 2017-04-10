package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.play.silhouette.fakes.FakeGestaltSecurityModule
import controllers.util.db.ConnectionManager
import modules._
import org.specs2.mock.Mockito
import play.api.inject._
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.inject.guice.GuiceableModule.{fromGuiceModule, fromPlayBinding}
import play.api.test.PlaySpecification
import services.{MarathonClientFactory, SkuberFactory}

trait GestaltProviderMocking extends PlaySpecification with GestaltSecurityMocking with Mockito with ResourceScope {

  lazy val mockContainerService = mock[ContainerService]//.verbose
  lazy val mockProviderManager  = mock[ProviderManager]
  lazy val mockSecureController = mock[SecureController]

  /**
   * Get a Play Application configured with Guice. All Meta and Security modules are
   * disabled by default and the GestaltSecurityEnvironment is bound to a fake.
   */
  def application(
      disabled: Seq[Class[_]] = Seq.empty,
      additionalBindings: Seq[GuiceableModule] = Seq.empty): play.api.Application = {

    val defaultDisabled = Seq(
      classOf[MetaDefaultDCOS],
      classOf[MetaDefaultSkuber],
      classOf[ProdSecurityModule],
      classOf[MetaDefaultServices],
      classOf[HealthModule]
    )

    val sc: Seq[GuiceableModule] = Seq(
      FakeGestaltSecurityModule(fakeSecurityEnvironment()),
      bind(classOf[SecureController]).toInstance(mockSecureController),
      bind(classOf[SecurityClientProvider]).toInstance(mock[SecurityClientProvider]),
      bind(classOf[SecurityKeyInit]).toInstance(mock[SecurityKeyInit]),
      bind(classOf[MetaHealth]).toInstance(mock[MetaHealth]),
      bind(classOf[MetaServiceStatus]).toInstance(mock[MetaServiceStatus]),
      bind(classOf[ConnectionManager]).toInstance(connectionManager)
    )

    new GuiceApplicationBuilder()
      .disable((disabled ++ defaultDisabled): _*)
      .bindings((sc ++ additionalBindings): _*)
      .build
  }

  /**
   * Return a Play Application with ContainerService bound to a mock[ContainerService]
   */
  def containerApp(): play.api.Application = {
    application(additionalBindings = Seq(
      bind(classOf[ContainerService]).toInstance(mockContainerService),
      bind(classOf[ProviderManager]).toInstance(mockProviderManager),
      bind(classOf[SkuberFactory]).toInstance(mock[SkuberFactory]),
      bind(classOf[MarathonClientFactory]).toInstance(mock[MarathonClientFactory])
    ))
  }

  private[this] def uuid() = UUID.randomUUID()  

  /*
   * Still playing with this - DO NOT DELETE!
   */
//  type FakeFrameworkEnv = FakeGestaltFrameworkSecurityEnvironment[DummyAuthenticator]
//  type FrameworkEnvType = GestaltSecurityEnvironment[AuthAccountWithCreds, DummyAuthenticator]
//
//  def injectController[A : ClassTag](
//      auth: Option[GestaltAuthResponseWithCreds] = None,
//      env: Option[FakeFrameworkEnv]   = None,
//      overrides: Seq[GuiceableModule] = Seq.empty) = {
//
//    val authResponse = auth getOrElse dummyAuthResponseWithCreds()
//    val environment  = env getOrElse fakeSecurityEnvironment(authResponse)
//    val envBinding: GuiceableModule = bind(classOf[FrameworkEnvType]).toInstance(environment)
//
//    new GuiceApplicationBuilder().overrides(
//      (envBinding +: overrides):_*)
//      .build()
//      .injector
//      .instanceOf[A]
//  }
}
