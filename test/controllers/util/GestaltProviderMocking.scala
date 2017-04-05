package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.api.{GestaltAPICredentials, GestaltAccount, GestaltAuthResponse, GestaltDirectory, GestaltSecurityClient, GestaltSecurityConfig, ResourceLink => SecurityLink}
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltAuthResponseWithCreds}
import com.galacticfog.gestalt.security.play.silhouette.fakes.{FakeGestaltFrameworkSecurityEnvironment, FakeGestaltSecurityModule}
import com.galacticfog.gestalt.security.play.silhouette.modules.{GestaltDelegatedSecurityConfigModule, GestaltFrameworkSecurityConfigModule, GestaltSecurityModule}
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import modules.{MetaDefaultServices, ProdSecurityModule}
import org.specs2.mock.Mockito
import play.api.inject.bind
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.inject.guice.GuiceableModule.{fromGuiceModule, fromPlayBinding}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.test.{FakeRequest, PlaySpecification}

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
      classOf[GestaltFrameworkSecurityConfigModule],
      classOf[GestaltDelegatedSecurityConfigModule],
      classOf[GestaltSecurityModule],
      classOf[ProdSecurityModule],
      classOf[MetaDefaultServices])

    val sc: GuiceableModule = bind(classOf[SecureController]).toInstance(mockSecureController)

    new GuiceApplicationBuilder()
      .disable((disabled ++ defaultDisabled): _*)
      .bindings(FakeGestaltSecurityModule(fakeSecurityEnvironment()))
      .bindings((sc +: additionalBindings): _*)
      .build
  }

  /**
   * Return a Play Application with ContainerService bound to a mock[ContainerService]
   */
  def containerApp(
      disabled: Seq[Class[_]] = Seq.empty,
      additionalBindings: Seq[GuiceableModule] = Seq.empty): play.api.Application = {
    application(additionalBindings = Seq(
      bind(classOf[ContainerService]).toInstance(mockContainerService),
      bind(classOf[ProviderManager]).toInstance(mockProviderManager)
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
