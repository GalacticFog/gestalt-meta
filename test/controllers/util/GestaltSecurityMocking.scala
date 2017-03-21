package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.meta.providers.ProviderManager

import scala.reflect.ClassTag
import scala.reflect.runtime.universe
import org.specs2.mock.Mockito
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.api.GestaltAPICredentials
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltAuthResponse
import com.galacticfog.gestalt.security.api.GestaltDirectory
import com.galacticfog.gestalt.security.api.GestaltSecurityClient
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import com.galacticfog.gestalt.security.api.{ResourceLink => SecurityLink}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltAuthResponseWithCreds
import com.galacticfog.gestalt.security.play.silhouette.fakes.FakeGestaltFrameworkSecurityEnvironment
import com.galacticfog.gestalt.security.play.silhouette.fakes.FakeGestaltSecurityModule
import com.galacticfog.gestalt.security.play.silhouette.modules.GestaltDelegatedSecurityConfigModule
import com.galacticfog.gestalt.security.play.silhouette.modules.GestaltFrameworkSecurityConfigModule
import com.galacticfog.gestalt.security.play.silhouette.modules.GestaltSecurityModule
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import modules.{MetaDefaultServices, ProdSecurityModule}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.inject.guice.GuiceableModule
import play.api.inject.guice.GuiceableModule.fromGuiceModule
import play.api.inject.guice.GuiceableModule.fromPlayBinding
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.test.FakeRequest
import play.api.test.PlaySpecification


trait GestaltSecurityMocking extends PlaySpecification with Mockito with ResourceScope {

  lazy val testAuthResponse = dummyAuthResponseWithCreds()
  lazy val testCreds: GestaltAPICredentials = testAuthResponse.creds
  lazy val containerService = mock[ContainerService]//.verbose
  lazy val providerManager = mock[ProviderManager]
  lazy val secureController = mock[SecureController]

  lazy val user = AuthAccountWithCreds(testAuthResponse.account, Seq.empty, Seq.empty, testCreds, dummyRootOrgId)  

  val account2auth = dummyAuthResponseWithCreds()
  val account2creds = account2auth.creds
  val account2 = AuthAccountWithCreds(account2auth.account, Seq.empty, Seq.empty, account2creds, dummyRootOrgId)

  def dummyAuthResponse(userInfo: Map[String,String] = Map(), groups: Seq[SecurityLink] = Seq(), orgId: UUID = uuid()): GestaltAuthResponse = {
    val defaultStr = "foo"
    val directory = GestaltDirectory(uuid(), defaultStr, None, uuid())
    val account = GestaltAccount(
      userInfo.get("id") map (UUID.fromString(_)) getOrElse uuid(),
      userInfo.getOrElse("username", defaultStr),
      userInfo.getOrElse("firstName", defaultStr),
      userInfo.getOrElse("lastName", defaultStr),
      userInfo.get("description") orElse Option(defaultStr),
      userInfo.get("email") orElse Option(defaultStr),
      userInfo.get("phoneNumber") orElse Option(defaultStr),
      directory
    )
    GestaltAuthResponse(
      account = account,
      groups = groups,
      rights = Seq(),
      orgId = orgId
    )
  }

  def dummyCreds(authHeader: Option[String] = None): GestaltAPICredentials = {
    val header = authHeader getOrElse s"Bearer ${uuid()}"
    GestaltAPICredentials.getCredentials(header).get
  }

  def dummyAuthResponseWithCreds(
      groups: Seq[SecurityLink] = Seq(), 
      orgId: UUID = uuid(),
      creds:  GestaltAPICredentials = dummyCreds()): GestaltAuthResponseWithCreds = {
    val directory = GestaltDirectory(uuid(), "test-directory", None, uuid())
    val account = GestaltAccount(
      id = uuid(),
      username = "someUser",
      firstName = "John",
      lastName = "Doe",
      description = Option("a crash-test dummy"),
      email = Option("john@doe.com"),
      phoneNumber = Option("+15058675309"),
      directory = directory)
    new GestaltAuthResponseWithCreds(
      account = account,
      groups = groups,
      rights = Seq(),
      orgId = orgId,
      creds = creds)
  }

  def fakeSecurityEnvironment(
      auth: GestaltAuthResponseWithCreds = testAuthResponse,
      config: GestaltSecurityConfig = mock[GestaltSecurityConfig],
      client: GestaltSecurityClient = mock[GestaltSecurityClient]) = {
    
    FakeGestaltFrameworkSecurityEnvironment[DummyAuthenticator](
      identities = Seq(auth.creds -> auth),
      config = config,
      client = client)
  }
  
  def fakeAuthRequest(method: String, path: String, creds: GestaltAPICredentials) =
    FakeRequest(method, path).withHeaders(AUTHORIZATION -> creds.headerValue)  

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
      
    val sc: GuiceableModule = bind(classOf[SecureController]).toInstance(secureController)
    
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
      bind(classOf[ContainerService]).toInstance(containerService),
      bind(classOf[ProviderManager]).toInstance(providerManager)
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
