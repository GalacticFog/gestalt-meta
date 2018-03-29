package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.meta.providers.ProviderManager

import scala.reflect.ClassTag
import scala.reflect.runtime.universe
import org.specs2.mock.Mockito
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.api.{GestaltAPICredentials, GestaltAccount, GestaltAuthResponse, GestaltDirectory, GestaltOrg, GestaltOrgSync, GestaltSecurityClient, GestaltSecurityConfig, ResourceLink => SecurityLink}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltAuthResponseWithCreds
import com.galacticfog.gestalt.security.play.silhouette.fakes.FakeGestaltFrameworkSecurityEnvironment
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.test.FakeRequest
import play.api.test.PlaySpecification

trait GestaltSecurityMocking extends PlaySpecification with Mockito with ResourceScope {

  lazy val testAuthResponse = GestaltSecurityMocking.dummyAuthResponseWithCreds(Seq.empty, dummyRootOrgId)
  lazy val testCreds: GestaltAPICredentials = testAuthResponse.creds
  lazy val user = AuthAccountWithCreds(testAuthResponse.account, Seq.empty, Seq.empty, testCreds, dummyRootOrgId)


  def dummyAuthResponse(userInfo: Map[String,String] = Map(),
                        groups: Seq[SecurityLink] = Seq(),
                        orgId: UUID = uuid()): GestaltAuthResponse =
    GestaltSecurityMocking.dummyAuthResponse(userInfo, groups, orgId)

  def dummyCreds(authHeader: Option[String] = None): GestaltAPICredentials =
    GestaltSecurityMocking.dummyCreds(authHeader)

  def dummyAuthResponseWithCreds( groups: Seq[SecurityLink] = Seq(),
                                  orgId: UUID = uuid(),
                                  creds:  GestaltAPICredentials = dummyCreds()): GestaltAuthResponseWithCreds =
    GestaltSecurityMocking.dummyAuthResponseWithCreds(groups, orgId, creds)

  val account2auth = dummyAuthResponseWithCreds()
  val account2creds = account2auth.creds
  val account2 = AuthAccountWithCreds(account2auth.account, Seq.empty, Seq.empty, account2creds, dummyRootOrgId)

  val dummyOrgSync = GestaltOrgSync(
    orgs = Seq(GestaltOrg(dummyRootOrgId, "root", "root", None, None, Seq.empty)),
    accounts = Seq(testAuthResponse.account),
    groups = Seq.empty,
    admin = Some(testAuthResponse.account.getLink())
  )

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

}

object GestaltSecurityMocking {

  private[this] def uuid() = UUID.randomUUID()

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
      orgId = orgId,
      extraData = None
    )
  }

  def dummyCreds(authHeader: Option[String] = None): GestaltAPICredentials = {
    val header = authHeader getOrElse s"Bearer ${uuid()}"
    GestaltAPICredentials.getCredentials(header).get
  }

  def dummyAuthResponseWithCreds( groups: Seq[SecurityLink] = Seq(),
                                  orgId: UUID = uuid(),
                                  creds:  GestaltAPICredentials = dummyCreds()): GestaltAuthResponseWithCreds = {
    val directory = GestaltDirectory(uuid(), "test-directory", None, orgId)
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
      creds = creds,
      extraData = None
    )
  }

}
