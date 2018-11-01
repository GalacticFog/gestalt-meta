package controllers.util

import java.util.UUID


import org.specs2.mock.Mockito
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.api.{DIRECTORY_TYPE_INTERNAL, GestaltAPICredentials, GestaltAccount, GestaltAuthResponse, GestaltDirectory, GestaltOrg, GestaltOrgSync, GestaltSecurityClient, GestaltSecurityConfig, ResourceLink => SecurityLink}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltAuthResponseWithCreds
import com.galacticfog.gestalt.security.play.silhouette.fakes.FakeGestaltFrameworkSecurityEnvironment
import play.api.test.FakeRequest
import play.api.test.PlaySpecification

trait GestaltSecurityMocking extends PlaySpecification with Mockito with ResourceScope {

  lazy val testAuthResponse = GestaltSecurityMocking.dummyAuthResponseWithCreds(Seq.empty, dummyRootOrgId)
  lazy val testCreds: GestaltAPICredentials = testAuthResponse.creds
  lazy val user = AuthAccountWithCreds(testAuthResponse.account, Seq.empty, Seq.empty, testCreds, dummyRootOrgId)


  lazy val testAdminAuthResponse = GestaltSecurityMocking.dummyAuthResponseWithCreds(Seq.empty, dummyRootOrgId, userInfo = Map(
    "id" -> adminUserId.toString,
    "username" -> "root",
    "firstName" -> "Rooty",
    "lastName" -> "McRootFace",
    "email" -> "root@root.com",
    "phoneNumber" -> "+15555555555"
  ))
  lazy val testAdminCreds: GestaltAPICredentials = testAdminAuthResponse.creds
  lazy val adminUser = AuthAccountWithCreds(testAdminAuthResponse.account, Seq.empty, Seq.empty, testAdminCreds, dummyRootOrgId)

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

  def fakeSecurityEnvironment( auth: Seq[GestaltAuthResponseWithCreds] = Seq(testAuthResponse, testAdminAuthResponse),
                               config: GestaltSecurityConfig = mock[GestaltSecurityConfig],
                               client: GestaltSecurityClient = mock[GestaltSecurityClient]) = {
    FakeGestaltFrameworkSecurityEnvironment(
      identities = auth.map(a => a.creds -> a),
      securityConfig = config,
      securityClient = client
    )
  }

  def fakeAuthRequest(method: String, path: String, creds: GestaltAPICredentials) =
    FakeRequest(method, path).withHeaders(AUTHORIZATION -> creds.headerValue)

}

object GestaltSecurityMocking {

  private[this] def uuid() = UUID.randomUUID()

  def dummyAuthResponse(userInfo: Map[String,String] = Map(), groups: Seq[SecurityLink] = Seq(), orgId: UUID = uuid()): GestaltAuthResponse = {
    val defaultStr = "foo"
    val directory = GestaltDirectory(uuid(), defaultStr, None, uuid(), directoryType = DIRECTORY_TYPE_INTERNAL.label)
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
                                  creds:  GestaltAPICredentials = dummyCreds(),
                                  userInfo: Map[String,String] = Map()
                                ): GestaltAuthResponseWithCreds = {
    val directory = GestaltDirectory(uuid(), "test-directory", None, orgId, directoryType = DIRECTORY_TYPE_INTERNAL.label)
    val account = GestaltAccount(
      userInfo.get("id") map (UUID.fromString(_)) getOrElse uuid(),
      userInfo.getOrElse("username", "someUser"),
      userInfo.getOrElse("firstName", "John"),
      userInfo.getOrElse("lastName", "Doe"),
      userInfo.get("description") orElse Option("a crash-test dummy"),
      userInfo.get("email") orElse Option("john@doe.com"),
      userInfo.get("phoneNumber") orElse Option("+15058675309"),
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
